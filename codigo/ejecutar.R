# Ejecutar desde la carpeta proyecto_gasolina: source("codigo/ejecutar.R").
# R realiza la simulación completa; MiniZinc solo selecciona una fila evaluada.
necesarios <- c("readxl", "jsonlite")
faltan <- necesarios[!vapply(necesarios, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltan)) stop("Instale primero: ", paste(faltan, collapse = ", "))
source("codigo/modelo.R", encoding = "UTF-8")
dir.create("resultados", showWarnings = FALSE)
dir.create("minizinc", showWarnings = FALSE)

# Parámetros del caso hipotético y del experimento acordado.
capacidad <- 40000
meta <- .99
calentamiento <- 28
dias_medidos <- 91
n_inicial <- 100
n_validacion <- 500
semilla_busqueda <- 20261120
semilla_validacion <- 20261121
url_fuente <- paste0("https://assets.publishing.service.gov.uk/media/",
  "6ac517e0d9c21d62c02267f6/20261008_Average_road_fuel_sales_deliveries_and_stock_levels.xlsx")
archivo <- "datos/original_desnz.xlsx"
if (!file.exists(archivo)) download.file(url_fuente, archivo, mode = "wb")

# Rango de la tabla de ventas de esta edición fija del archivo oficial.
original <- readxl::read_excel(archivo, sheet = "Main table - sales",
                             range = "A8:Q7316")
gasolina <- original[original$`Fuel Type` == "Petrol", ]
datos <- data.frame(fecha = as.Date(gasolina$Date),
                    ventas_litros = gasolina$`United Kingdom`)
datos <- datos[format(datos$fecha, "%Y") %in% c("2024", "2025"), ]
datos <- datos[order(datos$fecha), ]
datos$dia_semana <- as.integer(format(datos$fecha, "%u"))
stopifnot(!anyDuplicated(datos$fecha), all(is.finite(datos$ventas_litros)),
          all(datos$ventas_litros >= 0),
          identical(datos$fecha, seq(as.Date("2024-01-01"), as.Date("2025-12-31"), by = "day")))
write.csv(datos, "datos/gasolina_diaria.csv", row.names = FALSE, fileEncoding = "UTF-8")
ajuste <- datos[format(datos$fecha, "%Y") == "2024", ]
prueba <- datos[format(datos$fecha, "%Y") == "2025", ]
mu <- mean(ajuste$ventas_litros)
base <- data.frame(id = 0L, s = round(2 * mu / 100) * 100,
                   S = round(4 * mu / 100) * 100)

# Seis umbrales por seis niveles, fijados sin usar el período de prueba.
politicas <- expand.grid(s = round(mu * c(.5, .75, 1, 1.25, 1.5, 1.75) / 100) * 100,
                        S = round(mu * c(2, 2.5, 3, 3.5, 4, 4.5) / 100) * 100)
politicas$id <- seq_len(nrow(politicas))
politicas <- politicas[c("id", "s", "S")]
stopifnot(nrow(politicas) == 36, nrow(unique(politicas[c("s", "S")])) == 36,
          all(politicas$s < politicas$S), all(politicas$S <= capacidad))
fechas_sim <- seq(as.Date("2025-01-06"), by = "day",
                  length.out = calentamiento + dias_medidos)
remuestrear <- function(n, semilla) {
  set.seed(semilla)
  dias <- as.integer(format(fechas_sim, "%u"))
  vapply(dias, function(dia) {
    muestra <- ajuste$ventas_litros[ajuste$dia_semana == dia]
    stopifnot(length(muestra) >= 40)
    sample(muestra, size = n, replace = TRUE)
  }, numeric(n))
}
demanda_100 <- remuestrear(n_inicial, semilla_busqueda)
demanda_500 <- remuestrear(n_validacion, semilla_validacion)
diagnosticos <- list()
evaluar <- function(politica, demanda, escenario, plazo = 1,
                    fechas = fechas_sim, guardar_traza = FALSE) {
  x <- simular(demanda, politica$s, politica$S, capacidad,
               plazo, calentamiento, guardar_traza)
  m <- x$metricas
  fraccion_perdida <- sum(m$perdida_litros) / sum(m$demanda_litros)
  servicio <- 1 - fraccion_perdida
  # Variación del servicio entre trayectorias (aproximación delta de la razón).
  se_servicio <- if (nrow(m) > 1) {
    sd(m$perdida_litros - fraccion_perdida * m$demanda_litros) /
      sqrt(nrow(m)) / mean(m$demanda_litros)
  } else NA_real_
  se_inventario <- if (nrow(m) > 1) sd(m$inventario_promedio) / sqrt(nrow(m)) else NA_real_
  fila <- data.frame(id = politica$id, s = politica$s, S = politica$S,
    escenario = escenario, simulaciones = nrow(m), dias = ncol(demanda) - calentamiento,
    inventario_promedio = mean(m$inventario_promedio), servicio = servicio,
    servicio_ic_inferior = max(0, servicio - 1.96 * se_servicio),
    servicio_ic_superior = min(1, servicio + 1.96 * se_servicio),
    inventario_ic_inferior = mean(m$inventario_promedio) - 1.96 * se_inventario,
    inventario_ic_superior = mean(m$inventario_promedio) + 1.96 * se_inventario,
    perdida_litros = mean(m$perdida_litros), demanda_litros = mean(m$demanda_litros),
    dias_faltantes = mean(m$dias_faltantes), pedidos = mean(m$pedidos),
    entregas = mean(m$entregas), pedido_litros = mean(m$pedido_litros),
    recibido_litros = mean(m$recibido_litros))
  fila$volumen_medio_pedido <- if (sum(m$pedidos) > 0) sum(m$pedido_litros) / sum(m$pedidos) else NA_real_
  fila$volumen_medio_entrega <- if (sum(m$entregas) > 0) sum(m$recibido_litros) / sum(m$entregas) else NA_real_
  fila$cumple_meta <- servicio >= meta
  diagnosticos[[length(diagnosticos) + 1]] <<- x$diagnosticos
  if (guardar_traza) {
    x$traza$fecha <- fechas
    write.csv(x$traza, paste0("resultados/traza_", escenario, "_", politica$id, ".csv"), row.names = FALSE)
  }
  list(resumen = fila, replicas = m)
}
evaluar_tabla <- function(tabla, demanda, escenario) {
  do.call(rbind, lapply(seq_len(nrow(tabla)), function(i)
    evaluar(tabla[i, ], demanda, escenario)$resumen))
}
ordenar <- function(tabla) tabla[order(!tabla$cumple_meta,
    ifelse(tabla$cumple_meta, tabla$inventario_promedio, -tabla$servicio),
    tabla$entregas, tabla$S, tabla$s), ]

busqueda <- evaluar_tabla(politicas, demanda_100, "busqueda_100")
write.csv(busqueda, "resultados/busqueda_36.csv", row.names = FALSE)
ids_finalistas <- head(ordenar(busqueda)$id, 3)
finalistas <- evaluar_tabla(politicas[match(ids_finalistas, politicas$id), ],
                            demanda_500, "validacion_500")
finalistas <- ordenar(finalistas)
write.csv(finalistas, "resultados/finalistas_500.csv", row.names = FALSE)
factibles <- finalistas[finalistas$cumple_meta, ]
if (!nrow(factibles)) stop("Ninguna finalista cumple 99% en las 500 réplicas; revise los resultados sin afirmar factibilidad.")
ganadora <- factibles[1, c("id", "s", "S")]
base_mc <- evaluar(base, demanda_500, "validacion_500", guardar_traza = TRUE)
mejor_mc <- evaluar(ganadora, demanda_500, "validacion_500", guardar_traza = TRUE)

# Prueba reservada y estrés: las políticas se mantienen sin reoptimizar.
seleccion <- rbind(base, ganadora)
comparaciones <- list(base_mc$resumen, mejor_mc$resumen)
demanda_prueba <- matrix(prueba$ventas_litros, nrow = 1)
for (i in seq_len(nrow(seleccion))) {
  p <- seleccion[i, ]
  comparaciones[[length(comparaciones) + 1]] <- evaluar(p, demanda_prueba,
    "historico_2025", fechas = prueba$fecha, guardar_traza = TRUE)$resumen
  comparaciones[[length(comparaciones) + 1]] <- evaluar(p, demanda_500,
    "plazo_2_dias", plazo = 2)$resumen
  comparaciones[[length(comparaciones) + 1]] <- evaluar(p, demanda_500 * 1.2,
    "demanda_mas_20_por_ciento")$resumen
}
comparacion <- do.call(rbind, comparaciones)
comparacion$politica <- ifelse(comparacion$id == 0, "Base", "Seleccionada")
write.csv(comparacion, "resultados/comparacion.csv", row.names = FALSE)
write.csv(rbind(data.frame(politica = "Base", replica = seq_len(n_validacion), base_mc$replicas),
                data.frame(politica = "Seleccionada", replica = seq_len(n_validacion), mejor_mc$replicas)),
          "resultados/replicas_500.csv", row.names = FALSE)
delta <- base_mc$replicas$inventario_promedio - mejor_mc$replicas$inventario_promedio
mejora <- mean(delta)
ic_delta <- mejora + c(-1, 1) * 1.96 * sd(delta) / sqrt(length(delta))

# Datos enteros para MiniZinc: pérdida admitida calculada con conservadurismo.
# Los totales de demanda son idénticos para todas las finalistas.
demanda_total <- round(sum(demanda_500[, (calentamiento + 1):ncol(demanda_500)]))
perdida_total <- ceiling(finalistas$perdida_litros * n_validacion - 1e-7)
inventario_centilitros <- round(finalistas$inventario_promedio * 100)
stopifnot(all((perdida_total <= floor(demanda_total / 100)) == finalistas$cumple_meta))
vector_dzn <- function(nombre, x) paste0(nombre, " = [", paste(x, collapse = ", "), "];\n")
cat(paste0("n = 3;\ncapacidad = ", capacidad, ";\nperdida_maxima = ", floor(demanda_total / 100), ";\n",
  vector_dzn("id", finalistas$id), vector_dzn("umbral", finalistas$s),
  vector_dzn("objetivo", finalistas$S), vector_dzn("inventario", inventario_centilitros),
  vector_dzn("perdida", perdida_total)),
  file = "minizinc/escenarios.dzn")
cat("pendiente_ejecucion\n", file = "minizinc/estado.txt")
if (file.exists("minizinc/salida.txt")) unlink("minizinc/salida.txt")
controles <- list(
  inventario_minimo = min(vapply(diagnosticos, function(x) x$inventario_minimo, numeric(1))),
  inventario_maximo = max(vapply(diagnosticos, function(x) x$inventario_maximo, numeric(1))),
  error_balance_max = max(vapply(diagnosticos, function(x) x$error_balance_max, numeric(1))))
resumen <- list(media_2024 = mu, media_2025 = mean(prueba$ventas_litros), capacidad = capacidad,
  base = as.list(base[1, ]), seleccionada = as.list(mejor_mc$resumen[1, ]),
  dias_cobertura_base = c(s = base$s / mu, S = base$S / mu),
  mejora_inventario_litros = mejora, mejora_ic = ic_delta,
  reduccion_inventario_porcentaje = 100 * mejora / mean(base_mc$replicas$inventario_promedio),
  datos = list(filas_2024 = nrow(ajuste), filas_2025 = nrow(prueba),
    prueba_inicio_evaluado = as.character(prueba$fecha[calentamiento + 1]),
    prueba_fin = as.character(max(prueba$fecha)), fuente = url_fuente,
    md5_original = unname(tools::md5sum(archivo))),
  simulacion = list(inicial = n_inicial, validacion = n_validacion, calentamiento = calentamiento,
    dias_medidos = dias_medidos, semilla_busqueda = semilla_busqueda,
    semilla_validacion = semilla_validacion, ids_finalistas = ids_finalistas),
  controles = controles, version_R = R.version.string,
  version_readxl = as.character(packageVersion("readxl")),
  version_jsonlite = as.character(packageVersion("jsonlite")))
jsonlite::write_json(resumen, "resultados/resumen.json", auto_unbox = TRUE, pretty = TRUE, digits = NA)
writeLines(capture.output(sessionInfo()), "resultados/sesion_R.txt")

# Figuras reproducibles con gráficos de R; unidades explícitas y paleta común.
azul <- "#185A78"; naranja <- "#B65324"
png("resultados/figura_1_datos.png", 1800, 1000, res = 180)
par(mfrow = c(1, 2), mar = c(5, 6, 3, 1), mgp = c(4, 1, 0), las = 1)
plot(datos$fecha, datos$ventas_litros, type = "l", col = azul,
     xlab = "Fecha", ylab = "Ventas promedio por estación (L/día)", main = "Serie de referencia")
abline(v = as.Date("2025-01-01"), col = naranja, lty = 2)
boxplot(split(ajuste$ventas_litros, factor(ajuste$dia_semana, levels = 1:7,
     labels = c("Lun", "Mar", "Mié", "Jue", "Vie", "Sáb", "Dom"))),
     col = "#D9E7ED", xlab = "Día de la semana en 2024", ylab = "Ventas (L/día)",
     main = "Datos para el remuestreo")
dev.off()
png("resultados/figura_2_politicas.png", 1500, 1000, res = 180)
par(mar = c(5, 6, 3, 1), mgp = c(4, 1, 0), las = 1)
plot(busqueda$inventario_promedio, 100 * busqueda$servicio,
     pch = 19, col = ifelse(busqueda$cumple_meta, azul, "#C1C7CC"),
     xlab = "Inventario promedio aproximado (L)", ylab = "Demanda atendida (%)",
     main = "36 políticas con 100 trayectorias comunes", ylim = c(min(busqueda$servicio) * 100 - .1, 100.1))
abline(h = 99, lty = 2, col = naranja)
idx <- match(ids_finalistas, busqueda$id)
points(busqueda$inventario_promedio[idx], 100 * busqueda$servicio[idx], pch = 1, cex = 1.7, lwd = 2, col = naranja)
legend("bottomright", c("Cumple 99%", "No cumple", "Finalista", "Meta 99%"),
       col = c(azul, "#C1C7CC", naranja, naranja), pch = c(19, 19, 1, NA), lty = c(NA, NA, NA, 2), bty = "n")
dev.off()
png("resultados/figura_3_validacion.png", 1800, 950, res = 180)
par(mfrow = c(1, 2), mar = c(5, 6, 3, 1), mgp = c(4, 1, 0), las = 1)
boxplot(list(Base = base_mc$replicas$inventario_promedio,
             Seleccionada = mejor_mc$replicas$inventario_promedio),
        col = c("#C1C7CC", "#D9E7ED"), ylab = "Inventario promedio (L)",
        main = "500 trayectorias de validación")
mc <- rbind(base_mc$resumen, mejor_mc$resumen)
centros <- barplot(100 * mc$servicio, names.arg = c("Base", "Seleccionada"),
                   col = c("#C1C7CC", azul), ylim = c(0, 105),
                   ylab = "Demanda atendida (%)", main = "Servicio volumétrico")
abline(h = 99, lty = 2, col = naranja)
text(centros, 100 * mc$servicio + 2, sprintf("%.3f%%", 100 * mc$servicio), cex = .8)
dev.off()
png("resultados/figura_4_traza.png", 1800, 950, res = 180)
par(mfrow = c(1, 2), mar = c(5, 6, 3, 1), mgp = c(4, 1, 0), las = 1)
for (p in list(base, ganadora)) {
  tr <- read.csv(paste0("resultados/traza_validacion_500_", p$id, ".csv"))
  tr <- tr[tr$evaluado, ][1:28, ]
  plot(seq_len(nrow(tr)), tr$cierre_litros, type = "s", col = azul, lwd = 1.5,
       ylim = c(0, capacidad), xlab = "Día evaluado de una trayectoria",
       ylab = "Inventario al cierre (L)", main = if (p$id == 0) "Política base" else "Política seleccionada")
  abline(h = c(p$s, p$S), lty = c(2, 3), col = c(naranja, "#555555"))
  points(which(tr$recepcion_litros > 0), tr$cierre_litros[tr$recepcion_litros > 0], pch = 17, col = azul)
  legend("topright", c("Cierre", "s", "S", "Día con entrega"),
         col = c(azul, naranja, "#555555", azul), lty = c(1, 2, 3, NA), pch = c(NA, NA, NA, 17), cex = .7, bty = "n")
}
dev.off()
cat(sprintf("Media2024=%.6f; base=(%d,%d); seleccionada=(%d,%d); servicio=%.6f\n",
            mu, base$s, base$S, ganadora$s, ganadora$S, mejor_mc$resumen$servicio))
print(comparacion[, c("politica", "escenario", "inventario_promedio", "servicio", "pedidos", "entregas")], row.names = FALSE)
