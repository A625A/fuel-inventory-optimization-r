# Verificación de artefactos y cálculos después de ejecutar.R.
stopifnot(requireNamespace("jsonlite", quietly = TRUE))
r <- jsonlite::read_json("resultados/resumen.json", simplifyVector = TRUE)
d <- read.csv("datos/gasolina_diaria.csv")
p <- read.csv("resultados/busqueda_36.csv")
f <- read.csv("resultados/finalistas_500.csv")
c <- read.csv("resultados/comparacion.csv")
mu <- mean(d$ventas_litros[substr(d$fecha, 1, 4) == "2024"])
stopifnot(abs(mu - r$media_2024) < 1e-8,
          r$base$s == round(2 * mu / 100) * 100,
          r$base$S == round(4 * mu / 100) * 100,
          nrow(p) == 36, nrow(unique(p[c("s", "S")])) == 36,
          all(p$simulaciones == 100), all(p$s < p$S), all(p$S <= 40000),
          nrow(f) == 3, all(f$simulaciones == 500),
          r$seleccionada$id %in% f$id,
          r$seleccionada$servicio >= .99,
          all(c$servicio >= 0 & c$servicio <= 1),
          all(c$inventario_promedio >= 0 & c$inventario_promedio <= 40000),
          all(c$perdida_litros >= 0),
          all(c$pedidos >= 0), all(c$entregas >= 0),
          r$controles$error_balance_max < 1e-5,
          r$controles$inventario_minimo >= -1e-8,
          r$controles$inventario_maximo <= 40000 + 1e-8,
          r$datos$filas_2024 == 366, r$datos$filas_2025 == 365)
cat("OK: media/base, 36/100, tres/500, selección factible, fechas y controles físicos.\n")
