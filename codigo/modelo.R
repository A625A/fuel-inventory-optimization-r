# Política (s,S), revisión al cierre y entregas al inicio del día t + plazo.
# Cada fila de demanda es una trayectoria; cada columna es un día.
simular <- function(demanda, s, S, capacidad = 40000, plazo = 1,
                    calentamiento = 28, traza = FALSE) {
  stopifnot(is.matrix(demanda), is.numeric(demanda),
            nrow(demanda) > 0, ncol(demanda) > 0,
            all(is.finite(demanda)), all(demanda >= 0))
  parametros <- c(s, S, capacidad, plazo, calentamiento)
  stopifnot(length(parametros) == 5, all(is.finite(parametros)),
            s >= 0, s < S, S <= capacidad, capacidad > 0,
            plazo >= 1, plazo == as.integer(plazo),
            calentamiento >= 0, calentamiento == as.integer(calentamiento),
            calentamiento < ncol(demanda))
  n <- nrow(demanda)
  dias <- ncol(demanda)
  medidos <- dias - calentamiento
  inventario <- rep(S, n)
  transito <- matrix(0, n, plazo)
  acumulado <- matrix(0, n, 10)
  colnames(acumulado) <- c("inventario_promedio", "demanda_litros",
                          "perdida_litros", "dias_faltantes", "pedidos",
                          "entregas", "pedido_litros", "recibido_litros",
                          "servido_litros", "dias")
  recibido_total <- servido_total <- rep(0, n)
  minimo <- S
  maximo <- S
  error_max <- 0
  filas <- if (traza) vector("list", dias) else NULL

  for (t in seq_len(dias)) {
    recibido <- transito[, 1]
    if (plazo > 1) transito[, seq_len(plazo - 1)] <-
      transito[, 2:plazo, drop = FALSE]
    transito[, plazo] <- 0
    disponible <- inventario + recibido
    vendido <- pmin(disponible, demanda[, t])
    perdido <- demanda[, t] - vendido
    inventario <- disponible - vendido
    posicion <- inventario + rowSums(transito)
    pedido <- ifelse(posicion <= s, S - posicion, 0)
    transito[, plazo] <- transito[, plazo] + pedido

    # Controles físicos sobre todas las trayectorias, incluido calentamiento.
    recibido_total <- recibido_total + recibido
    servido_total <- servido_total + vendido
    error_max <- max(error_max, abs(S + recibido_total - servido_total - inventario))
    minimo <- min(minimo, inventario)
    maximo <- max(maximo, disponible)
    stopifnot(all(inventario >= -1e-8), all(disponible <= capacidad + 1e-8),
              all(pedido >= -1e-8),
              all(inventario + rowSums(transito) <= capacidad + 1e-8),
              error_max < 1e-5)

    if (t > calentamiento) {
      acumulado <- acumulado + cbind(
        (disponible + inventario) / 2, demanda[, t], perdido,
        as.numeric(perdido > 1e-8), as.numeric(pedido > 1e-8),
        as.numeric(recibido > 1e-8), pedido, recibido, vendido, rep(1, n))
    }
    if (traza) filas[[t]] <- data.frame(
      dia = t, evaluado = t > calentamiento, demanda_litros = demanda[1, t],
      recepcion_litros = recibido[1], disponible_litros = disponible[1],
      vendido_litros = vendido[1], perdida_litros = perdido[1],
      cierre_litros = inventario[1], posicion_antes_pedido = posicion[1],
      pedido_litros = pedido[1], pendiente_litros = sum(transito[1, ]))
  }
  metricas <- as.data.frame(acumulado)
  metricas$inventario_promedio <- metricas$inventario_promedio / medidos
  metricas$servicio <- ifelse(metricas$demanda_litros > 0,
                             1 - metricas$perdida_litros / metricas$demanda_litros,
                             NA_real_)
  list(metricas = metricas,
       traza = if (traza) do.call(rbind, filas) else NULL,
       diagnosticos = list(inventario_minimo = minimo, inventario_maximo = maximo,
                          error_balance_max = error_max))
}
