# Pruebas pequeñas con resultados calculados a mano; ejecutar desde la raíz.
source(file.path("codigo", "modelo.R"), encoding = "UTF-8")
iguales <- function(a, b) stopifnot(isTRUE(all.equal(a, b, tolerance = 1e-9)))
rechaza <- function(expr) stopifnot(inherits(try(force(expr), silent = TRUE), "try-error"))

x <- simular(matrix(rep(4, 4), nrow = 1), s = 7, S = 10,
             capacidad = 10, plazo = 1, calentamiento = 0, traza = TRUE)
iguales(x$traza$recepcion_litros, c(0, 4, 4, 4))
iguales(x$traza$cierre_litros, rep(6, 4))
iguales(x$traza$pedido_litros, rep(4, 4))
iguales(x$metricas$pedidos, 4)
iguales(x$metricas$entregas, 3)
iguales(x$metricas$inventario_promedio, 8)

y <- simular(matrix(rep(4, 5), nrow = 1), s = 7, S = 10,
             capacidad = 10, plazo = 2, calentamiento = 0, traza = TRUE)
iguales(y$traza$recepcion_litros, c(0, 0, 4, 4, 4))
iguales(y$traza$cierre_litros, c(6, 2, 2, 2, 2))
iguales(y$traza$pedido_litros, rep(4, 5))
iguales(y$traza$pendiente_litros, c(4, 8, 8, 8, 8))
stopifnot(all(y$traza$cierre_litros + y$traza$pendiente_litros <= 10))

z <- simular(matrix(c(20, 1, 20), nrow = 1), 2, 10, 10, 1, 0, TRUE)
iguales(z$traza$perdida_litros, c(10, 0, 11))
iguales(z$traza$cierre_litros, c(0, 9, 0))
iguales(z$metricas$servicio, 20 / 41)
stopifnot(z$diagnosticos$error_balance_max < 1e-8)

cero <- simular(matrix(0, 2, 5), 2, 10, 10, 1, 0)
iguales(cero$metricas$pedidos, c(0, 0))
iguales(cero$metricas$inventario_promedio, c(10, 10))
stopifnot(all(is.na(cero$metricas$servicio)))

w <- simular(matrix(c(4, 4, 4), nrow = 1), 7, 10, 10, 1, 1)
iguales(w$metricas$demanda_litros, 8)
iguales(w$metricas$entregas, 2)
rechaza(simular(matrix(-1, 1, 3), 2, 10, 10, 1, 0))
rechaza(simular(matrix(NA_real_, 1, 3), 2, 10, 10, 1, 0))
rechaza(simular(matrix(1, 1, 3), 10, 10, 10, 1, 0))
rechaza(simular(matrix(1, 1, 3), 2, 11, 10, 1, 0))
rechaza(simular(matrix(1, 1, 3), 2, 10, 10, 0, 0))
rechaza(simular(matrix(1, 1, 3), 2, 10, 10, 1, 3))
cat("OK: plazos 1/2, tránsito, pérdidas, capacidad, balance, calentamiento y entradas.\n")
