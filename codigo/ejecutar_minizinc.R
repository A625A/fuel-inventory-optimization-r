# Opcional: ejecutar después de ejecutar.R, desde la raíz del proyecto.
# Si no está en PATH: Sys.setenv(MINIZINC = "ruta/al/ejecutable/minizinc").
stopifnot(requireNamespace("jsonlite", quietly = TRUE))
binario <- Sys.getenv("MINIZINC", unset = Sys.which("minizinc"))
if (!nzchar(binario) || !file.exists(binario)) {
  writeLines("no_disponible: el proyecto principal se ejecutó únicamente en R",
             "minizinc/estado.txt")
  message("MiniZinc no disponible. R ya realizó la selección completa.")
} else {
  version <- system2(binario, "--version", stdout = TRUE, stderr = TRUE)
  salida <- system2(binario, c("--solver", "gecode", "minizinc/seleccion.mzn",
                              "minizinc/escenarios.dzn"), stdout = TRUE, stderr = TRUE)
  estado <- attr(salida, "status")
  if (!is.null(estado) && estado != 0) stop(paste(salida, collapse = "\n"))
  writeLines(c(version, "", salida), "minizinc/salida.txt")
  lineas_id <- grep("^id = [0-9]+;", salida, value = TRUE)
  stopifnot(length(lineas_id) > 0, any(grepl("==========", salida, fixed = TRUE)))
  id <- as.integer(sub("^id = ([0-9]+);$", "\\1", tail(lineas_id, 1)))
  r <- jsonlite::read_json("resultados/resumen.json", simplifyVector = FALSE)
  stopifnot(id == r$seleccionada$id)
  f <- read.csv("resultados/finalistas_500.csv")
  elegida <- f[f$id == id, ]
  stopifnot(nrow(elegida) == 1, elegida$servicio >= .99, elegida$S <= r$capacidad)
  r$minizinc <- list(estado = "ejecutado_y_verificado", id = id, version = version[1])
  jsonlite::write_json(r, "resultados/resumen.json", auto_unbox = TRUE, pretty = TRUE, digits = NA)
  writeLines("ejecutado_y_verificado: coincide con selección de R", "minizinc/estado.txt")
  cat("OK: MiniZinc demostró optimalidad sobre tres finalistas y coincide con R.\n")
}
