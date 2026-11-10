# Genera los dos PDF del CV a partir de los datos de _cv/*.yml:
#   cv.pdf        CV completo (es el que enlaza la web)
#   cv-short.pdf  CV resumido (2 paginas)
# Se lanza desde la raiz de la web:  Rscript _cv/pdf/render_cv.R
# Los dos PDF se copian a la raiz y a docs/ (lo que publica GitHub Pages).

library(rmarkdown)

raiz <- normalizePath(".")
plantilla <- file.path(raiz, "_cv", "pdf", "cv.Rmd")
if (!file.exists(plantilla)) {
  stop("Lanza este script desde la raiz de la web: Rscript _cv/pdf/render_cv.R")
}

compilar <- function(salida, short) {
  message("== Generando ", salida, if (short) " (resumido)" else " (completo)")
  # Se compila en _cv/pdf/, donde estan la plantilla LaTeX, las fuentes y la foto
  pdf <- render(plantilla, output_file = salida, params = list(short = short),
                envir = new.env(), quiet = TRUE)
  for (destino in c(raiz, file.path(raiz, "docs"))) {
    if (dir.exists(destino)) file.copy(pdf, file.path(destino, salida), overwrite = TRUE)
  }
  file.remove(pdf)
  unlink(file.path(dirname(pdf), c(sub("\\.pdf$", ".log", salida),
                                   "publicaciones.bib", "congresos.bib")))
}

compilar("cv.pdf", short = FALSE)
compilar("cv-short.pdf", short = TRUE)
message("== Listo: cv.pdf y cv-short.pdf en la raiz de la web y en docs/")
