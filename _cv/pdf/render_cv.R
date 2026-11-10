# Genera los PDF del CV a partir de los datos de _cv/*.yml.
#
#   Archivo            Version
#   cv.pdf             completo, ingles (el que enlaza la web)
#   cv-short.pdf       resumido, ingles
#   cv-es.pdf          completo, espanol
#   cv-short-es.pdf    resumido, espanol
#
# Se lanza desde la raiz de la web. Sin argumentos genera todas; con
# argumentos, solo las que pidas (idioma y/o version, en cualquier orden):
#
#   Rscript _cv/pdf/render_cv.R              # las cuatro
#   Rscript _cv/pdf/render_cv.R es           # espanol, completo y resumido
#   Rscript _cv/pdf/render_cv.R short        # resumido, ingles y espanol
#   Rscript _cv/pdf/render_cv.R en full      # solo cv.pdf
#
# Desde RStudio (con la web como directorio de trabajo):
#   source("_cv/pdf/render_cv.R"); generar_cv("es", "short")
#
# Los PDF en ingles se copian a la raiz y a docs/ (lo que publica GitHub
# Pages). Los PDF en espanol solo a la raiz: no se publican ni se suben a git.

library(rmarkdown)

IDIOMAS <- c("en", "es")
VERSIONES <- c("full", "short")

nombre_pdf <- function(lang, version) {
  paste0("cv", if (version == "short") "-short", if (lang != "en") paste0("-", lang), ".pdf")
}

generar_cv <- function(idiomas = IDIOMAS, versiones = VERSIONES) {
  raiz <- normalizePath(".")
  plantilla <- file.path(raiz, "_cv", "pdf", "cv.Rmd")
  if (!file.exists(plantilla)) {
    stop("Lanza este script desde la raiz de la web: Rscript _cv/pdf/render_cv.R")
  }
  for (lang in idiomas) for (version in versiones) {
    salida <- nombre_pdf(lang, version)
    message("== Generando ", salida)
    # Se compila en _cv/pdf/, donde estan la plantilla LaTeX, las fuentes y la foto
    pdf <- render(plantilla, output_file = salida,
                  params = list(short = version == "short", lang = lang),
                  envir = new.env(), quiet = TRUE)
    # La web solo publica el CV en ingles: los demas idiomas se quedan en la
    # raiz (y fuera de git, ver .gitignore), no en docs/
    destinos <- if (lang == "en") c(raiz, file.path(raiz, "docs")) else raiz
    for (destino in destinos) {
      if (dir.exists(destino)) file.copy(pdf, file.path(destino, salida), overwrite = TRUE)
    }
    file.remove(pdf)
    unlink(file.path(dirname(pdf), c(sub("\\.pdf$", ".log", salida),
                                     "publicaciones.bib", "congresos.bib")))
  }
  message("== Listo: PDF en la raiz de la web (los de ingles, tambien en docs/)")
}

# Al lanzarlo con Rscript, lee los argumentos (en, es, full, short)
if (!interactive() && sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  desconocidos <- setdiff(args, c(IDIOMAS, VERSIONES))
  if (length(desconocidos)) {
    stop("Argumento no valido: ", paste(desconocidos, collapse = ", "),
         ". Usa ", paste(c(IDIOMAS, VERSIONES), collapse = ", "), ".")
  }
  generar_cv(
    idiomas   = if (any(args %in% IDIOMAS)) intersect(IDIOMAS, args) else IDIOMAS,
    versiones = if (any(args %in% VERSIONES)) intersect(VERSIONES, args) else VERSIONES
  )
}
