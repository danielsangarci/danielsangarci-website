# Exporta tus publicaciones y congresos a BibTeX para importarlos en el editor
# CVN de FECYT (https://cvn.fecyt.es/editor): Importar -> Formato a importar:
# BibTeX -> elegir el archivo -> Importar -> Confirmar -> marcar -> Anadir.
#
# Se lanza desde la raiz de la web:  Rscript _cv/cvn/exportar_cvn.R
#
# Genera en _cv/cvn/:
#   publicaciones.bib        articulos, capitulos y preprints (de publications.yml)
#   congresos.bib            charlas y posters (de _cv/conferences.yml)
#   *-ascii.bib              lo mismo, con los acentos escritos al estilo LaTeX
#                            ({\'a}, {\~n}...). Usalos si el editor muestra mal
#                            los acentos de los archivos normales.

source("_cv/leer_cv.R")

salida <- file.path("_cv", "cvn")
dir.create(salida, showWarnings = FALSE)

# Acentos y letras especiales en formato LaTeX (para los *-ascii.bib)
.latex <- c(
  "á" = "{\\'a}", "é" = "{\\'e}", "í" = "{\\'i}", "ó" = "{\\'o}", "ú" = "{\\'u}",
  "Á" = "{\\'A}", "É" = "{\\'E}", "Í" = "{\\'I}", "Ó" = "{\\'O}", "Ú" = "{\\'U}",
  "à" = "{\\`a}", "è" = "{\\`e}", "ò" = "{\\`o}", "À" = "{\\`A}", "È" = "{\\`E}",
  "ä" = "{\\\"a}", "ë" = "{\\\"e}", "ï" = "{\\\"i}", "ö" = "{\\\"o}", "ü" = "{\\\"u}",
  "Ü" = "{\\\"U}", "ñ" = "{\\~n}", "Ñ" = "{\\~N}", "ç" = "{\\c c}", "Ç" = "{\\c C}",
  "ő" = "{\\H o}", "ł" = "{\\l}", "Ł" = "{\\L}", "ś" = "{\\'s}", "Ś" = "{\\'S}",
  "ń" = "{\\'n}", "ć" = "{\\'c}", "ź" = "{\\'z}", "ż" = "{\\.z}", "ą" = "{\\k a}",
  "ę" = "{\\k e}", "Á" = "{\\'A}", "’" = "'", "–" = "--", "—" = "---",
  "¿" = "?`", "¡" = "!`", "≈" = "~"
)
a_ascii <- function(x) {
  for (letra in names(.latex)) x <- gsub(letra, .latex[[letra]], x, fixed = TRUE)
  # Lo que quede fuera de la tabla, sin acento (mejor que un caracter roto)
  iconv(x, "UTF-8", "ASCII//TRANSLIT", sub = "")
}

escribir <- function(entradas, archivo) {
  texto <- paste(entradas, collapse = "\n\n")
  writeLines(texto, file.path(salida, archivo), useBytes = TRUE)
  writeLines(a_ascii(texto), file.path(salida, sub("\\.bib$", "-ascii.bib", archivo)),
             useBytes = TRUE)
  message("  ", file.path(salida, archivo), " (", length(entradas), " entradas)")
}

# --- Publicaciones: la referencia BibTeX de cada trabajo de publications.yml.
# Los preprints se escriben como @article con la revista = servidor (bioRxiv)
# y la nota "Preprint", porque el editor no tiene un tipo propio para ellos.
pubs <- Filter(function(p) !isTRUE(p$hide) && !is.null(p$bibtex),
               yaml::read_yaml("publications.yml"))
bib_pubs <- vapply(pubs, function(p) {
  b <- p$bibtex
  if (grepl("^@misc", b)) {
    b <- sub("^@misc", "@article", b)
    b <- sub("howpublished = ", "journal = ", b, fixed = TRUE)
    b <- sub("\n}$", ",\n  note = {Preprint}\n}", b)
  }
  if (!is.null(p$doi) && !grepl("url = ", b, fixed = TRUE)) {
    b <- sub("\n}$", sprintf(",\n  url = {https://doi.org/%s}\n}", p$doi), b)
  }
  b
}, character(1))

# --- Congresos: @inproceedings con el congreso, el lugar y el tipo de
# presentacion (en el CVN van en "Trabajos presentados en congresos").
etiqueta <- c(talk = "Ponencia oral", poster = "Póster", invited = "Ponencia invitada")
congresos <- cv_seccion(cv_leer("_cv"), "conferences", "full")
bib_congresos <- vapply(seq_along(congresos), function(i) {
  e <- congresos[[i]]
  autores <- paste(trimws(strsplit(e$authors, ";")[[1]]), collapse = " and ")
  campos <- c(
    author    = autores,
    title     = gsub("\\*", "", e$title),
    booktitle = e$event,
    address   = paste(c(e$city, e$country), collapse = ", "),
    year      = substr(as.character(e$date), 1, 4),
    month     = if (grepl("-", e$date)) as.integer(sub(".*-", "", e$date)) else NA,
    note      = etiqueta[[e$type]]
  )
  campos <- campos[!is.na(campos)]
  clave <- sprintf("congreso%s_%02d", campos[["year"]], i)
  paste0("@inproceedings{", clave, ",\n",
         paste(sprintf("  %s = {%s}", names(campos), campos), collapse = ",\n"),
         "\n}")
}, character(1))

message("Archivos para el editor CVN:")
escribir(bib_pubs, "publicaciones.bib")
escribir(bib_congresos, "congresos.bib")
