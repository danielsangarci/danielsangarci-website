# Quita del sitemap las paginas ocultas: las que llevan
# <meta name="robots" content="noindex...">.
# Quarto las incluye siempre; se ejecuta despues de cada render
# (post-render en _quarto.yml).
f <- "docs/sitemap.xml"
if (file.exists(f)) {
  xml <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  bloques <- regmatches(xml, gregexpr("<url>.*?</url>\\s*", xml))[[1]]
  oculta <- vapply(bloques, function(b) {
    loc <- sub(".*<loc>https?://[^/]+/?(.*?)</loc>.*", "\\1", b)
    pagina <- file.path("docs", if (loc == "" || grepl("/$", loc)) paste0(loc, "index.html") else loc)
    file.exists(pagina) &&
      any(grepl('name="robots" content="noindex', readLines(pagina, warn = FALSE), fixed = TRUE))
  }, logical(1))
  if (any(oculta)) {
    for (b in bloques[oculta]) xml <- sub(b, "", xml, fixed = TRUE)
    writeLines(xml, f, useBytes = TRUE)
  }
}
