# Funciones comunes del CV: leen los datos de _cv/*.yml, los comprueban y les
# dan formato. Las usan la plantilla del PDF (_cv/pdf/cv.Rmd) y la pagina de
# la web (cv.qmd). Solo necesitan el paquete yaml.

# ---------------------------------------------------------------------------
# Lectura y comprobacion
# ---------------------------------------------------------------------------

# Campos obligatorios de cada archivo (o de cada lista dentro del archivo)
.cv_obligatorios <- list(
  experience     = c("title", "org", "city", "country", "start"),
  education      = c("title", "org", "city", "country", "start"),
  stays          = c("host", "org", "city", "country", "start"),
  internships    = c("title", "org", "city", "country", "start"),
  conferences    = c("title", "authors", "event", "country", "date", "type"),
  funding        = c("name", "funder", "start"),
  awards         = c("title", "date"),
  organization   = c("event", "role", "date"),
  reviews        = c("journal", "count"),
  workshops      = c("title", "date"),
  memberships    = c("name", "since"),
  courses        = c("title", "date"),
  references     = c("name", "role", "org")
)

# Campos con fecha: se comprueba que tengan formato AAAA o AAAA-MM
.cv_campos_fecha <- c("start", "end", "date", "since")

cv_leer <- function(dir = "_cv") {
  leer <- function(archivo) {
    ruta <- file.path(dir, archivo)
    if (!file.exists(ruta)) return(NULL)
    yaml::read_yaml(ruta)
  }

  cv <- list(
    profile     = leer("profile.yml"),
    experience  = leer("experience.yml"),
    education   = leer("education.yml"),
    stays       = leer("stays.yml"),
    internships = leer("internships.yml"),
    conferences = leer("conferences.yml"),
    funding     = leer("funding.yml"),
    awards      = leer("awards.yml"),
    memberships = leer("memberships.yml"),
    courses     = leer("courses.yml"),
    references  = leer("references.yml"),
    skills      = leer("skills.yml"),
    # private.yml no esta en GitHub: si falta, el PDF sale sin esos datos
    private     = leer("private.yml")
  )
  servicio <- leer("service.yml")
  cv$organization <- servicio$organization
  cv$reviews      <- servicio$reviews
  cv$workshops    <- servicio$workshops

  for (seccion in names(.cv_obligatorios)) {
    cv[[seccion]] <- .cv_comprobar(cv[[seccion]], seccion, .cv_obligatorios[[seccion]])
  }
  cv
}

.cv_comprobar <- function(entradas, seccion, obligatorios) {
  for (i in seq_along(entradas)) {
    e <- entradas[[i]]
    nombre <- e$title %||% e$name %||% e$host %||% e$event %||% e$journal %||% ""
    donde <- sprintf("%s, entrada %d (\"%s\")", seccion, i, substr(nombre, 1, 50))
    falta <- setdiff(obligatorios, names(e)[!vapply(e, .cv_vacio, logical(1))])
    if (length(falta)) {
      stop("CV: en ", donde, " falta: ", paste(falta, collapse = ", "), call. = FALSE)
    }
    for (campo in intersect(.cv_campos_fecha, names(e))) {
      valor <- as.character(e[[campo]])
      if (!grepl("^\\d{4}(-(0[1-9]|1[0-2]))?$", valor) && valor != "present") {
        stop("CV: en ", donde, " la fecha '", campo, ": ", valor,
             "' no es valida. Usa AAAA, AAAA-MM o present.", call. = FALSE)
      }
    }
    if (seccion == "conferences" && !e$type %in% c("talk", "poster", "invited")) {
      stop("CV: en ", donde, " 'type' debe ser talk, poster o invited.", call. = FALSE)
    }
  }
  entradas
}

.cv_vacio <- function(x) is.null(x) || length(x) == 0 || all(is.na(x)) || identical(x, "")

`%||%` <- function(a, b) if (.cv_vacio(a)) b else a

# ---------------------------------------------------------------------------
# Seleccion y orden
# ---------------------------------------------------------------------------

# version: "full" (CV completo), "short" (CV resumido) o "web"
cv_filtrar <- function(entradas, version = c("full", "short", "web")) {
  version <- match.arg(version)
  Filter(function(e) {
    if (version == "short" && isFALSE(e$short)) return(FALSE)
    if (version == "web" && isFALSE(e$web)) return(FALSE)
    TRUE
  }, entradas)
}

# Ordena de mas reciente a mas antiguo segun la fecha de inicio
cv_ordenar <- function(entradas) {
  if (!length(entradas)) return(entradas)
  clave <- vapply(entradas, function(e) {
    .cv_fecha_num(e$start %||% e$date %||% e$since)
  }, numeric(1))
  entradas[order(clave, decreasing = TRUE)]
}

cv_seccion <- function(cv, seccion, version = "full") {
  cv_ordenar(cv_filtrar(cv[[seccion]], version))
}

# ---------------------------------------------------------------------------
# Fechas
# ---------------------------------------------------------------------------

.cv_fecha_num <- function(x) {
  x <- as.character(x %||% "0")
  if (x == "present") return(9999)
  partes <- as.numeric(strsplit(x, "-")[[1]])
  partes[1] + (if (length(partes) > 1) (partes[2] - 1) / 12 else 0)
}

.cv_fecha_txt <- function(x) {
  x <- as.character(x)
  if (x == "present") return("present")
  partes <- strsplit(x, "-")[[1]]
  if (length(partes) == 1) return(partes[1])
  paste(month.abb[as.integer(partes[2])], partes[1])
}

# "Jan 2025 – present", "May 2024 – Jun 2024", "Oct 2021", "2022 – 2025"
cv_periodo <- function(start, end = NULL) {
  ini <- .cv_fecha_txt(start)
  if (.cv_vacio(end) || as.character(end) == as.character(start)) return(ini)
  paste(ini, "–", .cv_fecha_txt(end))
}

# ---------------------------------------------------------------------------
# Texto
# ---------------------------------------------------------------------------

# Lugar en una linea: "Org, Unit, City (Country)" (PDF) o
# "Org, Unit · City, Country" (web)
cv_lugar <- function(e, formato = c("pdf", "web")) {
  formato <- match.arg(formato)
  org <- paste(c(e$org, e$unit), collapse = ", ")
  if (formato == "pdf") sprintf("%s, %s (%s)", org, e$city, e$country)
  else sprintf("%s · %s, %s", org, e$city, e$country)
}

cv_descripcion <- function(e, formato = c("pdf", "web")) {
  formato <- match.arg(formato)
  if (formato == "web") e$description_web %||% e$description %||% ""
  else e$description %||% ""
}

# Markdown sencillo (*cursiva*, **negrita**, [texto](url)) a LaTeX
cv_md_latex <- function(x) {
  if (.cv_vacio(x)) return(x)
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\\\textasciitilde{}", x)
  x <- gsub("\\[([^]]+)\\]\\(([^)]+)\\)", "\\\\href{\\2}{\\1}", x)
  x <- gsub("\\*\\*([^*]+)\\*\\*", "\\\\textbf{\\1}", x)
  x <- gsub("\\*([^*]+)\\*", "\\\\textit{\\1}", x)
  x
}

# Termina una frase con punto si no lo tiene ya
cv_punto <- function(x) if (grepl("[.?!]$", x)) x else paste0(x, ".")

# ---------------------------------------------------------------------------
# Bibliografias para el PDF (vitae las formatea con el estilo CSL)
# ---------------------------------------------------------------------------

# Publicaciones: usa la referencia BibTeX de cada trabajo de publications.yml.
# Los preprints se escriben como @article con la revista = servidor (bioRxiv),
# y las especies en cursiva del titulo se protegen para que el estilo no las
# pase a minuscula.
cv_bib_publicaciones <- function(publicaciones_yml, salida) {
  pubs <- yaml::read_yaml(publicaciones_yml)
  pubs <- Filter(function(p) !isTRUE(p$hide) && !is.null(p$bibtex), pubs)
  bib <- vapply(pubs, function(p) {
    b <- p$bibtex
    if (grepl("^@misc", b)) {
      b <- sub("^@misc", "@article", b)
      b <- sub("howpublished = ", "journal = ", b, fixed = TRUE)
    }
    if (!is.null(p$title) && grepl("<i>", p$title)) {
      titulo <- p$title
      titulo <- gsub("&amp;", "\\\\&", titulo)
      titulo <- gsub("<i>([^<]+)</i>", "{\\\\textit{\\1}}", titulo)
      titulo <- gsub("\\s+", " ", titulo)
      b <- sub("title = \\{[^\n]*\\},", paste0("title = {", gsub("\\\\", "\\\\\\\\", titulo), "},"), b)
    }
    b
  }, character(1))
  writeLines(bib, salida, useBytes = TRUE)
  invisible(salida)
}

# Congresos: se escriben como @article para que salgan como
# "Autores (año). Titulo. Congreso, Ciudad, Pais (Talk)."
cv_bib_congresos <- function(congresos, salida) {
  etiqueta <- c(talk = "Talk", poster = "Poster", invited = "Invited talk")
  bib <- vapply(seq_along(congresos), function(i) {
    e <- congresos[[i]]
    autores <- paste(trimws(strsplit(e$authors, ";")[[1]]), collapse = " and ")
    titulo <- gsub("\\*([^*]+)\\*", "{\\\\textit{\\1}}", e$title)
    lugar <- if (.cv_vacio(e$city)) "" else sprintf("   volume = {%s},\n", e$city)
    sprintf(paste0("@article{congreso%d,\n   author = {%s},\n   title = {%s},\n",
                   "   journal = {%s},\n%s   pages = {%s (%s)},\n   year = {%s}\n}\n"),
            i, autores, titulo, e$event, lugar, e$country,
            etiqueta[[e$type]], substr(as.character(e$date), 1, 4))
  }, character(1))
  writeLines(bib, salida, useBytes = TRUE)
  invisible(salida)
}

# ---------------------------------------------------------------------------
# Bloques de la pagina web (Markdown de Quarto con las clases de custom.scss)
# ---------------------------------------------------------------------------

# Linea de tiempo: experiencia, estudios, practicas, estancias
cv_web_linea <- function(entradas, tipo = c("puesto", "estancia")) {
  tipo <- match.arg(tipo)
  hitos <- vapply(entradas, function(e) {
    if (tipo == "estancia") {
      puesto <- sprintf("%s · %s, %s", e$org, e$city, e$country)
      lugar <- paste(c(e$unit, paste("Host:", e$host)), collapse = " · ")
    } else {
      puesto <- e$title
      lugar <- cv_lugar(e, "web")
    }
    desc <- cv_descripcion(e, "web")
    paste0("::: cv-hito\n",
           "[", cv_periodo(e$start, e$end), "]{.cv-fecha}\n\n",
           "[", puesto, "]{.cv-puesto}\n\n",
           "[", lugar, "]{.cv-lugar}\n",
           if (nzchar(desc)) paste0("\n", desc, "\n") else "",
           ":::\n")
  }, character(1))
  cat("::: cv-linea\n", paste(hitos, collapse = "\n"), ":::\n", sep = "")
}

cv_web_congresos <- function(congresos) {
  etiqueta <- c(talk = "[Talk]{.cv-tipo}",
                poster = "[Poster]{.cv-tipo .cv-tipo--poster}",
                invited = "[Invited talk]{.cv-tipo}")
  items <- vapply(congresos, function(e) {
    lugar <- paste(c(e$city, if (e$country == "Online") "online" else e$country), collapse = ", ")
    sprintf("- %s **%s** *%s*, %s (%s).", etiqueta[[e$type]], cv_punto(e$title),
            e$event, lugar, substr(as.character(e$date), 1, 4))
  }, character(1))
  cat("::: cv-charlas\n", paste(items, collapse = "\n"), "\n:::\n", sep = "")
}

# Tarjeta con etiqueta (icono de Font Awesome) y contenido en Markdown
cv_web_tarjeta <- function(icono, etiqueta, contenido, nota = NULL) {
  paste0("::: cv-tarjeta\n",
         "[{{< fa ", icono, " >}} ", etiqueta, "]{.cv-tarjeta__etiqueta}\n\n",
         paste(contenido, collapse = "\n"), "\n",
         if (!.cv_vacio(nota)) paste0("\n[", nota, "]{.cv-tarjeta__nota}\n") else "",
         ":::\n")
}

cv_web_tarjetas <- function(tarjetas) {
  cat("::: cv-tarjetas\n", paste(unlist(tarjetas), collapse = "\n"), ":::\n", sep = "")
}

cv_web_lista <- function(x) paste0("- ", x)

# "B2" -> "B2 (upper intermediate)": nombres habituales de los niveles del
# Marco Comun Europeo. `etiqueta` (el campo label de skills.yml) los sustituye.
cv_nivel_idioma <- function(nivel, etiqueta = NULL) {
  if (tolower(nivel) == "native") return("native")
  nombre <- etiqueta %||% unname(c(A1 = "beginner", A2 = "elementary", B1 = "intermediate",
                                   B2 = "upper intermediate", C1 = "advanced",
                                   C2 = "proficient")[toupper(nivel)])
  if (.cv_vacio(nombre)) nivel else sprintf("%s (%s)", nivel, nombre)
}
