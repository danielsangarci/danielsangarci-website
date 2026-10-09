#!/usr/bin/env Rscript
# =====================================================================
#  Actualiza la lista de publicaciones de la web
# =====================================================================
#
# Fuentes:
#   - ORCID     -> que trabajos son tuyos
#   - OpenAlex  -> tipo de trabajo, Open Access, PDF, citas, y los
#                  preprints y datasets asociados a cada articulo
#   - Crossref  -> autores, revista, volumen, paginas y resumen
#
# Junta todo con `manual.yml` (trabajos sin DOI y retoques a mano) y
# escribe `publications.yml` (en la raiz), que es lo que pinta `publications.qmd`.
#
# Uso (desde la carpeta del proyecto):
#   Rscript _publications/actualizar.R            # consulta internet si la copia tiene > 1 dia
#   Rscript _publications/actualizar.R --forzar   # consulta internet siempre
#   Rscript _publications/actualizar.R --offline  # solo reconstruye con la copia guardada
#
# Quarto lo ejecuta solo antes de renderizar (pre-render en _quarto.yml),
# y GitHub lo ejecuta cada semana (.github/workflows/publicaciones.yml).
# Si no hay internet, se usa la ultima copia guardada y la web no se rompe.
# ---------------------------------------------------------------------

ORCID     <- "0000-0002-0710-6292"
MI_NOMBRE <- "S[aá]nchez[- ‐‑]Garc[ií]a"   # se pone en negrita
CADUCIDAD <- 24 * 3600                                 # segundos

suppressPackageStartupMessages({
  library(jsonlite)
  library(yaml)
  library(curl)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || identical(a, "")) b else a
primero <- function(x) if (length(x)) x[[1]] else NULL

# Se puede lanzar desde la raiz del proyecto o desde esta carpeta
if (!dir.exists("_publications") && file.exists("manual.yml")) setwd("..")
carpeta  <- "_publications"
f_manual <- file.path(carpeta, "manual.yml")
f_cache  <- file.path(carpeta, "cache.json")
f_salida <- "publications.yml"   # fuera de _publications: Quarto ignora las carpetas con "_"

args    <- commandArgs(trailingOnly = TRUE)
forzar  <- "--forzar" %in% args
offline <- "--offline" %in% args

# Dentro de `quarto render`, solo trabaja si se renderiza esta pagina
en_quarto <- Sys.getenv("QUARTO_PROJECT_RENDER_FILES")
if (nzchar(en_quarto) && !grepl("publications\\.qmd", en_quarto)) quit(save = "no")


# ---------------------------------------------------------------------
#  Utilidades de texto
# ---------------------------------------------------------------------

# Deja solo <i>, <sub> y <sup>; escapa todo lo demas
html_seguro <- function(x) {
  if (is.null(x)) return(NULL)
  x <- gsub("<(/?)(jats:)?(italic|em)>", "<\\1i>", x)
  x <- gsub("<(/?)(jats:)?(sub|sup)>", "<\\1\\3>", x)
  x <- gsub("<(/?)(i|sub|sup)>", "\u0001\\1\\2\u0002", x)
  x <- gsub("<[^>]+>", " ", x)
  x <- gsub("&(?!(amp|lt|gt|quot|#[0-9]+);)", "&amp;", x, perl = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\u0001", "<", gsub("\u0002", ">", x))
  trimws(gsub("\\s+", " ", x))
}

# *cursiva* (estilo markdown) -> <i>cursiva</i>
cursivas <- function(x) gsub("\\*([^*]+)\\*", "<i>\\1</i>", x)

desescapar <- function(x) gsub("&amp;", "&", gsub("&lt;", "<", gsub("&gt;", ">", x, fixed = TRUE), fixed = TRUE), fixed = TRUE)

sin_html <- function(x) trimws(gsub("\\s+", " ", gsub("<[^>]+>", "", x)))

guiones <- function(x) if (is.null(x)) NULL else gsub("(?<=\\w)-{1,2}(?=\\w)", "–", x, perl = TRUE)

limpiar_doi <- function(x) tolower(sub("^https?://(dx\\.)?doi\\.org/", "", x %||% ""))

# "Jean-Claude" -> "J.-C.", "Istvan Elek" -> "I. E.", "I.E." -> "I. E."
iniciales <- function(nombre) {
  if (is.null(nombre) || !nzchar(nombre)) return("")
  partes <- Filter(nzchar, strsplit(nombre, "[[:space:].]+")[[1]])
  paste(vapply(partes, function(p) {
    paste0(substr(strsplit(p, "-")[[1]], 1, 1), ".", collapse = "-")
  }, ""), collapse = " ")
}

unir_autores <- function(a) {
  a <- ifelse(grepl(MI_NOMBRE, a), paste0("<strong>", a, "</strong>"), a)
  n <- length(a)
  if (n <= 1) return(paste(a, collapse = ""))
  if (n == 2) return(paste(a, collapse = " and "))
  paste0(paste(a[-n], collapse = ", "), ", and ", a[n])
}

# Para emparejar preprints y datasets con su articulo
VACIAS <- c("the", "and", "for", "from", "with", "their", "between", "across",
            "into", "data", "dataset", "supplementary", "material", "del",
            "las", "los", "para", "una", "con", "por")
ascii <- function(t) tolower(gsub("['`^~\"]", "", iconv(sin_html(t), to = "ASCII//TRANSLIT")))
palabras <- function(t) {
  w <- strsplit(gsub("[^a-z0-9 ]", " ", ascii(t)), " +")[[1]]
  w <- w[nchar(w) > 2 & !w %in% VACIAS]
  unique(sub("s$", "", w))
}
parecido <- function(a, b) {
  pa <- palabras(a); pb <- palabras(b)
  if (!length(pa) || !length(pb)) return(0)
  length(intersect(pa, pb)) / min(length(pa), length(pb))
}


# ---------------------------------------------------------------------
#  Descarga (ORCID + OpenAlex + Crossref)
# ---------------------------------------------------------------------

leer_json <- function(url) {
  h <- new_handle(timeout = 30)
  handle_setheaders(h, Accept = "application/json",
                    `User-Agent` = "danielsangarci.com publications page")
  r <- curl_fetch_memory(url, handle = h)
  if (r$status_code != 200) stop("HTTP ", r$status_code, " en ", url)
  txt <- rawToChar(r$content)
  Encoding(txt) <- "UTF-8"
  fromJSON(txt, simplifyVector = FALSE)
}
intentar <- function(url) tryCatch(leer_json(url), error = function(e) NULL)

tipo_comun <- function(oa_tipo, cr_tipo) {
  t <- oa_tipo %||% cr_tipo %||% "article"
  switch(t,
    "posted-content" = , "preprint" = "preprint",
    "dataset" = "dataset",
    "book-chapter" = , "book-section" = , "book-part" = "chapter",
    "book" = , "monograph" = , "edited-book" = "book",
    "article")
}

resumen_openalex <- function(ii) {
  if (is.null(ii) || !length(ii)) return(NULL)
  pos <- unlist(lapply(names(ii), function(w) setNames(unlist(ii[[w]]), rep(w, length(ii[[w]])))))
  paste(names(sort(pos)), collapse = " ")
}

registro <- function(doi, w, cr) {
  fuente  <- w$primary_location$source$display_name
  autores <- if (length(cr$author)) {
    vapply(cr$author, function(a) {
      if (is.null(a$family)) return(a$name %||% "")
      ini <- iniciales(a$given)
      if (nzchar(ini)) paste0(a$family, ", ", ini) else a$family
    }, "")
  } else {
    vapply(w$authorships %||% list(), function(a) {
      p <- strsplit(a$author$display_name %||% a$raw_author_name %||% "", " ")[[1]]
      if (length(p) < 2) return(paste(p, collapse = ""))
      paste0(p[length(p)], ", ", iniciales(paste(p[-length(p)], collapse = " ")))
    }, "")
  }
  autores_bib <- if (length(cr$author)) {
    vapply(cr$author, function(a) {
      if (is.null(a$family)) a$name %||% "" else paste0(a$family, ", ", a$given %||% "")
    }, "")
  } else autores

  resumen <- cr$abstract
  if (!is.null(resumen)) resumen <- sub("^\\s*(Abstract|ABSTRACT|Summary)\\s*", "",
                                         sin_html(gsub("<jats:title>[^<]*</jats:title>", "", resumen)))
  resumen <- resumen %||% resumen_openalex(w$abstract_inverted_index)

  list(
    doi      = doi,
    tipo     = tipo_comun(w$type, cr$type),
    anio     = w$publication_year %||% primero(primero(cr$issued$`date-parts`)),
    fecha    = w$publication_date %||% NULL,
    titulo   = html_seguro(primero(cr$title) %||% w$title %||% w$display_name),
    autores  = as.list(autores),
    autores_bib = as.list(autores_bib),
    revista  = primero(cr$`container-title`) %||% (if (!is.null(fuente)) sub("\\s*\\(.*\\)$", "", fuente)),
    volumen  = cr[["volume"]] %||% w$biblio$volume,
    numero   = cr[["issue"]] %||% w$biblio$issue,
    paginas  = cr[["page"]] %||% cr$`article-number` %||%
               (if (!is.null(w$biblio$first_page)) paste(unique(c(w$biblio$first_page, w$biblio$last_page)), collapse = "-")),
    editorial = cr$publisher,
    resumen  = resumen,
    oa       = isTRUE(w$open_access$is_oa),
    oa_url   = w$open_access$oa_url,
    pdf      = w$best_oa_location$pdf_url,
    web      = w$primary_location$landing_page_url %||% paste0("https://doi.org/", doi),
    citas    = w$cited_by_count %||% 0
  )
}

descargar <- function() {
  message("Consultando ORCID, OpenAlex y Crossref...")
  orcid <- leer_json(sprintf("https://pub.orcid.org/v3.0/%s/works", ORCID))
  dois_orcid <- unique(unlist(lapply(orcid$group, function(g) {
    ids <- Filter(function(i) identical(i$`external-id-type`, "doi"), g$`external-ids`$`external-id`)
    vapply(ids, function(i) limpiar_doi(i$`external-id-value`), "")
  })))

  oa <- leer_json(sprintf("https://api.openalex.org/works?filter=author.orcid:%s&per-page=200", ORCID))$results
  oa <- Filter(function(w) !is.null(w$doi), oa)
  names(oa) <- vapply(oa, function(w) limpiar_doi(w$doi), "")
  for (d in setdiff(dois_orcid, names(oa))) {
    w <- intentar(paste0("https://api.openalex.org/works/doi:", d))
    oa[[d]] <- w %||% list()
  }

  lapply(names(oa), function(d) {
    w  <- oa[[d]]
    cr <- if (identical(w$type, "dataset")) NULL else intentar(paste0("https://api.crossref.org/works/", d))$message
    r  <- registro(d, w, cr)
    r$en_orcid <- d %in% dois_orcid
    r
  })
}


# ---------------------------------------------------------------------
#  Copia guardada: solo se consulta internet una vez al dia
# ---------------------------------------------------------------------

# La antiguedad de la copia se mide por la fecha del archivo, asi que
# cache.json solo cambia cuando cambian los datos (no cada dia).
cache <- if (file.exists(f_cache)) read_json(f_cache, simplifyVector = FALSE)$trabajos else NULL
edad  <- if (is.null(cache)) Inf else
  as.numeric(difftime(Sys.time(), file.mtime(f_cache), units = "secs"))

if (!offline && (forzar || edad > CADUCIDAD)) {
  fallo  <- FALSE
  nuevos <- tryCatch({
    n <- Filter(function(t) !is.null(t$titulo), descargar())
    if (!length(n)) stop("las fuentes no devolvieron ningun trabajo")
    n
  }, error = function(e) {
    message("No se pudo consultar internet (", conditionMessage(e), "). Uso la copia guardada.")
    fallo <<- TRUE
    NULL
  })
  # Con --forzar (lo que usa GitHub) el fallo no se esconde: la tarea sale en
  # rojo y GitHub avisa por email. La lista publicada no se toca.
  if (fallo && forzar) quit(save = "no", status = 1)
  if (!is.null(nuevos)) {
    # Si un trabajo que ya estaba deja de aparecer, casi siempre es un fallo
    # puntual de ORCID/OpenAlex: se conserva. Para quitarlo de verdad,
    # usa `hide: true` en manual.yml.
    dois_nuevos <- vapply(nuevos, function(t) t$doi, "")
    perdidos <- Filter(function(t) !t$doi %in% dois_nuevos, cache %||% list())
    for (t in perdidos) message("Aviso: ", t$doi, " ya no aparece en las fuentes; se mantiene.")
    nuevos <- c(nuevos, perdidos)

    if (identical(toJSON(nuevos, auto_unbox = TRUE, null = "null"),
                  toJSON(cache, auto_unbox = TRUE, null = "null"))) {
      Sys.setFileTime(f_cache, Sys.time())
    } else {
      write_json(list(trabajos = nuevos), f_cache, auto_unbox = TRUE, pretty = TRUE, null = "null")
    }
    cache <- nuevos
  }
}

# Sin datos de internet ni copia guardada: no se toca la lista publicada
# (en GitHub el fallo queda marcado en rojo y avisa por email).
if (is.null(cache)) {
  message("Sin datos de ORCID/OpenAlex y sin copia guardada: publications.yml no se modifica.")
  quit(save = "no", status = if (forzar) 1 else 0)
}
trabajos <- cache
manual   <- tryCatch(read_yaml(f_manual), error = function(e) {
  message("\nERROR en _publications/manual.yml: ", sub(".*\\) ", "", conditionMessage(e)),
          "\nRevisa la sangria, las comillas y que ningun DOI este repetido.",
          "\nLa web no se actualiza hasta que se corrija.\n")
  quit(save = "no", status = 1)
})
ajustes  <- manual$ajustes %||% list()
names(ajustes) <- tolower(names(ajustes))
opciones <- manual$options %||% list()
aj_de    <- function(doi) if (is.null(doi)) NULL else ajustes[[tolower(doi)]]
es_no    <- function(x) identical(tolower(as.character(x %||% "")), "none") || isFALSE(x)


# ---------------------------------------------------------------------
#  Emparejar preprints y datasets con su articulo
# ---------------------------------------------------------------------

# Solo salen como entrada propia los trabajos que estan en tu ORCID: OpenAlex
# a veces atribuye trabajos ajenos. Lo que solo esta en OpenAlex puede servir
# como boton (Preprint, Data) de un trabajo tuyo, pero nunca como entrada.
en_orcid  <- function(t) !isFALSE(t$en_orcid)
tipos     <- vapply(trabajos, function(t) t$tipo, "")
datasets  <- trabajos[tipos == "dataset"]
preprints <- trabajos[tipos == "preprint"]
entradas  <- Filter(en_orcid, trabajos[!tipos %in% c("dataset", "preprint")])

# A que entrada se engancha un preprint o un dataset: la que diga `link_to`
# en manual.yml, o si no la de titulo mas parecido (por encima del umbral)
destino <- function(t, umbral) {
  forzado <- aj_de(t$doi)$link_to
  if (es_no(forzado)) return(NA)
  dois <- vapply(entradas, function(e) e$doi %||% "", "")
  if (!is.null(forzado)) {
    i <- match(limpiar_doi(forzado), dois)
    if (is.na(i)) message("Aviso: link_to de ", t$doi, " apunta a ", forzado, ", que no esta en la lista.")
    return(i)
  }
  sims <- vapply(entradas, function(e) parecido(t$titulo, e$titulo), 0)
  if (length(sims) && max(sims) >= umbral) which.max(sims) else NA
}

# Un preprint de un articulo ya publicado pasa a ser su boton "Preprint"
for (p in preprints) {
  i <- destino(p, 0.75)
  if (!is.na(i)) {
    entradas[[i]]$preprint <- entradas[[i]]$preprint %||% paste0("https://doi.org/", p$doi)
  } else if (en_orcid(p)) {
    entradas[[length(entradas) + 1]] <- p
  }
}
for (d in datasets) {
  i <- destino(d, 0.6)
  if (!is.na(i)) entradas[[i]]$datos <- c(entradas[[i]]$datos, paste0("https://doi.org/", d$doi))
}


# ---------------------------------------------------------------------
#  Formato final de cada entrada
# ---------------------------------------------------------------------

ETIQUETAS <- c(article = "Journal article", preprint = "Preprint",
               chapter = "Book chapter", book = "Book")

bibtex <- function(e, autores_bib) {
  tipo_bib <- switch(e$tipo, chapter = "incollection", book = "book",
                     preprint = "misc", "article")
  apellido <- ascii(sub(",.*", "", autores_bib[1] %||% "anon"))
  palabra  <- primero(setdiff(regmatches(ascii(e$titulo), gregexpr("[a-z]{4,}", ascii(e$titulo)))[[1]], VACIAS)) %||% ""
  clave    <- paste0(gsub("[^a-z]", "", apellido), e$anio, palabra)
  esc <- function(x) gsub("([&%$#_])", "\\\\\\1", desescapar(sin_html(x)))
  campos <- c(
    title     = esc(e$titulo),
    author    = paste(autores_bib, collapse = " and "),
    year      = e$anio,
    journal   = if (tipo_bib == "article") esc(e$revista),
    booktitle = if (tipo_bib == "incollection") esc(e$revista),
    howpublished = if (tipo_bib == "misc") esc(e$revista),
    editor    = if (!is.null(e$editores)) sub("\\s*\\(Eds?\\.\\)", "", e$editores),
    volume    = e$volumen, number = e$numero,
    pages     = if (!is.null(e$paginas)) gsub("[-–]+", "--", e$paginas),
    publisher = if (tipo_bib %in% c("incollection", "book")) e$editorial,
    doi       = e$doi
  )
  campos <- campos[!vapply(campos, is.null, TRUE) & nzchar(campos)]
  paste0("@", tipo_bib, "{", clave, ",\n",
         paste0("  ", names(campos), " = {", campos, "}", collapse = ",\n"),
         "\n}")
}

formatear <- function(e) {
  aj <- aj_de(e$doi)
  if (isTRUE(aj$hide)) return(NULL)

  # Correcciones a mano de los datos importados
  if (!is.null(aj$title))     e$titulo    <- html_seguro(cursivas(aj$title))
  if (!is.null(aj$authors))   e$autores   <- e$autores_bib <- as.list(aj$authors)
  if (!is.null(aj$venue))     e$revista   <- aj$venue
  if (!is.null(aj$volume))    e$volumen   <- as.character(aj$volume)
  if (!is.null(aj$issue))     e$numero    <- as.character(aj$issue)
  if (!is.null(aj$pages))     e$paginas   <- as.character(aj$pages)
  if (!is.null(aj$year))      { e$anio <- aj$year; e$fecha <- NULL }
  if (!is.null(aj$type))      e$tipo      <- aj$type
  if (!is.null(aj$editors))   e$editores  <- aj$editors
  if (!is.null(aj$publisher)) e$editorial <- aj$publisher
  titulo <- e$titulo
  paginas <- guiones(e$paginas)

  revista <- if (!is.null(e$revista)) paste0("<i>", html_seguro(e$revista), "</i>")
  detalle <- if (e$tipo == "chapter") {
    paste0("In ", if (!is.null(e$editores)) paste0(html_seguro(e$editores), ", "), revista,
           if (!is.null(paginas)) paste0(" (pp. ", paginas, ")"),
           if (!is.null(e$editorial)) paste0(". ", html_seguro(e$editorial)))
  } else {
    vol <- paste0(e$volumen %||% "", if (!is.null(e$numero)) paste0("(", e$numero, ")") else "")
    paste(c(revista, if (nzchar(vol)) vol, paginas), collapse = ", ")
  }

  autores <- unlist(e$autores)
  # `data: none` quita los datasets detectados; una URL o una lista los suma
  datos   <- if (es_no(aj$data)) NULL else unique(c(unlist(e$datos), unlist(aj$data)))
  web     <- e$url %||% (if (!is.null(e$doi)) paste0("https://doi.org/", e$doi)) %||% e$pdf

  item <- list(
    type       = e$tipo,
    type_label = unname(ETIQUETAS[e$tipo]) %||% "Publication",
    year       = as.integer(e$anio),
    title      = titulo,
    url        = web,
    authors    = unir_autores(autores),
    venue      = detalle,
    doi        = e$doi,
    pdf        = aj$pdf %||% e$pdf,
    # Copia gratuita en un repositorio, si no hay PDF directo
    free       = if (is.null(aj$pdf %||% e$pdf) && !is.null(e$oa_url) && !is.null(e$doi) &&
                     !grepl(e$doi, tolower(e$oa_url), fixed = TRUE)) e$oa_url,
    preprint   = if (es_no(aj$preprint)) NULL else aj$preprint %||% e$preprint,
    data       = if (length(datos)) as.list(datos),
    code       = aj$code,
    note       = if (!is.null(aj$note)) html_seguro(cursivas(aj$note)),
    oa         = if (isTRUE(e$oa)) TRUE,
    cited      = if (!isFALSE(opciones$citations) && !is.null(e$citas) && e$citas > 0) as.integer(e$citas),
    abstract   = if (!is.null(aj$abstract %||% e$resumen)) html_seguro(cursivas(aj$abstract %||% e$resumen)),
    bibtex     = bibtex(e, unlist(e$autores_bib %||% e$autores)),
    orden      = e$fecha %||% paste0(e$anio, "-00-00")
  )
  Filter(Negate(is.null), item)
}

extra <- lapply(manual$extra %||% list(), function(x) list(
  tipo = x$type %||% "article", anio = x$year,
  titulo = html_seguro(cursivas(x$title)),
  autores = as.list(x$authors), revista = x$venue,
  volumen = x$volume, numero = x$issue, paginas = x$pages,
  editores = x$editors, editorial = x$publisher,
  url = x$url, pdf = x$pdf, doi = x$doi
))

items <- Filter(Negate(is.null), lapply(c(entradas, extra), formatear))
orden <- order(vapply(items, function(i) i$orden, ""), decreasing = TRUE)
items <- lapply(items[orden], function(i) { i$orden <- NULL; i })

nuevo <- paste0(
  "# ARCHIVO GENERADO por _publications/actualizar.R -- no editar a mano.\n",
  "# Para cambiar algo, edita _publications/manual.yml.\n",
  as.yaml(items, indent.mapping.sequence = TRUE,
          handlers = list(logical = function(x) structure(ifelse(x, "true", "false"), class = "verbatim")))
)
viejo <- if (file.exists(f_salida)) paste(readLines(f_salida, encoding = "UTF-8", warn = FALSE), collapse = "\n") else ""

# Trabajos que aparecen por primera vez en la web. GitHub abre un aviso
# (issue) con ellos para que compruebes que son tuyos y estan bien.
if (nzchar(viejo)) {
  clave_de <- function(i) i$doi %||% sin_html(i$title)
  antes    <- vapply(yaml.load(viejo) %||% list(), clave_de, "")
  novedad  <- Filter(function(i) !clave_de(i) %in% antes, items)
  lineas   <- vapply(novedad, function(i) paste0(
    "- ", desescapar(sin_html(i$title)), " (", i$year, ", ", i$type_label, ")",
    if (!is.null(i$doi)) paste0(" https://doi.org/", i$doi) else ""), "")
  for (l in lineas) message("Nuevo en la web: ", sub("^- ", "", l))
  f_nov <- Sys.getenv("PUBS_NOVEDADES")
  if (length(lineas) && nzchar(f_nov)) writeLines(lineas, f_nov, useBytes = TRUE)
}
if (!identical(trimws(viejo), trimws(nuevo))) {
  writeLines(nuevo, f_salida, useBytes = TRUE)
  message("publications.yml actualizado: ", length(items), " trabajos.")
} else {
  message("publications.yml sin cambios (", length(items), " trabajos).")
}
