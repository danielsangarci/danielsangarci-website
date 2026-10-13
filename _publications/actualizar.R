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
SCHOLAR   <- "5EQSDqwAAAAJ"                            # id del perfil de Google Scholar
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
  x <- gsub("\\s+", " ", x)
  x <- gsub("\\(\\s+", "(", gsub("\\s+\\)", ")", x))
  x <- gsub("\\s+([,;:])", "\\1", x)
  trimws(x)
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

leer_json <- function(url, cabeceras = list()) {
  h <- new_handle(timeout = 30)
  do.call(handle_setheaders, c(list(h, Accept = "application/json",
                                    `User-Agent` = "danielsangarci.com publications page"), cabeceras))
  r <- curl_fetch_memory(url, handle = h)
  if (r$status_code != 200) stop("HTTP ", r$status_code, " en ", url)
  txt <- rawToChar(r$content)
  Encoding(txt) <- "UTF-8"
  fromJSON(txt, simplifyVector = FALSE)
}
intentar <- function(url) tryCatch(leer_json(url), error = function(e) NULL)

tipo_comun <- function(oa_tipo, cr_tipo) {
  # Crossref primero: es quien registra el DOI. OpenAlex fusiona a veces un
  # preprint con su articulo publicado y entonces lo llama "article".
  t <- cr_tipo %||% oa_tipo %||% "article"
  if (identical(t, "journal-article")) t <- "article"
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

  tipo <- tipo_comun(w$type, cr$type)
  revista <- if (tipo == "preprint") {
    # El servidor del preprint (bioRxiv, SSRN...), no la revista de la
    # version publicada con la que OpenAlex lo haya fusionado
    primero(cr$institution)$name %||% primero(cr$`container-title`) %||%
      (if (!is.null(fuente)) sub("\\s*\\(.*\\)$", "", fuente))
  } else {
    primero(cr$`container-title`) %||% (if (!is.null(fuente)) sub("\\s*\\(.*\\)$", "", fuente))
  }
  preprint_de <- primero(Filter(function(r) identical(r$`id-type`, "doi"),
                                cr$relation$`is-preprint-of` %||% list()))$id

  list(
    doi      = doi,
    tipo     = tipo,
    openalex = w$id,
    preprint_de = if (!is.null(preprint_de)) limpiar_doi(preprint_de),
    anio     = w$publication_year %||% primero(primero(cr$issued$`date-parts`)),
    fecha    = w$publication_date %||% NULL,
    titulo   = html_seguro(primero(cr$title) %||% w$title %||% w$display_name),
    autores  = as.list(autores),
    autores_bib = as.list(autores_bib),
    revista  = revista,
    volumen  = cr[["volume"]] %||% (if (tipo != "preprint") w$biblio$volume),
    numero   = cr[["issue"]] %||% (if (tipo != "preprint") w$biblio$issue),
    paginas  = cr[["page"]] %||% cr$`article-number` %||%
               (if (tipo != "preprint" && !is.null(w$biblio$first_page)) paste(unique(c(w$biblio$first_page, w$biblio$last_page)), collapse = "-")),
    editorial = cr$publisher,
    resumen  = resumen,
    oa       = isTRUE(w$open_access$is_oa),
    oa_url   = w$open_access$oa_url,
    pdf      = w$best_oa_location$pdf_url,
    web      = w$primary_location$landing_page_url %||% paste0("https://doi.org/", doi),
    citas    = w$cited_by_count %||% 0,
    citas_anio = lapply(w$counts_by_year %||% list(), function(c) list(anio = c$year, n = c$cited_by_count))
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
    if (r$tipo != "dataset") r <- c(r, semantic_scholar(d))
    r
  })
}

# Citas en Semantic Scholar. Sin clave admite pocas consultas por segundo:
# se espera entre una y otra y se reintenta una vez. Si aun asi falla, se
# conservan las cifras de la semana anterior (ver la copia guardada).
# Con una clave gratuita (variable S2_API_KEY) va mas holgado.
semantic_scholar <- function(doi) {
  url <- paste0("https://api.semanticscholar.org/graph/v1/paper/DOI:", doi,
                "?fields=citationCount,influentialCitationCount,url")
  clave <- Sys.getenv("S2_API_KEY")
  cab <- if (nzchar(clave)) list(`x-api-key` = clave) else list()
  for (intento in 1:2) {
    Sys.sleep(if (intento == 1) 1.2 else 4)
    r <- tryCatch(leer_json(url, cab), error = function(e) e)
    if (!inherits(r, "error")) {
      return(list(s2_citas = r$citationCount %||% 0,
                  s2_influyentes = r$influentialCitationCount %||% 0,
                  s2_url = r$url))
    }
    if (!grepl("HTTP 429", conditionMessage(r))) break   # 404: no esta en S2
  }
  list()
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

    # Semantic Scholar falla a veces sin clave: se conservan sus cifras previas
    previos <- setNames(cache %||% list(), vapply(cache %||% list(), function(t) t$doi, ""))
    nuevos <- lapply(nuevos, function(t) {
      viejo <- previos[[t$doi]]
      if (is.null(t$s2_citas) && !is.null(viejo$s2_citas)) {
        t[c("s2_citas", "s2_influyentes", "s2_url")] <- viejo[c("s2_citas", "s2_influyentes", "s2_url")]
      }
      t
    })

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

# Dos DOI de la misma ficha de OpenAlex son el mismo trabajo: se queda uno
ids_vistos <- character()
entradas <- Filter(function(e) {
  id <- e$openalex %||% ""
  if (nzchar(id) && id %in% ids_vistos) {
    message("Aviso: ", e$doi, " es el mismo trabajo que otro de la lista; se omite.")
    return(FALSE)
  }
  ids_vistos <<- c(ids_vistos, id)
  TRUE
}, entradas)

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
  # La revista o el servidor declaran en Crossref de que articulo es preprint
  if (!is.null(t$preprint_de) && !is.na(i <- match(t$preprint_de, dois))) return(i)
  # OpenAlex los ha fusionado en la misma ficha
  ids <- vapply(entradas, function(e) e$openalex %||% "", "")
  if (!is.null(t$openalex) && !is.na(i <- match(t$openalex, ids))) return(i)
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
#  Google Scholar (a traves de SerpApi)
# ---------------------------------------------------------------------
# Google Scholar no tiene API y bloquea las consultas automaticas: SerpApi
# lo consulta por nosotros. Necesita la clave en la variable SERPAPI_KEY
# (en GitHub, como secreto; en local, opcional en ~/.Renviron). Sin clave,
# o si falla, se usan las ultimas cifras guardadas en scholar.json.
# Para no gastar consultas del plan gratuito, en local se consulta como
# mucho una vez cada 6 dias; en GitHub (--forzar), una vez por semana.

f_scholar  <- file.path(carpeta, "scholar.json")
scholar    <- if (file.exists(f_scholar)) read_json(f_scholar, simplifyVector = FALSE) else NULL
clave_serp <- Sys.getenv("SERPAPI_KEY")
edad_sch   <- if (is.null(scholar)) Inf else
  as.numeric(difftime(Sys.time(), file.mtime(f_scholar), units = "days"))

descargar_scholar <- function() {
  message("Consultando Google Scholar (SerpApi)...")
  r <- leer_json(paste0("https://serpapi.com/search.json?engine=google_scholar_author&hl=en&num=100",
                        "&author_id=", SCHOLAR, "&api_key=", clave_serp))
  if (!is.null(r$error)) stop(r$error)
  if (!length(r$articles)) stop("SerpApi no devolvio articulos")
  dato <- function(nombre) {
    fila <- Filter(function(f) !is.null(f[[nombre]]), r$cited_by$table %||% list())
    primero(fila)[[nombre]]$all
  }
  list(
    total = dato("citations"),
    h     = dato("h_index"),
    articulos = lapply(r$articles, function(a) list(
      titulo = a$title, anio = suppressWarnings(as.integer(a$year)),
      citas  = a$cited_by$value %||% 0))
  )
}

if (nzchar(clave_serp) && !offline && (forzar || edad_sch > 6)) {
  nuevo_sch <- tryCatch(descargar_scholar(), error = function(e) {
    message("No se pudo consultar Google Scholar (", sub("api_key=[^&]*", "api_key=***", conditionMessage(e)),
            "). Uso las ultimas cifras guardadas.")
    NULL
  })
  if (!is.null(nuevo_sch)) {
    if (identical(toJSON(nuevo_sch, auto_unbox = TRUE, null = "null"),
                  toJSON(scholar, auto_unbox = TRUE, null = "null"))) {
      Sys.setFileTime(f_scholar, Sys.time())
    } else {
      write_json(nuevo_sch, f_scholar, auto_unbox = TRUE, pretty = TRUE, null = "null")
    }
    scholar <- nuevo_sch
  }
}

# Citas de Scholar de un trabajo, emparejando por titulo (Scholar no da DOI).
# Si Scholar tiene varias versiones del mismo trabajo, se toma la mayor.
citas_scholar <- function(titulo, anio) {
  if (is.null(scholar) || is.null(titulo)) return(NULL)
  n <- vapply(scholar$articulos, function(a) {
    misma <- parecido(titulo, a$titulo) >= 0.8 &&
      (is.null(a$anio) || is.null(anio) || abs(a$anio - as.integer(anio)) <= 2)
    if (misma) as.numeric(a$citas %||% 0) else NA_real_
  }, 0)
  if (all(is.na(n))) NULL else max(n, na.rm = TRUE)
}


# ---------------------------------------------------------------------
#  Cursivas automaticas de los nombres cientificos (GBIF)
# ---------------------------------------------------------------------
# Muchas revistas registran el titulo y el resumen sin formato. Aqui se
# buscan los candidatos a nombre cientifico y se ponen en cursiva solo si
# GBIF los reconoce exactamente:
#   - binomios: "Lasius niger"
#   - abreviados: "C. floricola", si en el mismo texto hay un genero con C
#     y GBIF reconoce la especie completa
#   - generos sueltos: "Myrmica", solo si ese genero aparece en algun
#     binomio de la lista, o va seguido de "sp."/"spp." y GBIF lo reconoce
# Las respuestas de GBIF se guardan en gbif.json: cada nombre se consulta
# una sola vez. Excepciones en manual.yml (`italics`, `no_italics`).

f_gbif <- file.path(carpeta, "gbif.json")
gbif   <- if (file.exists(f_gbif)) read_json(f_gbif, simplifyVector = FALSE) else list()
gbif_n <- length(gbif)
si_cursiva <- unlist(manual$italics %||% list())
no_cursiva <- unlist(manual$no_italics %||% list())
NO_EPITETO <- c("and", "the", "for", "from", "with", "are", "were", "was", "its",
                "their", "this", "that", "into", "between", "across", "after",
                "del", "las", "los", "para", "con", "por", "una", "que", "como")

consulta_gbif <- function(clave, url, ok) {
  if (!is.null(gbif[[clave]])) return(isTRUE(gbif[[clave]]))
  if (offline) return(FALSE)
  r <- intentar(url)
  if (is.null(r)) return(FALSE)            # sin respuesta: se reintenta otro dia
  res <- isTRUE(ok(r))
  gbif[[clave]] <<- res
  res
}
url_gbif <- function(nombre, rango = NULL) paste0(
  "https://api.gbif.org/v1/species/match?strict=true&name=", URLencode(nombre),
  if (!is.null(rango)) paste0("&rank=", rango))
es_especie <- function(nombre) {
  if (nombre %in% si_cursiva) return(TRUE)
  if (nombre %in% no_cursiva) return(FALSE)
  consulta_gbif(nombre, url_gbif(nombre), function(r)
    identical(r$matchType, "EXACT") && isTRUE(r$rank %in% c("SPECIES", "SUBSPECIES", "VARIETY")))
}
es_genero <- function(nombre) {
  if (nombre %in% si_cursiva) return(TRUE)
  if (nombre %in% no_cursiva) return(FALSE)
  consulta_gbif(paste0("genus:", nombre), url_gbif(nombre, "GENUS"), function(r)
    identical(r$matchType, "EXACT") && identical(r$rank, "GENUS"))
}

# Aplica `f` solo al texto que no esta ya dentro de <i>...</i>
fuera_de_cursiva <- function(x, f) {
  m <- gregexpr("<i>.*?</i>", x, perl = TRUE)
  dentro <- regmatches(x, m)[[1]]
  fuera  <- vapply(regmatches(x, m, invert = TRUE)[[1]], f, "", USE.NAMES = FALSE)
  paste0(c(rbind(fuera, c(dentro, ""))), collapse = "")
}
reemplazar <- function(x, patron, f) {
  m <- gregexpr(patron, x, perl = TRUE)
  if (m[[1]][1] == -1) return(x)
  regmatches(x, m) <- list(vapply(regmatches(x, m)[[1]], f, "", USE.NAMES = FALSE))
  x
}
PATRON_BINOMIO <- "\\b[A-Z][a-z]+ [a-z][a-z-]{2,}\\b"
binomio_valido <- function(b) {
  p <- strsplit(b, " ")[[1]]
  !p[2] %in% NO_EPITETO && es_especie(b)
}

# Generos confirmados en toda la lista: los de los binomios reconocidos y
# los que la revista ya puso en cursiva
generos_de <- function(x) {
  if (is.null(x)) return(character())
  plano <- sin_html(x)
  bin <- regmatches(plano, gregexpr(PATRON_BINOMIO, plano, perl = TRUE))[[1]]
  bin <- Filter(binomio_valido, bin)
  ya  <- regmatches(x, gregexpr("(?<=<i>)[A-Z][a-z]+(?=[ <])", x, perl = TRUE))[[1]]
  unique(c(sub(" .*", "", bin), ya, sub(" .*", "", si_cursiva)))
}
GENEROS <- unique(unlist(lapply(entradas, function(e) c(generos_de(e$titulo), generos_de(e$resumen)))))
GENEROS <- setdiff(GENEROS, no_cursiva)

cursivas_auto <- function(x) {
  if (is.null(x)) return(NULL)
  propios <- intersect(generos_de(x), GENEROS)
  # 1. binomios
  x <- fuera_de_cursiva(x, function(t) reemplazar(t, PATRON_BINOMIO, function(b)
    if (binomio_valido(b)) paste0("<i>", b, "</i>") else b))
  # 2. abreviados: "C. floricola"
  x <- fuera_de_cursiva(x, function(t) reemplazar(t, "\\b[A-Z]\\. ?[a-z][a-z-]{2,}\\b", function(a) {
    letra <- substr(a, 1, 1); epiteto <- sub("^[A-Z]\\. ?", "", a)
    if (epiteto %in% NO_EPITETO) return(a)
    for (g in c(propios, setdiff(GENEROS, propios))) {
      if (startsWith(g, letra) && es_especie(paste(g, epiteto))) return(paste0("<i>", a, "</i>"))
    }
    a
  }))
  # 3. generos sueltos
  x <- fuera_de_cursiva(x, function(t) reemplazar(t, "\\b[A-Z][a-z]{3,}\\b(?= spp?\\.)|\\b[A-Z][a-z]{3,}\\b", function(g) {
    if (g %in% no_cursiva) return(g)
    if (g %in% GENEROS) return(paste0("<i>", g, "</i>"))
    g
  }))
  x <- fuera_de_cursiva(x, function(t) reemplazar(t, "\\b[A-Z][a-z]{3,}(?= spp?\\.)", function(g)
    if (!g %in% no_cursiva && es_genero(g)) paste0("<i>", g, "</i>") else g))
  gsub("</i> <i>", " ", x, fixed = TRUE)
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
  if (is.null(aj$title) && !isTRUE(e$manual)) e$titulo <- cursivas_auto(e$titulo)
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

  preprint <- if (es_no(aj$preprint)) NULL else aj$preprint %||% e$preprint
  free <- if (is.null(aj$pdf %||% e$pdf) && !is.null(e$oa_url) && !is.null(e$doi) &&
              !grepl(e$doi, tolower(e$oa_url), fixed = TRUE) &&
              !identical(tolower(e$oa_url), tolower(preprint %||% ""))) e$oa_url

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
    free       = free,
    preprint   = preprint,
    data       = if (length(datos)) as.list(datos),
    code       = aj$code,
    note       = if (!is.null(aj$note)) html_seguro(cursivas(aj$note)),
    oa         = if (isTRUE(e$oa)) TRUE,
    # Metricas (se apagan todas con `options: citations: false`)
    metrics    = if (!isFALSE(opciones$citations)) TRUE,
    cited      = if (!isFALSE(opciones$citations) && !is.null(e$citas) && e$citas > 0) as.integer(e$citas),
    cites_by_year = if (!isFALSE(opciones$citations) && length(e$citas_anio))
                      lapply(e$citas_anio, function(c) list(year = as.integer(c$anio), n = as.integer(c$n))),
    s2_cited   = if (!isFALSE(opciones$citations) && !is.null(e$s2_citas)) as.integer(e$s2_citas),
    s2_influential = if (!isFALSE(opciones$citations) && !is.null(e$s2_influyentes)) as.integer(e$s2_influyentes),
    s2_url     = if (!isFALSE(opciones$citations)) e$s2_url,
    # Google Scholar: lo escrito a mano en manual.yml manda sobre SerpApi
    scholar    = if (!isFALSE(opciones$citations)) {
                   n <- aj$scholar %||% e$scholar %||% citas_scholar(e$titulo, e$anio)
                   if (!is.null(n) && n > 0) as.integer(n)
                 },
    abstract   = if (!is.null(aj$abstract)) html_seguro(cursivas(aj$abstract)) else
                   if (!is.null(e$resumen)) cursivas_auto(html_seguro(e$resumen)),
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
  url = x$url, pdf = x$pdf, doi = x$doi, scholar = x$scholar, manual = TRUE
))

items <- Filter(Negate(is.null), lapply(c(entradas, extra), formatear))
orden <- order(vapply(items, function(i) i$orden, ""), decreasing = TRUE)
items <- lapply(items[orden], function(i) { i$orden <- NULL; i })

# Total del perfil de Google Scholar, para el resumen de arriba (va en la
# primera entrada porque la plantilla solo recibe la lista de trabajos)
if (length(items) && !isFALSE(opciones$citations) && !is.null(scholar$total)) {
  items[[1]]$scholar_total <- as.integer(scholar$total)
  if (!is.null(scholar$h)) items[[1]]$scholar_h <- as.integer(scholar$h)
}

if (length(gbif) != gbif_n) {
  gbif <- gbif[order(names(gbif))]
  write_json(gbif, f_gbif, auto_unbox = TRUE, pretty = TRUE)
}

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
