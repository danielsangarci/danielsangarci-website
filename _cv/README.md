# Datos del CV

Todo el contenido del CV está en los archivos `.yml` de esta carpeta. De aquí
salen la página `cv.qmd` de la web (solo en inglés) y cuatro PDF:

| PDF               | Versión                                   |
|-------------------|-------------------------------------------|
| `cv.pdf`          | completo, inglés (el que enlaza la web)   |
| `cv-short.pdf`    | resumido (2 páginas), inglés              |
| `cv-es.pdf`       | completo, español                         |
| `cv-short-es.pdf` | resumido (2 páginas), español             |

Para añadir algo, edita el archivo de la sección y vuelve a generar, desde la
raíz de la web:

```bash
Rscript _cv/pdf/render_cv.R              # los cuatro PDF
Rscript _cv/pdf/render_cv.R es           # solo los dos en español
Rscript _cv/pdf/render_cv.R short        # solo los dos resumidos
Rscript _cv/pdf/render_cv.R es short     # solo cv-short-es.pdf
quarto render cv.qmd                     # la página de la web
```

Desde RStudio: `source("_cv/pdf/render_cv.R"); generar_cv("es", "short")`.

Los PDF quedan en la raíz de la web. Los de inglés se copian también a
`docs/` y se publican (la web enlaza `cv.pdf`). Los de español **no se
publican**: no van a `docs/` y están en `.gitignore`, así que tampoco se suben
a GitHub. Son solo para enviarlos tú.

Las publicaciones no se escriben aquí: salen de `publications.yml`, que se
actualiza solo desde ORCID (ver `_publications/`).

| Archivo              | Sección                                             |
|----------------------|-----------------------------------------------------|
| `profile.yml`        | nombre, puesto, contacto, resumen de la web, origen del mapa |
| `experience.yml`     | puestos de trabajo                                  |
| `education.yml`      | estudios                                            |
| `stays.yml`          | estancias de investigación                          |
| `internships.yml`    | prácticas                                           |
| `conferences.yml`    | charlas y pósters                                   |
| `funding.yml`        | proyectos y becas                                   |
| `awards.yml`         | premios                                             |
| `service.yml`        | organización de eventos, revisiones, workshops      |
| `memberships.yml`    | sociedades                                          |
| `courses.yml`        | cursos                                              |
| `skills.yml`         | habilidades técnicas e idiomas                      |
| `references.yml`     | referencias (nombre, relación, institución)         |
| `private.yml`        | teléfono y correos de las referencias (NO se sube a GitHub) |

La plantilla del PDF está en `pdf/cv.Rmd` (con la clase LaTeX, las fuentes y
la foto). Las funciones que leen y dan formato a los datos, para el PDF y la
web, están en `leer_cv.R`.

## Reglas comunes

- **Fechas** en formato `AAAA-MM` (o solo `AAAA`). Para un puesto actual,
  `end: present`. El texto ("Jan 2025 – present") y el orden (de más reciente
  a más antiguo) se calculan solos: da igual en qué orden escribas las
  entradas.
- **Cursiva y negrita** con `*texto*` y `**texto**`, como en Markdown. Sirve
  en la web y en el PDF.
- **`description`** es el texto del PDF. **`description_web`** es opcional: una
  versión más corta para la web. Si no está, la web usa `description`.
- **`short: false`** quita la entrada del CV resumido (`cv-short.pdf`). Por
  defecto todo sale en los dos.
- **`web: false`** quita la entrada de la web (sigue en el PDF).
- **`coords: [latitud, longitud]`** pinta el lugar en el mapa "My journey" del
  PDF (en `education.yml`, `experience.yml` y `stays.yml`).

Si falta un campo obligatorio o una fecha está mal escrita, la compilación se
para y dice qué archivo y qué entrada hay que corregir.

## CVN (FECYT)

El CVN y el CVA solo se pueden generar en el editor de FECYT, pero las
publicaciones y los congresos no hace falta teclearlos:

```bash
Rscript _cv/cvn/exportar_cvn.R
```

crea `_cv/cvn/publicaciones.bib` y `_cv/cvn/congresos.bib`. En el editor:
**Importar** → Formato a importar: **BibTeX** → elegir el archivo →
**Importar** → **Confirmar** → marcar las entradas → **Añadir**. Si los
acentos salen mal, usa los archivos `-ascii.bib`.

## Español

Los datos se escriben en inglés. La traducción de cada campo va justo debajo,
con el sufijo `_es`:

```yaml
- title: PhD in Biology
  title_es: Doctorado en Biología
  description: PhD Thesis: ...
  description_es: Tesis doctoral: ...
```

- Si falta una traducción, el PDF en español usa el texto en inglés y, al
  generarlo, avisa de qué falta («faltan N traducciones…»).
- **Ciudades y países** se traducen solos (Seville → Sevilla, Poland →
  Polonia) con la lista `places` de `textos.yml`. Si falta uno, añádelo ahí o
  escribe `city_es` / `country_es` en la entrada.
- **Títulos de sección, meses y frases fijas** están en `textos.yml`.
- **Publicaciones y congresos** se citan en su idioma original; solo cambia el
  formato de la cita (p. ej. «En» en vez de «In»).
- Para añadir otro idioma (p. ej. francés), añade `fr:` en `textos.yml`, los
  campos `_fr` que quieras y `"fr"` a `IDIOMAS` en `pdf/render_cv.R`.
