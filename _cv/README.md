# Datos del CV

Todo el contenido del CV está en los archivos `.yml` de esta carpeta. De aquí
salen la página `cv.qmd` de la web y los PDF (`cv.pdf` y `cv-short.pdf`).
Para añadir algo, edita el archivo de la sección y vuelve a generar:

```bash
Rscript _cv/pdf/render_cv.R      # los dos PDF
quarto render cv.qmd             # la página de la web
```

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
