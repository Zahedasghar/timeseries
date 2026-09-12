# Time Series Econometrics & Macroeconomic Forecasting (EC614)

Course materials, hands-on R labs and applied Pakistani case studies for
**EC614 — Macroeconomic Forecasting**, taught by Prof. Dr. Zahid Asghar,
School of Economics, Quaid-i-Azam University, Islamabad.

**Website: <https://zahedasghar.github.io/timeseries/>**

---

## Repository layout

| Folder | Contents |
|---|---|
| `lectures/` | The 14 weekly lecture documents, `week-01-…` to `week-14-…`, in teaching order. These make up the site. |
| `labs/` | Hands-on R walkthroughs. `labs/r-foundations/` covers R itself for students starting from zero. |
| `applications/` | Applied Pakistan studies: remittances, exchange rate, exports and trade, tax revenue, solar prices, KSE. |
| `assessments/` | Exam papers, assignment briefs, term paper specification. |
| `slides/` | Standalone decks (PPTX and revealjs) not part of the lecture sequence. |
| `R/` | Standalone scripts — `setup/`, `textbook/` (Enders, Stock–Watson), `methods/`, `applications/`. |
| `data/` | Datasets used across the course. `data/large/` holds files too big for git (ignored). |
| `references/` | Reading material, the IMF forecast-uncertainty module, xts/zoo chapters. |
| `shiny/` | Small interactive Shiny apps. |
| `assets/` | Site theme, shared SCSS/CSS, images, LaTeX beamer themes, bibliography. |
| `docs/` | **Generated site output. Never edit by hand — `quarto render` overwrites it.** |
| `_archive/` | Superseded drafts, previous renders and build artefacts. Git-ignored; delete when you no longer want them. |

## Building the site

```bash
quarto render            # full site into docs/
quarto preview           # live preview while editing
quarto render lectures/week-11-vector-autoregressions.qmd   # one document
```

GitHub Pages is served from the `docs/` folder on `main`
(*Settings → Pages → Source: Deploy from a branch → main → /docs*).
`docs/.nojekyll` stops Jekyll from stripping Quarto's `_files` directories.

Chunk output is frozen under `_freeze/`, so a rebuild only re-executes documents
you have actually edited. Delete `_freeze/` to force a full re-run.

## Software — first-time setup

Run this once, before the first render. It installs only what is missing, so it
is safe to re-run:

```r
source("R/setup/install-packages.R")
```

No Python or `reticulate` is needed. The Python in lecture 1 is shown as static,
syntax-highlighted code rather than an executed chunk, so knitr never loads a
Python engine.

## Conventions

- Code chunks read data from `data/` using project-relative paths; `_quarto.yml`
  sets `execute-dir: project`, so `read_csv("data/foo.csv")` works from any
  document regardless of which folder it sits in.
- Images live in `assets/images/` and are referenced as `../assets/images/...`
  from documents one level deep.
- Rendered output never lives beside its source. If you find a `.html` next to a
  `.qmd` outside `docs/`, it is stale.

### Styling — there are exactly two files

| File | Used by | How |
|---|---|---|
| `assets/theme.scss` | every HTML page | set once in `_quarto.yml`; documents do **not** declare their own theme |
| `assets/slides.scss` | every revealjs deck | `theme: [default, ../assets/slides.scss]` in the document front matter |

Both share the same brand palette (dark green `#2C5F2D`, muted brown `#8B7355`,
Georgia typography) and define the same helper classes, so `[text]{.flow}`,
`{.yellow}`, `{.hi}`, `{.centered}`, `{.small}` and friends behave identically in
slides and on pages. Add a new utility to both files, or to neither.

The eight scattered style files that preceded them — `custom.scss`,
`custom2.scss`, `styles.scss`, `ryan.scss`, `custom.css`, `styles.css`,
`logo.css`, `logo1.css` — are in `_archive/superseded/styles/`. Two of them were
the cause of render failures: `custom.css` held SCSS syntax under a `.css`
extension, and `styles.css` had literal `<style>` tags inside it. Do not bring
them back.

## Two things that will break the render if you undo them

**1. In SCSS, always put a space after the colon.** Quarto's CSS-variable
extractor cannot parse a declaration whose value begins with a digit or a dot
immediately after the colon. `gap:1rem` and `font-size:.9em` both crash it with
`SCSSParsingError: Expecting punctuation "}"`; `gap: 1rem` and `font-size: .9em`
are fine. Letters are unaffected, which is why `display:block` never complained.
This is what produced the `_quarto_internal_scss_error.scss` files.

**2. Do not set `$code-color` in `scss:defaults`.** Quarto declares it in its own
earlier layer, so assigning it there raises `variable used before declaration`.
The theme styles inline code with a rule instead:
`code:not(.sourceCode) { color: $qau-green-d; }`.

Both files were verified by compiling them with Quarto itself — a clean render,
no errors, no warnings, no SCSS dump file.

## Licence

Teaching material © Prof. Dr. Zahid Asghar. Code may be reused with attribution.
