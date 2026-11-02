# RDstat web app (Shinylive)

Three screens: analysis of variance (CRD, RCBD, split plot; mean comparison, plots, assumption checks), correlation (Pearson / Spearman / Kendall, heatmap and scatter plots) and linear regression (multiple, quadratic, cubic; diagnostics, prediction). Each has copy and Excel/plot download. It runs the recovered STAR engine inside the browser via webR.

Build and test:

```
Rscript scripts/build_app.R                 # copies engine/R into prototype/app/star (git-ignored)
Rscript prototype/tests/test_server.R       # headless test of the server logic on native R
Rscript -e 'shinylive::export("prototype/app", "prototype/site")'
```

Serve `prototype/site` over http with a normal browser (it needs a service worker; the Claude desktop browser pane refuses to register one).
Deploy: `.github/workflows/pages.yml` builds and publishes to GitHub Pages (repo settings -> Pages -> Source: GitHub Actions).

Code layout: `app/app.R` holds the theme, home/info pages, `ui` and the `server` skeleton; `app/src/` holds the helpers, UI building blocks, screens and example definitions (`helpers.R`, `ui_*.R`, `designs.R`) and one `server_*.R` file per screen (ANOVA, t-test, correlation, regression, feedback). `app.R` sources them in order, the server files with `local = TRUE` so they share `input`, `output` and `session`. Ids keep the screen prefix (`tt_`, `corr_`, ...) instead of Shiny module namespaces, so no package is needed and Shinylive bundles the folder as is.
