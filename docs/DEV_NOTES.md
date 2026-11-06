# Development notes

Problems met while splitting `app.R` and adding the "R code to repeat this analysis" cards (PR 1 and PR 2), what was done about them, and what is still open. Written so the next person does not meet the same surprises.

## Code layout

- `prototype/app/app.R`: theme, Home / Methods / About pages, `ui`, and the `server` skeleton.
- `prototype/app/src/`: the rest, sourced by `app.R` in a fixed order (definitions depend on earlier files).
  - top level: `helpers.R`, `ui_components.R`, `ui_screens.R`, `tt_display.R`, `designs.R`, `script_gen.R`
  - inside `server`, with `local = TRUE`: `server_plot.R`, `server_aov.R`, `server_shared.R`, `server_tt.R`, `server_corr.R`, `server_reg.R`, `server_feedback.R`
- Ids keep their screen prefix (`tt_`, `corr_`, `reg_`); Shiny module namespaces were not used. The UI helpers and the JS conditions (`input.go_tt > 0`) use flat ids, so namespacing would touch every id and the whole of `tests/test_server.R`. It is still possible later if a screen needs to be reused.

## Pitfalls

| Problem | Cause | What was done |
|---|---|---|
| App did not start: `object 'output' not found` | Shiny sources every file in an app's `R/` folder before `app.R`. The `server_*.R` files use `input`/`output`/`session`. | Folder is `src/`, not `R/`. `testServer()` does not autoload, so the headless tests did not catch it: also start the app with `runApp()` after structural changes. |
| `tests/test_server.R` stopped early with an empty `Error:` | `prototype/app/star/` is generated from `engine/R` and git-ignored; an old copy lacked the title-line reader. | Run `Rscript scripts/build_app.R` before the tests (CI does). |
| Matrix in a generated script was filled wrongly | Inside the app, `star/zzz_compat.R` masks `data.frame()`, `read.csv()` and friends with `stringsAsFactors = TRUE`, so character columns became factors and were used as indices. | Generated scripts set `stringsAsFactors = FALSE` explicitly and read text as `character`. |
| `unused argument (console = FALSE)` | An old `LSD.test` from the recovered engine masks agricolae's. | Scripts call `agricolae::LSD.test` etc. |
| Launch config pointed at R 4.5.2 | Only R 4.6.1 is installed. | Added the `star-app-r46` config to `.claude/launch.json` (uses `PORT`). |
| Browser pane could not run Shinylive | It refuses service workers. | Serve the export with a local http server and open it in Chrome (see `prototype/README.md`). |

## R code cards (`src/script_gen.R`)

Each results screen ends with a card that has a plain R script and a Copy button.

- Covered: t-test (independent, paired, one sample), correlation, regression (line, quadratic, cubic), ANOVA for CRD and RCBD with one or two factors (also with a transformed response), split plot (RCBD and CRD), combined analysis over environments (RCBD and CRD), repeated measures over time (RCBD and CRD; the split-plot script plus `mauchly.test()` and `anova.mlm(test = "Spherical")`).
- Not covered: an RCBD with missing responses, where the app estimates the missing values with STAR's procedure; the card says so.
- Scripts never call `star_*` functions. They use base R and `agricolae` (and `readxl` for Excel files).
- Built-in examples carry their data inside the script; uploaded files are read with the separator found from the first line, all columns as text, and numbers go through an embedded `parse_number()` that mirrors `star_parse_number()`. Shapiro-Wilk goes through `shapiro_safe()`, which skips the same cases as the app. Title lines above the header are not handled (the script only says to add `skip =`).
- Tests (`tests/test_server.R`) run each generated script in a temporary folder and compare statistics, p values, CV, means, letters and simple effects with the app's results. Add such a check for any new analysis.
- Review findings fixed after the first version: number parsing, delimiter, example data, Shapiro-Wilk guard, readxl note.

## Repeated measures

- `star_repeated()` (engine) runs `star_splitplot()` with treatment = main plot and time = sub plot, and passes the sphericity-corrected p values of time and treatment x time through the internal `within_p` argument, so significance, letters and simple effects follow the corrected tests while `$anova` keeps the uncorrected ones.
- In the app `is_split()` is also TRUE for `RepCRD` / `RepRCBD` (they share the split-plot pickers, error terms and two-level diagnostics); `is_rep()` marks where the wording or outputs differ.
- The epsilons come from the residual SSCP of orthonormal time contrasts after removing the treatment means only (blocks pooled, as in Error(b)); its trace equals SS Error(b). Tests compare them with `anova.mlm()`.

## Open

- Run the Shinylive export again after changes to `src/` (done once for the t-test: `src/` is bundled and the app works in webR; the other screens were checked natively and in the browser only).
- Uploaded files with title lines in the generated scripts.
- RCBD with missing responses has no script.
