# STAR: reverse-engineering and porting notes

Target: IRRI **STAR** (Statistical Tool for Agricultural Research), installed at `C:\STAR`.
Goal: reuse the R statistics engine on a modern R (option "A": wrap the R package behind an API later).

## Layout

Restructured: `reference/` is read-only material from the reverse engineering; `engine/` is the single living copy of the R code; `prototype/` is the web app.

| Path | What |
|---|---|
| `reference/r-original/` | All 257 functions of the original R package STAR 2.0.2, one `.R` per function, pristine (from `deparse`, comments lost) |
| `reference/original-outputs/` | Printed output of the original STAR on R 2.15.1 for the shared test cases |
| `engine/` | Installable R package for R 4.5.2: the port, edited as listed below. Tests in `engine/tests/` |
| `prototype/` | Shiny/Shinylive web app. `prototype/app/star/` is generated from `engine/R` by `scripts/build_app.R` (git-ignored) |
| `scripts/` | `extract_star.R` (recover R sources), `gen_namespace.R`, `install_deps.R`, `build_app.R`, `compare_cases.R` (run the shared cases on any R), `smoke.R` |
| `input/` | (git-ignored, local only) copies of the original jar files |

Scope decision (2026-10-18): the **randomisation / field-layout functions were removed** from the engine and the app (all `design*`, `randomizeLattice`, `GenerateFactor`, `FactorList`, `printLayout`; 34 files) because few people use them. They remain in `reference/r-original/`. This also removes the dependency on DiGGer. The engine now has 223 functions.

## Tests

`Rscript -e 'install.packages("engine", repos = NULL, type = "source")'` then, from `engine/tests`, `Rscript testthat.R`. The suite runs the 23 cases in `engine/tests/testthat/cases.R` and compares the printed output with `expected/` (produced by the original STAR on R 2.15.1) after normalising whitespace and rounding numbers to 3 significant digits. Cases run in one test block because the original functions change `options(digits, width)` and later cases print with them. `Kmeans`, `ClusterAgglo`, `Discriminant`, `FitDistrib` are not in the suite (random labels / not yet working with my arguments).

## How the program works

- Eclipse E4 RCP (Java 6, OSGi). Menu item -> `*Handler` (`@Execute`, checks a project and a data tab are active) -> `*Dialog` -> a manager in `RJavaManager1.0.jar`.
- Managers build R command strings and call `Rengine.eval` (JRI). Pattern: `read.csv` into `dataRead`, `sink(out.txt)`, `result <- try(Fn(data = "dataRead", ...))`, then `write.csv` or an error message, then `rm(list=ls())` and `rEngine.end()`.
- `initStar()` runs `library(STAR)`; `initPBtool()` runs `library(PBTools)` (PBTools not analysed).
- Note the data argument is the **name** of a data frame as a string (`data = "dataRead"`); the `.default` methods also accept a data frame.

## Changes made in `engine/` (relative to the recovered sources)

1. **Package skeleton**: wrote `DESCRIPTION` (Depends R >= 4.0, Imports listed), copied the 56 `.rda` datasets from the old install, kept the original `NAMESPACE` as `NAMESPACE.orig`.
2. **S3 registration** (`scripts/gen_namespace.R`): modern R no longer finds unregistered S3 methods inside a namespace, so every generic failed with "no applicable method". The script finds generics (bodies calling `UseMethod`) and writes 88 `S3method()` lines and explicit `export()` lines (excluding the zzz_compat shims so attaching STAR does not mask base functions), plus `import(stats, graphics, grDevices, utils)`. Re-run it after adding methods.
3. **`R/zzz_compat.R`**: shims masking `data.frame`, `read.csv`, `read.table`, `read.delim` inside the namespace with `stringsAsFactors = TRUE`. Needed because R 4.0 changed the default; e.g. `NormalityTest` relied on `levels()` of a character column and produced a column named `NA`. This is a blunt fix; individual call sites could be changed instead.
4. **`PCA.default.R`**: added `matx <- match.arg(matx)`. Since R 4.2 `if (c("corr","cov") == "corr")` is an error (the old Java caller always passed one value).

Dependencies installed from CRAN into the user library `C:\Users\Lenovo_01\AppData\Local\R\win-library\4.5`: lme4, multcomp, fitdistrplus, leaps, vegan, permute, ade4, R.methodsS3, R.oo (plus their dependencies).

## Status (`scripts/smoke.R`, R 4.5.2)

Passing (runs without error; **output numbers not yet compared with the original**): DescriptiveStatistics, CompareMeans (independent), NormalityTest, Crosstab, AOVTest (RCBD, SplitRCBD, LSD), LinearRegressionAnalysis, BivariateCorrelationTest, PCA (graphs off), ClusterKmeans (graph off), designRCBD, designCRD, designLSD, designBIBD, designLattice, designSplit.

Not covered by tests: most of the other ~230 functions, all graph functions, `combAOVTest`, `ssaTest`, MANOVA, discriminant, non-linear regression, distribution fitting, file output (`sink`, csv, png).

Findings on the two items that looked broken:
- `designLattice`: not a bug. It needs `numFieldRow` to be a multiple of sqrt(number of treatments) (GUI supplies it); default 1 fails with "subscript out of bounds". A friendlier error would be a worthwhile fix.
- `designAlphaLattice`: not run. It calls **DiGGer**.

## Former blocker: DiGGer (moot now: design functions are out of scope)

- `DiGGer` 0.2-3 (2011, Neil Coombes, NSW DPI) is not on CRAN (not in the index, `Archive/DiGGer/` returns 404). The old install has only a Windows binary built for R 2.14 (`C:\RPackages\R-2.15.1\library\DiGGer`).
- Its DESCRIPTION license is "NSW DPI Freeware Licence" (not GPL); the stated source URL `austatgen.org/files/software/downloads` returned HTTP 403 when probed. Redistribution terms are unverified.
- Affected STAR functions (they reference DiGGer): `designAlphaLattice`, `designLatinizedAlpha`, `designLatinizedRowCol`, `designRowColumn`.
- Not obtainable for modern R so far; the old binary will not load in R 4.5. Options later: find source from the author, or replace with another optimal-design generator (e.g. `agricolae`, `blocksdesign`) and compare outputs.

## Licensing

STAR R package: GPL (>= 2), so derivatives must stay GPL. The Java GUI's license was not found in the jar; check with IRRI before redistributing. DiGGer: see above.

## Suggested next steps

1. Build a regression suite: run the original R 2.15.1 and the port on the same inputs (the datasets in `engine/data`) and diff the outputs.
2. Widen smoke coverage to every exported function.
3. Wrap the port in an HTTP API (e.g. plumber) returning structured results instead of `sink` text.
4. Decide what to do about DiGGer.

## Update: additional fixes found by comparing against R 2.15.1

The earlier smoke test only checked "no error", which hid wrong output (e.g. AOVTest printed "The data set does not contain any observations" and still returned). Comparison against the original is the real test: `scripts/compare_cases.R <outdir>` run on both R versions, outputs in `reference/original-outputs` (R 2.15.1, original packages) and (not stored) (R 4.5.2 port).

5. **`AOVTest.default.R`, `BIBDTest.default.R`**: `cbind(data, setVar = "1")` now yields a character column (R 4.0), so `nlevels()` was 0 and no analysis ran. Changed to `factor("1")`.
6. **`R/zzz_compat.R`**: `nchar()` wrapper. R < 3.3 returned 2 for `NA`; newer returns `NA`, which made `sum(colwidth)` NA and broke `printAOVTable` ("invalid 'times' argument") in every ANOVA/regression table. The wrapper passes `keepNA = FALSE`.

Run the comparison with:
```
scripts/compare_cases.R reference/original-outputs   # with C:\RPackages\R-2.15.1\bin\i386\Rscript.exe, R_LIBS_USER unset
scripts/compare_cases.R /tmp/new   # with R 4.5.2 and R_LIBS_USER pointing at the user library (then diff against reference/original-outputs, or run the testthat suite)
```

### Result (27 cases)

Identical output: DescriptiveStatistics (x2), CompareMeans independent/paired, Correlation (Pearson, Spearman), Crosstab, LinearRegressionAnalysis (incl. CI, covariance, VIF), HeteroskedasticityTest, skewness/kurtosis helpers, KruskalWallisTest.

Differences that are not errors in the port:
- trailing whitespace in lines printed by R's own `print.summary.lm`/`cat` (AOV*, 4-10 lines each);
- last digits of the Shapiro-Wilk statistic/p-value (R fixed the algorithm in 4.x; e.g. 0.04880736 vs 0.04880824);
- `prcomp` print headings (`Rotation (n x k) = (4 x 4):`) and a row name (`mean of x`) in CompareMeans one-sample;
- k-means cluster numbering differs because `set.seed(1)` + `sample()` changed in R 3.6; cluster contents are the same sets with permuted labels. Any randomised output (all `design*` randomisations) will differ for the same seed.

Not verified: `ClusterAgglo`, `Discriminant`, `FitDistrib` fail with the same error on both R versions with my test arguments (`memSummary` / `tempData` not found / `EXPR must be a length 1 vector`); either my calls are incomplete or these are original bugs. Needs checking against how `STARAnalysisManager.java` calls them. Not compared at all: graph functions, designs (random), combAOVTest, ssaTest, MANOVA, nonlinear regression, PowerTransform, data-manipulation functions.

## Update: one-factor ANOVA (CRD and RCBD) with structured output

- `engine/R/star_aov.R` (new, not in the original): `star_aov(data, response, treatment, block = NULL, design = "CRD" | "RCBD", test, alpha)` wraps the original `AOVTest` and returns `anova`, `means` (with SE and mean-separation letters), `cv`, `grand_mean`, a one-sentence `summary` and the classic printed `report`. `star_plot_means()` draws means with SE bars and letters. `star_aov_rcbd()` is kept as a thin wrapper.
- Tested against the original STAR on R 2.15.1: RCBD rice example (F = 8.54, CV = 9.42) and CRD insecticide example (F = 9.83, CV = 15.09, HSD letters ab/c/a/bc/bc/ab/a).
- Bug found by the CRD example: STAR pads treatment names with trailing spaces in its comparison tables ("Azodin       "), so matching them against the factor levels dropped two treatments. Names are now trimmed before matching.
- App: design selector (CRD / RCBD), results tab, plot tab, classic report tab, Excel download. `prototype/tests/test_server.R` covers both designs.
- The user wrote "CDR"; this was interpreted as CRD (completely randomized design).

## Update: two treatment factors (factorial CRD and RCBD)

- `star_aov(treatment = c("A", "B"))` now handles two factors for both designs. Results: ANOVA with the `A:B` row, `effects` (F, p, significant), `means` (one data frame per factor), `cells` (cell means with SE) and `simple` (each factor compared within each level of the other, only when the interaction is significant, as STAR does). With a non-significant interaction the significant main effects are compared instead. With `test = "auto"` STAR's rule applies per effect (LSD if <= 5 levels, else Tukey HSD).
- Checked against the original STAR on R 2.15.1: RCBD Nitrogen x Variety (all ANOVA rows, CV 10.07, letters of both simple-effect tables) and CRD supp x dose (ToothGrowth: F 15.57 / 92.00 / 4.11, CV 19.30, letters).
- Limits: at most two factors; the design must be complete (every treatment combination present); marginal means assume a balanced layout. Three-factor and split-plot layouts are not covered yet.
- App: choose one or two treatment factors, four built-in examples, interaction/simple-effect tables laid out like STAR's, grouped-bar interaction plot, Excel with sheets ANOVA / Means / Cell means / Simple effects / Info.

## Update: split plot in RCBD

- `star_splitplot(data, response, main, sub, block, test, alpha)` (new, `engine/R/star_splitplot.R`): model `Rep + A*B + Error(Rep:A)`, ANOVA with Error(a) and Error(b), CV(a) and CV(b), main effects (A with Error(a), B with Error(b)), and with a significant interaction the simple effects: B within A (Error(b)) and A within B (pooled error of (a) and (b), Satterthwaite df, floored), as STAR does. Returns the same `star_aov` structure plus `cv_a`/`cv_b`, so the app and plots work unchanged.
- Checked against the original STAR on R 2.15.1 (Gomez rice, Nitrogen x Variety): all ANOVA rows, CV(a) = 6.88, CV(b) = 10.79 and the letters of both simple-effect tables.
- Limits: complete and balanced layout only (one observation per block x main x sub), no missing values; split plot in CRD, split-split and strip plots are not covered; assumption checks are not computed yet.
- App: design "Split plot in RCBD" with main-plot and sub-plot pickers and a built-in example.
