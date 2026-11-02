# Structured API on top of AOVTest. Reference values come from the ORIGINAL STAR 2.0.2 on R 2.15.1
# (see reference/original-outputs and engine/tests/testthat/expected).
data(GomezSplitRCBD, package = "STAR", envir = environment())
data(Insecticides, package = "STAR", envir = environment())
rcbd <- subset(GomezSplitRCBD, Variety == "IR5")[, c("Rep", "Nitrogen", "Yield")]
tg <- ToothGrowth; tg$dose <- factor(tg$dose)

test_that("RCBD one factor: tables match the original STAR report (F = 8.54, p = 0.0022, CV = 9.42)", {
  r <- star_aov(rcbd, "Yield", "Nitrogen", "Rep", design = "RCBD", test = "LSD")
  expect_s3_class(r, "star_aov")
  expect_equal(r$anova$Source, c("Rep", "Nitrogen", "Error", "Total"))
  expect_equal(r$anova$DF, c(2, 5, 10, 17))
  expect_equal(round(r$anova$F[2], 2), 8.54)
  expect_equal(round(r$p_treatment, 4), 0.0022)
  expect_equal(round(r$cv, 2), 9.42)
  expect_equal(round(r$grand_mean, 1), 6155.5)
  expect_equal(names(r$means), "Nitrogen")
  expect_equal(r$means$Nitrogen$Group, c("b", "a", "a", "a", "a", "a"))
  expect_identical(r$significant, TRUE)
  expect_match(r$summary, "significant effect")
  expect_match(r$report, "ANOVA TABLE", all = FALSE)
  expect_equal(star_aov_rcbd(rcbd, "Yield", "Nitrogen", "Rep", test = "LSD")$anova, r$anova)
})

test_that("CRD one factor: Insecticides matches the original STAR (F = 9.83, CV = 15.09, HSD letters)", {
  r <- star_aov(Insecticides, "GrainYield", "Insecticide", design = "CRD", test = "HSD")
  expect_equal(r$design, "CRD")
  expect_equal(r$anova$Source, c("Insecticide", "Error", "Total"))
  expect_equal(r$anova$DF, c(6, 21, 27))
  expect_equal(round(r$anova$SS[1], 1), 5587174.9)
  expect_equal(round(r$anova$F[1], 2), 9.83)
  expect_lt(r$p_treatment, 0.0001)
  expect_equal(round(r$cv, 2), 15.09)
  m <- r$means$Insecticide
  expect_equal(m$Treatment, levels(Insecticides$Insecticide))
  expect_equal(round(m$Mean, 2), c(2128.00, 1316.00, 2551.75, 1796.00, 1681.00, 2126.75, 2678.00))
  expect_equal(m$Group, c("ab", "c", "a", "bc", "bc", "ab", "a"))
  expect_equal(m$N, rep(4, 7))
})

test_that("RCBD two factors (Nitrogen x Variety) match the original STAR", {
  r <- star_aov(GomezSplitRCBD, "Yield", c("Nitrogen", "Variety"), "Rep", design = "RCBD")
  expect_equal(r$anova$Source, c("Rep", "Nitrogen", "Variety", "Nitrogen:Variety", "Error", "Total"))
  expect_equal(r$anova$DF, c(2, 5, 3, 15, 46, 71))
  expect_equal(round(r$anova$SS[4], 4), 69343486.9306)
  expect_equal(round(r$anova$F[2:4], 2), c(19.99, 98.42, 15.18))
  expect_equal(round(r$cv, 2), 10.07)
  expect_equal(round(r$grand_mean, 2), 5478.90)
  expect_identical(r$interaction_significant, TRUE)
  expect_equal(r$effects$Effect, c("Nitrogen", "Variety", "Nitrogen:Variety"))
  expect_true(all(is.na(r$means$Nitrogen$Group)))        # main effects are not compared when the interaction is significant
  expect_equal(r$tests_used[["Nitrogen"]], "HSD")        # 6 levels
  expect_equal(r$tests_used[["Variety"]], "LSD")         # 4 levels
  expect_match(r$summary, "interaction is significant")
  expect_equal(nrow(r$cells), 24)
  # Nitrogen at each level of Variety (HSD), letters for N = 1..6
  sm <- r$simple[["Nitrogen within Variety"]]
  expect_equal(sm$Group[sm$Variety == "C4-63"], c("b", "a", "a", "a", "a", "a"))
  expect_equal(sm$Group[sm$Variety == "IR8"], c("d", "c", "bc", "bc", "ab", "a"))
  expect_equal(sm$Group[sm$Variety == "Peta"], c("a", "a", "a", "a", "b", "b"))
  expect_equal(round(sm$Mean[sm$Variety == "IR8"], 4), c(4252.6667, 5672.0000, 6400.0000, 6732.6667, 7563.3333, 8700.6667))
  # Variety at each level of Nitrogen (LSD), letters for C4-63, IR5, IR8, Peta
  sv <- r$simple[["Variety within Nitrogen"]]
  expect_equal(sv$Group[sv$Nitrogen == "0"], c("b", "a", "a", "a"))
  expect_equal(sv$Group[sv$Nitrogen == "60"], c("ab", "a", "ab", "b"))
  expect_equal(sv$Group[sv$Nitrogen == "180"], c("b", "b", "a", "c"))
  expect_equal(attr(sv, "test"), "LSD")
})

test_that("CRD two factors (supp x dose) match the original STAR", {
  r <- star_aov(tg, "len", c("supp", "dose"), design = "CRD")
  expect_equal(r$anova$Source, c("supp", "dose", "supp:dose", "Error", "Total"))
  expect_equal(r$anova$DF, c(1, 2, 2, 54, 59))
  expect_equal(round(r$anova$F[1:3], 2), c(15.57, 92.00, 4.11))
  expect_equal(round(r$p_treatment, 4), 0.0002)
  expect_equal(round(r$cv, 2), 19.30)
  expect_equal(round(r$grand_mean, 2), 18.81)
  expect_identical(r$interaction_significant, TRUE)
  s1 <- r$simple[["supp within dose"]]                    # LSD
  expect_equal(s1$Group[s1$dose == "0.5"], c("a", "b"))
  expect_equal(s1$Group[s1$dose == "2"], c("a", "a"))
  s2 <- r$simple[["dose within supp"]]
  expect_equal(s2$Group[s2$supp == "OJ"], c("c", "b", "a"))
  expect_equal(round(s2$Mean[s2$supp == "VC"], 4), c(7.98, 16.77, 26.14))
})

test_that("without a significant interaction the main effects are compared instead", {
  set.seed(1)
  d <- expand.grid(A = c("a1", "a2"), B = c("b1", "b2", "b3"), rep = 1:4)
  d$y <- 10 + 3 * (d$A == "a2") + as.integer(factor(d$B)) * 0 + rnorm(nrow(d), sd = 0.5)
  r <- star_aov(d, "y", c("A", "B"), design = "CRD")
  expect_identical(r$interaction_significant, FALSE)
  expect_null(r$simple)
  expect_identical(r$effects$Significant[r$effects$Effect == "A"], TRUE)
  expect_false(anyNA(r$means$A$Group))
  expect_match(r$summary, "interaction is not significant")
})

test_that("CRD ignores a block argument and needs no block", {
  r1 <- star_aov(PlantGrowth, "weight", "group", design = "CRD", test = "HSD")
  r2 <- star_aov(PlantGrowth, "weight", "group", block = "group", design = "CRD", test = "HSD")
  expect_equal(r1$anova, r2$anova)
  expect_equal(round(r1$anova$F[1], 2), 4.85)      # original STAR: F = 4.85, p = 0.0159
  expect_equal(round(r1$p_treatment, 4), 0.0159)
})

test_that("the direct aov() fit gives the same table as STAR's own AOVTest", {
  legacy_table <- function(d, response, factors, block, design) {
    res <- .star_aov_legacy(star_prepare_data(d, response, factors, block), response, factors, block, design, "auto", 0.05)$res
    unname(as.matrix(ConstructAOVTable(res$aovTable[[1]][[1]])))
  }
  native_table <- function(r) unname(as.matrix(r$anova[-1]))
  layouts <- list(
    list(rcbd, "Yield", "Nitrogen", "Rep", "RCBD"),
    list(GomezSplitRCBD, "Yield", c("Nitrogen", "Variety"), "Rep", "RCBD"),
    list(tg[-c(1, 5, 22, 40), ], "len", c("supp", "dose"), NULL, "CRD"),     # unbalanced CRD
    list(warpbreaks, "breaks", c("wool", "tension"), NULL, "CRD")
  )
  for (a in layouts) {
    r <- star_aov(a[[1]], a[[2]], a[[3]], a[[4]], design = a[[5]], report = FALSE)
    expect_equal(native_table(r), legacy_table(a[[1]], a[[2]], a[[3]], a[[4]], a[[5]]), tolerance = 1e-10)
  }
})

test_that("report = FALSE leaves the classic printout to star_report()", {
  full <- star_aov(GomezSplitRCBD, "Yield", c("Nitrogen", "Variety"), "Rep", design = "RCBD", test = "duncan")
  lazy <- star_aov(GomezSplitRCBD, "Yield", c("Nitrogen", "Variety"), "Rep", design = "RCBD", test = "duncan", report = FALSE)
  expect_null(lazy$report)
  expect_identical(star_report(lazy), full$report)
  lazy$report <- full$report
  expect_identical(lazy, full)
})

test_that("CRD: rows with a missing response are left out; column names may contain spaces", {
  crd_na <- Insecticides
  crd_na$GrainYield[c(2, 9)] <- NA
  r <- star_aov(crd_na, "GrainYield", "Insecticide", design = "CRD")
  expect_equal(r$anova$DF, c(6, 19, 25))
  expect_match(r$data_notes, "2 row\\(s\\) with a missing response were left out", all = FALSE)
  expect_equal(r$anova$F, star_aov(crd_na[-c(2, 9), ], "GrainYield", "Insecticide", design = "CRD")$anova$F)

  sp <- setNames(tg, c("tooth length", "supp", "dose mg"))
  r <- star_aov(sp, "tooth length", c("supp", "dose mg"), design = "CRD")
  expect_equal(r$anova$Source, c("supp", "dose mg", "supp:dose mg", "Error", "Total"))
  expect_equal(r$anova$F, star_aov(tg, "len", c("supp", "dose"), design = "CRD")$anova$F)
})

test_that("every mean-comparison choice works and 'auto' follows STAR's rule", {
  for (tst in c("LSD", "HSD", "duncan", "SNK", "scheffe")) {
    r <- star_aov(rcbd, "Yield", "Nitrogen", "Rep", design = "RCBD", test = tst)
    expect_equal(r$tests_used[["Nitrogen"]], tst)
    expect_equal(nrow(r$means$Nitrogen), 6)
    expect_true(any(grepl(c(LSD = "Least Significant", HSD = "Tukey", duncan = "Duncan", SNK = "Newman", scheffe = "Scheffe")[[tst]], r$report)))
  }
  expect_equal(star_aov(rcbd, "Yield", "Nitrogen", "Rep", design = "RCBD")$tests_used[["Nitrogen"]], "HSD")   # 6 levels > 5
  expect_equal(star_aov(PlantGrowth, "weight", "group")$tests_used[["group"]], "LSD")                          # 3 levels <= 5
})

test_that("bad input gives clear errors", {
  expect_error(star_aov(rcbd, "Nope", "Nitrogen", "Rep", design = "RCBD"), "not found")
  expect_error(star_aov(rcbd, "Yield", "Yield", "Rep", design = "RCBD"), "different columns")
  expect_error(star_aov(data.frame(r = rep(1:3, 6), g = rep(c("a", "b"), each = 9), y = letters[1:18]), "y", "g"), "no numbers")
  expect_error(star_aov(rcbd, "Yield", "Nitrogen", design = "RCBD"), "needs a block")
  expect_error(star_aov(data.frame(y = 1:4, g = "a"), "y", "g"), "at least two levels")
  expect_error(star_aov(tg, "len", c("supp", "dose", "len")), "different columns|Only one or two")
  expect_error(star_aov(tg[!(tg$supp == "OJ" & tg$dose == "0.5"), ], "len", c("supp", "dose")), "no observations")
})

test_that("plots run without error", {
  one <- star_aov(Insecticides, "GrainYield", "Insecticide", design = "CRD")
  two <- star_aov(GomezSplitRCBD, "Yield", c("Nitrogen", "Variety"), "Rep", design = "RCBD")
  f <- tempfile(fileext = ".png"); png(f)
  expect_silent(star_plot_means(one))
  expect_silent(star_plot_means(two, "Nitrogen"))
  expect_silent(star_plot_means(two, "interaction"))
  expect_error(star_plot_means(two, "nope"), "Unknown effect")
  dev.off(); unlink(f)
})

# ---- messy input data -------------------------------------------------------------------------------------------
test_that("star_parse_number reads common messy number formats", {
  x <- c("12.5", " 1,250.5 ", "1.250,5", "12,5", "1,250", "12 kg", "45%", "n/a", "", "-", "abc", "-3.2")
  expect_equal(star_parse_number(x), c(12.5, 1250.5, 1250.5, 12.5, 1250, 12, 45, NA, NA, NA, NA, -3.2))
  expect_equal(star_parse_number(c(1, 2)), c(1, 2))
})

test_that("star_aov repairs a messy data set and reports what it did", {
  m <- rcbd
  m$Yield <- as.character(m$Yield)
  m$Yield[3] <- "n/a"; m$Yield[5] <- paste0(m$Yield[5], " kg")
  m$Nitrogen <- as.character(m$Nitrogen); m$Nitrogen[2] <- paste0(" ", m$Nitrogen[2], "  ")
  r <- star_aov(m, "Yield", "Nitrogen", "Rep", design = "RCBD", test = "LSD")
  expect_s3_class(r, "star_aov")
  expect_match(r$data_notes, "read as numbers", all = FALSE)
  expect_match(r$data_notes, "extra spaces", all = FALSE)
  expect_match(r$data_notes, "missing", all = FALSE)
  c2 <- data.frame(g = rep(c("a", "b", ""), each = 4), y = c(1, 2, 3, 4, 3, 4, 5, 7, 9, 9, 9, 9))
  r2 <- star_aov(c2, "y", "g")                          # rows without a treatment label are removed
  expect_match(r2$data_notes, "removed", all = FALSE)
  expect_equal(sum(r2$means$g$N), 8)
  m$Yield[7] <- "oops"                                  # two missing responses: STAR cannot estimate them
  expect_error(star_aov(m, "Yield", "Nitrogen", "Rep", design = "RCBD"), "Too many missing")
})

test_that("star_aov refuses columns that cannot be analysed, with a clear reason", {
  d <- data.frame(g = rep(c("a", "b"), each = 6), id = paste0("s", 1:12), y = c(1:6, 3:8) + rep(c(0, .1), 6))
  expect_error(star_aov(transform(d, y = rep("text", 12)), "y", "g"), "no numbers")
  expect_error(star_aov(transform(d, y = c(rep("x", 8), "1", "2", "3", "4")), "y", "g"), "mostly text")
  expect_error(star_aov(transform(d, y = 5), "y", "g"), "constant")
  expect_error(star_aov(d, "y", "id"), "different level on every row")
  expect_error(star_aov(transform(d, g = "a"), "y", "g"), "at least two levels")
  expect_error(star_aov(transform(d, g = NA_character_), "y", "g"), "No rows")
})

test_that("a numeric treatment or block column is treated as a factor and sorted numerically", {
  d <- data.frame(dose = rep(c(10, 2, 5), each = 4), y = c(1, 2, 1.5, 2.2, 5, 6, 5.5, 6.2, 9, 8, 9.5, 8.7))
  r <- star_aov(d, "y", "dose")
  expect_equal(r$means$dose$Treatment, c("2", "5", "10"))
})

test_that("star_read_table detects delimiter, encoding and reads xlsx as text", {
  w <- function(txt, enc = "UTF-8") { f <- tempfile(fileext = ".csv"); writeBin(iconv(txt, "UTF-8", enc, toRaw = TRUE)[[1]], f); f }
  d1 <- star_read_table(w("g;y\na;1,5\nb;2,5\n"), "a.csv")
  expect_equal(names(d1), c("g", "y")); expect_equal(d1$y, c("1,5", "2,5"))
  d2 <- star_read_table(w("t,y\ncafé,1\nb,2\n", "latin1"), "a.csv")
  expect_equal(d2$t[1], "café")
  # Thai Windows-874 / TIS-620, as Excel on a Thai Windows saves "CSV (Comma delimited)"
  tis <- function(txt) { u <- utf8ToInt(txt); th <- u >= 0x0E01 & u <= 0x0E5B; u[th] <- u[th] - 0x0E01 + 0xA1
    f <- tempfile(fileext = ".csv"); writeBin(as.raw(u), f); f }
  d3 <- star_read_table(tis("พันธุ์,ซ้ำที่ 1,ซ้ำที่ 2\nกข41,5557,5706\nปทุมธานี 1,4652,4965\n"), "a.csv")
  expect_equal(names(d3), c("พันธุ์", "ซ้ำที่ 1", "ซ้ำที่ 2"))
  expect_equal(d3[[1]], c("กข41", "ปทุมธานี 1"))
  expect_equal(d3[[3]], c("5706", "4965"))
  # UTF-16 with a BOM, as Excel saves "Unicode Text" (tab-delimited)
  u16 <- function(txt, le = TRUE) { u <- utf8ToInt(txt); lo <- u %% 256; hi <- u %/% 256
    f <- tempfile(fileext = ".txt"); writeBin(as.raw(c(if (le) c(0xFF, 0xFE) else c(0xFE, 0xFF), rbind(if (le) lo else hi, if (le) hi else lo))), f); f }
  d4 <- star_read_table(u16("พันธุ์\tYield\r\nกข41\t5557\r\n"), "a.txt")
  expect_equal(names(d4), c("พันธุ์", "Yield")); expect_equal(d4[[1]], "กข41")
  expect_equal(star_read_table(u16("g\ty\nกข41\t1\n", le = FALSE), "a.txt")$g, "กข41")
  # UTF-8 with a BOM
  d5 <- star_read_table(w("﻿พันธุ์,y\nกข41,1\n"), "a.csv")
  expect_equal(names(d5)[1], "พันธุ์")
  # an ordinary file gives no notes
  expect_length(attr(d1, "notes"), 0)
  skip_if_not_installed("openxlsx")
  f <- tempfile(fileext = ".xlsx"); openxlsx::write.xlsx(data.frame(g = c("a", "b"), y = c(1.5, 2.5)), f)
  expect_equal(star_read_table(f, "a.xlsx")$y, c("1.5", "2.5"))
  expect_error(star_read_table(w("only\n1\n"), "a.csv"), "at least two columns")
})

test_that("star_read_table skips title lines above the header and summary rows below the data", {
  w <- function(txt) { f <- tempfile(fileext = ".csv"); writeLines(txt, f, useBytes = TRUE); f }
  d <- star_read_table(w(c("Rice yield trial, Suphan Buri 2025", "", "Variety,Rep,Yield", "RD41,1,5557", "RD41,2,5706",
                           "RD47,1,4652", "RD47,2,4965", ",,", "Mean,,5220", "CV (%),,4.1", "LSD 0.05,,310")), "a.csv")
  expect_equal(names(d), c("Variety", "Rep", "Yield"))
  expect_equal(nrow(d), 4)
  expect_equal(d$Yield, c("5557", "5706", "4652", "4965"))
  n <- attr(d, "notes")
  expect_length(n, 2)
  expect_match(n[1], "Skipped 1 line above the column names: “Rice yield trial Suphan Buri 2025”", fixed = TRUE)
  expect_match(n[2], "Skipped 3 summary rows below the data: “Mean”, “CV (%)”, “LSD 0.05”", fixed = TRUE)
  # Thai labels, a summary label inside the data is kept, and a semicolon file with decimal commas under a title
  th <- star_read_table(w(c("การทดลอง;ปี 2568", "พันธุ์;ซ้ำ;ผลผลิต", "รวม;1;1,5", "กข41;1;2,5", "เฉลี่ย;;2,0")), "a.csv")
  expect_equal(names(th), c("พันธุ์", "ซ้ำ", "ผลผลิต"))
  expect_equal(th[[1]], c("รวม", "กข41"))
  expect_equal(th[[3]], c("1,5", "2,5"))
  # a missing value in the first data row does not make the header look like a title
  m <- star_read_table(w(c("g,y,z", "a,,1", "b,2,3")), "a.csv")
  expect_equal(names(m), c("g", "y", "z")); expect_true(is.na(m$y[1])); expect_length(attr(m, "notes"), 0)
  # empty columns before and after the table (it starts in column B, or rows end in extra commas)
  e <- star_read_table(w(c(",g,y,,", ",a,1,,", ",b,2,,")), "a.csv")
  expect_equal(names(e), c("g", "y")); expect_equal(e$y, c("1", "2"))
  # a note typed beside one plot does not push the header down; the unnamed notes column is left out
  o <- star_read_table(w(c("g,r,y", "a,1,4.5", "a,2,4.7,missing plot", "b,1,5.1", "b,2,5.3")), "a.csv")
  expect_equal(names(o), c("g", "r", "y")); expect_equal(nrow(o), 4)
  expect_match(attr(o, "notes"), "“missing plot”", fixed = TRUE)
  # a title merged over a block of cells is not copied into every cell of it
  cells <- data.frame(a = c("Trial", NA, "g", "a", "b"), b = c(NA, NA, "y", "1", "2"), stringsAsFactors = FALSE)
  t2 <- star_table_from_cells(star_fill_merged(cells, cbind(r1 = 1L, c1 = 1L, r2 = 2L, c2 = 2L))$cells)
  expect_equal(names(t2), c("g", "y")); expect_equal(t2$y, c("1", "2"))
  expect_true(star_is_summary_label("Grand mean:")); expect_false(star_is_summary_label("RD41"))
  skip_if_not_installed("writexl")
  f <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(data.frame(a = c("Trial 1", "Variety", "RD41", "RD47", "Average"), b = c(NA, "Yield", "5557", "4652", "5105")), f, col_names = FALSE)
  x <- star_read_table(f, "a.xlsx")
  expect_equal(names(x), c("Variety", "Yield")); expect_equal(x$Variety, c("RD41", "RD47"))
  expect_length(attr(x, "notes"), 2)
})

test_that("star_read_table repeats labels typed once for several rows (merged cells)", {
  w <- function(txt) { f <- tempfile(fileext = ".csv"); writeLines(txt, f, useBytes = TRUE); f }
  # CSV saved from a sheet with merged variety cells: the name is on the first row of each group only
  d <- star_read_table(w(c("Variety,Rep,Yield", "RD41,1,5557", ",2,5706", ",3,5410", "RD47,1,4652", ",2,4965", ",3,4810")), "a.csv")
  expect_equal(d$Variety, rep(c("RD41", "RD47"), each = 3))
  expect_match(attr(d, "notes"), "Column Variety: each label was typed once for 3 rows", fixed = TRUE)
  # replication numbers merged over the varieties (sorted by replication)
  r <- star_read_table(w(c("Rep,Variety,Yield", "1,A,5", ",B,6", "2,A,5.5", ",B,6.2")), "a.csv")
  expect_equal(r$Rep, c("1", "1", "2", "2"))
  # not filled: a response measured on every other plot, and blank labels in groups of different sizes
  y <- star_read_table(w(c("g,y", "a,1.5", "b,", "c,2.7", "d,")), "a.csv")
  expect_true(all(is.na(y$y[c(2, 4)]))); expect_length(attr(y, "notes"), 0)
  u <- star_read_table(w(c("g,y", "a,1", ",2", "b,3", ",4", ",5")), "a.csv")
  expect_equal(sum(is.na(u$g)), 3)
  skip_if_not_installed("openxlsx")
  # Excel: a title merged across the table, the variety merged over its replications
  wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, "Data")
  openxlsx::writeData(wb, "Data", "Rice trial 2025", startRow = 1)
  openxlsx::writeData(wb, "Data", data.frame(Variety = c("RD41", NA, NA, "RD47", NA, NA), Rep = rep(1:3, 2), Yield = c(5557, 5706, 5410, 4652, 4965, 4810)), startRow = 2)
  openxlsx::mergeCells(wb, "Data", cols = 1:3, rows = 1); openxlsx::mergeCells(wb, "Data", cols = 1, rows = 3:5); openxlsx::mergeCells(wb, "Data", cols = 1, rows = 6:8)
  f <- tempfile(fileext = ".xlsx"); openxlsx::saveWorkbook(wb, f)
  x <- star_read_table(f, "a.xlsx")
  expect_equal(names(x), c("Variety", "Rep", "Yield"))
  expect_equal(x$Variety, rep(c("RD41", "RD47"), each = 3))
  n <- attr(x, "notes")
  expect_match(n[1], "Repeated the label of 2 merged cells on every row they cover (4 cells filled in).", fixed = TRUE)
  expect_match(n[2], "Skipped 1 line above the column names", fixed = TRUE)
  # a two-row heading: "พันธุ์" merged down two rows, "ผลผลิต" merged across the replications above ซ้ำที่ 1, 2, 3
  wb <- openxlsx::createWorkbook(); openxlsx::addWorksheet(wb, "S")
  openxlsx::writeData(wb, "S", t(c("พันธุ์", "ผลผลิต", NA, NA)), colNames = FALSE, startRow = 1)
  openxlsx::writeData(wb, "S", t(c(NA, "ซ้ำที่ 1", "ซ้ำที่ 2", "ซ้ำที่ 3")), colNames = FALSE, startRow = 2)
  openxlsx::writeData(wb, "S", data.frame(a = c("กข41", "กข47"), b = c(5557, 4652), c = c(5706, 4965), d = c(5410, 4810)), colNames = FALSE, startRow = 3)
  openxlsx::mergeCells(wb, "S", cols = 1, rows = 1:2); openxlsx::mergeCells(wb, "S", cols = 2:4, rows = 1)
  f2 <- tempfile(fileext = ".xlsx"); openxlsx::saveWorkbook(wb, f2)
  h <- star_read_table(f2, "a.xlsx")
  expect_equal(names(h), c("พันธุ์", "ซ้ำที่ 1", "ซ้ำที่ 2", "ซ้ำที่ 3"))
  expect_equal(h[["ซ้ำที่ 3"]], c("5410", "4810"))
  # a file that is not a valid zip gives no merges rather than an error
  expect_equal(nrow(star_xlsx_merges(w("not a zip"))), 0)
})

test_that("diagnostics: outliers are found (row 5 of Insecticides is borderline, a x6 value is clear), a clean set is not flagged", {
  clean <- star_aov(rcbd, "Yield", "Nitrogen", "Rep", design = "RCBD")$diagnostics
  expect_s3_class(clean, "star_diagnostics")
  expect_equal(clean$n_outliers, 0)
  bad <- Insecticides; bad$GrainYield[5] <- bad$GrainYield[5] * 6
  dg <- star_aov(bad, "GrainYield", "Insecticide", design = "CRD")$diagnostics
  expect_gte(dg$n_outliers, 1)
  expect_equal(dg$outliers$Row[1], "5")
  expect_identical(dg$ok, FALSE)
  expect_match(dg$messages[1], "outlier")
})

test_that("diagnostics: work for two factors with blocks and report variance heterogeneity", {
  dg <- star_aov(tg, "len", c("supp", "dose"), design = "CRD")$diagnostics
  expect_true(is.finite(dg$normality$p)); expect_true(is.finite(dg$homogeneity$p))
  het <- rcbd; het$Yield[het$Nitrogen == levels(het$Nitrogen)[1]] <- het$Yield[het$Nitrogen == levels(het$Nitrogen)[1]] * c(1, 3, 0.2)
  expect_s3_class(star_diagnostics(het, "Yield", "Nitrogen", "Rep"), "star_diagnostics")
})

test_that("Bonferroni letters, partial eta squared, APA text and the Lilliefors test", {
  r <- star_aov(Insecticides, "GrainYield", "Insecticide", design = "CRD", test = "bonferroni")
  lsd <- star_aov(Insecticides, "GrainYield", "Insecticide", design = "CRD", test = "LSD")
  expect_equal(r$anova, lsd$anova)
  expect_equal(r$means$Insecticide$Treatment, lsd$means$Insecticide$Treatment)
  expect_false(identical(r$means$Insecticide$Group, lsd$means$Insecticide$Group))   # Bonferroni is more conservative
  ss <- r$anova$SS
  expect_equal(r$effects$PartialEta2[1], ss[1] / (ss[1] + ss[2]))
  expect_match(r$apa[1], "^The main effect of Insecticide on GrainYield was significant, F\\(6, 21\\) = 9\\.83, p < \\.001, partial eta squared = \\.74\\.$")
  expect_true(is.finite(r$diagnostics$ks$p))
})

# ---- ggplot2 versions of the plots ------------------------------------------------------------------------------
test_that("ggplot2 mean plots build for every effect, error type and layout", {
  skip_if_not_installed("ggplot2")
  one <- star_aov(Insecticides, "GrainYield", "Insecticide", design = "CRD")
  two <- star_aov(GomezSplitRCBD, "Yield", c("Nitrogen", "Variety"), "Rep", design = "RCBD")
  expect_s3_class(star_ggplot_means(one), "ggplot")
  expect_s3_class(star_ggplot_means(one, data = Insecticides), "ggplot")        # observations added
  for (er in c("se", "sd", "ci")) expect_s3_class(star_ggplot_means(two, "Nitrogen", data = GomezSplitRCBD, error = er), "ggplot")
  for (ef in c("interaction", "interaction_line")) {
    expect_s3_class(star_ggplot_means(two, ef), "ggplot")
    expect_s3_class(star_ggplot_means(two, ef, data = GomezSplitRCBD), "ggplot")
  }
  expect_error(star_ggplot_means(two, "nope"), "Unknown effect")
  expect_error(star_ggplot_means(one, "interaction"), "two-factor")
  expect_error(star_ggplot_means(one, points = TRUE), "needs the 'data'")
  f <- tempfile(fileext = ".png"); png(f)
  for (p in list(star_ggplot_means(one, data = Insecticides), star_ggplot_means(two, "interaction", data = GomezSplitRCBD),
                 star_ggplot_means(two, "interaction_line"), star_ggplot_diagnostics(one))) expect_no_error(print(p))
  dev.off(); unlink(f)
})

test_that("SD and CI error bars follow from SE and N", {
  skip_if_not_installed("ggplot2")
  one <- star_aov(Insecticides, "GrainYield", "Insecticide", design = "CRD")
  m <- one$means$Insecticide
  d <- ggplot2::ggplot_build(star_ggplot_means(one, error = "sd"))$data
  expect_equal(d[[2]]$ymax, m$Mean + m$SE * sqrt(m$N))        # layer 2 = error bars
  ci <- ggplot2::ggplot_build(star_ggplot_means(one, error = "ci"))$data[[2]]
  expect_equal(ci$ymax, m$Mean + qt(0.975, m$N - 1) * m$SE)
})

test_that("star_ggsave writes png, svg and pdf", {
  skip_if_not_installed("ggplot2")
  one <- star_aov(Insecticides, "GrainYield", "Insecticide", design = "CRD")
  p <- star_ggplot_means(one)
  for (ext in c("png", "svg", "pdf")) {
    f <- tempfile(fileext = paste0(".", ext)); star_ggsave(p, f, dpi = 100)
    expect_gt(file.size(f), 500); unlink(f)
  }
  expect_error(star_ggsave(p, tempfile(fileext = ".bmp")), "png, .svg or .pdf")
})

test_that("custom title and axis labels are used; empty text keeps the default", {
  skip_if_not_installed("ggplot2")
  one <- star_aov(Insecticides, "GrainYield", "Insecticide", design = "CRD")
  p <- star_ggplot_means(one, main = "My title", xlab = "Product", ylab = "Yield (kg/ha)")
  expect_equal(c(p$labels$title, p$labels$x, p$labels$y), c("My title", "Product", "Yield (kg/ha)"))
  q <- star_ggplot_means(one, main = "", xlab = "  ", ylab = NULL)
  expect_equal(c(q$labels$title, q$labels$x, q$labels$y), c("Mean GrainYield by Insecticide", "Insecticide", "Mean GrainYield"))
})
