test_that("star_gg_font() switches to the Thai font only when the plot has Thai text", {
  withr::local_options(star.plot_family_thai = "Thai Font")
  fam <- function(p) p$theme$text$family %||% ""
  latin <- ggplot2::ggplot(data.frame(Variety = c("RD41", "RD47"), y = 1:2), ggplot2::aes(Variety, y)) + ggplot2::geom_col()
  expect_identical(star_gg_font(latin), latin)
  expect_identical(fam(star_gg_font(latin + ggplot2::labs(title = "ผลผลิต"))), "Thai Font")
  # Thai only in the data (treatment names) or only in a column name used in aes() (the default axis title)
  in_data <- ggplot2::ggplot(data.frame(Variety = c("กข41", "กข47"), y = 1:2), ggplot2::aes(Variety, y)) + ggplot2::geom_col()
  expect_identical(fam(star_gg_font(in_data)), "Thai Font")
  in_aes <- ggplot2::ggplot(data.frame(`พันธุ์` = c("a", "b"), y = 1:2, check.names = FALSE), ggplot2::aes(`พันธุ์`, y)) + ggplot2::geom_col()
  expect_identical(fam(star_gg_font(in_aes)), "Thai Font")
  # no Thai font configured: unchanged
  withr::local_options(star.plot_family_thai = "")
  expect_identical(star_gg_font(in_data), in_data)
})

test_that("star_gg_fit_width() wraps the subtitle to the figure width, line by line", {
  sub <- paste(c(rep("word", 40), "\nsecond line"), collapse = " ")
  p <- ggplot2::ggplot() + star_gg_theme(9, family = "") + ggplot2::labs(subtitle = sub)
  narrow <- strsplit(star_gg_fit_width(p, 8.5 / 2.54)$labels$subtitle, "\n")[[1]]
  wide <- strsplit(star_gg_fit_width(p, 10)$labels$subtitle, "\n")[[1]]
  expect_gt(length(narrow), length(wide))
  expect_identical(trimws(utils::tail(wide, 1)), "second line")
  expect_identical(gsub("\\s+", " ", paste(narrow, collapse = " ")), gsub("\\s+", " ", sub))
  # no subtitle, or not a ggplot: unchanged
  bare <- ggplot2::ggplot()
  expect_identical(star_gg_fit_width(bare, 3), bare)
  expect_identical(star_gg_fit_width("x", 3), "x")
})

test_that("text inside plots follows base_size", {
  expect_equal(.star_gg_text_size(12, 1) * ggplot2::.pt, 12)
  expect_equal(.star_gg_text_size(9) / .star_gg_text_size(18), 0.5)
})

test_that("Excel cells are read as typed: numbers with 15 digits, no exponent, empty cells NA", {
  f <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(data.frame(num = c(0.1 + 0.2, 2.3199999999999998, 100000, 1e-20),
                                 lgl = c(TRUE, NA, FALSE, NA), txt = c("RD41", NA, "กข47", "")), f)
  cells <- star_excel_cells(f)
  expect_identical(names(cells), c("V1", "V2", "V3"))
  expect_identical(cells$V1, c("num", "0.3", "2.32", "100000", "0.00000000000000000001"))
  expect_identical(cells$V2, c("lgl", "TRUE", NA, "FALSE", NA))
  expect_identical(cells$V3[1:4], c("txt", "RD41", NA, "กข47"))
  expect_identical(star_cell_text(list(NA, 4.1, "a", NULL)), c(NA, "4.1", "a", NA))
})
