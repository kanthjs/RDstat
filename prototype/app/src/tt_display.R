# t-test tables; numbers below 1 get more decimals
tt_digits <- function(r) if (max(abs(r$groups$Mean), na.rm = TRUE) < 1) 4 else 2
tt_desc_display <- function(r) {
  z <- r$groups; k <- tt_digits(r)
  out <- data.frame(Group = z$Group, N = ifelse(is.na(z$N), "", formatC(z$N, format = "d")), Mean = fmt_n(z$Mean, k), Median = fmt_n(z$Median, k), SD = fmt_n(z$SD, k),
                    SE = fmt_n(z$SE, k), Min = fmt_n(z$Min, k), Max = fmt_n(z$Max, k), check.names = FALSE, stringsAsFactors = FALSE)
  names(out)[1] <- switch(r$type, independent = r$group, paired = "Measurement", one = "Variable")
  out
}
tt_test_display <- function(r) {
  z <- r$test; k <- tt_digits(r)
  out <- data.frame(Method = z$Method, t = fmt_n(z$t, 3), DF = vapply(z$DF, .star_tt_df, ""), p = fmt_p(z$P), Difference = fmt_n(z$Difference, k),
                    SE = fmt_n(z$SE, k), Lower = fmt_n(z$Lower, k), Upper = fmt_n(z$Upper, k), check.names = FALSE, stringsAsFactors = FALSE)
  names(out)[7:8] <- paste0(c("Lower ", "Upper "), format(100 * r$conf_level), "% CI")
  if (r$alternative != "two.sided") names(out)[4] <- paste0("p (one-sided, ", r$alternative, ")")
  if (nrow(out) > 1) out$Used <- ifelse(seq_len(nrow(out)) == r$chosen, "yes", "")
  out
}
tt_nonpar_display <- function(r) {
  z <- r$nonparametric; k <- tt_digits(r)
  out <- data.frame(Method = z$Method, Statistic = z$Statistic, p = fmt_p(z$P), Estimate = fmt_n(z$Estimate, k),
                    Lower = fmt_n(z$Lower, k), Upper = fmt_n(z$Upper, k), check.names = FALSE, stringsAsFactors = FALSE)
  names(out)[5:6] <- paste0(c("Lower ", "Upper "), format(100 * r$conf_level), "% CI")
  if (r$alternative != "two.sided") names(out)[3] <- paste0("p (one-sided, ", r$alternative, ")")
  out
}
tt_normal_display <- function(r) {
  z <- r$normality
  data.frame(Data = z$Data, N = formatC(z$N, format = "d"), `Shapiro-Wilk W` = fmt_n(z$W, 4), p = fmt_p(z$P),
             Verdict = ifelse(is.na(z$P), "not tested", ifelse(z$P < r$alpha, "departs from normal", "no evidence against normal")),
             check.names = FALSE, stringsAsFactors = FALSE)
}
