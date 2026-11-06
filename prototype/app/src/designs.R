examples <- list(
  CRD = c("One factor: insecticides (7 x 4 reps)" = "crd1", "Two factors: tooth growth (supp x dose)" = "crd2"),
  RCBD = c("One factor: rice yield vs nitrogen" = "rcbd1", "Two factors: nitrogen x variety" = "rcbd2"),
  SplitCRD = c("Rice: irrigation (main plot) x variety (sub plot), simulated" = "splitcrd1"),
  SplitRCBD = c("Rice: nitrogen (main plot) x variety (sub plot)" = "split1"),
  CombRCBD = c("Rice: nitrogen rates in the dry and wet seasons (Gomez & Gomez)" = "comb_rcbd1"),
  CombCRD = c("Pot trial: tillers of four varieties at three sites, simulated" = "comb_crd1"),
  RepRCBD = c("Plant height of four fertilizers at 30, 60, 90 and 120 days, simulated" = "rep_rcbd1"),
  RepCRD = c("Pot trial: plant height of four fertilizers at 30-120 days, simulated" = "rep_crd1"))
example_setup <- list(
  crd1 = list(file = "example_crd.csv", resp = "GrainYield", trt = "Insecticide", blk = NULL),
  crd2 = list(file = "example_crd2.csv", resp = "len", trt = c("supp", "dose"), blk = NULL),
  rcbd1 = list(file = "example_rcbd.csv", resp = "Yield", trt = "Nitrogen", blk = "Rep"),
  rcbd2 = list(file = "example_rcbd2.csv", resp = "Yield", trt = c("Nitrogen", "Variety"), blk = "Rep"),
  splitcrd1 = list(file = "example_splitcrd.csv", resp = "Yield", trt = c("Irrigation", "Variety"), blk = "Rep"),
  split1 = list(file = "example_split.csv", resp = "Yield", trt = c("Nitrogen", "Variety"), blk = "Rep"),
  comb_rcbd1 = list(file = "example_comb_rcbd.csv", resp = "GrainYield", trt = "NitrogenRate", env = "Season", blk = "Rep"),
  comb_crd1 = list(file = "example_comb_crd.csv", resp = "Tillers", trt = "Variety", env = "Site", blk = NULL),
  rep_rcbd1 = list(file = "example_rep_rcbd.csv", resp = "Height", trt = c("Fertilizer", "Day"), blk = "Block"),
  rep_crd1 = list(file = "example_rep_crd.csv", resp = "Height", trt = c("Fertilizer", "Day"), blk = "Pot"))
# repeated measures over time are analysed as a split plot in time (treatment = main plot, time = sub plot), so they
# share the split-plot screens; is_rep() picks out the places where the wording or the tests differ
is_rep <- function(design) design %in% c("RepCRD", "RepRCBD")
is_split <- function(design) design %in% c("SplitCRD", "SplitRCBD", "RepCRD", "RepRCBD")
is_comb <- function(design) design %in% c("CombCRD", "CombRCBD")
needs_block <- function(design) !design %in% c("CRD", "CombCRD")
