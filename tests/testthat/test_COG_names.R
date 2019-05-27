context("LCA_alg")
library(chordomics)

test_data <- data.frame(
  uniprots = c("P21514, P21515", "P41543"),
  other = c("other", "data"),
  Uniprot = c("P21514, P21515", "P41543"),
  AllCOGs = c("COG2200 COG2771 COG3124", ""),
  UniqueCOGs = c("COG2200 COG2771 COG3124", ""),
  stringsAsFactors = F
)
results <- structure(
  list(
    uniprots = c("P21514, P21515", "P41543"),
    other = c("other", "data"),
    Uniprot = c("P21514, P21515", "P41543"),
    AllCOGs = c("COG2200 COG2771 COG3124",  ""),
    UniqueCOGs = c("COG2200", NA),
    COG_Category = c("T", NA),
    COG_Name = c("EAL domain, c-di-GMP-specific phosphodiesterase class I (or its enzymatically inactive variant)", NA)),
  row.names = c(NA, -2L),
  class = "data.frame")
test_that("Check function fetching list of COGs for uniprot IDs", {
  expect_equal(
    chordomics:::COG_names(df=test_data, UniqueCOGs = "UniqueCOGs"),
    results
  )
})

