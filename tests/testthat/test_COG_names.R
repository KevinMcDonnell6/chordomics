context("LCA_alg")
library(chordomics)

test_data <- data.frame(uniprots=c("P21514, P21515", "P41543"), other=c("other", "data"), stringsAsFactors = F)
test_results <- cbind(test_data, data.frame(Uniprot=c("P21514, P21515", "P41543"),
                                            AllCOGs=c("COG2200 COG2771 COG3124", ""),
                                            UniqueCOGs=c("COG2200 COG2771 COG3124", ""), stringsAsFactors = F)
                      )
test_that("Check function fetching list of COGs for uniprot IDs", {
  expect_equal(
    chordomics:::Get_COG(df=test_data, UniprotColumn="uniprots"),
    test_results
  )
})

