context("LCA_alg")
library(chordomics)
library(ncbiLineages)
#processData <-  processMGRAST("mgm4762935.3", DATA_DIR, output, e=environment())
#dput(processData[1:6,1:3])
test_input <-
  structure(
    list(
      id = c(
        "mgm4762935.3|contig_101_1169455_length_3868_multi_2_in_0_out_0_1_1043_+",
        "mgm4762935.3|contig_101_1488739_length_29947_multi_2_in_0_out_0_1_611_-",
        "mgm4762935.3|contig_101_1488739_length_29947_multi_2_in_0_out_0_15169_16711_+",
        "mgm4762935.3|contig_101_1488739_length_29947_multi_2_in_0_out_0_21654_22613_-",
        "mgm4762935.3|contig_101_1488739_length_29947_multi_2_in_0_out_0_24170_25363_-",
        "mgm4762935.3|contig_101_1488739_length_29947_multi_2_in_0_out_0_27029_27858_+"
      ),
      taxids_by_seq = c("55802","83483", "53363;1744;69014;49338","61435","2208;2214;33075","2209;49338"),
      COGs_by_seq = c(NA, "COG0013", "COG0513", "COG2041", "COG0229", "COG0605")),
    row.names = c(NA, 6L),
    class = "data.frame"
  )

test_that("Test assign_taxa", {
  expect_equal(c("Euryarchaeota", "Euryarchaeota", "", "Chloroflexi", "", ""),
               chordomics:::assign_taxa(df = test_input,shinylogs = NULL)$phylum)
})

