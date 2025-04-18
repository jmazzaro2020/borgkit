library(readxl)

overextension_example_excel <- system.file("extdata", "Overextension_drug_example.xlsx", package = "BorgenAssay")
overextension_example_df <- read_excel(overextension_example_excel)
usethis::use_data(overextension_example_df, overwrite = TRUE)
