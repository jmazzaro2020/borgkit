library(readxl)
Tau_gaps_example_excel <- system.file("extdata", "tau_gaps.xlsx", package = "borgkit")
tau_gaps_example_df <- read_excel(Tau_gaps_example_excel)
usethis::use_data(tau_gaps_example_df, overwrite = TRUE)
