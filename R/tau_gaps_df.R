#' Tau Gap Scoring Data
#'
#' This dataset contains results from tau gap scoring assays for various knockout (KO) genotypes
#' across two time points (Day 1 and Day 6). It is used to evaluate gap formation phenotypes
#' in C. elegans under different genetic conditions.
#'
#' @format A data frame with each row representing an individual observation. The columns include:
#' \describe{
#'   \item{KO}{The genotype of the animal (knockout strain name).}
#'   \item{day}{The experimental day on which the data was collected (either "1" or "6").}
#'   \item{gaps}{The mean number of gaps observed for the given KO and day.}
#'   \item{error}{The standard error associated with the gap count.}
#' }
#'
#' @source Data derived from the `tau_gaps.xlsx` file located in the `inst/extdata/` directory
#' of the Borg-kit package.
"tau_gaps"
