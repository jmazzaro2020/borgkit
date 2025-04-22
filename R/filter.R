#' Filter out rows where the phenotype is "wt"
#'
#' This function takes a data frame as input and returns a new data frame
#' with rows where the 'phenotype' column is not equal to "wt".
#'
#' @param n A data frame containing a column named 'phenotype'.
#' @returns A data frame with rows where the 'phenotype' column is not "wt".
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @export
#'
#' @examples
#' # Assuming you have a data frame named data
#' # run phenotype_filter_wt(data) and save it to a name you want to use

phenotype_filter_wt <- function(n) {

  #check if the phenotype column exists
   if (!"phenotype" %in% colnames(n)) {
    stop("Column `phenotype` not found in the input data frame.")
  }

  #check if wt is present in the phenotype column
  if (!"wt" %in% n$phenotype) {
    warning("No rows with phenotype 'wt' found — nothing will be filtered.")
  }
  n %>%
    dplyr::filter(phenotype != "wt")
}

