#' Set the order of phenotype levels in the overextension data.
#'
#' This function reorders the levels of the 'phenotype' factor column in a data frame.
#' You can either provide a specific order for the levels or let the function
#' use the unique phenotype names in the order they appear in the data.
#'
#' @param n A data frame containing a column named 'phenotype'.
#' @param phenotype_column The name of the column that contains the phenotypes, in quotation marks
#' @param ... list the name of each phenotype in the order you want them, with the first in the list
#'    being the top of the stacked bar graph. Each should be in quotation marks with a comma separating them.
#' @return The input data frame with the 'phenotype' column converted to an **ordered factor**
#'   with levels set according to the list provided for ...
#'
#' @export
#'
#' @examples
#'
#' # Example usage with a custom order of levels
#' # ordered_data_pheno <- set_phenotype_order(example_data_pheno, "phenotype", "severe", "mod")
#' # levels(ordered_data_pheno$phenotype)

set_phenotype_order <- function(n, phenotype_column, ...) {
  # ai gave me the ... idea
  phenotype_levels <- c(...)
  n[[phenotype_column]] <- factor(n[[phenotype_column]], levels = phenotype_levels, ordered = TRUE)

  return(n)
}
