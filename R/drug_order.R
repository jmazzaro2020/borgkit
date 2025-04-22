#' Set the order of drug levels in the overextension data.
#'
#' This function reorders the levels of the 'drug' factor column in a data frame.
#' You can either provide a specific order for the levels or let the function
#' use the unique drug names in the order they appear in the data.
#'
#' @param n A data frame containing a column named 'drug'.
#' @param drug_column The name of the column that contains the drugs, in quotation marks
#' @param ... list the name of each drug in the order you want them. Each should be in
#'    quotation marks with a comma separating them.
#' @return The input data frame with the 'drug' column converted to an **ordered factor**
#'   with levels set according to the list provided for ...
#' @examples
#'
#' # Example usage:
#' # ordered_data_drug <- set_drug_order(example_data, "drug", "DMSO", "Taxol", "Colchicine")
#' # levels(ordered_data_drug$drug)

set_drug_order <- function(n, drug_column, ...) {
  # ai gave me the ... idea
  drug_levels <- c(...)
  missing_levels <- setdiff(drug_levels, unique(n[[drug_column]]))
  if (length(missing_levels) > 0) {
    warning("The following drug levels are not present in the data: ",
            paste(missing_levels, collapse = ", "))
  }

  n[[drug_column]] <- factor(n[[drug_column]], levels = drug_levels, ordered = TRUE)

  return(n)
}
