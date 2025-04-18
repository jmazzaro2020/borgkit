#' Create a stacked bar plot with error bars, with optional faceting.
#'
#' `overextension_stacked_bar()` generates a stacked bar plot to visualize
#' the distribution of a phenotype across different genotypes, and can account for
#' drug usage. It includes error bars to represent variability and offers optional faceting based on
#' specified columns.
#'
#' @param data A data frame containing the data for the plot. It is expected
#'   to have columns for 'genotype', 'countTotal', 'phenotype', and 'SE' (standard error).
#'   The 'drug' and 'concentration_mM' columns are optional for faceting.
#' @param x_label Character string specifying the label for the x-axis.
#'   Defaults to "Genotype".
#' @param y_label Character string specifying the label for the y-axis.
#'   Defaults to "Count".
#' @param plot_title Character string specifying the title of the plot.
#'   Defaults to "Your title here".
#' @param fill_colors Named character vector specifying the colors to use for
#'   the different levels of the 'phenotype' column. Defaults to
#'   `c("severe" = "black", "mod" = "grey60")`.
#' @param error_bar_width Numeric value specifying the width of the error bars.
#'   Defaults to 0.3.
#' @param error_bar_color Character string specifying the color of the error bars.
#'   Defaults to "black".
#' @param facet_by Character vector specifying the names of the columns in `data`
#'   to use for faceting the plot. For example, `c("drug", "concentration_mM")`.
#'   If `NULL`, no faceting will be applied. Defaults to
#'   `c("drug", "concentration_mM")`.
#'
#' @returns A ggplot object representing the stacked bar plot with error bars
#'   and optional facets.
#'
#' @details
#' The function assumes that the input `data` frame contains columns named:
#' \itemize{
#'   \item{\strong{genotype}:} A factor or character column representing the genotype.
#'   \item{\strong{countTotal}:} A numeric column representing the total count for each genotype and phenotype.
#'   \item{\strong{phenotype}:} A factor or character column representing the phenotype.
#'   \item{\strong{SE}:} A numeric column representing the standard error for the error bars.
#'   \item{\strong{drug}:} A factor or character column representing the drug treatment.
#'   \item{\strong{concentration_mM}:} A numeric column representing the drug concentration in mM. Ensure this matches the actual column name in your data.
#' }
#'
#' @examples
#' # Example data
#' data <- data.frame(
#'   genotype = rep(c("A", "B"), each = 4),
#'   phenotype = rep(c("severe", "mod"), each = 2, times = 2),
#'   countTotal = c(20, 30, 15, 35, 25, 25, 10, 40),
#'   SE = c(2, 3, 1.5, 3.5, 2.5, 2.5, 1, 4),
#'   drug = factor(rep(c("Drug1", "None"), each = 4)),
#'   concentration_mM = rep(c(1, 0), each = 4)
#' )
#'
#' # Basic stacked bar plot
#' overextension_stacked_bar(data)
#'
#' # Stacked bar plot with custom labels and title
#' overextension_stacked_bar(data,
#'   x_label = "Genetic Variant",
#'   y_label = "Number of Individuals",
#'   plot_title = "Phenotype Distribution"
#' )
#'
#' # Stacked bar plot with custom colors
#' overextension_stacked_bar(data,
#'   fill_colors = c("severe" = "red", "mod" = "lightblue")
#' )
#'
#' # Different error bar appearance
#' custom_error_bars_plot <- overextension_stacked_bar(data,
#'   error_bar_width = 0.5,
#'   error_bar_color = "darkgrey"
#' )
#'
#' # Stacked bar plot without faceting
#' overextension_stacked_bar(data, facet_by = NULL)
#'
#' # Stacked bar plot faceted by multiple variables
#' overextension_stacked_bar(data, facet_by = c("drug", "concentration_mM"))
#'
#' # Faceting by only one variable
#' facet_by_drug_plot <- overextension_stacked_bar(data,
#'   facet_by = "drug"
#' )
#'
#' @importFrom ggplot2 ggplot
#' @export
overextension_stacked_bar <- function(data,
                                      x_label = "Genotype",
                                      y_label = "Count",
                                      plot_title = "Your title here",
                                      fill_colors = c("severe" = "black", "mod" = "grey60"),
                                      error_bar_width = 0.3,
                                      error_bar_color = "black",
                                      facet_by = c("drug", "concentration_mM")) {

  plot <- ggplot2::ggplot(data,
                          ggplot2::aes(x = .data[["genotype"]],
                              y = .data[["countTotal"]],
                              fill = .data[["phenotype"]])) +
   ggplot2::geom_bar(stat = "identity", position = "stack") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = countTotal - SE,
                      ymax = countTotal + SE),
                  width = error_bar_width,
                  color = error_bar_color) +
    ggplot2::labs(x = x_label,
         y = y_label,
         fill = "Phenotype",
         title = plot_title) +
    ggplot2::scale_fill_manual(values = fill_colors) +
    ggplot2::theme_classic()

  if (!is.null(facet_by)) {
    facet_formula <- as.formula(paste(". ~", paste(facet_by, collapse = " + ")))
    plot <- plot + ggplot2::facet_grid(facet_formula)
  } #I didn't know how to make the facet part customizable, and followed an example given by ai.

  return(plot)
}
