#' Create a bar graph of tau gap scoring.
#'
#' This function takes a data frame as input and generates a bar graph
#' visualizing tau gap scores, grouped by KO type and day, with error bars.
#'
#' @param data A data frame containing the data for the bar graph.
#'   It should have columns named:
#'   \itemize{
#'     \item{\strong{genotype}}: A factor or character column representing the KO genotype
#'     \item{\strong{day}}: A numeric or character column representing the day,
#'     \item{\strong{gaps}}: A numeric column containing the number of gaps.
#'     \item{\strong{error}}: A numeric column containing the error values for
#'       the error bars.
#'   }
#' @param x_label what you want the x axis to be labeled as
#' @param y_label what you want to y axis to be labeled as
#' @param plot_title what you want the title of the plot to be.
#' @param y_limits what you want the scale of the y-axis to be based on the lowest and
#'  highest values for your gap data. The default is 0 to 5, or y_limits = c(0, 5)
#'
#' @returns A ggplot object representing the bar graph.
#'
#' @examples
#' #tau_gap_plot <- tau_gap_bargraph(tau_data, "Genotype", "Gaps", "Scoring Gaps in the Motor Cord")
#' #tau_gap_plot
#' #If you have a value in the gaps column higher than 5, do the following:
#' #tau_gap_bargraph(data, y_limits = c(0, 6))
#'
#' @export

tau_gap_bargraph <- function(data,
                             x_label = "Genotype",
                             y_label = "# of Gaps",
                             plot_title = "your title here",
                             y_limits = c(0, 5))
{
  data$day <- as.factor(data$day) #i was getting an error in the scale_fill_manual(). AI troubleshooting
                                  #told me to make day a factor.

  #checking that the columns are correctly named
  col_names <- c("genotype", "day", "gaps", "error")
  missing_cols <- setdiff(col_names, colnames(data))
  if(length(missing_cols)>0) {
    stop("Error - The following columns are missing from the data:",
         paste(missing_cols, collapse = ", "))
  }

  # I couldn't figure out how to fix my error bars.
  # AI said to get the stacked heights for error bars, and showed me how
  data <- data %>%
    dplyr::group_by(across(c(genotype, !!!rlang::syms(facet_by)))) %>%
    dplyr::arrange(phenotype, .by_group = TRUE) %>%
    dplyr::mutate(cumulative = cumsum(countTotal),
                  ymax = if_else(row_number() == n(), cumulative + SE, NA_real_),
                  ymin = if_else(row_number() == n(), cumulative - SE, NA_real_)) %>%
    dplyr::ungroup()

  plot <- ggplot2::ggplot(data,
                       ggplot2::aes(x = .data[["genotype"]],
                           y = .data[["gaps"]],
                           fill = .data[["day"]])) +
    ggplot2::geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
    ggplot2::geom_errorbar(aes(ymin = .data[["gaps"]] - .data[["error"]],
                      ymax = .data[["gaps"]] + .data[["error"]]),
                  position = position_dodge(width = 0.8), width = 0.25) +
    ggplot2::labs(
      title = plot_title,
      x = x_label,
      y = y_label,
      fill = "Day"
    ) +
    ggplot2::theme_classic() +
    ggplot2::scale_fill_manual(values = c("1" = "grey60", "6" = "black")) +
    ggplot2::scale_y_continuous(limits = y_limits)

  return(plot)
}

