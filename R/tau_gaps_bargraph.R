#' Create a bar graph of tau gap scoring.
#'
#' This function takes a data frame as input and generates a bar graph
#' visualizing tau gap scores, grouped by KO type and day, with error bars.
#'
#' @param data A data frame containing the data for the bar graph.
#'   It should have columns named:
#'   \itemize{
#'     \item{\strong{KO}}: A factor or character column representing the KO genotype
#'     \item{\strong{gaps}}: A numeric column containing the number of gaps.
#'     \item{\strong{day}}: A numeric or character column representing the day,
#'     \item{\strong{error}}: A numeric column containing the error values for
#'       the error bars.
#'   }
#' @param x_label what you want the x axis to be labeled as
#' @param y_label what you want to y axis to be labeled as
#' @param plot_title what you want the title of the plot to be.
#'
#' @returns A ggplot object representing the bar graph.
#'
#' @examples
#' #tau_gap_plot <- tau_gap_bargraph(tau_data, "KO", "Gaps", "Scoring Gaps in the Motor Cord")
#' #tau_gap_plot
#'
#' @export

tau_gap_bargraph <- function(data,
                             x_label = "Genotype",
                             y_label = "# of Gaps",
                             plot_title = "your title here")
{
  data$day <- as.factor(data$day) #i was getting an error in the scale_fill_manual(). AI troubleshooting
                                  #told me to make day a factor.
  p <- ggplot2::ggplot(data,
                       ggplot2::aes(x = .data[["KO"]],
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
    ggplot2::scale_y_continuous(limits = c(0, 3))

  return(p)
}

