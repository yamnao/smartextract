## Function to plot information saved in the logfile
#' @details Function to plot information
#' @param log_file Metadata file generated during extraction
#' @return jpg figure
#' @export
f_plot_log_file <- function(log_file){

  # Calculate proportions for Plot 1
  data_plot1 <- log_file |>
    dplyr::count(type_survey) |>
    dplyr::mutate(percentage = n / sum(n) * 100,
           label = paste0(round(percentage, 1), "%"))

  # Plot 1: Pie chart for type_survey with percentages
  plot1 <- ggplot2::ggplot(data = data_plot1, ggplot2::aes(x = "", y = n, fill = type_survey)) +
    ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::theme_void() +
    ggplot2::geom_text(ggplot2::aes(label = label), position = ggplot2::position_stack(vjust = 0.5)) +
    ggplot2::labs(title = "Analysis of SMART surveys by type.", fill="Type of Surveys") +
    ggplot2::scale_fill_manual(values = c("#56B4E9", "#009E73", "#F0E442")) # Adjust colors if needed

  # Calculate proportions for Plot 2
  data_plot2 <- log_file |>
    dplyr::count(is_issue) |>
    dplyr::mutate(percentage = n / sum(n) * 100,
           label = paste0(round(percentage, 1), "%"))

  # Plot 2: Pie chart for is_issue with percentages
  plot2 <- ggplot2::ggplot(data = data_plot2, ggplot2::aes(x = "", y = n, fill = is_issue)) +
    ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::theme_void() +
    ggplot2::geom_text(ggplot2::aes(label = label), position = ggplot2::position_stack(vjust = 0.5)) +
    ggplot2::labs(title = "Percentage of issues raised when extracting SMART contents.", fill = "Issues found:") +
    ggplot2::scale_fill_manual(values = c("FALSE" = "#56B4E9", "TRUE" = "#E69F00"))

  log_file$min_year_survey <- as.numeric(lubridate::year(as.Date(paste0(log_file$min_year_survey, '/', log_file$min_month_survey, '/', log_file$min_day_survey))))
  plot3 <- ggplot2::ggplot(data = log_file, ggplot2::aes(min_year_survey))+
    ggplot2::geom_histogram(fill="#009E73")+
    ggplot2::theme_bw() +
    ggplot2::labs(x=ggplot2::element_blank(), y=ggplot2::element_blank(),
                  title='Analysis of SMART survey content extracted per year.')

  plot4 <- cowplot::plot_grid(plot1, plot2, plot3, labels = "AUTO")
  return(plot4)
}
