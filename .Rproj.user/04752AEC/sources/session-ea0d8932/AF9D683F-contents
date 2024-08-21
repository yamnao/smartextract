## Function to plot information saved in the logfile
#' @details Function to plot information
#' @param log_file Metadata file generated during extraction
#' @return jpg figure
#' @export
f_plot_log_file <- function(log_file){
  # Type of survey
  plot1 <- ggplot2::ggplot(data = log_file, ggplot2::aes(x = type_survey)) +
    ggplot2::geom_bar(fill="#56B4E9") +
    ggplot2::theme_bw() +
    ggplot2::labs(x=ggplot2::element_blank(), y=ggplot2::element_blank(),
                  title='Comparison of SMART Surveys per type')
  plot2 <- ggplot2::ggplot(data = log_file, ggplot2::aes(is_issue))+
    ggplot2::geom_bar(fill="#E69F00") +
    ggplot2::scale_x_discrete(labels=c("FALSE" = "Without any issue",
                                       "TRUE" = 'With an Issue'))+
    ggplot2::theme_bw() +
    ggplot2::labs(x=ggplot2::element_blank(), y=ggplot2::element_blank(),
                  title='Comparison of SMART Surveys with and without any issues')
  plot3 <- ggplot2::ggplot(data = log_file, ggplot2::aes(min_year_survey))+
    ggplot2::geom_histogram(fill="#009E73")+
    ggplot2::theme_bw() +
    ggplot2::labs(x=ggplot2::element_blank(), y=ggplot2::element_blank(),
                  title='Comparison of SMART Surveys per year')
  plot4 <- cowplot::plot_grid(plot1, plot2, plot3, labels = "AUTO")
  return(plot4)
}
