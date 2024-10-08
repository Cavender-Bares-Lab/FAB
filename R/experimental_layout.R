#' @title Experimental Layout
#'
#' @description It creates a pdf of the experimental layout showing individual trees within
#' plots. Useful as a field guide.
#'
#' @param design A \code{data.frame} or \code{data.table} with the description
#' of the experimental layout. It most contain columns with the labels: "block",
#' "plot", "species", "row", and "column".
#'
#' @param output A path and name of the file to export. If \code{NULL}; it will
#' export a pdf to the current working directory under \code{layout.pdf}.
#'
#' @return A \code{PDF} describing the distribution of individuals within
#' each plot.
#'
#' @importFrom ggplot2 ggplot
#'
#' @export
experimental_layout <- function(design, outfile = NULL) {

  # Plot function
  plot_function <- function(plotoi) {

    p <- ggplot(data = plotoi, aes(x = column, y = row, label = species)) +
      #geom_point() +
      geom_text(hjust= 0.5, vjust= .5) +
      scale_x_continuous(n.breaks = max(plotoi$column)) +
      scale_y_reverse(n.breaks = max(plotoi$row)) +
      theme_bw(base_size = 12) +
      labs(title = paste0("Plot: ", plotoi$plot[1]),
           subtitle = paste0("Block: ", plotoi$block[1], " Plot size (m2): ", plotoi$Area_m2[1]))

    return(p)

  }

  # Select unique plot ids
  plot_id <- unique(design$plot)

  # Export pdf
  if(is.null(outfile) == TRUE) {
    name <- paste0(getwd(), "/layout.pdf")
  } else {
    name <- outfile
  }

  # Apply plot in a batch to export
  pdf(name)

  for (i in 1:length(plot_id)) {

    plotoi <- design[plot == plot_id[i], ]
    print(plot_function(plotoi))

  }

  dev.off()

}
