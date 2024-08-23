#' @title Split large plots
#' 
#' @description It split large plots into four subplots.
#' 
#' @param inventory_large A \code{data.table} or \code{data.frame} with the inventory data. 
#' It most contain three columns labeled: 'plot', 'row', and 'column'.
#' Only use plots that require splitting into subplots.
#' 
#' @details The function assume that all the plots contain the same amount of rows
#' and columns of trees planted. It also assume that rows and columns are numbered
#' continuously from 0 to the max number of rows and columns.
#'  
#' @return A \code{data.table) with the subplots. The function will add a new column 
#' with the new plot id ('plot_new) and will modify the row and column number based
#' on the new plots.

split_plots <- function(inventory) {
  
  # Make a copy
  frame <- inventory
  
  # Identify range
  vtree_lines <- unique(frame$row)
  htree_lines <- unique(frame$column)
  
  # Max column and rows
  line_max <- max(vtree_lines, htree_lines)
  half_plot <- line_max/2
  
  # Define new range
  lower_range <- c(1, half_plot)
  upper_range <- c(half_plot+1, line_max)
  
  frame$plot_new <- "0"
  frame[row >= lower_range[1] & row <= lower_range[2] & column >= lower_range[1] & column <= lower_range[2], plot_new := paste0(plot, "a")]
  frame[row >= lower_range[1] & row <= lower_range[2] & column >= upper_range[1] & column <= upper_range[2], plot_new := paste0(plot, "b")]
  frame[row >= upper_range[1] & row <= upper_range[2] & column >= lower_range[1] & column <= lower_range[2], plot_new := paste0(plot, "c")]
  frame[row >= upper_range[1] & row <= upper_range[2] & column >= upper_range[1] & column <= upper_range[2], plot_new := paste0(plot, "d")]
  
  value <- 1
  
  # Rename rows and columns according to new plots
  for(i in upper_range[1]:upper_range[2]) {
    
    # Rename row and columns
    frame[row == i, row := value]
    frame[column == i, column := value]
    value <- value + 1
    
  }
  
  # Return frame
  return(frame)
  
}