#' @title Remove plot edge
#' 
#' @description Remove the border line of trees planted within the plots to 
#' avoid edge effects in future analyses. The function first identify the edge line
#' for each plot based on rows and columns, than it remove edge trees.
#' 
#' @param inventory A \code{data.table} or \code{data.frame}. It most contain 
#' three columns labeled: 'plot', 'row', and 'columns'.
#' 
#' @return A \code{data.table} without at the edge line of the plots.
#' 
#' @export
remove_edge <- function(inventory) {
  
  # Makes a copy
  frame <- as.data.table(inventory)
  
  # Identify unique plots
  plot_id <- unique(frame$plot)
  
  # Function to apply in batch
  edge <- function(X,
                   plot_id,
                   frame) {
    
    subplot <- frame[plot == plot_id[X]]
    vrange <- range(subplot$row)
    hrange <- range(subplot$column)
    
    subplot <- subplot[row != vrange[1] & row != vrange[2],]
    subplot <- subplot[column != hrange[1] & column != hrange[2],]
    
    return(subplot)
    
  }
  
  # Batch application
  edge_frame <- lapply(X = 1:length(plots),
                       FUN = edge,
                       plot_id = plot_id,
                       frame = frame)
  
  edge_frame <- do.call(rbind, edge_frame)
  
  return(mortality_frame)
  
}