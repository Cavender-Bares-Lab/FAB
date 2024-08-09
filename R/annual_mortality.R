#' @title Annual Mortality
#'
#' @description It computes the annual mortality on plots since its established.
#'
#' @param inventory A \code{data.table} or \code{data.frame}. It most contain X columns
#' labeled: 'block', 'plot', 'species', and 'dead'.
#' @param per_species Logic. If \code{TRUE}, it estimate the mortality per species
#' whitin plots.
#'
#' @return It returns a \code{data.table} with the estimations of morality per plot.
#'
#' @author J. Antonio Guzmán Q.
#' 
#' @importFrom data.table as.data.table
#' @importFrom data.table data.table
#'
#' @examples

inventory <- fread("data/FAB2_growth.csv")

annual_mortality <- function(inventory, per_species = FALSE) {
  
  frame <- inventory
  frame$year <- year(frame$measurement_date)
  plots <- unique(frame$plot)
  
  mortality <- function(X,
                        plots = plots,
                        frame = frame,
                        per_species = per_species) {
    
    
    plot <- frame[plot == plots[X],]
    years <- unique(plot$year)
    
    complete <- data.table()
    
    for(i in 1:length(years)) {
      
      plot_year <- plot[year == years[i]]
      perc_mortality <- nrow(plot_year[deadmissing == "Yes",])/nrow(plot_year)*100
      
      to_complete <- data.table(plot = plots[X],
                                year = years[i],
                                perc_mortality = perc_mortality)
      
      complete <- rbind(complete, to_complete)
      
    }
    
  }
}
  
  
}