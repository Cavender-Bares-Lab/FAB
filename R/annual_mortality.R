#' @title Annual Mortality
#'
#' @description It computes the annual mortality on plots since its established.
#'
#' @param inventory A \code{data.table} or \code{data.frame} with the inventory data. 
#' It most contain four columns labeled: 'block', 'plot', 'species', and 'dead'.
#' @param per_species Logic. If \code{TRUE}, it estimate the mortality per species
#' whitin plots.
#'
#' @return It returns a \code{data.table} with the estimations of morality per plot.
#'
#' @author J. Antonio Guzmán Q.
#' 
#' @importFrom data.table rbind
#' @importFrom data.table data.table
#'
#' @examples
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
      
      if(per_species == FALSE) {
        
        perc_mortality <- nrow(plot_year[deadmissing == "Yes",])/nrow(plot_year)*100
        
        to_complete <- data.table(block = unique(plot_year$block)[1],
                                  plot = plots[X],
                                  year = years[i],
                                  perc_mortality = perc_mortality)
        
        complete <- rbind(complete, to_complete)
        
      } else if(per_species == TRUE) {
        
        n_species <- unique(plot_year$species)
        
        for(ii in 1:length(n_species)) {
          
          perc_mortality <- nrow(plot_year[species == n_species[ii] & deadmissing == "Yes",])/nrow(plot_year[species == n_species[ii]])*100
          
          to_complete <- data.table(block = unique(plot_year$block)[1],
                                    plot = plots[X],
                                    species = n_species[ii],
                                    year = years[i],
                                    perc_mortality = perc_mortality)
          
          complete <- rbind(complete, to_complete)
          
        }
      }
    }
    
    return(complete)
  }
  
  mortality_frame <- lapply(X = 1:length(plots),
                            FUN = mortality,
                            plots = plots,
                            frame = frame,
                            per_species = per_species)
  
  mortality_frame <- do.call(rbind, mortality_frame)
  
  return(mortality_frame)
  
}