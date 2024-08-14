################################################################################
#' @title Net biodiversity Effect
################################################################################

#' @description It estimate the Net Biodiversity Effect (NBE) its two components: 
#' complementary effect (CE) and selection effect (SE).
#' 
#'
#' @param experiment_design A \code{data.frame} with columns representing species 
#' and rows representing plots. It cell most contain relative abundance of species
#' per plot. Plot IDs should be embedded as \code{rownames} otherwise plots IDs 
#' will be returned as row number.
#' 
#' @param measured_feature A \code{data.frame} with the same characteristics of
#' \code{experiment_design}, but with the feature measured (e.g., biomass, 
#' growth rates) within cells.
#'
#' @return A data.table with the estimations of Net biodiversity effect (NBE) and 
#' its two components: complementary effect (CE) and selection effect (SE) per 
#' plot.
#'
#' @author J. Antonio Guzmán Q. and Laura Williams
#' 
#' @importFrom data.table as.data.table
#' @importFrom data.table data.table

NBE <- function(experiment_design, measured_feature) {
  
  # Test for failures
  
  
  
  
  ## Step 1
  # Identify monocultures
  
  
  
}