################################################################################
#' @title Net biodiversity Effect
################################################################################

#' @description It estimate the Net Biodiversity Effect (NBE) its two components: 
#' complementary effect (CE) and selection effect (SE).
#' 
#' @param experiment_design A \code{data.frame} with columns representing species 
#' and rows representing plots. Each cell most contain relative abundance of species
#' per plot. Plot IDs should be embedded as \code{rownames} otherwise row numbers 
#' will be returned as plot IDs.
#' 
#' @param measured_feature If it is a \code{data.frame} should 
#' describe measurements at the species level (e.g., biomass, growth rates), with 
#' columns as species and rows as plots similar to \code{experiment_design}. If it
#' is a numeric vector it should describe measured feature at the community level.
#'
#' @return A data.table with the estimations of Net biodiversity effect (NBE) and 
#' its two components: complementary effect (CE) and selection effect (SE) per 
#' plot.
#'
#' @author J. Antonio Guzmán Q. and Laura Williams
#' 
#' @importFrom data.table as.data.table
#' @importFrom data.table data.table


experiment_design <- fread("data/experiment_design.csv")
measured_feature <- fread("data/measured_feature.csv")
measured_feature <- rnorm(nrow(measured_feature), 5, 2)

NBE <- function(experiment_design, measured_feature) {
  
  # Test for failures
  if(is.data.frame(measured_feature) == TRUE) {
    if(!all.equal(dim(experiment_design), dim(measured_feature))) {
      stop("Dimentions between datasets does not match")
    }
  } else if(is.numeric(measured_feature)) {
    if(!all.equal(dim(experiment_design)[1], length(measured_feature))) {
      stop("The number of plots among the design and measured features does not match")
    }
  }
  
  ## Step 1
  # Identify monocultures
  monocultures <- apply(experiment_design, 1, function(i) sum(i>0))
  monocultures <- monocultures == 1
  
  # Get features per monoculture
  if(is.data.frame(measured_feature) == TRUE) {
    
    monocultures_features <- measured_feature[monocultures == TRUE, ]
    monocultures_features[monocultures_features == 0] <- NA
    monocultures_mean <- colMeans(monocultures_features, na.rm = TRUE)
    
  } else if(is.numeric(measured_feature) == TRUE) {
    
    monocultures_features <- measured_feature[monocultures == TRUE]
    monocultures_frame <- experiment_design[monocultures == TRUE,]
    monocultures_frame[monocultures_frame == 1] <- monocultures_features
    monocultures_frame[monocultures_frame == 0] <- NA
    monocultures_mean <- colMeans(monocultures_frame, na.rm = TRUE)
    
  }
  
  
  
  
  
  
  
}