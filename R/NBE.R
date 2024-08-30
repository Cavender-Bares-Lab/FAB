#' @title Net Biodiversity Effect
#'
#' @description Estimates the Net Biodiversity Effect (NBE) and its two components:
#' Complementary Effect (CE) and Selection Effect (SE). The function is based
#' on Loreau & Hector (2001) <doi:10.1038/35083573>.
#'
#' @param experiment_design A \code{data.frame} with columns representing species
#' and rows representing plots. Each cell contains the relative abundance of species
#' per plot. Plot IDs should be embedded as \code{rownames} otherwise row numbers
#' will be returned as plot IDs.
#'
#' @param measured_feature If it is a \code{data.frame} it should
#' describe measurements at the species level (e.g., biomass, growth rates), with
#' columns as species and rows as plots similar to \code{experiment_design}. Each
#' cell must contain the measured feature; use \code{NA} if the feature was not measured.
#' If it is a numeric vector it should describe the measured feature at the
#' community level.
#'
#' @return A data.table with the estimations of the Net Biodiversity Effect (NBE) and
#' its two components, Complementary Effect (CE) and Selection Effect (SE) per
#' plot.
#'
#' @references Loreau, M., Hector, A. 2001. Partitioning selection and
#' complementarity in biodiversity experiments. Nature 412, 72–76 (2001).
#'  <doi:10.1038/35083573>
#'
#' @author J. Antonio Guzmán Q. and Laura Williams
#'
#' @importFrom data.table as.data.table
#' @importFrom data.table data.table
#'
#' @export
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
  n_sp <- monocultures
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

  ## Step2
  # Expected community features
  expected <- experiment_design * monocultures_mean
  expected[expected == 0] <- NA

  ## Step3
  # Define observed and predicted
  plot <- data.table(plot = rownames(experiment_design),
                     nsp = n_sp,
                     CE = NA,
                     SE = NA,
                     NE = NA)

  ## Step 4
  # Estimate NBE
  idplots <- unique(plot$plot)

  for(i in 1:length(idplots)) {

    # Number of species
    Y <- na.exclude(as.numeric(measured_feature[i,]))
    M <- na.exclude(as.numeric(expected[i,]))
    N <- n_sp[i]
    dRY <- (Y/M) - (1/N)
    covar <- sum((dRY-mean(dRY)) * (M-mean(M)))/N
    SE <- N*covar
    CE <- N*mean(dRY)*mean(M)
    NE <- SE + CE

    plot$NE[i] <- NE
    plot$CE[i] <- CE
    plot$SE[i] <- SE

  }

  return(plot)

}
