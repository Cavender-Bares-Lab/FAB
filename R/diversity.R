#' @title Pyhlogenetic or Functional Diversity
#'
#' @description Derives diversity metrics from phylogenetic trees or cluster trees
#' of plant traits. This is a function that encapsulate functions from \code{picante}.
#'
#' @param inventory A \code{data.table} or \code{data.frame} with the inventory data.
#' It most contain three columns: 'plot', 'species', and 'values'; in that order.
#' The last column should be the metric used to weight diversity estimations
#' (e.g., DBH, above ground biomass, or volume).
#'
#' @param tree A \code{phylo} tree object, or a phylogenetic covariance matrix.
#'
#' @param functional_traits A \code{data.table} or \code{data.frame} with functional
#' trait data per columns and species rows. The first column must be labeled as
#' 'species', with species that match those from \code{inventory}.
#'
#' @param scale_traits Logic. If \code{TRUE} and \code{functional_traits} is not \code{NULL},
#' it scales \code{traits}.
#'
#' @details
#' The function first summarize forest inventories, then it calculate metrics
#' of species richness (SR), phylogenetic or functional species variability (PSV or FSV),
#' phylogenetic or functional species richness (PSR or FSR), phylogenetic or Functional
#' species evenness (PSE or FSE), phylogenetic or functional species clustering (PSC or FSC),
#' and Faith's phylogenetic or functional diversity. Similar metrics can be
#' derived on functional information, but using an \code{hclust} tree
#' of similarity of traits among species folloiwing (Cavender-Bares et al. 2012).
#' If you use this function in your work, please also acknowledge \code{picante} package.
#'
#' @references
#' Faith D.P. (1992) Conservation evaluation and phylogenetic diversity.
#' Biological Conservation, 61, 1-10.
#'
#' Helmus M.R., Bland T.J., Williams C.K. & Ives A.R. (2007) Phylogenetic
#' measures of biodiversity. American Naturalist, 169, E68-E83
#'
#' Cavender-Bares, J., and P. B. Reich. 2012. Shocks to the system: Community
#' assembly of the oak savanna in a 40‐year fire frequency experiment.
#' Ecology 93:S52 - S69.
#'
#' Cadotte, M. W., J. Cavender-Bares, D. Tilman, and T. H. Oakley. 2009. Using
#' phylogenetic, functional and trait diversity to understand patterns of plant
#' community productivity. Plos One 4:e5695.
#'
#' @returns It returns a \code{data.table} with metrics of diversity.
#'
#' @author Jeannine Cavender-Bares and J. Antonio Guzmán Q.
#'
#' @importFrom data.table data.table
#' @importFrom picante psv
#' @importFrom picante psr
#' @importFrom picante pse
#' @importFrom picante psc
#' @importFrom picante pd
#'
#' @examples
diversity <- function(inventory,
                      phylo_tree = NULL,
                      functional_matrix = NULL,
                      weight = c("abundance", "n_indiviuals"),
                      scale_traits = FALSE) {

  # Test presence of datasets
  if(is.null(phylo_tree) == TRUE & is.null(functional_matrix) == TRUE) {
    stop("Please provide a phylo_tree or a functional matrix")
  }

  # Make a copy and rename
  frame <- inventory
  colnames(frame) <- c("plot", "species", "value")

  # Create a summary per species if required
  species_summary <- frame[, .(ntrees = .N,
                               total_value = sum(value, na.rm = TRUE)),
                           by = c("plot", "species")]

  # From rows summary to matrix
  community <- species_summary[, c("plot", "total_value", "species")]
  community$species <- chartr(" ", "_", community$species)
  community <- sample2matrix(community)
  community <- decostand(community, method = "total")

  #' Create file for compile results
  frame_diversity <- data.table(plot = rownames(community))

  # Maching with communities and tree
  matched <- match.phylo.comm(phy = tree,
                              comm = community)

  # Derive metrics
  frame_diversity$SR <- psv(matched$comm,
                            matched$phy)$SR

  frame_diversity$PSV <- psv(matched$comm,
                            matched$phy)$PSVs

  frame_diversity$PSR <- psr(matched$comm,
                            matched$phy)$PSR

  frame_diversity$PSE <- pse(matched$comm,
                            matched$phy)$PSE

  frame_diversity$PSC <- psc(matched$comm,
                             matched$phy)$PSC

  frame_diversity$Faith <- pd(matched$comm,
                              matched$phy,
                              include.root=TRUE)$PD

  return(frame)

}
