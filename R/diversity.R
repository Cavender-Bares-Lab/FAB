#' @title Pyhlogenetic or Functional Diversity
#'
#' @description Derive diversity metrics from phylogenetic trees or cluster trees
#' of plant traits. This is a function that encapsulate functions from \code{picante}.
#'
#' @param inventory A \code{data.table} or \code{data.frame} with the inventory data.
#' It most contain three columns: 'plot', 'species', and 'values'; in that order.
#' The last column should be the metric used to weight diversity estimations
#' (e.g., DBH, above ground biomass, or volume).
#'
#' @param tree A \code{phylo} tree object,  or a phylogenetic covariance matrix.
#'
#' @details
#' The function first summarize forest inventories, then it calculate metrics
#' of species richness (SR), phylogenetic species variability (PSV),
#' phylogenetic species richness (PSR), phylogenetic species evenness (PSE),
#' phylogenetic species clustering (PSC), and Faith's phylogenetic diversity. Similar
#' metrics can be derived on functional information, but using an \code{hclust} tree
#' of similarity of traits among species. If you use this function in your work,
#' please also acknowledge \code{picante} package.
#'
#' @References
#' Faith D.P. (1992) Conservation evaluation and phylogenetic diversity.
#' Biological Conservation, 61, 1-10.
#'
#' Helmus M.R., Bland T.J., Williams C.K. & Ives A.R. (2007) Phylogenetic
#' measures of biodiversity. American Naturalist, 169, E68-E83
#'
#' @returns It returns a \code{data.table} with metrics of diversity.
#'
#' @author J. Antonio Guzmán Q.
#'
#' @importFrom data.table data.table
#' @importFrom picante
#'
#' @examples
diversity <- function(inventory,
                      tree = NULL,
                      weight = c("abundance", "n_indiviuals")) {

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
