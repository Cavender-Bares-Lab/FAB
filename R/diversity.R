#' @title Pyhlogenetic or Functional Diversity
#'
#' @description Derive diversity metrics from phylogenetic trees or cluster trees
#' of plant traits. This is a function that emcapsulate functions from \package{picante}.
#'
#' @param inventory A \code{data.table} or \code{data.frame} with the inventory data.
#' It most contain three columns: 'plot', 'species', and 'values'; in that order.
#' The last column should be the metric used to weight diversity estimations
#' (e.g., DBH, above ground biomass, or volume).
#'
#' @param tree A \code{phylo} tree object,  or a phylogenetic covariance matrix.
#'
#' @return It returns a \code{data.table} with metrics of diversity.
#'
#' @author J. Antonio Guzmán Q.
#'
#' @importFrom data.table rbind
#' @importFrom data.table data.table
#' @importFrom picante
#'
#' @examples

library(hillR)
library(picante)


diversity <- function(inventory,
                      tree = NULL,
                      weight = c("abundance", "n_indiviuals")) {

  # Make a copy and rename
  frame <- inventory
  colnames(frame) <- c("plot", "species", "value")

  # Summary per species
  species_summary <- frame[, .(ntrees = .N,
                               total_value = sum(value, na.rm = TRUE)),
                           by = c("plot", "species")]

  # Make the summary a matrix
  community <- species_summary[, c("plot", "total_value", "species")]
  community$species <- chartr(" ", "_", community$species)
  community <- sample2matrix(community)





}
