# Looking at Packages on CRAN, using total correlation
rm(list = ls())
library(crandep)
library(data.table)
library(igraph)
library(ggplot2)
library(vsp)
library(Matrix)
data(cran_dependencies)
cran_dependencies <- as.data.table(cran_dependencies)

triplets <- cran_dependencies[,.N, by = .(from, to)]
triplets <- triplets[, pr_n := N/sum(N)]
from <- cran_dependencies[,.N, by = .(from)]
from <- from[, pr_n_from := N/sum(N)][,.(from,  pr_n_from)]
to <- cran_dependencies[,.N, by = .(to)]
to <- to[, pr_n_to := N/sum(N)][,.(to,  pr_n_to)]

triplets <- merge( triplets, to, all.x = 'true', by = 'to')
triplets <- merge( triplets, from, all.x = 'true', by = 'from')

triplets <- triplets[, pmi := log(pr_n) - log(pr_n_from * pr_n_to)][, pmi := pmi - 2][ pmi > 0]

cran_graph <- graph_from_data_frame(triplets[,.(from, to, pmi)], directed = TRUE)
cran_graph <- set_edge_attr(cran_graph, "weight", value= triplets$pmi)
cran_graph_fa <- vsp(cran_graph, rank = 100, scale = TRUE)
rownames(cran_graph_fa$Z) <- as_data_frame(cran_graph, 'vertices')[, 1]
embeddings <- cran_graph_fa$Z
nearest_pkgs <- function(pkg_name, mat1, n_similar = 10){
  vec1 <- mat1[rownames(mat1) == pkg_name]
  word_labels <- rownames(mat1)
  vec1 <- Matrix(vec1, nrow = 1, ncol = length(vec1))
  # mat1 <- t(mat1)
  mat_magnitudes <- rowSums(mat1^2)
  vec_magnitudes <- rowSums(vec1^2)
  sim <- (tcrossprod(vec1, mat1)/
      (sqrt(tcrossprod(vec_magnitudes, mat_magnitudes))))
  sim2 <- matrix(sim, dimnames = list(word_labels))
  
  w <- sim2[order(-sim2),,drop = FALSE]
  return(w[1:n_similar,])
}

nearest_pkgs(pkg_name = 'rstan', mat1 = (embeddings))
nearest_pkgs(pkg_name = 'brms', mat1 = (embeddings))
nearest_pkgs(pkg_name = 'rstanarm', mat1 = (embeddings))
nearest_pkgs(pkg_name = 'RcppEigen', mat1 = (embeddings))
nearest_pkgs(pkg_name = 'RcppArmadillo', mat1 = (embeddings))
