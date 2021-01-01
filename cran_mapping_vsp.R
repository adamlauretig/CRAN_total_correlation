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

triplets[, pmi := log(pr_n) - log(pr_n_from * pr_n_to)]

matrix(triplets[,.(from, to, pmi)], sparse = TRUE)
cran_graph <- graph_from_data_frame(triplets[,.(from, to, pmi)], directed = TRUE)
cran_graph <- set_edge_attr(cran_graph, "weight", value= triplets$pmi)

cran_graph

cran_graph_fa <- vsp(cran_graph, rank = 10, scale = TRUE)
dimnames(cran_graph_fa$Z)[[1]] <- triplets$from
