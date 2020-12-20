# Looking at Packages on CRAN, using total correlation
rm(list = ls())
library(crandep)
library(data.table)
library(igraph)
library(ggplot2)
data(cran_dependencies)
cran_dependencies <- as.data.table(cran_dependencies)


# using "total correlation" from p. 18 here (http://www.timvandecruys.be/media/papers/VandeCruys2011Two.pdf)
triplets <- cran_dependencies[,.N, by = .(from, to, type, reverse)]
triplets <- triplets[, pr_n := N/sum(N)]
from <- cran_dependencies[,.N, by = .(from)]
from <- from[, pr_n_from := N/sum(N)][,.(from,  pr_n_from)]
to <- cran_dependencies[,.N, by = .(to)]
to <- to[, pr_n_to := N/sum(N)][,.(to,  pr_n_to)]
type <- cran_dependencies[,.N, by = .(type)]
type <- type[, pr_n_type := N/sum(N)][,.(type,  pr_n_type)]
reverse <- cran_dependencies[,.N, by = .(reverse)]
reverse <- reverse[, pr_n_reverse := N/sum(N)][,.(reverse,  pr_n_reverse)]

triplets <- merge( triplets, reverse, all.x = 'true', by = 'reverse')
triplets <- merge( triplets, type, all.x = 'true', by = 'type')
triplets <- merge( triplets, to, all.x = 'true', by = 'to')
triplets <- merge( triplets, from, all.x = 'true', by = 'from')
triplets[, pmi := log(pr_n) - log(pr_n_from * pr_n_to * pr_n_type)]

from_stan <- triplets[ from == 'rstan'][ order(pmi)]
tops <- from_stan[from_stan[, .I[which.max(pmi)], by=.(type, reverse)]$V1]
tops[,.(from, to, type, reverse, pmi)]
