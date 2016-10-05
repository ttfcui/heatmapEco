#! /usr/local/bin/R

args <- commandArgs(trailingOnly=TRUE)
setwd(args[1])

repo <- "http://cran.r-project.org"
install.packages("dplyr", repos=repo)
install.packages("zoo", repos=repo)
install.packages("ggplot2", repos=repo)
install.packages("heatmapEco_0.25.tar.gz", repos=NULL, type="source")
