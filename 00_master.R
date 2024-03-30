#### Prepare paths and package ####

packages_load <- c("data.table", "here", "ggtext", "changepoint", "tidyverse",
                   "sparklyr", "devtools", "DiSCos", "xtable")


if (!require("pacman")) install.packages("pacman")
pacman::p_load(char = packages_load, character.only = TRUE)

setwd("/home/dvdijcke/")
# system("rm -r DiSCos")
# system("git clone https://github.com/Davidvandijcke/DiSCos")
setwd("DiSCos") # remove evmix from dependencies cause cant install on fucking server
devtools::load_all()
# devtools::install(dependencies=FALSE)
#
#
codeDir <- "/home/dvdijcke/ppl/code/" # here::here()

setwd(codeDir) # sets cd to program directory

dir <- "/home/dvdijcke/ppl" # sub("/[^/]+$", "", codeDir)# get main directory
dataIn <- file.path(dir, "data", "in")
dataOut <- file.path(dir, "data", "out")
tabs <- file.path(dir, "results", "tabs")
figs <- file.path(dir, "results", "figs") # "/Users/davidvandijcke/Dropbox (University of Michigan)/Apps/Overleaf/RTO/figs"
mods <- file.path(dir, "results", "models")
