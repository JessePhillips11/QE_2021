# Quantitative Ecology 2021
# Biodiversity
# 29/06/2021
# Jesse Phillips


# Load Packages -----------------------------------------------------------

library(vegan)
library(ggplot2)
library(dplyr)
library(BiodiversityR)


# View Data ---------------------------------------------------------------

spp <- read.csv("data/SeaweedsSpp.csv")
spp <- select(spp, -1) #remove X column

dim(spp) #outputs dimensions as ROWS then COLUMNS

spp[1:10, 1:10] #outputs first 10 rows and first 10 columns


# Alpha Diversity ---------------------------------------------------------

# Species Richness

