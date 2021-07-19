# Quantitative Ecology
# Principal Coordinate Analysis 
# 16/07/2021
# Jesse Phillips

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(vegan)


# Read in Data ------------------------------------------------------------

spe <- read.csv("data/DoubsSpe.csv", row.names = 1)
spe <- dplyr::slice(spe, -8) # remove row with no observations 


# Calculate Suitable Dissimilarity Matrix ---------------------------------

spe_bray <- vegdist(spe) # Bray-Curtis dissimilarity for abundance data


# Do the PCoA -------------------------------------------------------------

spe_pcoa <- capscale(spe_bray ~ 1) # from the vegan package. 
summary(spe_pcoa)
  # Species Scores are missing. Information from original variables (species) are not available
  # Instead of providing dissimilarity matrix, we can provide raw species table
  # to retain spcies information. 
spe_pcoa <- capscale(spe ~ 1, distance = "bray")
summary(spe_pcoa)

# We can still access the eigenvalues and calculate the percentage inertia explained by the first 3 axes
round(sum(spe_pcoa$CA$eig[1:3]) / sum(spe_pcoa$CA$eig) * 100, 2) # result in %, rounded to 2 decimal places


# Ordination Diagrams -----------------------------------------------------

plot(spe_pcoa, scaling = 1, main = "PCoA fish abundances - biplot scaling 1")
plot(spe_pcoa, scaling = 2, main = "PCoA fish abundances - biplot scaling 2")


# We can improve the plots by building them from scratch 
pl1 <- ordiplot(spe_pcoa, type = "none", scaling = 1, main = "PCoA fish abundances - biplot scaling 1")
points(pl1, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl1, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl1, "species", col = "blue4", cex = 0.9)
text(pl1, "sites", col = "red4", cex = 0.9)

pl2 <- ordiplot(spe_pcoa, type = "none", scaling = 2, main = "PCoA fish abundances - biplot scaling 2")
points(pl2, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl2, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl2, "species", col = "blue4", cex = 0.9)
text(pl2, "sites", col = "red4", cex = 0.9)


# We can also fit response surfaces using ordisurf()
require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe, tmp <- ordisurf(spe_pcoa ~ Satr, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Satr"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_pcoa ~ Scer, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Scer"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_pcoa ~ Teso, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Teso"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_pcoa ~ Cogo, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Cogo"))
abline(h = 0, v = 0, lty = 3)

env <- read.csv("data/DoubsEnv.csv", row.names = 1)
env <- dplyr::slice(env, -8) # because row 8 was removed earlier

(spe_pcoa_env <- envfit(spe_pcoa, env, scaling = 2)) 
plot(spe_pcoa_env, col = "grey40")
plot(spe_pcoa_env, p.max = 0.05, col = "red")
