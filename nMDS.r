# Quantitative Ecology 
# Non-Metric Multidimensional Scaling (nMDS)
# 20/07/2021
# Jesse Phillips 


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(vegan)


# Read in Data ------------------------------------------------------------

spe <- read.csv("data/DoubsSpe.csv", row.names = 1)
spe <- dplyr::slice(spe, -8)


# Do the nMDS -------------------------------------------------------------

spe_nmds <- metaMDS(spe, distance = "bray")
spe_nmds # Stress = 0.07
#summary(spe_nmds)  ... is not useful


# Ordination Diagram ------------------------------------------------------

stressplot(spe_nmds, main = "Shepard plot")

ordiplot(spe_nmds, type = "text", cex = 1.5, main = paste0("nMDS stress = ", round(spe_nmds$stress, 2)))

plot(spe_nmds, type = "text", main = "Goodness of fit")
points(spe_nmds, display = "sites", cex = goodness(spe_nmds) * 200) # bigger bubbles indicate a worse fit

# We can also build ordination plots from scratch
pl <- ordiplot(spe_nmds, type = "none", main = "nMDS Fish Abundances")
points(pl, "sites", pch = 21, cex = 1.75, col = "grey80", bg = "grey80")
points(pl, "species", pch = 21, col = "turquoise", arrows = TRUE)
text(pl, "species", col = "blue4", cex = 0.9)
text(pl, "sites", col = "red4", cex = 0.9)
abline(h = 0, v = 0, lty = 3)

# Or we can fit response surfaces and project environmental drivers
require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe, tmp <- ordisurf(spe_nmds ~ Satr, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Satr"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_nmds ~ Scer, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Scer"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_nmds ~ Teso, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Teso"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_nmds ~ Cogo, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Cogo"))
abline(h = 0, v = 0, lty = 3)

env <- read.csv("data/DoubsEnv.csv", row.names = 1)
env <- dplyr::slice(env, -8)

(spe_nmds_env <- envfit(spe_nmds, env)) 
plot(spe_nmds_env, col = "grey40")
plot(spe_nmds_env, p.max = 0.05, col = "red")


# QUESTIONS ---------------------------------------------------------------

# Done on assessment_unconstrained_ordination.r 
