# Quantitative Ecology
# Unconstrained Ordination: Re-cap and Assessment 
# 19/07/2021
# Jesse Phillips 


# INSTRUCTIONS ------------------------------------------------------------

  # 1. Using 2 unconstrained ordination techniques, analyse the mite data
      # SECTION A: nMDS -> mite environmental data
      # SECTION B: CA -> mite species data

  # 2. Using 2 other unconstrained ordination techniques, analyse the dune data
      # SECTION C: PCA -> dune species data (tb-PCA)
      # SECTION D: PCoA -> dune environmental data


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(vegan)
library(cluster)
source("cleanplot.pca.R")


# Read in Data ------------------------------------------------------------

data("mite") # mite species abundance data
data("mite.env") # mite environmental data 
data("dune") # dune species abundance data
data("dune.env") # dune environmental data


# SECTION A: nMDS ---------------------------------------------------------

glimpse(mite.env) # Variables are not homogeneous

# Calculate distance matrix with Gower's distance
mite.env.matrix <- as.matrix(daisy(mite.env, metric = "gower"))

# Do nMDS 
mite.env.nMDS <- metaMDS(mite.env.matrix) 
mite.env.nMDS # Stress = 0.18 (in between fair and suspect)

stressplot(mite.env.nMDS)

# Plot sites
pl1 <- ordiplot(mite.env.nMDS, type = "none", main = "nMDS Mite Environments")
points(pl1, "sites", pch = 20, col = "black", cex = 1.25)
text(pl1, "sites", pos = 2, cex = 0.75)
abline(h = 0, v = 0, lty = 3)

# Do envfit() and plot environmental variables
mite.env.fit <- envfit(mite.env.nMDS, mite.env)
plot(mite.env.fit, col = "red", cex = 0.75)

  # FINDINGS
    # nMDS represents the ordering relationships among objects. 
    # Tight clusters of sites will be relatively similar in terms of their
    # environmental variables. 
    # Sites can be grouped by the environmental variable that predominantly 
    # impacts it. 


# SECTION B: CA -----------------------------------------------------------

head(mite) # Abundance data, appropriate for CA

mite.ca <- cca(mite)
summary(mite.ca) # Total Inertia = 1.696, 75% explained by first 7 components 

  # CA Ordination Diagrams
CA_plot_1 <- ordiplot(mite.ca, type = "none", scaling = 1, main = "CA Mite Abundances - Scaling 1")
text(CA_plot_1, "sites", cex = 0.6, col = "navy")
text(CA_plot_1, "species", cex = 0.5, col = "darkred")

CA_plot_2 <- ordiplot(mite.ca, type = "none", scaling = 2, main = "CA Mite Abundances - Scaling 2")
text(CA_plot_2, "sites", cex = 0.5, col = "navy")
text(CA_plot_2, "species", cex = 0.6, col = "darkred")

  # FINDINGS 
    # Scaling 1 -> ordination of sites:
    # Tight cluster of sites on the left, all numbered between 5 and 29
    # Those sites are likely to be comprised of similar species (and relative frequencies of those species)
    # Brachy and ONOV feature heavily in those sites
    # The big cluster of species in the top left contributes more highly to those sites than the others
    # The remaining sites mostly feature in a cluster on the right (31-70)
    # The species in the bottom right contribute mostly to these sites. 
    # Scaling 2 -> ordination of species:
    # Tight cluster of species in the top left will have similar frequencies across the sites
    # The rest are slightly more dispersed. 
    # The tight cluster of species is related to the tight cluster of sites from above (5-29)
    # These species are more likely to be found at these sites.
    # Conclusion:
    # There are certain sites (those found in the 5-29 cluster) which have a higher 
    # mite diversity and abundance than the rest.  
      

# SECTION C: PCA ----------------------------------------------------------

# I'm kinda only doing a PCA for this species data because I'm saving the PCoA
# for the non-homogeneous environmental data. Also I'm experimenting with the 
# Hellinger pre-transformation.

head(dune) # Abundance data

dune_pa <- decostand(dune, method = "pa") # convert to presence-absence data
dune_hel <- decostand(dune_pa, method = "hellinger") # transform data to Hellinger distance (immune to double-zero problem)

# Do PCA
dune.pca <- rda(dune_hel)
summary(dune.pca)

# Use cleanplot.pca() to create ordination diagrams
cleanplot.pca(dune.pca, scaling = 1)
cleanplot.pca(dune.pca, scaling = 2)

  # FINDINGS
    # Scaling 1 -> distance between objects represents Euclidean distances
    # Site 1 is close to site 2, 3 close to 4, and so on. There is a general linear
    # gradient that the sites follow (with some exception)
    # Species outside circle of equilibrium contribution makes a higher contribution 
    # to where the sites fall on the plot. 
    # This amounts to 10 of the 30 species.
    # Scaling 2 -> angles between species reflect their correlations 


# SECTION D: PCoA ---------------------------------------------------------

glimpse(dune.env) # Variables are not homogeneous

# calculate distance matrix with Gower's distance
dune.env.matrix <- as.matrix(daisy(dune.env, metric = "gower"))

# Do the PCoa
dune.env.pcoa <- capscale(dune.env.matrix ~ 1) 
summary(dune.env.pcoa)

# Plot sites
pl2 <- ordiplot(dune.env.pcoa, type = "none", main = "PCoA Dune Environments")
points(pl2, "sites", pch = 20, col = "black", cex = 1.25)
text(pl2, "sites", pos = 2, cex = 0.75)

# Do envfit() and plot environmental variables
dune.env.fit <- envfit(dune.env.pcoa, dune.env)
plot(dune.env.fit, col = "red", cex = 0.75)

  # FINDINGS 
    # PCoA provides Euclidean representation of relationships among objects based
    # on Gower's index 
    # No obvious linear gradient among the sites.
    # Environmental variables help explain relationships between sites. 
    # Moisture and Land-Use levels plot in linear gradients
    # Thickness of soil (A1) describes the relationship between sites 14 and 15 best,
    # as they plot away from the other sites. 
    # "Nature Conservation Management" and "0 Manure" plot together with sites 19 and 20,
    # which differentiate them from the other sites. 
    # Biological Farming relates to level 1 Manure and Moisture
    # Hobby Farming relates to level 2 Manure and Moisture
    # Standard Farming plots with level 3 Manure and Pasture Use, which explains  
    # the relationships between the sites in the bottom left of the plot.