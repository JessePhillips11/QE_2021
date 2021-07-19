# Quantitative Ecology
# Unconstrained Ordination: Re-cap and Assessment 
# 19/07/2021
# Jesse Phillips 


# INSTRUCTIONS ------------------------------------------------------------

  # 1. Using 2 unconstrained ordination techniques, analyse the mite data
      # SECTION A: PCA -> mite environmental data
      # SECTION B: CA -> mite species data

  # 2. Using 2 other unconstrained ordination techniques, analyse the dune data
      # SECTION C: PCoA -> dune species data
      # SECTION D: nMDS -> dune environmental data


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(vegan)


# Read in Data ------------------------------------------------------------

data("mite") # mite species abundance data
data("mite.env") # mite environmental data 
data("dune") # dune species abundance data
data("dune.env") # dune environmental data


# SECTION A: PCA ----------------------------------------------------------

glimpse(mite.env) # Variables are not homogeneous
mite.env$Substrate <- as.numeric(mite.env$Substrate)
mite.env$Shrub <- as.numeric(mite.env$Shrub)
mite.env$Topo <- as.numeric(mite.env$Topo)

mite.env.pca <- rda(mite.env, scale = TRUE)
summary(mite.env.pca) # Total Inertia = 5, 84% explained by first 3 components

  # PCA Ordination Diagrams
source("cleanplot.pca.R")
cleanplot.pca(mite.env.pca, scaling = 1) # Produces a much cleaner plot than biplot() function
cleanplot.pca(mite.env.pca, scaling = 2)

  # Findings
      # Principal component analysis of the environmental variables of the mite 
      # data produced a total inertia of 5, 67.5% of which is explained by the 
      # first 2 principal components, and 84.5% explained by the first 3. 
      # The Scaling 1 biplot shows us that substrate type (Substrate) and 
      # shrub density (Shrub) contributes significantly to what makes sites 
      # vary. Sites ordinated closer together tend to have similar environmental
      # conditions, so sites that are clustered together represent sites that
      # are similar.
      # Scaling 2 shows us that substrate type (Substrate) and substrate density
      # (SubsDens) are more closely related than any other variables. 

# SECTION B: CA -----------------------------------------------------------

mite.ca <- cca(mite)
summary(mite.ca) # Total Inertia = 1.696, 75% explained by first 7 components 

  # CA Ordination Diagrams
CA_plot_1 <- ordiplot(mite.ca, type = "none", scaling = 1, main = "CA Mite Abundances - Scaling 1")
text(CA_plot_1, "sites", cex = 0.75, col = "navy")
text(CA_plot_1, "species", cex = 0.75, col = "darkred")

CA_plot_2 <- ordiplot(mite.ca, type = "none", scaling = 2, main = "CA Mite Abundances - Scaling 2")
text(CA_plot_2, "sites", cex = 0.75, col = "navy")
text(CA_plot_2, "species", cex = 0.75, col = "darkred")

      

# SECTION C: PCoA ---------------------------------------------------------

head(dune) # Abundance data -> Bray-Curtis dissimilarity
dune.pcoa <- capscale(dune ~ 1, distance = "bray")
summary(dune.pcoa) # Total Inertia = 4.594, 78% explained by first 4 components

  #PCoA Ordination Diagrams
PCoA_plot_1 <- ordiplot(dune.pcoa, type = "none", scaling = 1, main = "PCoA Dunes - Scaling 1")
text(PCoA_plot_1, "species", col = "darkred", cex = 0.75)
text(PCoA_plot_1, "sites", col = "navy", cex = 0.8)

PCoA_plot_2 <- ordiplot(dune.pcoa, type = "none", scaling = 2, main = "PCoA Dunes - Scaling 2")
text(PCoA_plot_2, "species", col = "darkred", cex = 0.75)
text(PCoA_plot_2, "sites", col = "navy", cex = 0.8)



# SECTION D: nMDS ---------------------------------------------------------

glimpse(dune.env) # Variables are not homogeneous
dune.env$Moisture <- as.numeric(dune.env$Moisture)
dune.env$Manure <- as.numeric(dune.env$Manure)
dune.env$Management <- as.numeric(dune.env$Management)
dune.env$Use <- as.numeric(dune.env$Use)

  # Data is transformed, but suitable distance matrix still needs to be applied
dune.env.nmds <- metaMDS(dune.env, distance = "gower") # Gower coefficient used for heterogeneous data sets
dune.env.nmds # Stress = 0.13

  # nMDS Ordination Diagrams
stressplot(dune.env.nmds, main = "Shepard plot")

ordiplot(dune.env.nmds, type = "text", main = "nMDS Dunes")
abline(h = 0, v = 0, lty = 3)

plot(dune.env.nmds, type = "text", main = "Goodness of fit")
points(dune.env.nmds, display = "sites", cex = goodness(dune.env.nmds) * 200) # bigger bubbles indicate a worse fit
abline(h = 0, v = 0, lty = 3)
