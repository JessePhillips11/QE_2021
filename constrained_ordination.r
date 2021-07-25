# Quantitative Ecology
# Constrained Ordination: RDA, CCA and db-RDA
# 23/07/2021
# Jesse Phillips 


# Some Theory... ----------------------------------------------------------

# Redundancy Analysis (RDA) is a direct gradient analysis that highlights linear 
# relationships between components of response variables, i.e. variables that are 
# redundant with/explained by a set of predictors. RDA is an extension of a PCA
# with multiple linear regression.

# Canonical Correspondence Analysis (CCA) is an extension of a CA with multiple 
# regression, therefore also based on a Chi-squared metric. We don't have the choice
# of specifying which dissimilarity metric to use. CCA performs best when species 
# distribution is unimodal.

# distance-based Redundancy Analysis (db-RDA) can be viewed as the extension of 
# PCoA with multiple regression. We can specify any dissimilarity matrix as input. 



# Load Packages -----------------------------------------------------------

library(tidyverse)
library(betapart)
library(vegan)
library(gridExtra)
library(grid)
library(gridBase)
library(tidyr)


# Load Data ---------------------------------------------------------------

spp <- read.csv('data/SeaweedsSpp.csv', row.names = 1)
dim(spp) # 58 rows and 847 cols


# Setup Data --------------------------------------------------------------
  
# We want to focus on species turnover without a nestedness-resultant influence... 

Y.core <- betapart.core(spp) # computes basic qualities needed for computing multiple-site beta diversity measures and pairwise dissimilarity matrices

Y.pair <- beta.pair(Y.core, index.family = "sor") # computes 3 distance matrices accounting for turnover, nestedness-resultant and total dissimilarity 

# Let Y1 be the turnover component (beta-sim):
Y1 <- as.matrix(Y.pair$beta.sim)


# Load Env Data -----------------------------------------------------------

load("data/SeaweedEnv.RData")
dim(env) # 58 rows and 18 cols 

# We select only some of the thermal variables; the rest are collinear with the ones we import
E1 <- dplyr::select(env, febMean, febRange, febSD, augMean,
                    augRange, augSD, annMean, annRange, annSD)

E1 <- decostand(E1, method = "standardize") # Calculate z-scores


# Load Bioregions ---------------------------------------------------------

# We can parition the data into 4 recognised bioregions and colour code the figure accordingly
bioreg <- read.csv('data/bioregions.csv', header = TRUE)
head(bioreg) # bolton = the bioregion 

# Load geographical coordinates for coastal section
sites <- read.csv("data/sites.csv")
sites <- sites[, c(2, 1)] # swaps the cols around so Lon is first, then Lat
head(sites)
dim(sites) # 58 rows and 2 cols 


# Start the db-RDA --------------------------------------------------------

# fit the full model:
rda_full <- capscale(Y1 ~., E1)
summary(rda_full) 
  # species scores are missing, since we provided only a distance matrix 

# Is the fit significant?
anova(rda_full) # F = 40.88 and p < 0.05 --> yes

# Compute adjusted R-squared:
rda_full_R2 <- RsquareAdj(rda_full)$adj.r.squared
round(rda_full_R2, 2)

# Inertia accounted for by constraints:
round(sum(rda_full$CCA$eig), 2)

# Remaining (unconstrained) inertia:
round(sum(rda_full$CA$eig), 2)

# Total inertia:
round(rda_full$tot.chi, 2)

# What is the variation explained by the full set of environmental variables?
round(sum(rda_full$CCA$eig) / rda_full$tot.chi * 100, 2) # in %


# VIF ---------------------------------------------------------------------

# We check for collinearity using variance inflation factors (VIF), and retain a 
# subset of non-collinear variables to include in the reduced/final model.
# Commonly, values over 10 indicate redundant constraints, so we can run VIF 
# iteratively, each time removing the highest VIF until they are all mostly below 10

vif.cca(rda_full)
  # drop annMean

E2 <- dplyr::select(E1, -annMean)
rda_sel1 <- capscale(Y1 ~., E2) # new, temp RDA
vif.cca(rda_sel1)
  # drop febMean

E3 <- dplyr::select(E2, -febMean)
rda_sel2 <- capscale(Y1 ~., E3)
vif.cca(rda_sel2)
  # this looks acceptable

# We can construct the final model and calculate the significance
rda_final <- rda_sel2
anova(rda_final) # F = 45.68 and p < 0.05 

# Which axes are significant?
anova(rda_final, by = 'axis')

# Extract significant variables in E3 that are influential in the final model 
(rda_final_axis_test <- anova(rda_final, by = "terms"))

# The significant variables are:
rda_final_ax <- which(rda_final_axis_test[, 4] < 0.05)
rda_final_sign_ax <- colnames(E3[,rda_final_ax])
rda_final_sign_ax

# The adjusted R-squared for the constraints:
round(rda_final_R2 <- RsquareAdj(rda_final)$adj.r.squared, 2) 

# Variance explained by reduced (final) model:
round(sum(rda_final$CCA$eig) / rda_final$tot.chi * 100, 2)

# Biplot scores for constraining variables:
scores(rda_final, display = "bp", choices = c(1:2))



# Ordination Diagrams -----------------------------------------------------

# Recreating the figure from Smit et al. (2017):

# use scaling = 1 or scaling = 2 for site and species scaling, respectively
rda_final_scrs <- scores(rda_final, display = c("sp", "wa", "lc", "bp"))
# below I plot the wa (site) scores rather than lc (constraints) scores
site_scores <- data.frame(rda_final_scrs$site) # the wa scores
site_scores$bioreg <- bioreg$bolton
site_scores$section <- seq(1:58)

biplot_scores <- data.frame(rda_final_scrs$biplot)
biplot_scores$labels <- rownames(biplot_scores)
biplot_scores_sign <- biplot_scores[biplot_scores$labels %in% rda_final_sign_ax,]

ggplot(data = site_scores, aes(x = CAP1, y = CAP2, colour = bioreg)) +
  geom_point(size = 5.0, shape = 24, fill = "white") +
  geom_text(aes(label = section), size = 3.0, col = "black") +
  geom_label(data = biplot_scores_sign,
             aes(CAP1, CAP2, label = rownames(biplot_scores_sign)),
             color = "black") +
  geom_segment(data = biplot_scores_sign,
               aes(x = 0, y = 0, xend = CAP1, yend = CAP2),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               color = "lightseagreen", alpha = 1, size = 0.7) +
  xlab("CAP1") + ylab("CAP2") +
  ggtitle(expression(paste("Significant thermal variables and ", beta[sim]))) +
  theme_grey() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        aspect.ratio = 0.8)



# Dealing with Factor Variables -------------------------------------------

E4 <- E3
# append the bioregs after the thermal vars
E4$bioreg <- bioreg$bolton
head(E4)
rda_cat <- capscale(Y1 ~., E4)
plot(rda_cat)

  # default plot looks okay...
  # but not great. Plot the class (factor) centroids in ggplot():

# also extractthe factor centroids for the bioregions
rda_cat_scrs <- scores(rda_cat, display = c("sp", "wa", "lc", "bp", "cn"))
site_scores <- data.frame(rda_cat_scrs$site) # the wa scores
site_scores$bioreg <- bioreg$bolton
site_scores$section <- seq(1:58)

biplot_scores <- data.frame(rda_cat_scrs$biplot)
biplot_scores$labels <- rownames(biplot_scores)
biplot_scores_sign <- biplot_scores[biplot_scores$labels %in% rda_final_sign_ax,]

bioreg_centroids <- data.frame(rda_cat_scrs$centroids)
bioreg_centroids$labels <- rownames(bioreg_centroids)

ggplot(data = site_scores, aes(CAP1, CAP2, colour = bioreg)) +
  geom_point(size = 5.0, shape = 24, fill = "white") +
  geom_text(aes(label = section), size = 3.0, col = "black") +
  geom_label(data = biplot_scores_sign,
             aes(CAP1, CAP2, label = rownames(biplot_scores_sign)),
             color = "black") +
  geom_segment(data = biplot_scores_sign,
               aes(x = 0, y = 0, xend = CAP1, yend = CAP2),
               arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
               color = "lightseagreen", alpha = 1, size = 0.7) +
  geom_label(data = bioreg_centroids,
             aes(x = CAP1, y = CAP2,
                 label = labels), size = 4.0,
             col = "black", fill = "yellow") +
  xlab("CAP1") + ylab("CAP2") +
  ggtitle(expression(paste("Significant thermal variables and ", beta[sim]))) +
  theme_grey() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "none",
        aspect.ratio = 0.8)
