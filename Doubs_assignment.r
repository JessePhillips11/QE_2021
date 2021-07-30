# Quantitative Ecology
# Integrative Assignment: Doubs River Study
# 28/07/2021
# Jesse Phillips

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(vegan)
library(betapart)
library(ggcorrplot)
library(gridExtra)
library(gridBase)
library(grid)
library(cluster)
library(ggpubr)


# Get Data ----------------------------------------------------------------

spp <- read.csv("data/DoubsSpe.csv", row.names = 1)
env <- read.csv("data/DoubsEnv.csv", row.names = 1)
sites <- read.csv("data/DoubsSpa.csv", row.names = 1)

spp <- slice(spp, -8) # Row 8 is removed, as it has no observations whatsoever
env <- slice(env, -8) # It also has to be removed here
sites <- slice(sites, -8) # and here

# Env Variables (Correlations) --------------------------------------------

corr <- cor(env) # matrix of correlations amongst env variables
pmat <- cor_pmat(env) # matrix of p-values for correlations

# Plot of pairwise correlations
corpl <- ggcorrplot(corr,
           hc.order = TRUE, # orders plot by hierarchical clustering
           type = "lower", # lower triangle only
           lab = TRUE, # displays values
           p.mat = pmat, # only shows significant correlations
           insig = "blank", # blocks out the rest
           legend.title = "") 


# Separate Beta Diversity of spp ------------------------------------------

# We want to discuss the effects of species turnover (beta-sim)
spp_pa <- decostand(spp, method = "pa") # convert abundance to presence-absence data

Y.core <- betapart.core(spp_pa) # computes basic qualities needed for computing multiple-site beta diversity measures and pairwise dissimilarity matrices

Y.pair <- beta.pair(Y.core, index.family = "sor") # computes 3 Sorenson dissimilarity matrices accounting for turnover, nestedness-resultant and total dissimilarity 

# Let Y1 be the turnover component (beta-sim):
Y1 <- as.matrix(Y.pair$beta.sim)


# db-RDA -------------------------------------------------------------------
#(see p. 156 of textbook)

# Select evn variables that aren't highly correlated
E1 <- select(env, -dfs, -pho, -bod, -alt)

# Standardize env variables
E1 <- decostand(E1, method = "standardize")

# Fit full model
rda_full <- capscale(Y1 ~ ., E1)

# Summary of full model
summary(rda_full)

# Is model fit significant?
anova(rda_full) # F = 4.63 and p < 0.05 || yes 

rda_full_R2 <- RsquareAdj(rda_full)$adj.r.squared

vif.cca(rda_full) # check for any more collinearity 

# Significance 
anova(rda_full, by = 'axis')

anova(rda_full, by = "terms") #slo, flo, nit, oxy

table_of_env_vars <- anova(rda_full, by = "terms")

# Extract significant variables 
(sig_vars <- anova(rda_full, by = "terms"))

# The significant variables are:
rda_sig_vars <- which(sig_vars[, 4] < 0.05)
rda_sig_vars <- colnames(E1[,rda_sig_vars])

# PLOTS
rda_scrs <- scores(rda_full, display = c("sp", "wa", "lc", "bp"))

lc_scores <- data.frame(rda_scrs$constraints) # linear constraints for each of the sites
site_scores <- data.frame(rda_scrs$sites) # site scores
site_scores$section <- seq(1:29)

biplot_scores <- data.frame(rda_scrs$biplot)
biplot_scores$labels <- rownames(biplot_scores)
biplot_scores_sign <- biplot_scores[biplot_scores$labels %in% rda_sig_vars,]

#ggplot(lc_scores, aes(x = CAP1, y = CAP2)) + 
 # geom_point()

ggplot(site_scores, aes(x = CAP1, y = CAP2)) + 
  geom_point(size = 5, shape = 21, fill = "white") +
  geom_text(aes(label = section), size = 3, col = "black") +
  geom_label(data = biplot_scores_sign,
             aes(CAP1, CAP2, label = rownames(biplot_scores_sign)),
             color = "navy") +
  geom_segment(data = biplot_scores_sign, 
               aes(x = 0, y = 0, xend = CAP1, yend = CAP2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "darkred", alpha = 1, size = 0.75) +
  ggtitle(expression(paste("Significant environmental variables and ", beta[sim]))) +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dotted")

plot(rda_full, scaling = 1)
plot(rda_full, scaling = 2)


# Cluster Analysis --------------------------------------------------------

# for species abundance data, compute matrix of chord distance among sites
spp.norm <- decostand(spp, method = "normalize")
spp.ch <- vegdist(spp.norm, "euc")

# Create clusters 
spp.ch.single <- hclust(spp.ch, method = "single") # single linkage agglomerative clustering
spp.ch.complete <- hclust(spp.ch, method = "complete") # complete linkage agglomerative clustering
spp.ch.UPGMA <- hclust(spp.ch, method = "average") # UPGMA agglomerative clustering
spp.ch.ward <- hclust(spp.ch, method = "ward") # Ward's minimum variance clustering

spp.ch.ward$height <- sqrt(spp.ch.ward$height) # plots easier
plot(spp.ward)

# Cophenetic correlation
cor(spp.ch, cophenetic(spp.ch.single)) # 0.599
cor(spp.ch, cophenetic(spp.ch.complete)) # 0.766
cor(spp.ch, cophenetic(spp.ch.UPGMA)) # 0.861
cor(spp.ch, cophenetic(spp.ch.ward)) # 0.759

# Gower's distance (looking for smallest)
sum((spp.ch - (cor(spp.ch, cophenetic(spp.ch.single))))^2) # 106.71
sum((spp.ch - (cor(spp.ch, cophenetic(spp.ch.complete))))^2) # 65.31
sum((spp.ch - (cor(spp.ch, cophenetic(spp.ch.UPGMA))))^2) # 51.73
sum((spp.ch - (cor(spp.ch, cophenetic(spp.ch.ward))))^2) # 66.45

# textbook p. 67 - 74

# Final Dendogram 
  # reorder 
spp.chwo <- reorder(spp.ch.ward, spp.ch)
  # resize
spp.chwo$height <- sqrt(spp.chwo$height)
  # plot with group labels
plot(spp.chwo, hang = -1, xlab = "4 Groups", sub = "", ylab = "Sqrt(Height)",
     main = "Chord - Ward (reordered)")
rect.hclust(spp.chwo, k = 4)



# Some ggarrange ----------------------------------------------------------

table <- as.tibble(rownames(table_of_env_vars[1:7,]))
table$F <- table_of_env_vars[1:7,3]
table$p <- table_of_env_vars[1:7,4]

stable.p <- ggtexttable(table)

text <- paste("Figure 1: Correlation plot of the Doubs River environmental data.",
              "Only significant positive and negative correlations are shown.",
              "Alongside, the output table of the test for significance of the",
              "environmental variables used in the db-RDA.", sep = " ")

text.p <- ggparagraph(text = text, face = "italic", size = 11, color = "black")

ggarrange(corpl, stable.p, text.p)
