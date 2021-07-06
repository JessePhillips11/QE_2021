# Quantitative Ecology
# Correlations and Associations
# 06/07/2021
# Jesse Phillips


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(vegan)
library(Hmisc) # for rcorr()
library(ggcorrplot) # for correlation plot


# Read Data ---------------------------------------------------------------

env <- read.csv("data/DoubsEnv.csv", row.names = 1) # row.names ensures that random X col doesn't appear
head(env)


# Correlations ------------------------------------------------------------

  # Correlations establish how the environmental variables relate to one another
  # across sample sites. No need to standardize, but in some cases data transformations
  # may be necessary.
round(cor(env), 2)

rcorr(as.matrix(env)) # Correlations AND associated p-vales given (to establish statistical significance)


# QUESTIONS (A) -----------------------------------------------------------

  # 1. Create a plot of pairwise correlations
corr <- cor(env) 
p.mat <- cor_pmat(env) # computes correlation matrix of p-values

ggcorrplot(corr,  # data
           hc.order = TRUE,  # reorders matrix using hierarchical clustering 
           type = "lower",  # displays lower triangle only 
           lab = TRUE,  # adds coefficients
           p.mat = p.mat,  #  bars non-significant correlations by...
           insig = "blank")  # ...blanking them out

  # 2. Name two top positive and two top negative statistically-significant correlations
      # POSITIVE: pho-amm and dfs-flo 
      # NEGATIVE: alt-dfs and alt-flo 

  # 3. For each, discuss the mechanism behind the relationships. Why do these relationships exist?
      # Phosphate concentration and Ammonium concentration (+):

      # Distance from source and Mean minimum discharge (+):

      # Altitude and Distance from source (-): 
        # The lower the altitude, the further away from the source. The source of 
        # the river is located at a high altitude, which flows downwards. 

      # Altitude and Mean minimum discharge (-): 


# Associations ------------------------------------------------------------

  # Doubs River fish species dataset contains abundance data
spp <- read.csv("data/DoubsSpe.csv", row.names = 1)
head(spp)

  # In order to calculate an association matrix, we must first transpose the data
spp_t <- t(spp) # columns and rows are switched 


# QUESTIONS (B) -----------------------------------------------------------

  # 1. Why do we need to transpose the data? 
      # Calculating an associations matrix requires the species to be in the rows.

  # 2. What are the properties of the transposed species table?
      # The properties of the transposed table are the same as that of the original 
      # matrix, but the columns and rows are swapped. So the species are treated 
      # as observations, and the sites are treated as variables. 


# Association Matrix ------------------------------------------------------

spp_assoc1 <- vegdist(spp_t, method = "jaccard") 
as.matrix((spp_assoc1))[1:10, 1:10] # display only first 10 rows and columns

spp_assoc2 <- vegdist(spp_t, method = "jaccard", binary = TRUE)
as.matrix((spp_assoc2))[1:10, 1:10]


# QUESTIONS (C) -----------------------------------------------------------

  # 1. What are the properties of the association matrix? How do these properties differ from a i) species dissimilarity matrix and ii) correlation matrix?
      # The properties of the association matrix are Jaccard's coefficients. This  
      # calculates the similarity between species (or, percentage overlap at each
      # site), whereas a dissimilarity matrix calculates the dissimlarity - the   
      # inverse - and a correlation matrix calculates the dependence of one variable  
      # on another.

  # 2. What is the difference between spp_assoc 1 and 2? Is the information markedly different?
      # spp_assoc1 calculates association based on abundance data, spp_assoc2 uses
      # presence-absence data. The information isn't markedly different, just less
      # extreme (variable) in the case of presence-absence data.

  # 3. Explain the kind of insight we can glean from a species association matrix.
      # Using the Jaccard similarity, we can infer which species appear most at the
      # various sites, as a higher coefficient means a higher amount of overlap. 