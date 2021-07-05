# Quantitative Ecology 2021
# Species dissimilarities 
# 04/07/2021
# Jesse Phillips 


# Load packages -----------------------------------------------------------

library(dplyr)
library(vegan)

# Read in data ------------------------------------------------------------

doubs <- read.csv("data/DoubsSpe.csv")
doubs <- select(doubs, -X) # remove redundant X col


# QUESTIONS ---------------------------------------------------------------

  # 1. Look at the dataset and explain its structure in words
doubs
      # The dataset contains the abundance data of 27 different species at 30 
      # different sites 

  # 2. Would we use Bray-Curtis or Jaccard dissimilarities? 
      # Bray-Curtis, as we are working with abundance data

  # 3. Apply the calculation
BC_doubs <- vegdist(doubs, method = "bray", diag = TRUE)

  # 4. Explain the meaning of the data in broad terms
      # The Bray-Curtis index quantifies the dissimilarity of the species 
      # compositions between sites. Pairwise calculates are made between every
      # site, and indices close to 0 mean sites are very similar, while indices
      # close to 1 mean sites are very dissimilar. 

  # 5. Examine it more closely: what general pattern comes out?
      # When moving from site 1 to site 30, the sites rapidly become more and 
      # more dissimilar from site 1. From site 19 onwards, the sites seem to be
      # equally dissimilar from site 1. This is interpreted as site 1's species
      # composition being completely different from the majority of the other
      # sites
 
  # 6. Plot this pattern 
BC_doubs_plot <- as.data.frame(as.matrix(BC_doubs))

ggplot(BC_doubs_plot, aes(x = 1:30, y = BC_doubs_plot[,1])) +
  geom_line(col = "red4", size = 1) + 
  labs(x = "Sites", y = "Bray-Curtis Dissimilarity Index") +
  theme_bw()

  # 7. What explanations can you offer for this pattern?
      # The environmental conditions in site 1 are very different from that in 
      # site 5, and there seems to be a sharp environmental gradient across the 
      # first few sites. From sites 5 to 20, there are probable fluctuations in
      # environmental conditions, are there is dissimilarity among those sites.
      # The environment from sites 20 to 30 seems to be constant - but entirely 
      # different from that in site 1 - as all those sites are equally 
      # dissimilar from site 1. 

  # 8. Using the decostand() function, create presence/absence data, and apply the appropriate vegdist() function to obtain a suitable dissimilarity matrix
PA_doubs <- decostand(doubs, method = "pa") 

Sor_doubs <- vegdist(PA_doubs, binary = TRUE, diag = TRUE)

  # 9. Create another plot and examine the pattern 
Sor_doubs_plot <- as.data.frame(as.matrix(Sor_doubs))

ggplot(Sor_doubs_plot, aes(x = 1:30, y = Sor_doubs_plot[,1])) +
  geom_line(col = "blue3", size = 1) + 
  labs(x = "Sites", y = "Sorenson Dissimilarity Index") + 
  theme_bw()

      # The shape of the curve is very similar to the curve created with abundance
      # data, though not identical. The peaks and troughs in sites 5 to 15 aren't
      # as extreme, which means that similar species occur in those sites, but 
      # their abundances vary more. 