# Quantitative Ecology 
# Principle Component Analysis
# 08/07/2021
# Jesse Phillips 


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(vegan)


# Read in Data ------------------------------------------------------------

env <- read.csv("data/DoubsEnv.csv", row.names = 1)
head(env)


# Do the PCA --------------------------------------------------------------

  # rda() function - without specifying constrains - does the PCA.
  # PCA automatically standardizes data (which is necessary for this environmental data)
  # when argument scale = TRUE. 
  # PCA preserves Euclidean distances and relationships detected are linear. 

env.pca <- rda(env, scale = TRUE)
env.pca

  # Eigenvalues for unconstrained axes show relative importance of resultant reduced axes
  # can be used to determine proportion of total inertia captured by one of the axes
  # To extract the first eigenvalue we can do:
round(env.pca$CA$eig[1], 3)

  # The total inertia is:
sum(env.pca$CA$eig)

  # So the proportion of variation explained by the first PC is:
round(env.pca$CA$eig[1] / sum(env.pca$CA$eig) * 100, 1) # result in %


# QUESTIONS (A) -----------------------------------------------------------

  # Why can a PCA not explain all of the variation in a dataset? In other words, why is it best to only use the first few Principal Components? What is explained by the remaining PC axes?
      # PCA is not a statistical test, but rather a heuristic procedure that allows
      # a user to represent major features of data along a reduced number of axes.
      # Therefore, the user would only need to interpret the number of axes necessary
      # to represent +-75% of the variance of the data (or however many axes represents
      # variance that is of interest). This is generally sufficient, and often the 
      # first two or three Principal Components represent this. 

# Species and Site Scores -------------------------------------------------

summary(env.pca)
 
  # Species scores indicate STRENGTH OF CONTRIBUTION of original environmental variables
  # to the Principal Components. 
  # Don't be fooled by the name! We are working with environmental data, called 'species
  # scores' by the software.
  # The larger (more +) and smaller (more -) values indicate greater contribution, 
  # albeit in opposite directions. 

  # Site scores plot the position of the sites in 2D or 3D ordination space.
  # Sites spread further apart differ much in terms of environmental conditions.

  # When calling SCALING 1 (to interpret species), the distances between points retain
  # their Euclidean distances, which allows for better interpretation of how sites
  # relate to one-another. 
  # When calling SCALING 2 (to interpret variables), the angles between variables are
  # preserved, with the consequence that smaller angles between variable vectors will
  # reflect stronger correlations. 


# Graphical Representations of Ordinations --------------------------------

biplot(env.pca, scaling = 1, main = "PCA Scaling 1", choices = c(1,2))
biplot(env.pca, sclaing = 2, main = "PCA Scaling 2", choices = c(1,2))

  # We can create biplots with the cleanplot.pca() function from Numeral Ecology in R
    # First we need to load the function from its R file
source("cleanplot.pca.R")
cleanplot.pca(env.pca, scaling = 1)
cleanplot.pca(env.pca, scaling = 2)

  # We can plot underlying environmental gradients using odisurf() function in vegan
biplot(env.pca, type = c("text", "points"), col = c("black", "black"))
ordisurf(env.pca ~ bod, env, add = TRUE, col = "turquoise", knots = 1)
ordisurf(env.pca ~ alt, env, add = TRUE, col = "salmon", knots = 1)
  

# QUESTIONS (B) -----------------------------------------------------------

  # 1. Replicate analysis on environmental data included in...
      # [A] Bird Communities in Yushan Mountain 
ybirds.env <- read.delim('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_env.txt', row.names = 1)
ybirds.env <- select(ybirds.env, c(-'Veg.', -'Veg_ext')) # PCA can only use numeric cols

birds.pca <- rda(ybirds.env, scale = TRUE)
summary(birds.pca)

par(mfrow = c(1,2)) # display 2 plots, zoom to see clearly
cleanplot.pca(birds.pca, scaling = 1)
cleanplot.pca(birds.pca, scaling = 2)

      # [B] Alpine Plant Communities in Aravo 
library (ade4)
data (aravo)
aravo.env <- aravo$env
aravo.env <- select(aravo.env, c(-'Form', -'ZoogD')) # only numeric cols

aravo.pca <- rda(aravo.env, scale = TRUE)
summary(aravo.pca)

par(mfrow = c(1,2)) # display 2 plots, zoom to see clearly
cleanplot.pca(aravo.pca, scaling = 1)
cleanplot.pca(aravo.pca, scaling = 2)

  
  # 2. Discuss the patterns observed. 
    # A - Explain the ordination diagram with particular reference to the pattern shown
      
      # In the Bird's PCA biplot (Scaling 1), the sites show is a gradient from 
      # left to right, roughly in a 'W' shape. The first cluster of variables 
      # display the highest values of tree density and diversity, as well as
      # secondary tree cover. The environment then shifts to that with high 
      # values of foliage height diversity, tree circumference and canopy cover. 
      # The next big cluster displays high values of elevation and conifers, and 
      # the final environmental change is of that to high ground cover. 
      # Overall, there is an environmental progression along the sites of:
      # Lots of trees, with high diversity -> Tall trees -> Conifers as the 
      # elevation climbs -> High percentage of ground cover.  

      # In the Aravo's PCA biplot (Scaling 1), the two main clusters form where 
      # aspect is low and high, with very few sites in between - this shows a lack
      # of a gradient, and rather two distinct environmental states. Aspect also 
      # has a very high species score for PC2, which shows that it contributes 
      # highly to the variance explained in the sites. 
    

    # C - If there are significant relationships between variables, provide mechanistic reasons for how they came about
      # In the Bird's PCA biplot (Scaling 2), we see strong positive correlations
      # between the group of variables: tree species diversity, tree density,   
      # secondary tree cover, herb cover and aspect. This group of variables is 
      # negatively correlated with another group: elevation, conifer percentage,
      # exposure, slope. This can be interpreted as sites at high elevation, on   
      # steep, exposed slopes are dominated by conifers. This is in contrast to 
      # sites at lower elevation, which have greater floral diversity and density.
      
      # In the Aravo's PCA biplot (Scaling 2), the variables Physical Disturbance
      # and Slope are highly positively correlated. This makes sense, as a steeper
      # slope would promote runoff and soil erosion, therefore higher physical
      # disturbance. 