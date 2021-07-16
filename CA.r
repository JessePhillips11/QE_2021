# Quantitative Ecology
# Correspondence Analysis
# 15/07/2021
# Jesse Phillips 


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(vegan)
library(ade4)


# Read in Data ------------------------------------------------------------

spe <- read.csv("data/DoubsSpe.csv", row.names = 1)
head(spe, 8)


# Do Correspondence Analysis ----------------------------------------------

spe_ca <- cca(spe)
  # Error: at least one of the rows in the matrix sums to 0. but which one?
apply(spe, 1, sum) # The '1' ensures the sum function is applied to the ROWS
  # We see that row 8 sums to 0, and therefor contains 0 species, we can omit it. 
spe <- spe[rowSums(spe) > 0,]

# Now we can do the CA
spe_ca <- cca(spe)
summary(spe_ca)

# Total inertia:
round(sum(spe_ca$CA$eig), 5)

# Inertia of the first axis(CA1):
round(spe_ca$CA$eig[1], 5)

# Inertia of CA1 and CA2:
round(sum(spe_ca$CA$eig[1:2]), 5)

# Fraction of the variance explained by CA1 and CA2 (in %):
round(sum(spe_ca$CA$eig[1:2]) / sum(spe_ca$CA$eig) * 100, 2)



# Ordination Diagrams -----------------------------------------------------

plot(spe_ca, scaling = 1, main = "CA fish abundances - biplot scaling 1")
plot(spe_ca, scaling = 2, main = "CA fish abundances - biplot scaling 2")

  # Scaling 1 emphasizes relationships between ROWS (Sites)
  # Scaling 2 emphasizes relationships between COLUMNS (Species)


# Now lets make biplots focused on 4 specific species. 
require('viridis')
palette(viridis(8))
par(mar = c(4, 4, 0.9, 0.5) + .1, mfrow = c(2, 2))
with(spe, tmp <- ordisurf(spe_ca ~ Satr, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Satr"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Scer, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Scer"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Teso, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Teso"))
abline(h = 0, v = 0, lty = 3)
with(spe, tmp <- ordisurf(spe_ca ~ Cogo, bubble = 3,
                          family = quasipoisson, knots = 2, col = 6,
                          display = "sites", main = "Cogo"))
abline(h = 0, v = 0, lty = 3)

# a posteriori projection of environmental variables in a CA
env <- read.csv("data/DoubsEnv.csv", row.names = 1)

# we removed the 8th row in spe, so do it here too
env <- dplyr::slice(env, -8)

# the last plot produced (CA scaling 2) must be active
# scaling 2 is default
(spe_ca_env <- envfit(spe_ca, env, scaling = 2))
plot(spe_ca_env, col = "grey40")
plot(spe_ca_env, p.max = 0.05, col = "red") # plot significant variables with a different colour



# QUESTIONS ---------------------------------------------------------------

# 1. How would you explain the patterns seen in the four panels of the above figure?
    # Each plot focuses on a particular species and their relationship with the 
    # sites. For each site, a bubble is plotted, and the size of the bubble is  
    # proportional to the species score of the species at that particular site.
    # So the pattern of the bubbles shows us the relative impact - by adundance -
    # a species has on the sites. The contour lines group sites by abundances of 
    # species, so  we can determine the gradient of species impact along the sites. 
    # In the Doubs data, the Satr species high adundances in the head and tail 
    # ends of the river, but it isn't present in the middle. Scer only really 
    # has high abundances at a handful of sites in the middle of the river. Teso
    # and Cogo have high abundances in the sites along the tail end of the river. 

# 2. Apply these approaches to the [A] Birds and [B] Aravo datasets

  # [A] Bird Communities
ybirds.spe <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_spe.txt', row.names = 1)

birds_ca <- cca(ybirds.spe)
summary(birds_ca)

par(mfrow = c(1,1)) # View 1 plot on full pane 
plot(spe_ca, scaling = 1, main = "CA Bird Communtites - Biplot Scaling 1")
plot(spe_ca, scaling = 2, main = "CA Bird Communtites - Biplot Scaling 2")

    # Posteriori projection of env variables
ybirds.env <- read.delim ('https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/ybirds_env.txt', row.names = 1)
birds_ca_env <- envfit(birds_ca, ybirds.env, scaling = 2) # Scaling 2 is used and must be active plot
plot(birds_ca_env, p.max = 0.05, col = "grey40") # significant variables are plotted


  # [B] Alpine Plant Communities in Aravo 
data(aravo)
aravo.spe <- aravo$spe

aravo_ca <- cca(aravo.spe)
summary(aravo_ca)

par(mfrow = c(1,1)) 
plot(aravo_ca, scaling = 1, main = "CA Alpine Plant Communities - Biplot Scaling 1")
plot(aravo_ca, scaling = 2, main = "CA Alpine Plant Communities - Biplot Scaling 2")

      # Posteriori projection of env variables
aravo.env <- aravo$env
aravo_ca_env <- envfit(aravo_ca, aravo.env, scaling = 2) 
plot(aravo_ca_env, p.max = 0.05, col = "grey40") 


# 3. Discuss patterns observed and explain ordination diagrams with particular reference to how species are influenced by major environmental drivers

  # 16/07/2021 16:55
  # I haven't gotten this far yet, but by later tonight I will. 