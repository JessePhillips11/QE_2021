# Quantitative Ecology 2021
# Biodiversity - Alpha Diversity
# 29/06/2021
# Jesse Phillips


# Load Packages -----------------------------------------------------------

library(vegan)
library(ggplot2)
library(dplyr)
library(BiodiversityR)


# View Data ---------------------------------------------------------------

spp <- read.csv("data/SeaweedsSpp.csv")
spp <- select(spp, -1) #remove X column

dim(spp) #outputs dimensions as ROWS then COLUMNS

spp[1:10, 1:10] #outputs first 10 rows and first 10 columns


# Alpha Diversity ---------------------------------------------------------

# 1. Species Richness
spp_rich <- diversityresult(spp, index = "richness", method = "each site")
    # function from BiodiversityR package
    # calculates species richness for each observation 
ggplot(spp_rich, aes(x = 1:58, y = richness)) +
  geom_line() +
  labs(x = "Coastal section, West to East", y = "Species Richness") 

  # if you struggle with BiodiversityR package, use vegan function
specnumber(spp, MARGIN = 1)

# 2. Univariate Diversity Indices 
light <- read.csv("data/light_levels.csv") # Abundance data is required for univariate diversity indices

  # Calculate richness as well as the Shannon and Simpson indices using diversity() function
light_div <- data.frame(  # create data frame
  site = c("low_light", "mid_light", "high_light"),  # set names
  richness = specnumber(light[,2:7], MARGIN = 1),   # calc spp richness in cols 2-7
  shannon = round(diversity(light[,2:7], MARGIN = 1, index = "shannon"),2), 
  simpson = round(diversity(light[,2:7], MARGIN = 1, index = "simpson"),2)
)
light_div

# 3. Dissimilarity Indices 
sor <- vegdist(spp, binary = TRUE) # use Sorensen index for presence-absence data (hence, binary = TRUE)
sor_df <- as.data.frame(round(as.matrix(sor), 4)) # convert to df, rounded to 4 decimal places
sor_df[1:20, 1:20] # first 20 rows and columns


# QUESTIONS ---------------------------------------------------------------

# 1. Why is matrix square, and what determines the number of rows/columns?
    # The dissimilarity of each site (observation/row) is compared to that of every other 
    # site in the data frame. Therefore 58 rows are compared against 58 rows, which will 
    # always return a square output. The number of rows/columns in the dissimilarity index
    # are determined by the number of rows in the original spp data frame. 

# 2. What is the meaning of the diagonal?
    # The diagonal represents the output of each site being compared to itself. Hence, the 
    # dissimilarity of 0.0000

# 3. What is the meaning of the non-diagonal elements?
    # The non-diagonal elements represents each site being compared to each other site. The 
    # value given represents the amount of dissimilarity between 0 and 1. The higher the  
    # number, the less similar a particular site is to another. 

# 4. Take the data in row 1 and create a line graph that shows these values as a function of section number
ggplot(sor_df, aes(x = 1:58, y = sor_df[,1])) +
  geom_line() +
  labs(x = "Coastal section, West to East", y = "Dissimilarity (Sorensen Index)")

# 5. Provide a mechanistic (ecological) explanation for why this figure takes the shape that it does
    # We know that our x-axis represents sites along our coastline, from west to east, so
    # we can infer that as we move from site 1 to site 58, the distance between sites 
    # increases and therefore the dissimilarity between sites increases. As you move from 
    # one site to another, the trend shows that the composition of species and species 
    # richness changes. This graph shows that site 58 has a near entirely different species
    # composition and richness than site 1, that can be explained by their difference in 
    # geographical location and differences in the physical properties of their respective
    # oceans. 