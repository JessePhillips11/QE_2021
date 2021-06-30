# Quantitative Ecology 2021
# Gamma Diversity 
# 30/06/2021
# Jesse Phillips


# Load Packages -----------------------------------------------------------

library(BiodiversityR)


# Read in Data ------------------------------------------------------------

spp <- read.csv("data/SeaweedsSpp.csv")
spp <- select(spp, -1) #remove X column


# Gamma Diversity ---------------------------------------------------------

  # number of columns gives number of species in this example
  # i.e. species richness along entire coastline
ncol(spp)
    # 847

  # function in biodiversityR package performs same task
diversityresult(spp, index = "richness", method = "pooled")
    # 846 ->> why the difference?


# QUESTIONS ---------------------------------------------------------------

  # 1. Why is there a difference between the two outputs?
      # The ncol() function gives the number of columns in the data frame, while the
      # "pooled" diversityresult() function calculates the diversity of the entire 
      # community. In this case, the number of columns (i.e. number of species) SHOULD
      # give us the total species diversity, BUT there is a species in the data frame
      # that does not appear in any of the sites - see code below.

# use colMeans to find mean values of every column, skim output to see if there's a 0 
colMean(spp)
# found it...
mean(spp$ANTNEM)
# since the mean of this species returns 0, there are only 0s and no 1s, meaning this species doesn't appear at any site

  # 2. Which is correct?
      # Therefore, 846 is the correct number for total species richness, and using 
      # diversityresult() is the more robust method. 


