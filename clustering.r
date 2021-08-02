# Quantitative Ecology'
# Cluster Analysis
# 02/08/2021
# Jesse Phillips


# Load Packages -----------------------------------------------------------

library(tidyverse) 
library(cluster)
library(ggcorrplot)
library(factoextra)
library(vegan)
library(ggpubr)


# Load Data ---------------------------------------------------------------

SDGs <- read_csv("data/WHO/SDG_complete.csv")
SDGs[1:5, 1:8] # View first 5 rows and first 8 columns 

# The parent locations:
unique(SDGs$ParentLocation)

# The number of countries:
length(SDGs$Location)

# Correlation analysis of explanatory variables 
corr <- round(cor(SDGs[3:ncol(SDGs)]), 1)
ggcorrplot(corr, type = 'upper', outline.col = "white", 
           colors = c("navy", "white", "#FC4E07"), 
           lab = TRUE)
  # there are highly correlated variables, but let's continue (not ideal)

# Standardize variables
SDGs_std <- decostand(SDGs[3:ncol(SDGs)], method = "standardize")
rownames(SDGs_std) <- SDGs$Location # carry location names into output


# Use the factoextra package to try and decide how many clusters to use

# using silhouette analysis
plt1 <- fviz_nbclust(SDGs_std, cluster::pam, method = "silhouette") + theme_grey()

# total within cluster sum of square / elbow analysis
plt2 <- fviz_nbclust(SDGs_std, cluster::pam, method = "wss") + theme_grey()

# gap statistics
plt3 <- fviz_nbclust(SDGs_std, cluster::pam, method = "gap_stat") + theme_grey()

ggarrange(plt1, plt2, plt3, nrow = 3)


# Let's proceed with 3 clusters (as 2 seem insufficient)
SDGs_pam <- pam(SDGs_std, metric = "euclidean", k = 3)

fviz_cluster(SDGs_pam, geom = "point", ellipse.type = "convex", palette = c("#FC4E07", "violetred3", "deepskyblue3"), ellipse.alpha = 0.05) +
  geom_text(aes(label = SDGs$Location), size = 2.5)


# We cannot clearly see SA, so let's edit the plot
# scale SA bigger for plotting
SDGs <- SDGs |> 
  mutate(col_vec = ifelse(Location == "South Africa", "black", "grey50"),
         scale_vec = ifelse(Location == "South Africa", 3.5, 2.5))

fviz_cluster(SDGs_pam, geom = "point", ellipse.type = "convex",
             palette = c("#FC4E07", "violetred3", "deepskyblue3"),
             ellipse.alpha = 0.05, pointsize = 2.0) +
  geom_text(aes(label = SDGs$Location), size = SDGs$scale_vec, col = SDGs$col_vec)

# Let's make it a star plot
fviz_cluster(SDGs_pam, palette = c("#FC4E07", "violetred3", "deepskyblue3"), ellipse.type = "euclid", 
             star.plot = TRUE, repel = TRUE, pointsize = SDGs$scale_vec * 0.8) + # SA, no 147, plotted slightly bigger
  theme_grey()


# Do silhouette analysis to check cluster fidelity
fviz_silhouette(SDGs_pam, palette = c("#FC4E07", "violetred3", "deepskyblue3"), ggtheme = theme_grey())

# Once happy with the number of clusters, find the median value for each cluster:
SDGs_centroids <- SDGs |> 
  mutate(cluster = SDGs_pam$clustering) |> 
  group_by(cluster) |> 
  summarise_at(vars(other_1:SDG3.b_5), median, na.rm = TRUE)
SDGs_centroids

# pam() can also provide the most representative example countries of each cluster. Note: medoids report the standardised data
SDGs_pam$medoids

# We can do a coloured pairwise scatterplot to check data details. We limit it here to the pairs of the first 7 columns because of the large number of possible combinations
pairs(SDGs[, 3:10], col = c("#FC4E07", "violetred3", "deepskyblue3")[SDGs_pam$clustering])



# QUESTIONS ---------------------------------------------------------------

  # 1. What happens if we use pam() to create four, five, or even six clusters?
SDGs_pam_test <- pam(SDGs_std, metric = "euclidean", k = 6)

fviz_cluster(SDGs_pam_test, geom = "point", ellipse.type = "convex", palette = c("#FC4E07", "violetred3", "deepskyblue3", "goldenrod", "green4", "red4"), ellipse.alpha = 0.05) +
  geom_text(aes(label = SDGs$Location), size = 2.5)
      # With an increase in number of clusters, the groupings of similar countries 
      # become smaller - and more specific. However (in particular with k = 4), we
      # see some big overlaps in the clusters. 

  # 2. In your reasoned opinion, what would be the optimal number of clusters to use?
      # 3 clusters provides a good balance between accuracy and readability. With 3 
      # clusters of countries, we can clearly see the groupings of "Poor health and 
      # well-being", "Developing health and well-being" and "Good health and well-being".
      # Alternatively, 4 clusters may be more efficient in grouping countries of 
      # similar standing, but due to the overlaps in ordination space, it makes it
      # more difficult to interpret. 

  # 3. Repeat the analysis using either kmeans() or hclust(). Are the results markedly different? Which clustering approach do you wish to proceed with?
SDGs_kmeans <- kmeans(SDGs_std, centers = 3, nstart = 25)

# Use fviz_cluster with kmeans data and original data
fviz_cluster(SDGs_kmeans, data = SDGs_std,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw())

# Calculate euclidean distances for hclust()
SDGs_euc <- vegdist(SDGs_std, method = "euclidian")
SDGs_hclust <- hclust(SDGs_euc, method = "complete")

plot(SDGs_hclust, hang = -1, cex = 0.5) # doesn't look good, too many countries 
      
      # pam() and kmeans() produce similar results, but let's continue with the 
      # pam() function. 

  # 4. Build upon the narrative that you have already developed in the previous assignment and describe the patterns that you observe at the end of your final cluster selection
      # In the previous assignment, it was observed that there were rough groupings
      # of: Africa || Western Pacific + Americas + Eastern Med || Europe + SE Asia
      # This cluster analysis confirms that the African countries are indeed a  
      # distinct group of countries, that fall under the category of poor "health
      # and well-being", as assessed by the indicators of SDG 3. 
      # The European and SE Asian countries form a distinct cluster on the other  
      # side of the spectrum, which would show very good "health and well-being". 
      # The middle cluster is slightly more ambiguous, the main countries that make
      # up the grouping are from the W Pacific, the Americas and E Mediterranean,
      # but it also includes a few countries from Africa, the Middle East and Asia. 
      # There is some overlap with this cluster, and the European cluster, showing
      # that in terms of the indicators of SDG 3, these countries are closer to 
      # 'good' than 'bad' - where we find the bulk of the African countries. 

  # 5. Regardless of how many clusters you choose, South Africa often seems to teeter at the edge between the group of African countries and some other parent location. Why?
      # SA - in terms of the indicators of SDG 3 - will always be more similar to the
      # 'middle' category than to the 'poor' category. Most African countries fall
      # into the bottom category and cluster together, but SA is just dissimilar 
      # enough to not be in that cluster. Even with 6 clusters, SA will always fall
      # into a separate cluster from the middle and north African countries. 
