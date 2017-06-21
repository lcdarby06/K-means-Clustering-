# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

install.packages(c("cluster", "rattle","NbClust"))

# Now load the data and look at the first few rows
data(wine, package="rattle")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
wine_scale <- scale(wine[-1])
head(wine_scale)
# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wine_scale)

# Exercise 2:
#   * How many clusters does this method suggest?
# This seems to suggest 2 or 3 clusters.

#   * Why does this method work? What's the intuition behind it?
# Ideally, you'd want the marginal improvement from adding another cluster to be relatively high, as in a significant reduction in the within-group variation.
#   * Look at the code for wssplot() and figure out how it works
# The function takes a data frame with the within-group SS being calculated for up to 15 clusters, starting at 2 clusters (nc=15). The within-group SS
# is defined as the number of rows in the data frame minus one multiplied by the variance function (var) applied across columns (2) of the data.
# The within-group sum of sqaures is applied over each cluster (for loop), and this wss is calculated as kmeans within-group sum of squares for each instance of an increasing number of clusters
# from 2 clusters to 15 clusters. The WSS for each cluster number is then plotted on the y axis with the corresponding number of clusters used on the X axis.

# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wine_scale, min.nc=2, max.nc=15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Numer of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
# This indicates 3 clusters.

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km

fit.km <- kmeans(wine_scale, centers = 3)

# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?
table(fit.km$cluster, wine$Type, dnn = c("cluster", "type"))
# Yes, it seems to work well. There is clear alignment between the clusters created by kmeans clustering and the original wine types. Only 6 observations don't align.

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?
# Yes, it seems to be good clustering based on the plot. There's not a lot of overlap.
library(cluster)
clusplot(wine_scale, clus = fit.km$cluster)
