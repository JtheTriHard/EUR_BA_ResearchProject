# Author: JTheTriHard
# Test Version

# Install necessary packages
#install.packages("Hmisc",dependencies = TRUE)
#install.packages("psych",dependencies = TRUE)
#install.packages("stargazer", dependencies = TRUE)
#install.packages("lm.beta", dependencies = TRUE )
#install.packages("tidyverse",dependencies = TRUE)
#install.packages("devtools", dependencies = TRUE)
#install.packages("plotly",dependencies = TRUE)
#install.packages("plyr",dependencies = TRUE)
#install.packages("fpc", dependencies = TRUE)
#install.packages("dbscan", dependencies = TRUE)

# Load data
dsBikeContract <- read.csv(file="~/Rcode/EUR/BA Final Project/EUR_BA_ResearchProject/Data/BikeSharingContracts.csv",stringsAsFactors=FALSE)
nInitialObs <- nrow(dsBikeContract)

set.seed(123)

#---------------------------------------------------
#
#             1. Data Preparation
#
#---------------------------------------------------

# Remove observations with NaN (3 removed)
dsBikeContract <- na.omit(dsBikeContract)

# Find rows/columns of dataset
nRows <- nrow(dsBikeContract)
nCols <- ncol(dsBikeContract)
cat("Number of Responses Removed: ",nInitialObs-nRows)

# EXPLANATORY VARS

# Create likert indicator as an average of the scores
myPsychOwn <- c("PsychOwn01", "PsychOwn02", "PsychOwn03","PsychOwn04")
dsBikeContract$PsychOwn <- psych::alpha(dsBikeContract[myPsychOwn],check.keys=TRUE,cumulative = FALSE)$scores

# Frequency of use (Scale 1-5)
dsBikeContract$UseFreq <- sample(1:5,nRows, replace = TRUE) # randomly assigned for testing purposes

# Minimum length of contract (1, 3, or 6 months)
dsBikeContract$MinLength <- sample(seq(from = 0, to = 6, by = 3),nRows, replace = TRUE) # randomly assigned for testing purposes
for (i in 1:nRows){
  if (dsBikeContract$MinLength[i]==0){
    dsBikeContract$MinLength[i] <- 1
  }
}

# CONTROL VARS

# Age (Survey requests yr of birth)
curYr = 2019
dsBikeContract$Age <- curYr - dsBikeContract$YrBirth

# Employment (1 = NA, 2 = Part-Time, 3 = Full-Time)
for (i in 1:nRows){
  if (dsBikeContract$HrPWork[i] >= 35){
    dsBikeContract$HrPWork[i] <- 3
  }
  else if (dsBikeContract$HrPWork[i] <35 && dsBikeContract$HrPWork[i] > 0){
    dsBikeContract$HrPWork[i] <- 2
  }
  else{
    dsBikeContract$HrPWork[i] <- 1
  }
}

# Location (See report for assignments)
dsBikeContract$Neighborhood <- sample(1:12,nRows, replace = TRUE) #randomly assigned for testing purposes
#dsBikeContract$Neighborhood <- dsBikeContract$cLocate

# Gender (1 = M, 2 = F): dGender

# TARGET VAR
# Willingness to engage in bike-sharing = IntentContract

# Remove all unused variables from test dataset
dsBikeContract <- dplyr::select(dsBikeContract,PsychOwn,UseFreq,MinLength,Age,HrPWork,Neighborhood,dGender,IntentContract)

# Save summary of non-normalized data
stargazer::stargazer(dsBikeContract,
                     align = TRUE ,
                     digits=2,
                     type = "html",
                     out = "~/Rcode/EUR/BA Final Project/EUR_BA_ResearchProject/Results/dsSummary.doc")


#---------------------------------------------------
#
#             2. Dimension Reduction
#
#---------------------------------------------------

# Reduce only the control variables down to a total of 3 dimensions to allow for 3D visualization
# Leave explanatory vars attached

ctrlVars <- dplyr::select(dsBikeContract,Age,HrPWork,Neighborhood,dGender)
pcaResults <- prcomp(ctrlVars,scale = TRUE)
pcaSummary <- summary(pcaResults)
# Scree plot
barplot(pcaSummary$importance[2,],
        ylab = "Proportion of Variance")
barplot(pcaSummary$importance[3,],
        ylab = "Cumulative Proportion of Variance")
cat("Proportion of Variance Not Accounted for: ",pcaSummary$importance[2,4])

# Biplot for first two components
biplot(pcaResults)
# observation 55, 99 is a significant outlier
# TO DO: Figure out how to display var lines on 3D plot?

# WARNING: The fourth component accounts for a significant portion of the variance (~20%)
# TO DO: Test other methods for determining if PCA is appropriate
# Continue for the sake of practicing the concept, but do not rely on for accurate analysis

# Plot on 2D plot
newCoords <- as.data.frame(pcaSummary$x)
newCoords <- newCoords[c("PC1","PC2","PC3")]
library(ggplot2)
ggplot(newCoords, aes(x=PC1,y=PC2))+
  geom_point(color = "red")
uniqueCoords2D <- sum(!duplicated(newCoords[,1:2])) 

# Plor on 3D plot
# Generates a plot with roughly 4 groups of planes and ~6 outliers
library(plotly)
plot_ly(newCoords,x = ~PC1, y = ~PC2, z = ~PC3,
                marker = list(size = 3))
uniqueCoords3D <- sum(!duplicated(newCoords[,1:3]))



# TROUBLE SHOOTING

# Checking for unique points in any combination of dimensionals for the PCA result always results in 175, can be indicating issue
uniqueCoords<-plyr::count(newCoords[,1:3])
cat("Number of Points Not Displayed Due to Overlap: ",nRows - nrow(uniqueCoords))

# Explore original data to check for possible causes of issues
plot_ly(dsBikeContract,x = ~Age, y = ~HrPWork, z = ~dGender,
        marker = list(size = 3))
reducedDF <- dsBikeContract[c("Age","HrPWork","dGender")]
unique_OG <- plyr::count(reducedDF)
# There are only 33 unique points out of the 412
# when only considering Age,Work,Gender (Neighborhood was randomized, and thus left out)
# The majority form lines in the 3D space, due to the scales of the var
# Thus likely the cause of the planes occurring in the PCA plot


#---------------------------------------------------
#
#                  3. Clustering
#
#---------------------------------------------------



# K MEANS

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(newCoords, k, nstart = 25 )$tot.withinss
}
# Compute and plot wss for k = 1 to k = 15
k.values <- 1:10
# extract wss for 2-15 clusters
wss_values <- purrr::map_dbl(k.values, wss)
# Plot elbow method. No clear elbow but a choice of 4 seems appropriate
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Use K-Means clustering for k = 
kMeansResult <- kmeans(newCoords, centers = 2, nstart = 25)

# Plot color-coded result
newCoords$Cluster <- kMeansResult$cluster
newCoords$Cluster <- as.factor(newCoords$Cluster)
plotly::plot_ly(newCoords,x = ~PC1, y = ~PC2, z = ~PC3,
        color = ~Cluster,
        marker = list(size = 3))

# Add cluster assignment to original dataset
dsBikeContract$Cluster <- newCoords$Cluster

# Plot explanatory vars separately for each cluster
# Plotly is known to have issues plotting within loops. Plotted without loop due to low amount of clusters
# See https://github.com/ropensci/plotly/issues/273 for solution

dsTemp <- subset(dsBikeContract, Cluster == 1)
plotly::plot_ly(dsTemp,x = ~PsychOwn, y = ~UseFreq, z = ~MinLength,
                size = ~IntentContract,
                marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(1, 20))

dsTemp <- subset(dsBikeContract, Cluster == 2)
plotly::plot_ly(dsTemp,x = ~PsychOwn, y = ~UseFreq, z = ~MinLength,
                size = ~IntentContract,
                marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(1, 20))



# Alternative: DBSCAN Clustering

# Determine optimal epsilon
dbscan::kNNdistplot(newCoords[,1:3], k = 5) #knee occurs around eps = 1
# Run dbscan
dbscanResult <- fpc::dbscan(newCoords[,1:3], eps = 1, MinPts = 5)
# Assign clusters
newCoords$Cluster_dbscan <- factor(dbscanResult$cluster)
# Plot
plotly::plot_ly(newCoords,x = ~PC1, y = ~PC2, z = ~PC3,
                color = ~Cluster_dbscan,
                marker = list(size = 3))



# Alternative: Cluster using the planes formed in the 3D PCA result (usually 2-4)




#---------------------------------------------------
#
#                   4. Analysis
#
#---------------------------------------------------