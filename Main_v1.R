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
set.seed(123)

# Load data
dsAll <- read.csv(file="~/Rcode/EUR/BA Final Project/EUR_BA_ResearchProject/Data/BikeSharingContracts.csv",stringsAsFactors=FALSE)
initialObsContract <- sum(dsAll$Contract==TRUE)
initialObsNo <- sum(dsAll$Contract==FALSE)

#---------------------------------------------------
#
#             1. Data Preparation
#
#---------------------------------------------------

# Remove respondents not aware of Swapfiets

# Remove respondents not living in the Netherlands

# Remove respondents with Gender = Other (Insignificant sample size)

# CONTROL VARS

# Age (Survey requests yr of birth)
curYr = 2019
dsContract$Age <- curYr - dsContract$YrBirth

# Enrollment Status
# Check n of respondents that are not students
ggplot2::ggplot(dsContract)
# Remove non-student respondents if not many
dsContract$Enroll <- TRUE

# Employment (Survey asks for hrs worked per week)
# (1 = NA, 2 = Part Time, 3 = Full Time)
for (i in 1:nRows){
  if (dsContract$HrPWork[i] >= 35){
    dsContract$HrPWork[i] <- 3
  }
  else if (dsContract$HrPWork[i] <35 && dsContract$HrPWork[i] > 0){
    dsContract$HrPWork[i] <- 2
  }
  else{
    dsContract$HrPWork[i] <- 1
  }
}

# Location (See report for assignments)
dsContract$Neighborhood <- dsContract$cLocate

# Gender (1 = M, 2 = F): dGender

# Education Level
dsContract$Education <- sample(1:6, nRows, replace = TRUE) # randomly assigned for testing purposes

# Employment (1 = NA, 2 = Part-Time, 3 = Full-Time)

# EXPLANATORY VARS

# Create likert indicator as an average of the scores
allPsychOwn <- c("PsychOwn01", "PsychOwn02", "PsychOwn03","PsychOwn04")
dsContract$PsychOwn <- psych::alpha(dsContract[allPsychOwn],check.keys=TRUE,cumulative = FALSE)$scores

# Create likert indicator as an average of the scores
allEnv <- c("PsychOwn01", "PsychOwn02", "PsychOwn03","PsychOwn04")
dsContract$PsychOwn <- psych::alpha(dsContract[myPsychOwn],check.keys=TRUE,cumulative = FALSE)$scores

# Minimum length of contract (1, 3, or 6 months)
dsContract$MinLength <- sample(seq(from = 0, to = 6, by = 3),nRows, replace = TRUE) # randomly assigned for testing purposes
for (i in 1:nRows){
  if (dsContract$MinLength[i]==0){
    dsContract$MinLength[i] <- 1
  }
}

# Frequency of use

# IF SWAPFIETS CONTRACT
# Type of Swapfiets
dsContract$Bike <- sample(1:3,nRows, replace = TRUE) # randomly assigned for testing purposes

# Regular Use (0 = TRUE, 1 = FALSE)
dsContract$UseFreq <- sample(0:1,nRows, replace = TRUE) # randomly assigned for testing purposes

# Length of Current Contract
dsContract$Contract <- sample(1:5,nRows, replace = TRUE) # randomly assigned for testing purposes


# TARGET VAR

# Intent to extend contract
dsContract$Extend <- sample(0:1,nRows, replace = TRUE) # randomly assigned for testing purposes

# Justification
dsContract$Extend <- sample(0:4,nRows, replace = TRUE) # randomly assigned for testing purposes

# Remove all unused variables from test dataset
dsContract <- dplyr::select(dsContract,PsychOwn,UseFreq,MinLength,Age,HrPWork,Neighborhood,dGender,IntentContract)

# IF SWAPFIETS CONTRACT
# Type of Swapfiets (Control)
dsContract$Bike <- sample(1:3,nRows, replace = TRUE) # randomly assigned for testing purposes

# ID var types
numVars <- c()
catVars <- c()
# Create set of respondents with contract
dsContract <- dplyr::filter(dsAll, dsAll$Contract == TRUE)

# Remove NaN entries
# Find rows/columns of datasets
nRows <- nrow(dsContract) # num of observations
nCols <- ncol(dsContract) # num of vars
cat("Number of responses removed from customer dataset: ",initialObsContract-nRows,"\n")



# IF NO SWAPFIETS CONTRACT

# Numerical vars
numVars2 <- c()
catVars2 <- c()


# Create set of respondents without contract
dsNoContract <- dplyr::filter(dsAll, dsAll$Contract == FALSE)

# Remove respondents with NaN entries
dsContract <- na.omit(dsContract)

# Save summary of non-normalized data
stargazer::stargazer(dsContract,
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

ctrlVars <- dplyr::select(dsContract,Age,HrPWork,Neighborhood,dGender)
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
        marker = list(size = 3))%>%
  layout(title = 'Survey Data After PCA')
uniqueCoords3D <- sum(!duplicated(newCoords[,1:3]))



# TROUBLE SHOOTING

# Checking for unique points in any combination of dimensionals for the PCA result always results in 175, can be indicating issue
uniqueCoords<-plyr::count(newCoords[,1])
cat("Number of Points Not Displayed Due to Overlap: ",nRows - nrow(uniqueCoords))

# Explore original data to check for possible causes of issues
plot_ly(dsContract,x = ~Age, y = ~HrPWork, z = ~dGender,
        marker = list(size = 3))%>%
  layout(title = 'Original Survey Data, Not Accounting For Neighborhood')
reducedDF <- dsContract[c("Age","HrPWork","dGender")]
unique_OG <- plyr::count(reducedDF)



# FAMD




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

# Use K-Means clustering for k = 2
kMeansResult <- kmeans(newCoords, centers = 2, nstart = 25)

# Plot color-coded result
newCoords$Cluster_kmeans <- as.factor(kMeansResult$cluster)
plotly::plot_ly(newCoords,x = ~PC1, y = ~PC2, z = ~PC3,
                color = ~Cluster_kmeans,
                marker = list(size = 3)) %>%
  layout(title = 'K-Means Clustering')

# Add cluster assignment to original dataset
dsContract$Cluster <- factor(newCoords$Cluster_kmeans)


# Plot explanatory vars separately for each cluster
# Plotly is known to have issues plotting within loops. Plotted without loop due to low amount of clusters
# See https://github.com/ropensci/plotly/issues/273 for solution

dsTemp <- subset(dsContract, Cluster == 1)
plotly::plot_ly(dsTemp,x = ~PsychOwn, y = ~UseFreq, z = ~MinLength,
                size = ~IntentContract,
                marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(1, 20))%>%
  layout(title = 'Survey Data, K-Means Cluster 1')

dsTemp <- subset(dsContract, Cluster == 2)
plotly::plot_ly(dsTemp,x = ~PsychOwn, y = ~UseFreq, z = ~MinLength,
                size = ~IntentContract,
                marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(1, 20))%>%
  layout(title = 'Survey Data, K-Means Cluster 2')



# Alternative: DBSCAN Clustering

# Determine optimal epsilon
dbscan::kNNdistplot(newCoords[,1:3], k = 5) #knee occurs around eps = 1
# Run dbscan
dbscanResult <- fpc::dbscan(newCoords[,1:3], eps = 0.8, MinPts = 5)
# Assign clusters
newCoords$Cluster_dbscan <- factor(dbscanResult$cluster)
# Plot
plotly::plot_ly(newCoords,x = ~PC1, y = ~PC2, z = ~PC3,
                color = ~Cluster_dbscan,
                marker = list(size = 3))%>%
  layout(title = 'DBSCAN Clustering')
dsContract$Cluster_dbscan <- factor(newCoords$Cluster_dbscan)



# Alternative: Cluster using the planes formed in the 3D PCA result (usually 2-4)
# requires ransac?
# may need to repurpose packages meant for computer vision feature detection



#---------------------------------------------------
#
#                   4. Analysis
#
#---------------------------------------------------





#---------------------------------------------------
#
#                 5. Prediction
#
#---------------------------------------------------