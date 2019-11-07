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

# Load data
dsBikeContract <- read.csv(file="~/Rcode/EUR/BA Final Project/EUR_BA_ResearchProject/Data/BikeSharingContracts.csv",stringsAsFactors=FALSE)
nInitialObs <- nrow(dsBikeContract)



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
# observation 99 is a significant outlier. Age is very similar to PC1 and neighborhood (random) is very similar to PC2
# TO DO: Figure out how to display var lines on 3D plot?

# WARNING: The fourth component accounts for a significant portion of the variance (~20%)
# TO DO: Test other methods for determining if PCA is appropriate
# Continue for the sake of practicing the concept, but do not rely on for accurate analysis

# Plot on 2D plot
newCoords <- as.data.frame(pcaSummary$x)
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

# Use K-Means clustering with a convergence condition of



# Check for most effective number of clusters



# Plot color-coded result



#---------------------------------------------------
#
#                   4. Analysis
#
#---------------------------------------------------