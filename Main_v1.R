# Author: JTheTriHard
# Final version

# Install necessary packages
#install.packages("psych",dependencies = TRUE)
#install.packages("stargazer", dependencies = TRUE)
#install.packages("tidyverse",dependencies = TRUE)
#install.packages("plotly",dependencies = TRUE)
#install.packages("fpc", dependencies = TRUE)
#install.packages("dbscan", dependencies = TRUE)
#install.packages("plyr",dependencies = TRUE)
#install.packages(c("FactoMineR", "factoextra"), dependencies = TRUE)


#install.packages("Hmisc",dependencies = TRUE)
#install.packages("lm.beta", dependencies = TRUE )
#install.packages("devtools", dependencies = TRUE)

set.seed(123)

# Example dataset used for initial testing
# dsTest <- read.csv(file=
#                      "~/Rcode/EUR/BA Final Project/EUR_BA_ResearchProject/Data/BikeSharingContracts.csv",stringsAsFactors=FALSE) 
# Qualtrics raw survey data
dsAll <- read.csv(file=
                    "~/Rcode/EUR/Adjusted BA Project/Data/SurveyResults.csv",stringsAsFactors=FALSE)

# ~ first 20 results to be thrown out due to survey changes

# Remove rows not corresponding to respondents, do not re-run
dsAll <- dsAll[-c(1:2),]


#---------------------------------------------------
#
#             1. Data Preparation
#
#---------------------------------------------------

library(tidyverse)

# Rename survey vars
# A var prefix of c denotes contract, an n denotes no contract
data.table::setnames(dsAll, 
                     old = c('X2','Q20','X3','X4','X5','X6','Q17','X7','X8','X9','X10_1','Q21_1','X11',
                             'X12_1','X12_2','X12_3','X12_4','X12_5','X13_1','X13_2','X13_3','X13_4','X13_5',
                             'Q18_1','Q18_2','Q18_3','Q18_4','Q18_5','X14','Q19','X15','Q22'), 
                     new = c('Contract','nFamiliar','NL','Gender','Age','Student','Work','Home','Education','cType','cSwapUsed','nBikeUsed','cContractLength',
                             'Env1','Env2','Env3','Env4','Env5','cPsych1','cPsych2','cPsych3','cPsych4','cPsych5',
                             'nPsych1','nPsych2','nPsych3','nPsych4','nPsych5','cExtend','nSign','cReason','Email'))

# Convert likert values to numeric
# Issues will likely arise from the fact that some vars do not have entries covering the full 1-5 range
likertVars <- c('Env1','Env2','Env3','Env4','Env5','cPsych1','cPsych2','cPsych3','cPsych4','cPsych5',
                'nPsych1','nPsych2','nPsych3','nPsych4','nPsych5')
dsLikert <- dsAll[likertVars]
dsLikert <- dsLikert %>%
  mutate_all(
    .funs = ~ as.integer(recode(
      .x = .,
      "Strongly disagree" = 1,
      "Somewhat disagree" = 2,
      "Neither agree nor disagree" = 3,
      "Somewhat agree"    = 4,
      "Strongly agree"    = 5
    ))
  )

# Cronbach alpha analysis
a.Env <- psych::alpha(dsLikert[c("Env1","Env2","Env3","Env4","Env5")],check.keys=TRUE,cumulative = FALSE)
a.cPsych <- psych::alpha(dsLikert[c("cPsych1","cPsych2","cPsych3","cPsych4","cPsych5")],check.keys=TRUE,cumulative = FALSE)
a.nPsych <- psych::alpha(dsLikert[c("nPsych1","nPsych2","nPsych3","nPsych4","nPsych5")],check.keys=TRUE,cumulative = FALSE)
remove(dsLikert)

# Construct likert scores
dsAll$Env <- a.Env$scores
dsAll$cPsych <- a.cPsych$scores
dsAll$nPsych <- a.nPsych$scores

# Convert vars to proper type
dsAll$Work[9] <- "22.5" #Hard-coded adjustment for single incorrect input
toNum <-c("Age","Work","cSwapUsed","nBikeUsed")
toFact <- c("Contract","nFamiliar","NL","Gender","Student","Home","Education","cType","cExtend","nSign","cContractLength")
dsAll[toNum] <- lapply(dsAll[toNum], as.numeric)
dsAll[toFact] <- lapply(dsAll[toFact],as.factor)

# Remove respondents not aware of Swapfiets
dsAll <- dsAll[dsAll$nFamiliar != "No",]

# Remove respondents not living in the Netherlands
dsAll <- dsAll[dsAll$NL != "No",]

# Remove respondents with Gender = Other (Insignificant sample size)
dsAll <- dsAll[dsAll$Gender != "Other/prefer not to disclose",]

# Check n of respondents that are not students
table(dsAll$Student)
# Remove non-student respondents if not many
dsAll <- dsAll[dsAll$Student != "No",]

# Convert yr of birth to age
dsAll$Age[7] <- 25 #Hard-coded adjustment for single incorrect input. NOT WORKING
dsAll <- dsAll[is.na(dsAll$Age) == FALSE,]
curYr = 2019
dsAll <- mutate(dsAll, Age = ifelse(Age > 1900, curYr - Age, Age)) # accounts for respondents that put in age instead of birth yr

# Convert hrs worked to employment
dsAll <- 
  mutate(dsAll, Work = case_when(is.na(Work) == TRUE ~ "NA", #consider changing to Unemployed
                                 Work >= 30 ~ "Full",
                                 Work < 1 ~ "Unemployed",
                                 TRUE ~ "Part"))
dsAll$Work <- as.factor(dsAll$Work)

# Create df of contract respondents
dsContract <- dsAll[dsAll$Contract == "Yes",]
cVars <- c("Gender","Age","Work","Home","Education","cType","cSwapUsed","cContractLength","Env","cPsych","cExtend")
dsContract <- dsContract[,cVars]
# Replace all cSwapUsed NA w/ column mean
temp <- mean(dsContract$cSwapUsed,na.rm = TRUE)
dsContract <- mutate(dsContract, cSwapUsed = ifelse(is.na(cSwapUsed) == TRUE, temp, cSwapUsed))

# Create df of no contract respondents
dsNoContract <- dsAll[dsAll$Contract == "No",]
nVars <- c("Gender","Age","Work","Home","Education","nBikeUsed","Env","nPsych","nSign")
dsNoContract <- dsNoContract[,nVars]
# Replace all nPsych NA w/ column mean
temp <- mean(dsNoContract$nPsych,na.rm = TRUE)
dsNoContract <- mutate(dsNoContract, nPsych = ifelse(is.na(nPsych) == TRUE, temp, nPsych))
remove(temp)

# Save summary of numerical data
stargazer::stargazer(dsContract,
                     align = TRUE ,
                     digits=2,
                     type = "html",
                     out = "~/Rcode/EUR/Adjusted BA Project/Results/ContractTable.doc")

stargazer::stargazer(dsNoContract,
                     align = TRUE ,
                     digits=2,
                     type = "html",
                     out = "~/Rcode/EUR/Adjusted BA Project/Results/NoContractTable.doc")



#---------------------------------------------------
#
#             2. Dimension Reduction
#
#---------------------------------------------------

# FAMD
cCtrlVars <- dplyr::select(dsContract,Gender,Age,Work,Home,Education,cType)
cFAMDResults <- FactoMineR::FAMD(cCtrlVars,ncp = 7, graph=TRUE)
factoextra::fviz_screeplot(cFAMDResults)
print(factoextra::get_eigenvalue(cFAMDResults))

# Plot contributions of vars to each component
factoextra::fviz_contrib(cFAMDResults, "var", axes = 1)
factoextra::fviz_contrib(cFAMDResults, "var", axes = 2)
factoextra::fviz_contrib(cFAMDResults, "var", axes = 3)
factoextra::fviz_contrib(cFAMDResults, "var", axes = 4)
factoextra::fviz_contrib(cFAMDResults, "var", axes = 5)
factoextra::fviz_contrib(cFAMDResults, "var", axes = 6)

# We find the first three vars only account for 56% of the variance
# Continue reduction to 3D for sake of visualization but refrain from using results for analysis

# Reduce dimensions and retrieve new coords
cFAMDResults.3D <- FactoMineR::FAMD(cCtrlVars,ncp = 3, graph=FALSE)
cCoords <- factoextra::get_famd_ind(cFAMDResults.3D)
cCoords <- as.data.frame(cCoords$coord)
plotly::plot_ly(cCoords,x = ~Dim.1, y = ~Dim.2, z = ~Dim.3,marker = list(size = 3)) %>% 
  plotly::layout(title = "Survey Data After FAMD for 3 Most Significant Dim")



#---------------------------------------------------
#
#                  3. Clustering
#
#---------------------------------------------------



# K MEANS
# Only usable for FAMD coords. Categorical nature of original vars voids usefulness

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(cCoords, k, nstart = 20 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 10
k.values <- 1:10
# extract wss for 2-15 clusters
wss_values <- purrr::map_dbl(k.values, wss)
# Plot elbow method. No clear elbow but a choice of 5 seems appropriate
plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

# Use K-Means clustering for k = 5
kMeansResult <- kmeans(cCoords, centers = 5, nstart = 25)

# Plot color-coded result
cCoords$Cluster_kmeans <- as.factor(kMeansResult$cluster)
plotly::plot_ly(cCoords,x = ~Dim.1, y = ~Dim.2, z = ~Dim.3,
                color = ~Cluster_kmeans,
                marker = list(size = 3)) %>%
  plotly::layout(title = 'K-Means Clustering')

# Add cluster assignment to original dataset
dsContract$Cluster.Kmeans <- factor(cCoords$Cluster_kmeans)




# Alternative: DBSCAN Clustering
# WARNING: Does not work at the moment due to low amount of data points

# Determine optimal epsilon
dbscan::kNNdistplot(cCoords[,1:3], k = 2) #knee occurs around eps = 1
# Run dbscan
dbscanResult <- fpc::dbscan(cCoords[,1:3], eps = 0.8, MinPts = 2)
# Assign clusters
cCoords$Cluster_dbscan <- factor(dbscanResult$cluster)
# Plot
plotly::plot_ly(cCoords,x = ~Dim.1, y = ~Dim.2, z = ~Dim.3,
                color = ~Cluster_dbscan,
                marker = list(size = 3))%>%
  plotly::layout(title = 'DBSCAN Clustering')
dsContract$Cluster.DBSCAN <- factor(cCoords$Cluster_dbscan)





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

#---------------------------------------------------
#
#             4. Statistical Analysis
#
#---------------------------------------------------

# The following uses the original data (no FAMD, clustering)



#---------------------------------------------------
#
#                 5. Prediction
#
#---------------------------------------------------

# Training method: Leave-One-Out Cross Validation

# Prediction method:




#---------------------------------------------------
#
#                   6. Removed
#
#---------------------------------------------------

# Saving old methods in case. Anything past this will not run

cPCAResults <- prcomp(cCtrlVars,scale = TRUE)
cPCASummary <- summary(cPCAResults)

# Scree plot
barplot(cPCASummary$importance[2,],
        ylab = "Proportion of Variance")
barplot(cPCASummary$importance[3,],
        ylab = "Cumulative Proportion of Variance")
#cat("Proportion of Variance Not Accounted for: ",pcaSummary$importance[2,4])

# Biplot for first two components
biplot(pcaResults)
# TO DO: Figure out how to display var lines on 3D plot?

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