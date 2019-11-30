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
#install.packages("clustMixType", dependencies = TRUE)
#install.packages("corrgram", dependencies = TRUE)
#install.packages("corrplot", dependencies = TRUE)
#install.packages("polycor",dependencies = TRUE)
#install.packages("aod", dependencies = TRUE)
#install.packages("e1071", dependencies = TRUE)


#install.packages("Hmisc",dependencies = TRUE)
#install.packages("lm.beta", dependencies = TRUE )
#install.packages("devtools", dependencies = TRUE)

set.seed(123)

# Example dataset used for initial testing
# dsTest <- read.csv(file=
#                      "~/Rcode/EUR/BA Final Project/EUR_BA_ResearchProject/Data/BikeSharingContracts.csv",stringsAsFactors=FALSE) 
# Qualtrics raw survey data
dsAll <- read.csv(file=
                    "~/Rcode/EUR/Adjusted BA Project/Data/SurveyResults2.csv",stringsAsFactors=FALSE)

# Remove rows not corresponding to respondents, do not re-run
dsAll <- dsAll[-c(1:2),]


#---------------------------------------------------
#
#             1. Data Preparation
#
#---------------------------------------------------

# TO DO: Basic var exploration (tables, visualization, etc.)

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
dsAll <- dsAll[is.na(dsAll$Age) == FALSE,]
curYr = 2019
dsAll <- mutate(dsAll, Age = ifelse(Age > 1900, curYr - Age, Age)) # accounts for respondents that put in age instead of birth yr

# Convert hrs worked to employment
dsAll <- 
  mutate(dsAll, Work = case_when(is.na(Work) == TRUE ~ "Unemployed", #check
                                 Work >= 30 ~ "Full",
                                 Work < 1 ~ "Unemployed",
                                 TRUE ~ "Part"))
dsAll$Work <- as.factor(dsAll$Work)

# Change dependent var to 0 1
dsAll <- dsAll %>%
  mutate(cExtend = ifelse(cExtend == "No",0,1))
dsAll$cExtend <- as.factor(dsAll$cExtend)

# Create df of contract respondents
dsContract <- dsAll[dsAll$Contract == "Yes",]
cVars <- c("Gender","Age","Work","Home","Education","cType","cSwapUsed","cContractLength","Env","cPsych","cExtend")
dsContract <- dsContract[,cVars]
# Replace all cSwapUsed NA w/ column mean
temp <- mean(dsContract$cSwapUsed,na.rm = TRUE)
dsContract <- mutate(dsContract, cSwapUsed = ifelse(is.na(cSwapUsed) == TRUE, temp, cSwapUsed))
# Remove empty inherited factor levels
dsContract <- droplevels(dsContract)
sapply(dsContract,levels)


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

# WARNING: Using the same amount of dim as vars does not result in cumulative variance of 100%

# FAMD
cCtrlVars <- dplyr::select(dsContract,Gender,Age,Work,Home,Education,cType)
cFAMDResults <- FactoMineR::FAMD(cCtrlVars,ncp = 11, graph=TRUE)
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

# Check FAMD for all vars except target
cFAMDResults.All <- FactoMineR::FAMD(dsContract[,1:10],ncp = 10, graph=TRUE)
print(factoextra::get_eigenvalue(cFAMDResults.All))

# Determine if any vars do not contribute significantly to 
# the components that explain the majority of the variance.
# This can be used in conjunction with the regression analysis to determine vars 
# that do not have a significant effect on the explanatory/target vars.



#---------------------------------------------------
#
#                  3. Clustering
#
#---------------------------------------------------

# WARNING: Go through after obtaining more data and check n of clusters
# TO DO: Change point shape in 3D plots based on cExtend

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

# Use K-Means clustering for k = 
kMeansResult <- kmeans(cCoords, centers = 6, nstart = 25)

# Plot color-coded result
cCoords$Cluster_kmeans <- as.factor(kMeansResult$cluster)
plotly::plot_ly(cCoords,x = ~Dim.1, y = ~Dim.2, z = ~Dim.3,
                color = ~Cluster_kmeans,
                marker = list(size = 3)) %>%
  plotly::layout(title = 'K-Means Clustering')

# Add cluster assignment to original dataset
dsContract$Cluster.Kmeans <- factor(cCoords$Cluster_kmeans)



# Alternative: DBSCAN 

# Determine optimal epsilon
dbscan::kNNdistplot(cCoords[,1:3], k = 5) #knee occurs around eps = 1
# Run dbscan
dbscanResult <- fpc::dbscan(cCoords[,1:3], eps = 1.8, MinPts = 5)
# Assign clusters
cCoords$Cluster_dbscan <- factor(dbscanResult$cluster)
# Plot
plotly::plot_ly(cCoords,x = ~Dim.1, y = ~Dim.2, z = ~Dim.3,
                color = ~Cluster_dbscan,
                marker = list(size = 3))%>%
  plotly::layout(title = 'DBSCAN Clustering')
dsContract$Cluster.DBSCAN <- factor(cCoords$Cluster_dbscan)
# Detects no clusters aside from outliers. Supports decision of control variables


# Alternative: K-PROTOTYPE
# This uses the original ctrl vars without requiring dim reduction

# Version 1:  Cluster based on ctrl vars ONLY
k.max <- 15
# Elbow method for optimal k
wss.Proto <- sapply(1:k.max, 
              function(k){clustMixType::kproto(cCtrlVars, k)$tot.withinss})
plot(1:k.max, wss.Proto,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

kProtoResult <- clustMixType::kproto(cCtrlVars, k=4)
cCoords$Cluster_proto <- factor(kProtoResult$cluster)

# Plot in 3D using dim reduction results for sake of visualization
plotly::plot_ly(cCoords,x = ~Dim.1, y = ~Dim.2, z = ~Dim.3,
                color = ~Cluster_proto,
                marker = list(size = 3))%>%
  plotly::layout(title = 'K Prototype Clustering: Control Vars')


# Version 2: Cluster based on all vars
cCombinedVars <- dsContract[,1:10]
wss.Proto <- sapply(1:k.max, 
                    function(k){clustMixType::kproto(cCombinedVars, k)$tot.withinss})
plot(1:k.max, wss.Proto,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

kProtoResult2 <- clustMixType::kproto(cCombinedVars, k=5)
cCoords$Cluster_proto2 <- factor(kProtoResult2$cluster)
# Plot in 3D using dim reduction results for sake of visualization
plotly::plot_ly(cCoords,x = ~Dim.1, y = ~Dim.2, z = ~Dim.3,
                color = ~Cluster_proto2,
                marker = list(size = 3))%>%
  plotly::layout(title = 'K Prototype Clustering: All Vars')



# Plot explanatory vars separately for each cluster
# Plotly is known to have issues plotting within loops. Plotted without loop due to low amount of clusters
# See https://github.com/ropensci/plotly/issues/273 for solution

# dsTemp <- subset(dsContract, Cluster.Kmeans == 1)
# plotly::plot_ly(dsTemp,x = ~PsychOwn, y = ~UseFreq, z = ~MinLength,
#                 size = ~IntentContract,
#                 marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(1, 20))%>%
#   layout(title = 'Survey Data, K-Means Cluster 1')
# 
# dsTemp <- subset(dsContract, Cluster.Kmeans == 2)
# plotly::plot_ly(dsTemp,x = ~PsychOwn, y = ~UseFreq, z = ~MinLength,
#                 size = ~IntentContract,
#                 marker = list(symbol = 'circle', sizemode = 'diameter'), sizes = c(1, 20))%>%
#   layout(title = 'Survey Data, K-Means Cluster 2')



#---------------------------------------------------
#
#             4. Statistical Analysis
#
#---------------------------------------------------

# The following uses the original data (no FAMD, clustering).
# If the addition of data results in reasonable clustering
# and there are a reasonable amount of points in each cluster, 
# then the following sections will be adjusted to be performed within each cluster.
# Ctrl: Control model; Exp: Explanatory model (what we are researching); All: Combined model

# Check correlation between vars
sapply(dsContract,class)
cCorResults <- polycor::hetcor(dsContract[,1:11], std.err = TRUE) #currently does not address cExtend due to no "No" occurring
cCorMtx <- cCorResults$correlations
corrplot::corrplot(cCorMtx, type="upper")

# Define models
cMdl.Ctrl <- cExtend ~ Gender + Age + Work + Home + Education + cType
cMdl.Exp <- cExtend ~ cSwapUsed + cContractLength + Env + cPsych
cMdl.All <- cExtend ~ Gender + Age + Work + Home + Education + cType + cSwapUsed + cContractLength + Env + cPsych

# Fit logit model
cRslt.Log.Ctrl <- glm(cMdl.Ctrl, data=dsContract, binomial(link="logit"))
cRslt.Log.Exp <- glm(cMdl.Exp, data=dsContract, binomial(link="logit"))
cRslt.Log.All <- glm(cMdl.All, data=dsContract, binomial(link="logit"))

# Generate table with summaries of models
stargazer::stargazer(cRslt.Log.Ctrl,cRslt.Log.Exp,cRslt.Log.All,
                     #covariate.labels = c("")
                     intercept.bottom = FALSE,
                     align = TRUE,
                     report=('vc*p'),
                     model.numbers = FALSE,
                     column.labels = c("Control","Explanatory","Complete"),
                     digits=3,
                     type = "html",
                     out = "~/Rcode/EUR/Adjusted BA Project/Results/sumAllMdls.doc")

# Test the overall effect of Home, Length (as at least one of their levels is significant)
cHomeTest <- aod::wald.test(b = coef(cRslt.Log.All), Sigma = vcov(cRslt.Log.All), Terms = 6:7)
cLengthTest <- aod::wald.test(b = coef(cRslt.Log.All), Sigma = vcov(cRslt.Log.All), Terms = 12:14)
stargazer::stargazer(cHomeTest$result,
                     title = "Wald Test for Home",
                     align = TRUE ,
                     digits=3,
                     type = "html",
                     out = "~/Rcode/EUR/Adjusted BA Project/Results/HomeTest.doc")
stargazer::stargazer(cLengthTest$result,
                     title = "Wald Test for Contract Length",
                     align = TRUE ,
                     digits=3,
                     type = "html",
                     out = "~/Rcode/EUR/Adjusted BA Project/Results/LengthTest.doc")
# Neither are significant (p > 0.3)

# Variable Significance Summary:
# Control: None
# Explanatory: cSwapUsed @ p < 0.1, Env @ < 0.01, cPsych @ < 0.1
# All: Env @ < 0.05, cPsych @ < 0.1
# Log Likelihoods reveal that the models rank from best to worst as: All > Exp > Ctrl


# LRT Hypothesis Testing for Comparison of Models:

# Ctrl vs All (Addition of explanatory vars)
anova(cRslt.Log.Ctrl,cRslt.Log.All,test="LRT")
# Significant at p < 0.001

# Exp vs All (Addition of control vars)
anova(cRslt.Log.Exp,cRslt.Log.All,test="LRT")
# No significance


#---------------------------------------------------
#
#                 5. Prediction
#
#---------------------------------------------------


# Define function to calculate performance measures
measurePerf <- function(p,y,tau) {
  p <- as.numeric(p>tau)
  y <- factor(y, levels = c(0,1))
  p <- factor(p, levels = c(0,1))
  # Make a classification table
  tbl <- table(Predicted = p, Observed = y)
  # Identify classifications
  TP <- tbl[2,2]
  FN <- tbl[1,2]
  TN <- tbl[1,1]
  FP <- tbl[2,1]
  # Measure performance   
  perf <- c(
    Accuracy    = (TP+TN)/sum(tbl),
    Sensitivity = TP/(TP + FN),
    Specificity = TN/(FP + TN),
    Precision   = TP/(FP + TP)
  )
  return(perf)
}

# Training method: Leave-One-Out Cross Validation (Manual)
# Warning: Outliers can have significant effects
cNewLabels.Log <- NULL
cNewLabels.Tree <- NULL
cNewLabels.Forest <- NULL
cNewLabels.Gbm <- NULL
cNewLabels.NB <- NULL


cObs <- nrow(dsContract)
for (i in 1:cObs){
  
  #Select only a single value for testing
  dsContract.Test <- dsContract[i,]
  dsContract.Train <- dsContract[-i,]
  
  # Fit models using training data
  cRslt.Log <- glm(cMdl.All, data = dsContract.Train, binomial(link = "logit"))
  numA <- cRslt.Log$df.null - cRslt.Log$df.residual
  mA <- round(sqrt(numA))
  nTrees <- 500
  cRslt.Tree <- rpart::rpart(cMdl.All, data = dsContract.Train, method = "class",
                           parms = list(split = "information"))
  cRslt.Forest<- randomForest::randomForest(cMdl.All, data = dsContract.Train, 
                                            ntree = 200, mtry = mA, importance = TRUE)
  cRslt.Gbm <- gbm::gbm(cMdl.All, data = dsContract.Train, distribution = "multinomial", 
                        n.trees = nTrees, interaction.depth=2, shrinkage = 0.01,
                        bag.fraction = 0.5, n.minobsinnode = 10)
  cRslt.NB <- e1071::naiveBayes(cMdl.All, data = dsContract.Train)
  
  # Determine prediction labels for single test obs and store
  cNewLabels.Log[i] <- predict(cRslt.Log, dsContract.Test, type = "response")
  cNewLabels.Tree[i] <- predict(cRslt.Tree, dsContract.Test)[,2] # PROBLEMATIC RESULTS
  cNewLabels.Forest[i] <- predict(cRslt.Forest, dsContract.Test, type = "prob")[,2]
  cNewLabels.Gbm[i] <- predict(cRslt.Gbm, dsContract.Test, type="response",n.trees=nTrees)[,2,]
  cNewLabels.NB[i] <- predict(cRslt.NB, dsContract.Test)
}

# Calculate performance measures
cTrueLabels <- dsContract$cExtend
cNewLabels.NB <- cNewLabels.NB - 1 #adjustment needed as it predict factor level instead of label
cMeasures.Logit <- measurePerf(cNewLabels.Log,cTrueLabels,0.5)
cMeasures.Forest <- measurePerf(cNewLabels.Forest,cTrueLabels,0.5)
cMeasures.Gbm <- measurePerf(cNewLabels.Gbm,cTrueLabels,0.5)
cMeasures.NB <- measurePerf(cNewLabels.NB,cTrueLabels,0.5)

# Prepare data for ROC cruves


# Plot ROCR curves


# Find AUCs





# Adjust models for var differences btw Contract & NoContract


# Predict nSign and determine performance



#---------------------------------------------------
#
#                 6. Limitations
#
#---------------------------------------------------

# Insufficient sample size

# Assumes conditions met for logistic regression

# Significance often occurs at 10% instead of the expected 5%

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