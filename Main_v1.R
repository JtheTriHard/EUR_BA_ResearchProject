# Author: JTheTriHard

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

set.seed(123)

# Example dataset used for initial testing
# dsTest <- read.csv(file=
#                      "~/Rcode/EUR/BA Final Project/EUR_BA_ResearchProject/Data/BikeSharingContracts.csv",stringsAsFactors=FALSE) 
# Qualtrics raw survey data
dsAll <- read.csv(file=
                    "~/Rcode/EUR/Adjusted BA Project/Data/SurveyResults3.csv",stringsAsFactors=FALSE)

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

# Change dependent vars to 0 1
dsAll <- dsAll %>%
  mutate(cExtend = ifelse(cExtend == "No",0,1))
dsAll$cExtend <- as.factor(dsAll$cExtend)
dsAll <- dsAll %>%
  mutate(nSign = ifelse(nSign == "No",0,1))
dsAll$nSign <- as.factor(dsAll$nSign)

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
# Note: Due to the space limitations and requirements of the project, this data will not be used further.
dsNoContract <- dsAll[dsAll$Contract == "No",]
nVars <- c("Gender","Age","Work","Home","Education","nBikeUsed","Env","nPsych","nSign")
dsNoContract <- dsNoContract[,nVars]
# Replace all nPsych NA w/ column mean
temp <- mean(dsNoContract$nPsych,na.rm = TRUE)
dsNoContract <- mutate(dsNoContract, nPsych = ifelse(is.na(nPsych) == TRUE, temp, nPsych))
remove(temp)
# Remove empty inherited factor levels
dsNoContract <- droplevels(dsNoContract)
sapply(dsNoContract,levels)

# Univariate analysis for numerical data
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

# Univariate analysis of categorical data for dsContract

# Gender
cnt <- table(dsContract$Gender)
prop <- cnt/sum(cnt)
tbl <- t(rbind(cnt,prop))
colnames(tbl) <- c("Count","Frequency")
stargazer::stargazer(tbl,
                     align = TRUE ,
                     digits=3,
                     type = "html",
                     out = "~/Rcode/EUR/Adjusted BA Project/Results/Gender_Tbl.doc")
catVarTbl <- tbl
# Work
cnt <- table(dsContract$Work)
prop <- cnt/sum(cnt)
tbl <- t(rbind(cnt,prop))
colnames(tbl) <- c("Count","Frequency")
stargazer::stargazer(tbl,
                     align = TRUE ,
                     digits=3,
                     type = "html",
                     out = "~/Rcode/EUR/Adjusted BA Project/Results/Work_Tbl.doc")
catVarTbl <- rbind(catVarTbl,tbl)
# Home
cnt <- table(dsContract$Home)
prop <- cnt/sum(cnt)
tbl <- t(rbind(cnt,prop))
colnames(tbl) <- c("Count","Frequency")
stargazer::stargazer(tbl,
                     align = TRUE,
                     digits=3,
                     type = "html",
                     out = "~/Rcode/EUR/Adjusted BA Project/Results/Home_Tbl.doc")
catVarTbl <- rbind(catVarTbl,tbl)

# Education
cnt <- table(dsContract$Education)
prop <- cnt/sum(cnt)
tbl <- t(rbind(cnt,prop))
colnames(tbl) <- c("Count","Frequency")
stargazer::stargazer(tbl,
                     align = TRUE,
                     digits=3,
                     type = "html",
                     out = "~/Rcode/EUR/Adjusted BA Project/Results/Education_Tbl.doc")
catVarTbl <- rbind(catVarTbl,tbl)

# cType
cnt <- table(dsContract$cType)
prop <- cnt/sum(cnt)
tbl <- t(rbind(cnt,prop))
colnames(tbl) <- c("Count","Frequency")
stargazer::stargazer(tbl,
                     align = TRUE,
                     digits=3,
                     type = "html",
                     out = "~/Rcode/EUR/Adjusted BA Project/Results/cType_Tbl.doc")
catVarTbl <- rbind(catVarTbl,tbl)

# cContractLength
cnt <- table(dsContract$cContractLength)
prop <- cnt/sum(cnt)
tbl <- t(rbind(cnt,prop))
colnames(tbl) <- c("Count","Frequency")
stargazer::stargazer(tbl,
                     align = TRUE,
                     digits=3,
                     type = "html",
                     out = "~/Rcode/EUR/Adjusted BA Project/Results/Length_Tbl.doc")
catVarTbl <- rbind(catVarTbl,tbl)

# cExtend
cnt <- table(dsContract$cExtend)
prop <- cnt/sum(cnt)
tbl <- t(rbind(cnt,prop))
colnames(tbl) <- c("Count","Frequency")
rownames(tbl) <- c("No","Yes")
stargazer::stargazer(tbl,
                     align = TRUE,
                     digits=3,
                     type = "html",
                     out = "~/Rcode/EUR/Adjusted BA Project/Results/cExtend_Tbl.doc")
catVarTbl <- rbind(catVarTbl,tbl)

#---------------------------------------------------
#
#             2. Dimension Reduction
#
#---------------------------------------------------

# FAMD
cCtrlVars <- dplyr::select(dsContract,Gender,Age,Work,Home,Education,cType)
cFAMDResults <- FactoMineR::FAMD(cCtrlVars, graph=FALSE)
factoextra::fviz_screeplot(cFAMDResults)
print(factoextra::get_eigenvalue(cFAMDResults))

# Plot contributions of vars to component
# factoextra::fviz_contrib(cFAMDResults, "var", axes = 1)

# We find the first three vars only account about 50% of the variance
# Continue reduction to 3D for sake of visualization but refrain from using results for analysis if limiting to 3D

# Reduce dimensions and retrieve new coords
cFAMDResults.3D <- FactoMineR::FAMD(cCtrlVars,ncp = 3, graph=FALSE)
cCoords <- factoextra::get_famd_ind(cFAMDResults.3D)
cCoords <- as.data.frame(cCoords$coord)
plotly::plot_ly(cCoords,x = ~Dim.1, y = ~Dim.2, z = ~Dim.3,marker = list(size = 3)) %>% 
  plotly::layout(title = "Survey Data After FAMD for 3 Most Significant Dim")

# Check FAMD for all vars except target
cFAMDResults.All <- FactoMineR::FAMD(dsContract[,1:10], graph=FALSE)
print(factoextra::get_eigenvalue(cFAMDResults.All))



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
# extract wss
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


# Alternative: DBSCAN 
# Also uses FAMD results

# Determine optimal epsilon
dbscan::kNNdistplot(cCoords[,1:3], k = 5) #knee occurs around eps = 1.5
# Run dbscan
dbscanResult <- fpc::dbscan(cCoords[,1:3], eps = 1.5, MinPts = 5)
# Assign clusters
cCoords$Cluster_dbscan <- factor(dbscanResult$cluster)
# Plot
plotly::plot_ly(cCoords,x = ~Dim.1, y = ~Dim.2, z = ~Dim.3,
                color = ~Cluster_dbscan,
                marker = list(size = 3))%>%
  plotly::layout(title = 'DBSCAN Clustering')
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

kProtoResult <- clustMixType::kproto(cCtrlVars, k=8)
cCoords$Cluster_proto_ctrl <- factor(kProtoResult$cluster)

# Plot in 3D using dim reduction results for sake of visualization
plotly::plot_ly(cCoords,x = ~Dim.1, y = ~Dim.2, z = ~Dim.3,
                color = ~Cluster_proto_ctrl,
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

kProtoResult2 <- clustMixType::kproto(cCombinedVars, k=8)
cCoords$Cluster_proto_all <- factor(kProtoResult2$cluster)
# Plot in 3D using dim reduction results for sake of visualization
plotly::plot_ly(cCoords,x = ~Dim.1, y = ~Dim.2, z = ~Dim.3,
                color = ~Cluster_proto_all,
                marker = list(size = 3))%>%
  plotly::layout(title = 'K Prototype Clustering: All Vars')



#---------------------------------------------------
#
#             4. Statistical Analysis
#
#---------------------------------------------------

# The following uses the original data (no FAMD or clustering).
# If the addition of data results in reasonable clustering
# and there are a reasonable amount of points in each cluster, 
# then the following sections can be adjusted to be performed within each cluster.
# Ctrl: Control model; Exp: Explanatory model (what we are researching); All: Combined model

# Check correlation between vars
sapply(dsContract,class)
cCorResults <- polycor::hetcor(dsContract[,1:11], std.err = TRUE) 
cCorMtx <- cCorResults$correlations
corrplot::corrplot(cCorMtx, type="upper", col=c("black"), method="number", cl.pos="n", tl.col="black")

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

# Test the overall effect of Home, Length, Education (as at least one of their levels is significant)
cHomeTest <- aod::wald.test(b = coef(cRslt.Log.All), Sigma = vcov(cRslt.Log.All), Terms = 6:7)
cHomeTest <- as.data.frame(cHomeTest$result)

cLengthTest <- aod::wald.test(b = coef(cRslt.Log.All), Sigma = vcov(cRslt.Log.All), Terms = 12:14)
cLengthTest <- as.data.frame(cLengthTest$result)

EducationTest <- aod::wald.test(b = coef(cRslt.Log.All), Sigma = vcov(cRslt.Log.All), Terms = 8:9)
EducationTest <- as.data.frame(EducationTest$result)

allWald <- t(cbind(cHomeTest,cLengthTest,EducationTest))
row.names(allWald) <- c("cHome","cLength","Education")
stargazer::stargazer(allWald,
                     align = TRUE ,
                     digits=3,
                     type = "html",
                     out = "~/Rcode/EUR/Adjusted BA Project/Results/allWald.doc")
# Neither are significant (p > 0.1)

# Variable Significance Summary:
# Control: None
# Explanatory: cSwapUsed @ p < 0.01, Env @ < 0.01
# All: Env @ < 0.05, cSwapUsed @ < 0.05
# Log Likelihoods reveal that the models rank from best to worst as: All > Exp > Ctrl

# LRT Hypothesis Testing for Comparison of Models:

# Ctrl vs All (Addition of explanatory vars)
anova(cRslt.Log.Ctrl,cRslt.Log.All,test="LRT")
# Significant at p < 0.01

# Exp vs All (Addition of control vars)
anova(cRslt.Log.Exp,cRslt.Log.All,test="LRT")
# Significant at p < 0.1


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
cMeasures.Log <- measurePerf(cNewLabels.Log,cTrueLabels,0.5)
cMeasures.Forest <- measurePerf(cNewLabels.Forest,cTrueLabels,0.5)
cMeasures.Gbm <- measurePerf(cNewLabels.Gbm,cTrueLabels,0.5)
cMeasures.NB <- measurePerf(cNewLabels.NB,cTrueLabels,0.5)
cMeasures.All <- rbind(cMeasures.Log,cMeasures.Forest,cMeasures.Gbm,cMeasures.NB)
rownames(cMeasures.All) <- c("Log","Forest","GBM","Naive Bayes")
stargazer::stargazer(cMeasures.All,
                     title = "Performance Measures for Classifiers",
                     align = TRUE ,
                     digits=3,
                     type = "html",
                     out = "~/Rcode/EUR/Adjusted BA Project/Results/Performance.doc")

library(ROCR)
# Calculate data for ROC cruves
cPrediction.Log <- prediction(cNewLabels.Log, cTrueLabels)
cPrediction.Forest <- prediction(cNewLabels.Forest, cTrueLabels)
cPrediction.Gbm <- prediction(cNewLabels.Gbm, cTrueLabels)
cPrediction.NB <- prediction(cNewLabels.NB, cTrueLabels)

cPerf.Log <- performance(cPrediction.Log, measure = "tpr", x.measure = "fpr")
cPerf.Forest <- performance(cPrediction.Forest, measure = "tpr", x.measure = "fpr")
cPerf.Gbm <- performance(cPrediction.Gbm, measure = "tpr", x.measure = "fpr")
cPerf.NB <- performance(cPrediction.NB, measure = "tpr", x.measure = "fpr")

# Plot ROCR curves
png("~/Rcode/EUR/Adjusted BA Project/Results/PlotROCR.png")
plot(cPerf.Log, lty=1, lwd = 2.0, col = "red", main = "Classifier Performance Curves for Contract Data")
plot(cPerf.Forest, lty=1, lwd=2.0, col="blue", add = TRUE)
plot(cPerf.Gbm, lty=1, lwd=2.0, col="green", add = TRUE)
plot(cPerf.NB, lty=1, lwd=2.0, col="purple", add = TRUE)
abline(a=0, b=1, lty=3, lwd=1.5)
mtext("Leave-One-Out Training", side = 3)
legend(0.6, 0.5, c("Logit Regression",
                   "Random Forest",
                   "Gradient Boosting Machine",
                   "Naive Bayesian Classifier"),
       col = c("red","blue","green","purple"),
       lwd = 3)
dev.off()

# Find AUCs
auc.Log <- performance(cPrediction.Log,measure = "auc")@y.values[[1]]
auc.Forest <- performance(cPrediction.Forest,measure = "auc")@y.values[[1]]
auc.Gbm <- performance(cPrediction.Gbm,measure = "auc")@y.values[[1]]
auc.NB <- performance(cPrediction.NB,measure = "auc")@y.values[[1]]
auc.all <- cbind(auc.Log, auc.Forest, auc.Gbm, auc.NB)
rownames(auc.all) <- c("AUC")
colnames(auc.all) <- c("Log","Forest","GBM","Naive Bayes")
stargazer::stargazer(auc.all,
                     title = "AUC Values for Classifiers",
                     align = TRUE ,
                     digits=3,
                     type = "html",
                     out = "~/Rcode/EUR/Adjusted BA Project/Results/AUC.doc")


# From the previous results, the log and random forest perform best for all vars.
# Therefore we will choose these classifiers for predictions for non-customers

# Recreate contract dataframe but with var names matching those of the no contract data
dsTrain <- dsContract %>%
  select(cSwapUsed,Env,cPsych,cExtend)
names(dsTrain) <- c("nBikeUsed","Env","nPsych","nSign")

# Define adjusted explanatory model
mdl.Exp <- nSign ~.
nTrueLabels <- dsNoContract$nSign

# Fit random forest using contract data
nRslt.Log <- glm(mdl.Exp, data = dsTrain, binomial(link = "logit"))
nRslt.Forest<- randomForest::randomForest(mdl.Exp, data = dsTrain, 
                                          ntree = 200, mtry = mA, importance = TRUE)

# Calculate predicted labels
nNewLabels <- predict(nRslt.Forest, dsNoContract, type = "prob")[,2]
nNewLabels.Log <- predict(nRslt.Log, dsNoContract, type = "response")

# Calculate performance measures
nMeasures.Forest <- measurePerf(nNewLabels,nTrueLabels,0.5)
nMeasures.Log <- measurePerf(nNewLabels.Log,nTrueLabels,0.5)

# Create ROC plot
nPrediction.Forest <- prediction(nNewLabels, nTrueLabels)
nPrediction.Log <- prediction(nNewLabels.Log, nTrueLabels)

nPerf.Forest <- performance(nPrediction.Forest, measure = "tpr", x.measure = "fpr")
nPerf.Log <- performance(nPrediction.Log, measure = "tpr", x.measure = "fpr")

png("~/Rcode/EUR/Adjusted BA Project/Results/NoContractROC.png")
plot(nPerf.Log, lty=1, lwd = 2.0, col = "red", main = "ROC for No Contract Data")
plot(nPerf.Forest, lty=1, lwd=2.0, col="blue", add = TRUE)
abline(a=0, b=1, lty=3, lwd=1.5)
mtext("Contract Data as Training Data", side = 3)
legend(0.6, 0.5, c("Logistic Regression","Random Forest"),
       col = c("red","blue"),
       lwd = 3)
dev.off()

# Find AUC
nAUC.Log <- performance(nPrediction.Log,measure = "auc")@y.values[[1]]
nAUC.Forest <- performance(nPrediction.Forest,measure = "auc")@y.values[[1]]
nAUC.all <- cbind(nAUC.Log, nAUC.Forest)
rownames(nAUC.all) <- c("AUC")
colnames(nAUC.all) <- c("Log","Forest")
stargazer::stargazer(nAUC.all,
                     title = "AUC Values for Classifiers for No Contract",
                     align = TRUE ,
                     digits=3,
                     type = "html",
                     out = "~/Rcode/EUR/Adjusted BA Project/Results/nAUC.doc")

#---------------------------------------------------
#
#                 6. Limitations
#
#---------------------------------------------------

# Insufficient sample size

# Assumes conditions met for logistic regression

# Significance often occurs at 10% instead of the preferred 5%