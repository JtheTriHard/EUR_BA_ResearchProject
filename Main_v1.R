# Test Version

# Install necessary packages
#install.packages("ggplot2",dependencies = TRUE)
#install.packages("Hmisc",dependencies = TRUE)
#install.packages("psych",dependencies = TRUE)
#install.packages("stargazer", dependencies = TRUE)
#install.packages("lm.beta", dependencies = TRUE )

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

# Age

# Gender

# Employment

# Location

# Willingness to engage in bike-sharing


#---------------------------------------------------
#
#             2. Dimension Reduction
#
#---------------------------------------------------