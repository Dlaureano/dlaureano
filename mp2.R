# Get Started on Mini-Proj2 Code
#install.packages("caTools")
library(caTools)

# Install and load mice package
#install.packages("mice")
library(mice)

#====================================================================
# Read data file
#====================================================================
miniproj2 = read.csv('MiniProject2.csv')
str(miniproj2)
head(miniproj2)

#====================================================================
# Explore existing target variable
#====================================================================
table(miniproj2$CreditRating)
# 0   1 1.5   2 2.5   3 
# 4  31   1  27   3 186 

which(is.na(miniproj2$CreditRating))
# CreditRating is missing (NA) in observations 95, 96, 238


#==============================================
#Multiple imputation: Impute missing values
#==============================================
set.seed(100)
miniproj2 = complete(mice(miniproj2))
sum(is.na(miniproj2))
summary(miniproj2)

#====================================================================
# Approach 2: Build new target variable
#==================================================================== 
# Build it in such a way to conserve the signal rate
# Think Events and Non-Events, like Poor Quality Care example seen in class

#miniproj2$LoanDefault <- 0
miniproj2$LoanDefault[miniproj2$CreditRating <= 2.5] <- 1 
miniproj2$LoanDefault[miniproj2$CreditRating > 2.5] <- 0
head(miniproj2)

# Explore new target variable
table(miniproj2$LoanDefault)
# 0   1 
# 189  66 

#====================================================================
# Removal of outliers
#====================================================================
rem_outliers <- function (data, col_names, col_ind) {
  
  vect_orig <- miniproj2[, col_ind]
  bp <- boxplot(vect_orig)
  bp_stat <- bp$stats[,1]
  lw <- bp_stat[1]
  hw <- bp_stat[5]
  
  vect_data <- data[, col_ind]
  res <- subset(data, vect_data >= lw & vect_data <= hw)#primary code that reduces #of obs. 
  #by removing outliers. Collect data in IQR area. 
  
  size <- nrow(res)
  col_name <- col_names[col_ind]
  prtln <- paste0(col_ind, "--", col_name, "--", size)
  print(prtln)
  
  return(res)
  
}

col_names <- names(miniproj2)
col_indexes <- c(3:18) #. Index of coloumns no user ID and sex
len <- length(col_indexes)

data <- miniproj2
for (i in 1:len) {#loop
  data <- rem_outliers(data, col_names, col_indexes[i]) #removes outliers and return same data
} # function is called again. 

Remove_indexes <- c(-5, -6, -13)
data1 = data[, Remove_indexes]


#=====================================================================
graphs
#=====================================================================

#col_indexes <- c(3:18)
#len <- length(col_indexes)

#data <- miniproj2
#for (i in 1:len) {
 # data <- rem_outliers(data, col_indexes[i])
  #print(nrow(data))
#}

#====================================================================
# Split train and test data sets
#====================================================================
set.seed(10)
spl = sample.split(data1$LoanDefault, SplitRatio = 0.7) #split data by ratio
CreditTrain = subset(data1,spl == TRUE) #split train
CreditTest = subset(data1, spl == FALSE) #split test

#====================================================================
# Modeling
#====================================================================
# Now proceed with modeling using CreditScore or LoanDefault as your new target variable
# Your code goes here...
# Build logistic regression model on train data set
#====================================================================
cor(CreditTrain[,3:15])
summary(CreditTrain)
# multiple-variable model
#CreditTest$MonthlyExpenses <- factor(CreditTest$MonthlyExpenses)
#CreditTest$MainHousehold <- factor(CreditTest$MainHousehold)
#CreditTest$FamilySize <- factor(CreditTest$FamilySize)

mod1Train = glm( LoanDefault ~ Age +FamilySize + MonthlyIncome + MonthlyExpenses +          
AvailableIncome + PerCapitaAvailableIncome + ParticipatesInCommunity + 
SocialFabric + TotalPaidAvg + LoanSizeAvg + UpfrontPaymentAvg + 
  LoanPeriodMonths + MonthlyPayment, 
            data = CreditTrain, family = "binomial" (link = "logit"), maxit =100)

summary(mod1Train)

mod1Train = glm( LoanDefault ~ FamilySize + MonthlyIncome + MonthlyExpenses +          
                   AvailableIncome + PerCapitaAvailableIncome + ParticipatesInCommunity + 
                   SocialFabric + TotalPaidAvg + LoanSizeAvg + UpfrontPaymentAvg + 
                   LoanPeriodMonths + MonthlyPayment, 
                 data = CreditTrain, family = "binomial" (link = "logit"), maxit =100)

summary(mod1Train)

mod1Train = glm( LoanDefault ~ FamilySize + MonthlyIncome + MonthlyExpenses +          
                   AvailableIncome + PerCapitaAvailableIncome + ParticipatesInCommunity + 
                   SocialFabric + TotalPaidAvg + LoanSizeAvg + UpfrontPaymentAvg + 
                   LoanPeriodMonths + MonthlyPayment, 
                 data = CreditTrain, family = "binomial" (link = "logit"), maxit =100)

summary(mod1Train)

mod1Train = glm( LoanDefault ~ FamilySize + MonthlyIncome + MonthlyExpenses +          
                   AvailableIncome + PerCapitaAvailableIncome + ParticipatesInCommunity + 
                   SocialFabric + TotalPaidAvg + LoanSizeAvg + UpfrontPaymentAvg + 
                   LoanPeriodMonths, 
                 data = CreditTrain, family = "binomial" (link = "logit"), maxit =100)

summary(mod1Train)

mod1Train = glm( LoanDefault ~ FamilySize + MonthlyIncome + MonthlyExpenses +          
                   AvailableIncome + PerCapitaAvailableIncome + ParticipatesInCommunity + 
                   SocialFabric + TotalPaidAvg + LoanSizeAvg + UpfrontPaymentAvg  
                   , 
                 data = CreditTrain, family = "binomial" (link = "logit"), maxit =100)

summary(mod1Train)


mod1Train = glm( LoanDefault ~ FamilySize + MonthlyIncome + MonthlyExpenses +          
                   AvailableIncome + PerCapitaAvailableIncome + 
                   SocialFabric + TotalPaidAvg + LoanSizeAvg + UpfrontPaymentAvg  
                 , 
                 data = CreditTrain, family = "binomial" (link = "logit"), maxit =100)

summary(mod1Train)


mod1Train = glm( LoanDefault ~ MonthlyIncome + MonthlyExpenses +          
                   AvailableIncome + PerCapitaAvailableIncome + 
                   SocialFabric + TotalPaidAvg + LoanSizeAvg + UpfrontPaymentAvg  
                 , 
                 data = CreditTrain, family = "binomial" (link = "logit"), maxit =100)

summary(mod1Train)

mod1Train = glm( LoanDefault ~ MonthlyIncome + MonthlyExpenses +          
                   AvailableIncome + 
                   SocialFabric + TotalPaidAvg + LoanSizeAvg + UpfrontPaymentAvg  
                 , 
                 data = CreditTrain, family = "binomial" (link = "logit"), maxit =100)

summary(mod1Train)

mod1Train = glm( LoanDefault ~ MonthlyIncome + MonthlyExpenses +          
                   AvailableIncome 
                    + TotalPaidAvg + LoanSizeAvg + UpfrontPaymentAvg  
                 , 
                 data = CreditTrain, family = "binomial" (link = "logit"), maxit =100)

summary(mod1Train)


mod1Train = glm( LoanDefault ~ MonthlyIncome +          
                   AvailableIncome 
                 + TotalPaidAvg + LoanSizeAvg + UpfrontPaymentAvg  
                 , 
                 data = CreditTrain, family = "binomial" (link = "logit"), maxit =100)

summary(mod1Train)

mod1Train = glm( LoanDefault ~          
                   AvailableIncome 
                 + TotalPaidAvg + LoanSizeAvg + UpfrontPaymentAvg  
                 , 
                 data = CreditTrain, family = "binomial" (link = "logit"), maxit =100)

summary(mod1Train)

mod1Train = glm( LoanDefault ~          
                   
                 TotalPaidAvg + LoanSizeAvg + UpfrontPaymentAvg  
                 , 
                 data = CreditTrain, family = "binomial" (link = "logit"), maxit =100)

summary(mod1Train)

mod1Train = glm( LoanDefault ~ LoanSizeAvg + UpfrontPaymentAvg  
                 , 
                 data = CreditTrain, family = "binomial" (link = "logit"), maxit =100)

summary(mod1Train)



#====================================================================
# Evaluate performance of model on train data set
#====================================================================
# Evaluating the Model
# The type="response" option tells R to output probabilities of the form P(Y = 1|X), 
# as opposed to other information such as the logit.
# Get predicted values on train data set
pred1 = predict(mod1Train, type = "response")
table(CreditTrain$LoanDefault, pred1 >= 0.5)

# Compute confusion matrix metrics on train data set
tbl1 = table(CreditTrain$LoanDefault, pred1 >= 0.5)
tbl1
# you can do it yourself..

#====================================================================
# test data set model
#====================================================================

mod2 = glm( LoanDefault ~ LoanSizeAvg + UpfrontPaymentAvg, 
            data = CreditTest, family = "binomial" (link = "logit"), maxit =100)
summary(mod2)

summary(CreditTest)


#====================================================================
# Evaluate performance of model on test data set - most important
#with Baseline Method
#====================================================================
pred2 = predict(mod2, type = "response")
table(CreditTest$LoanDefault, pred2 >= 0.5)

# Compute confusion matrix metrics on train data set
tbl2 = table(CreditTest$LoanDefault, pred2 >= 0.5)
tbl2
# you can do it yourself..
#======
#computes accuracy

tab3 <- table(CreditTest$LoanDefault, pred2)
tab3
(tab3[1]+tab3[4])/sum(tab3)


#==================
#train acc for tab4 (credit train)

tab4 <- table(CreditTrain$LoanDefault, pred1)
tab4
(tab4[1]+tab4[4])/sum(tab4)







