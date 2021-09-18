install.packages("caTools")
library(caTools)
install.packages("rio")
install.packages("openxlsx")
install.packages("xlsx")
library(openxlsx)
library(xlsx)


#=================
#load CSV File
Elantra = read.csv("Elantra.csv")
str(Elantra)
Elantra

#========
#saving to current working directory
write.csv(Elantra, "Elantra1.csv", row.names = TRUE)

#========
#

str(Elantra)

#--question 1A
#subset data
ElantraTrainingSet1 = subset(Elantra, Year == "2010" | Year == "2011" | Year == "2012" )
ElantraTestingSet1 = subset(Elantra, Year == "2013" | Year == "2014" )
#find anser for Ai)
ElentraReg = lm(ElantraSales ~ Unemployment + Queries + CPI.Energy + CPI.All, data = ElantraTrainingSet1)
summary(ElentraReg)
ElentraReg = lm(ElantraSales ~ Unemployment + Queries + CPI.All, data = ElantraTrainingSet1)
summary(ElentraReg)
ElentraReg = lm(ElantraSales ~ Unemployment + Queries, data = ElantraTrainingSet1)
summary(ElentraReg)
ElentraReg = lm(ElantraSales ~ Queries, data = ElantraTrainingSet1)
summary(ElentraReg)
plot(ElentraReg,pch = 16, col= "blue")
abline(ElentraReg)
#-- Question 1B which includes Month as an ind. variable
ElentraReg = lm(ElantraSales ~ Month + Unemployment + Queries + CPI.Energy + CPI.All, data = ElantraTrainingSet1)
summary(ElentraReg)
ElentraReg = lm(ElantraSales ~ Month + Unemployment + Queries + CPI.All, data = ElantraTrainingSet1)
summary(ElentraReg)
ElentraReg = lm(ElantraSales ~ Month + Unemployment + Queries, data = ElantraTrainingSet1)
summary(ElentraReg)
ElentraReg = lm(ElantraSales ~ Unemployment + Queries, data = ElantraTrainingSet1)
summary(ElentraReg)
ElentraReg = lm(ElantraSales ~ Queries, data = ElantraTrainingSet1)
summary(ElentraReg)
plot(ElentraReg,pch = 16, col= "blue")
abline(ElentraReg)

write.csv(Elantralc, "Elantralc.csv", row.names = TRUE) #saving doc and creating a new cvs file for Elantra

#--queston lc
Elantralc <-Elantra
str(Elantralc)
Elantralc

Elantralc$Month[Elantralc$Month == '1'] <- 'January'
Elantralc$Month[Elantralc$Month == '2'] <- 'February'
Elantralc$Month[Elantralc$Month == '3'] <- 'March'
Elantralc$Month[Elantralc$Month == '4'] <- 'April'
Elantralc$Month[Elantralc$Month == '5'] <- 'May'
Elantralc$Month[Elantralc$Month == '6'] <- 'June'
Elantralc$Month[Elantralc$Month == '7'] <- 'July'
Elantralc$Month[Elantralc$Month == '8'] <- 'August'
Elantralc$Month[Elantralc$Month == '9'] <- 'September'
Elantralc$Month[Elantralc$Month == '10'] <- 'October'
Elantralc$Month[Elantralc$Month == '11'] <- 'November'
Elantralc$Month[Elantralc$Month == '12'] <- 'December'


str(Elantralc)
Elantralc
Elantralc$Month = as.factor(Elantralc$Month)
Elantralc
str(Elantralc)
summary(Elantralc)

#Slit to train and testing
ElantralcTrainingSet1 = subset(Elantralc, Year == "2010" | Year == "2011" | Year == "2012" )
ElantralcTestingSet1 = subset(Elantralc, Year == "2013" | Year == "2014" )

#converting bin
y <- data.frame(model.matrix (~.,data = ElantralcTrainingSet1))

#compare train and y
head(ElantralcTrainingSet1)
head(y)

#remove intercept colum from y
y <- y[, 2:ncol(y)]
head(y)
ElantralcTrainingSet1 = y
str(y)

#part C
ElantraReg = lm(ElantraSales ~ MonthJanuary + MonthFebruary + MonthMarch + MonthMay 
                + MonthJune + MonthJuly + MonthSeptember + MonthAugust + MonthOctober 
                + MonthNovember + MonthDecember
                + Unemployment + Queries + CPI.Energy + CPI.All, data = ElantralcTrainingSet1)
summary(ElantraReg)
#2
ElantraReg = lm(ElantraSales ~ MonthJanuary + MonthFebruary + MonthMarch 
                + MonthJune + MonthJuly + MonthSeptember + MonthAugust + MonthOctober 
                + MonthNovember + MonthDecember
                + Unemployment + Queries + CPI.Energy + CPI.All, data = ElantralcTrainingSet1)
summary(ElantraReg)
#3
ElantraReg = lm(ElantraSales ~ MonthJanuary + MonthFebruary + MonthMarch 
                + MonthJune + MonthJuly + MonthSeptember + MonthOctober 
                + MonthNovember + MonthDecember
                + Unemployment + Queries + CPI.Energy + CPI.All, data = ElantralcTrainingSet1)
summary(ElantraReg)
#4
ElantraReg = lm(ElantraSales ~ MonthJanuary + MonthFebruary + MonthMarch 
                + MonthJune + MonthJuly + MonthSeptember + MonthOctober 
                + MonthNovember + MonthDecember
                + Unemployment + CPI.Energy + CPI.All, data = ElantralcTrainingSet1)
summary(ElantraReg)
#5
ElantraReg = lm(ElantraSales ~ MonthJanuary + MonthFebruary 
                + MonthJune + MonthJuly + MonthSeptember + MonthOctober 
                + MonthNovember + MonthDecember
                + Unemployment + CPI.Energy + CPI.All, data = ElantralcTrainingSet1)
summary(ElantraReg)
#6
ElantraReg = lm(ElantraSales ~ MonthJanuary + MonthFebruary 
                + MonthJuly + MonthSeptember + MonthOctober 
                + MonthNovember + MonthDecember
                + Unemployment + CPI.Energy + CPI.All, data = ElantralcTrainingSet1)
summary(ElantraReg)
#7
ElantraReg = lm(ElantraSales ~ MonthJanuary + MonthFebruary 
                 + MonthSeptember + MonthOctober 
                + MonthNovember + MonthDecember
                + Unemployment + CPI.Energy + CPI.All, data = ElantralcTrainingSet1)
summary(ElantraReg)
#8
ElantraReg = lm(ElantraSales ~ MonthJanuary + MonthFebruary 
                + MonthSeptember + MonthOctober 
                + MonthNovember
                + Unemployment + CPI.Energy + CPI.All, data = ElantralcTrainingSet1)
summary(ElantraReg)
#9
ElantraReg = lm(ElantraSales ~ MonthJanuary + MonthFebruary 
                + MonthOctober + MonthNovember
                + Unemployment + CPI.Energy + CPI.All, data = ElantralcTrainingSet1)
summary(ElantraReg)


#-- Question Cii)
#====================================================================
# Make predictions and evaluate performance of model on test data set
#====================================================================
#converting bin
xtest <- data.frame(model.matrix (~.,data = ElantralcTestingSet1))

#compare train and y
head(ElantralcTestingSet1)
head(xtest)

#remove intercept colum from y
xtest <- xtest[, 2:ncol(xtest)]
head(xtest)
ElantralcTestingSet1 = xtest
str(xtest)

# Making Predictions
ElantraSalesPredictions = predict(ElantraReg, newdata = ElantralcTestingSet1)

# Get predicted values on test data set
ElantraSalesPredictions

# Compute R square on test data set
SSE = sum((ElantralcTestingSet1$ElantraSales - ElantraSalesPredictions)^2)
SST = sum((ElantralcTestingSet1$ElantraSales - mean(ElantralcTrainingSet1$ElantraSales))^2) # Note: use train data set inside mean() expression
R_Square_Test = 1 - SSE/SST

write.xlsx(ElantralcTestingSet1, "ElantralcTestingSet1_mp1.xlsx")
write.xlsx(ElantralcTrainingSet1, "ElantralcTrainingSet1_mp1.xlsx")

