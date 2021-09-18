#=================
#load CSV File
Parole = read.csv("Parole.csv")
str(Parole)
Parole

#question 1
prop.table(table(Parole$Violator))#answer for question 1
View(Parole)
str(Parole)
summary(Parole)
table(Parole$Violator)
78/675

#====================================================================
str(Parole)
Parole$State = as.factor(Parole$State)
Parole$Crime = as.factor(Parole$Crime)
install.packages("caTools")
library(caTools)
set.seed(88)
spl = sample.split(Parole$Violator, SplitRatio = 0.7) #split data by ratio
ParoleTrain = subset(Parole, spl == TRUE) #split train
ParoleTest = subset(Parole, spl == FALSE) #split test
## Build logistic regression model on train data set
# parole = violators --> logistic regression
ParoleModel = glm(Violator~., data=ParoleTrain, family = "binomial") # for logistic ref
summary(ParoleModel)
##Calculate logistic probability
Male=1
RaceWhite=1
Age=50
StateLouisiana=0
StateOther=1
StateVirginia=0
TimeServed=3
MaxSentence=12
MultipleOffenses=0
CrimeDrugs=0
CrimeLarceny=1.
CrimeOther=0
logprobability= -3.29370+0.65662*Male-0.67930*RaceWhite+0.01739*Age+0.67688*StateLouisiana-0.17308*StateOther-3.38536*StateVirginia-0.06809*TimeServed+0.04536*MaxSentence+1.42426*MultipleOffenses-0.23931*CrimeDrugs+0.99710*CrimeLarceny+0.19106*CrimeOther
logprobability
probability=1/(1+exp(-logprobability))
probability
##Predicted probability
PredictTest = predict(ParoleModel, type = "response", newdata = ParoleTest)
summary(PredictTest)
z=table(ParoleTest$Violator, PredictTest > 0.5)
z
accuracy = (z[1]+z[4])/sum(z)
accuracy = (z[1]+z[4])/sum(z)
baseline_accuracy = (z[1]+z[3])/sum(z)

# Calculate AUC(area under the curve)
install.packages("ROCR")
library(ROCR)
ROCRpred = prediction(PredictTest, ParoleTest$Violator)
ROCCurve = performance(ROCRpred, "tpr", "fpr")
plot(ROCCurve)
plot(ROCCurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred, "auc")@y.values)


