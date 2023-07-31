#Importing Libraries
library(descr)
library(corrplot)
library(cluster)
library(MASS)
library(car)
#install.packages(c("rpart","rpart.plot","caTools","caret","class"))
library(rpart)
library(rpart.plot)
library(caTools)
library(caret)
library(class)


# Reading the DataSet
setwd("D:\\VIT\\SEM 6\\Data analytics\\DA LAB")
dataOG<-read.csv("drug200.csv")

head(dataOG)
dataOG = dataOG[1:6]
dataOG$BP<-as.factor(dataOG$BP)
dataOG$SEX<-as.factor(dataOG$Sex)
dataOG$Cholesterol<-as.factor(dataOG$Cholesterol)
dataOG$Drug<-as.factor(dataOG$Drug)

# for our convenience we are making all the headings as lower case
library(stringr)
names(dataOG)<-str_to_lower(string = names(dataOG),locale = "en")
head(dataOG,4)

anyNA(dataOG)


# EDA
str(dataOG)

#Visualising the count of Qualitative Variables
#1) Drug
freq(dataOG$drug,plot=TRUE,col=c("blue","green","gold","lightgreen","lightblue"),main="Number of drugs")
#2) Sex
freq(dataOG$sex,plot=TRUE,col=c("lightgreen","lightblue"),main="frequency of sex")
#3) BP levels
freq(dataOG$bp,plot=TRUE,col=c("red","green","blue"),main="BP levels")
#4) Cholestrol
freq(dataOG$cholesterol,plot=TRUE,col=c("lightgreen","lightblue"),main="Cholesterol")


#Visualising the count of Quanitative Variables
#1) Age
hist(dataOG$age,, main="Histogram of age",xlab="Age [year]", 
     ylab = "Absolute Frequency",
     col = "lightblue",breaks=8)
lines(density(dataOG$age))

hist(dataOG$age,freq=FALSE, main="Histogram of age",xlab="Age [year]", 
     ylab = "density",
     col = "lightblue",breaks=8)
lines(density(dataOG$age))

#2) Na to K ration in Blood
hist(dataOG$na_to_k,main="Histogram of Sodium to potassium Ratio in Blood",xlab="Sodium to potassium Ratio in Blood", 
     ylab = "Absolute Frequency",
     col = "lightblue",breaks=8)
lines(density(dataOG$na_to_k))

hist(dataOG$na_to_k,freq=FALSE,
     main="Histogram of Sodium to potassium Ratio in Blood",
     xlab="Sodium to potassium Ratio in Blood", 
     ylab = "density",
     col = "lightblue",breaks=8)
lines(density(dataOG$na_to_k))

# Histogram of Log
hist(log(dataOG$na_to_k), main="log Histogram of Sodium to potassium Ratio in Blood",xlab="log(Sodium to potassium Ratio in Blood)", 
     ylab = "Absolute Frequency",
     col = "lightblue")


hist(log(dataOG$na_to_k),freq=FALSE, 
     main="log Histogram of Sodium to potassium Ratio in Blood",
     xlab="log(Sodium to potassium Ratio in Blood)", 
     ylab = "density",
     col = "lightblue")
lines(density(log(dataOG$na_to_k)))


# The distribution is right Skewed


# Bi variate Analysis
boxplot(dataOG$age ~ dataOG$drug,
        col=c("blue","green","gold","lightgreen","lightblue"),
        main="Age to Drug",
        ylab="Age [year]",
        xlab="Drug")


boxplot(dataOG$na_to_k ~ dataOG$drug,
        col=c("blue","green","gold","lightgreen","lightblue"),
        main="Sodium/Potassium Ratio to Drug",
        ylab="Sodium Potassium Ratio",
        xlab="Drug")

crosstab(dataOG$sex,dataOG$drug,
         type = "j",
         col=c("blue","green","gold","lightgreen","lightblue"),
         xlab="Drug",
         ylab="Sex",
         main= "frequency count Sex ~ Drug")

crosstab(dataOG$drug,dataOG$bp, plot = FALSE)
crosstab(dataOG$drug,dataOG$cholesterol, plot = FALSE)


# Correlation of Age and NA to K ratio

plot(scale(dataOG[,c("age","na_to_k")]), main="Age ~ Na_to_K")
print("correlation coefficient:")
cor(dataOG[,c("age","na_to_k")])
print("Spearman's rank correlation coefficient:")
cor(dataOG[,c("age","na_to_k")],method="spearman")

# Potassium ratio and Cholesterol

boxplot(dataOG$na_to_k ~ dataOG$cholesterol,
        col=c("blue","green","gold","lightgreen","lightblue"),
        main="Sodium to potassium Ration in Blood and Cholesterol",
        ylab="Sodium to potassium Ration in Blood ",
        xlab="Cholesterol")




# Model Creation

## Splitting the Data
split_data <- sort(sample(nrow(dataOG), nrow(dataOG)*0.7))

trainData<-dataOG[split_data,]
testData<-dataOG[-split_data,]




## Regression Trees - A decision tree used for solving regression problems

mod_drug<-rpart(drug~age+sex+bp+cholesterol+na_to_k,data=trainData,method = "class",
                control = (rpart.control(xval=10,cp=0.003)),
                parms = list(split = "information"))

rpart.plot(mod_drug)

Prognose=predict(mod_drug,testData[-6],type = "class")
table(testData$drug,Prognose)



## KNN

fit_control<-trainControl(method = "repeatedcv",number = 100,
                          repeats=100)
set.seed(123)
knnModel <- caret::train(drug~age+sex+bp+cholesterol+na_to_k,data=trainData,method = "knn", trControl = fit_control,tuneGrid = expand.grid(k = 1:20))
knnModel
plot(knnModel)

trainData$drug<-as.factor(trainData$drug)
trainData$sex<-as.factor(trainData$sex)
trainData$cholesterol<-as.numeric(as.factor(trainData$cholesterol))
trainData$bp<-as.numeric(as.factor(trainData$bp))
trainData$sex<-as.numeric(trainData$sex)

testData$drug<-as.factor(testData$drug)
testData$sex<-as.factor(testData$sex)
testData$cholesterol<-as.numeric(as.factor(testData$cholesterol))
testData$bp<-as.numeric(as.factor(testData$bp))
testData$sex<-as.numeric(testData$sex)

set.seed(4)
modelknn<-knn(train=trainData[,1:5],test = testData[,1:5],cl=trainData$drug,k=19)
confusionMatrix(testData$drug,modelknn)
plot(modelknn)


head(trainData)
head(testData)