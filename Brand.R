library(readr)
library(tidyverse)
library(caret)
library(mlbench)
library(labeling)

#important to set working directory 
setwd("C:\\Users\\kenne\\OneDrive\\Desktop\\Bx Ubiqum\\C2Task2- Classification - brand preference\\Brand Preference")
Brand<- read.csv("CompleteResponses.csv")
Testset<- read.csv("SurveyIncomplete.csv")

#Plotting and data exploration
hist(Brand$salary);hist(Brand$age);hist(Brand$elevel);
hist(Brand$car); hist(Brand$zipcode); hist(Brand$credit); hist(Brand$brand)
summary(Brand$salary)

#ggplot data relationship 
ggplot(focusBrand, aes(x=age, y=salary, color = brand)) + 
  geom_point() +
  labs(title="Age, Salary towards Brand preference")

#geom_bar requires identiy - stat_count error 
ggplot(Brand, aes(x=age, y=elevel, fill=brand)) + 
  geom_bar(stat= "identity")

#add labels
plot(Brand$salary, Brand$brand,
     main="Relationship Between Salary and Brand",
     xlab="salary",
     ylab="brand")

# Subsetting data = leaving only age and salary 
focusvars <- names(Brand) %in% c("elevel", "zipcode", "car", "credit")
focusBrand <- Brand[!focusvars]
summary(focusBrand)

# Rename all levels, by name # variable name issue 
levels(focusBrand$brand) <- c("Acer", "Sony")
#Relabel data types
focusBrand$brand<- as.factor(focusBrand$brand)

#bin them 
library(mltools)
focusBrand$salary<- bin_data(focusBrand$salary,
                     bins = 5,
                     binType = "explicit",
                     boundaryType = "lcro]",
                     returnDT = FALSE,)
summary(Brand$brand)

#another way to Bin 
#str(bin(Brand$brand, nbins = 3, labels = c("young", "middle", "old")))


#CorrelationMatrix - "kendall", "spearman"
corBrand<- cor(Brand, method = c("pearson"))
round(corBrand, 2)
plot(corBrand)
heatmap(corBrand)

#ggplot heatmap 
library(reshape2) #need to melt cormat
melted_corBrand <- melt(corBrand)
ggplot(melted_corBrand, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

#Caret data splitting
set.seed(123)
trainBrand<- createDataPartition (focusBrand$brand, p= 0.75, 
                                  list = FALSE)
  str(trainBrand); head(trainBrand)
training <- focusBrand[ trainBrand,]
testing <- focusBrand[-trainBrand,]
  nrow(training);nrow(testing)

########################### C5.0
library(C50)
#Ctrl training 
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 3,
                     classProbs = TRUE)
                      #summaryFunction = twoClassSummary)
                      #verboseIter = TRUE,
                     
#scale is normalization, center finds the medium 
modelFit <- train(brand ~.,
            data = training, 
            method = "C5.0",
            tuneLength = 2,
            trControl = ctrl,
            metric= "ROC",
            preProc = c("center", "scale"))
plot (modelFit); summary(modelFit)
modelPredict<- predict(modelFit, newdata = testing)
modelProbs <- predict(modelFit, newdata = testing, type = "prob") #optional 

#confusionMatrix
confusionMatrix(data = modelPredict, testing$brand)
varImp(modelFit) #variable importance 
plot_num(modelFit,varImp)
ggplot(modelFit, X= variable.names, y= varImp)

#########################randomforest 
set.seed(998)
# create test set
randomBrand<- createDataPartition (Brand$brand, p= 0.75, 
                                    list = FALSE)
Rtraining <- Brand[ trainBrand,]
Rtesting <- Brand[-trainBrand,]

#Ctrl training 
Rctrl <- trainContro(method = "repeatedcv",
                      number=10,
                      repeats = 1)

randomFit <- train(brand ~.,
                    data = Rtraining,             # str()checking if calling correct dataset
                    method = "rf",
                    tuneLength = 1,
                    #tunegrid <- expand.grid(.mtry=2),
                    #metric= "ROC",)
                    trControl = Rctrl)

randomPredict<- predict(randomFit, newdata = Rtesting)
str(randomPredict); table(randomPredict)
randomProbs <- predict(randomFit, newdata = Rtesting, type = "prob")
#confusionMatrix
confusionMatrix(data = randomPredict, Rtesting$brand)
varImp(randomFit) 

####################
                  #Final- 5 bins salary with age towards brand 
####################                                     
Brand<- read.csv("CompleteResponses.csv")
Testset<- read.csv("SurveyIncomplete.csv")

#We ran the cormat
corBrand<- cor(Brand, method = c("pearson"))
round(corBrand, 2)
plot(corBrand)
heatmap(corBrand)

# Subsetting data = leaving only age and salary 
focusvars <- names(Brand) %in% c("elevel", "zipcode", "car", "credit")
focusBrand <- Brand[!focusvars]
summary(focusBrand)

#Found salary is the stronest factor and see from heatmap how it distributed
focusBrand$salary<- bin_data(focusBrand$salary,
                             bins = 5,
                             binType = "explicit",
                             boundaryType = "lcro]",
                             returnDT = FALSE,)

#after bin, plot again 
ggplot(focusBrand, aes(x=age, y=salary, color = brand)) + 
  geom_point() +
  labs(title="Age, Salary towards Brand preference")

# Rename all levels, by name # variable name issue 
levels(focusBrand$brand) <- c("Acer", "Sony")
#Relabel data types
focusBrand$brand<- as.factor(focusBrand$brand)

#Caret data splitting
set.seed(123)
trainBrand<- createDataPartition (focusBrand$brand, p= 0.75, 
                                  list = FALSE)
training <- focusBrand[ trainBrand,]
testing <- focusBrand[-trainBrand,]
summary(training); summary (testing)
#################C5.0
library(C50)
#Ctrl training 
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 3,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)


#scale is normalization, center finds the medium 
modelFit <- train(brand ~.,
                  data = training, 
                  method = "C5.0",
                  tuneLength = 2,
                  trControl = ctrl,
                  metric= "ROC",
                  preProc = c("center", "scale"))
plot (modelFit)
summary(modelFit)
modelPredict<- predict(modelFit, newdata = testing)
str(modelPredict)
summary(modelPredict):plot(modelPredict)
modelProbs <- predict(modelFit, newdata = testing, type = "prob")
head(modelProbs)
#confusionMatrix
confusionMatrix(data = modelPredict, testing$brand)
varImp(modelFit) 
ggplot(modelFit, X= variable.names, y= varImp)
summary(focusBrand)


###############################Predict in testset (surveyincomplete)
Testset<- read.csv("SurveyIncomplete.csv")

# Rename all levels, by name # variable name issue 
levels(Testset$brand) <- c("Acer", "Sony")
#Relabel data types
Testset$brand<- as.factor(Testset$brand)
#Bin your salary 
Testset$salary<- bin_data(Testset$salary,
                             bins = 5,
                             binType = "explicit",
                             boundaryType = "lcro]",
                             returnDT = FALSE,)
# Subsetting data = leaving only age and salary 
Testvars <- names(Testset) %in% c("elevel", "zipcode", "car", "credit")
Testset <- Testset[!Testvars]
summary(Testset)

#make prediction
brandPredict<- predict(modelFit, newdata = Testset)
summary(brandPredict);plot(brandPredict)
str(brandPredict)
summary(modelPredict);plot(modelPredict)
summary(focusBrand$brand); plot(focusBrand$brand)
#plot the prediction
ggplot(Testset, aes(x=age, y=salary, color = brandPredict, stat="count")) + 
  geom_point() +
  labs(title="Age, Salary towards Brand preference")

rbrind(focusBrand, Testset, brandPredict)

#compare two prediction 
plot(modelPredict, brandPredict, ylim=c(3500))

#t-test - still not sure if working 
set.seed(123)
t.test(modelPredict, brandPredict, paired = TRUE)


