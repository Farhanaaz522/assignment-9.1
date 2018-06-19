#Assignment17_Session17


#1. Use the below given data set
#   Data Set
#2. Perform the below given activities:
#a. Create classification model using logistic regression model
#b. verify model goodness of fit
#c. Report the accuracy measures
#d. Report the variable importance
#e. Report the unimportant variables
#f. Interpret the results
#g. Visualize the results



#Answers
#a)
#using dataset weight lifting exercises
#reading the dataset
wleb <- read.csv("C:/Users/mudassar/Desktop/farha/wleb.csv")
View(wleb)

#logistic regression
model<- glm(DrugR~BP+Chlstrl+Age+Prgnt+AnxtyLH, data = wleb ,family= binomial)
model
summary(model)

#classification 
library(caTools)
library(tree)
#splitting
set.seed(1)
split<- sample.split(wleb$DrugR,SplitRatio = 0.70)
wlebTrain <- subset(wleb,split == TRUE)
wlebTest<- subset(wleb, split == FALSE)

modelClassTree<- tree(DrugR~BP+Chlstrl+Age+Prgnt+AnxtyLH,data = wlebTrain)
plot(modelClassTree)

text(modelClassTree,pretty = 0 ,cex=0.75)
pred<- predict(modelClassTree,newdata= wlebTest)

predict<- predict(model,type="response")
head(predict,3)
wleb$predict <- predict
wleb$predictROUND<- round(predict,digits = 0)
#confusion matrix
table(wleb$DrugR,predict>= 0.5)

sum<- sum(table(wleb$DrugR,predict>= 0.5))

#f) & b) & c)
#Answers
#interpretation, Accuracy and model goodness  of our model
summary(model) 

#accuracy of our model
accuracy<- (13+12)/(30)
accuracy
#0.8333333333

library(verification)
predictTrain<- predict(model,wleb,type="response")
table(wleb$DrugR,predictTrain >=0.5)
head(predictTrain,3)
auc(wleb$DrugR,predictTrain)

