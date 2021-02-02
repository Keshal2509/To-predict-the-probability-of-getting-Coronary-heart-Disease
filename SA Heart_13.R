### Import data into R
SAheart_data = read.csv('C:\\Users\\Keshal Shah\\Desktop\\SA heart_13.csv')
View(SAheart_data)


names(SAheart_data)               ### To get the names of the data. (chd is the Target Variable.)
head(SAheart_data)                ### To get the first parts of the data.
sum(is.na(SAheart_data))          ### To check the missing values in the data.
str(SAheart_data)                 ### To display the structure of the data.


### To calculate the proportions of chd and non-chd cases.
propor=table(SAheart_data$chd_13)
prop.table(propor)
#  The case has close to 65% of cases with no CHD and 35% with CHD.
#  So in this model , it should give prediction accuracy greater than 65%.


### Drop Row.Names_13 Column (Scaling the Variables)
SAheart_data = subset(SAheart_data,select = -c(row.names_13)) 
dim(SAheart_data)               ### Dimensions of the data.


### Convert famhist_13 to 0 and 1 instead of Present or Absent 
SAheart_data$famhist_13 = ifelse(SAheart_data$famhist_13 == 'Present',1,0) 
str(SAheart_data)


### To split the dataset into training and testing set 
### caTools used for Tools: moving window statistics, GIF, etc. 
library(caTools)
set.seed(100)                  ### It sets the starting number and generate a sequence. 


### Training and Testing a model to 80:20 
sample = sample.split(SAheart_data$chd_13, SplitRatio = .80) 
training = subset(SAheart_data, sample == TRUE) 
testing = subset(SAheart_data, sample == FALSE) 


### Verifying the splitting of our data 
prop.table(table(training$chd_13))
prop.table(table(testing$chd_13))         ### For testing a model
#  From here, we see that the Training and Testing data has almost same proportion.


### 1. To fit Logistic Regression Model 
# Logistic regression is useful when we are predicting a binary outcome from a set of continuous predictor varibles. 
logreg1 = glm(chd_13 ~ sbp_13 + tobacco_13 + ldl_13 + adiposity_13 + famhist_13 + typea_13 + obesity_13 + alcohol_13 + age_13, data = training, family = binomial) 
summary(logreg1)
#  If the value of p is < 0.05 it refers that those variables are significant with respect to our Target variable.
#  So, we can observe that tobacco_13 , ldl_13 , famhist_13 , typea_13 and age_13 are statistically significant in the logistic regression model built in (a).


### Hence, We will fit the model only with the significant factors only.
logreg2 = glm(chd_13 ~ tobacco_13 + ldl_13 + famhist_13 + typea_13+ age_13, + data = training, family = binomial) 
summary(logreg2)


#  A lower AIC score indicates superior goodness-of-fit and a lesser tendency to over-fit.
#  In the second model we can see that we have achieved a lower AIC value. 
#  In the first model we got the AIC value 410.42 and in the second model we got the AIC value 404.32
#  Which is not very low but a considerate amount since we eliminated all other variables whose p value was greater than 0.05.


### Prediction for the 1st Model 
pred1 = predict(logreg1,newdata= testing) 
library(caret)                           ### To calculate Confusion Matrix 
pred1 = ifelse(pred1>0.5,1,0) 
confusionMatrix(factor(pred1),factor(testing$chd_13))
#  There are 14, where it was correctly predicted.


### To find the Precision of the 1st Model 
prec1 = table(factor(pred1),factor(test$chd_13)) 
precision(prec1)
#  So in the first Logistic Regression model, we get a Precision of 76% and Accuracy of 79%.


### Prediction for the 2nd Model 
pred2 = predict(logreg2,newdata= testing) 
pred2 = ifelse(pred2>0.5,1,0) 
confusionMatrix(factor(pred2),factor(testing$chd_13))
#  There are 10, where it was correctly predicted.


### To find the Precision of the 2nd Model 
prec2 = table(factor(pred2),factor(test$chd_13)) 
precision(prec2)
#  And in the second Logistic Regression model, we get a Precision of 72% and Accuracy of 75%. 
#  Out of the two models , First model gives a high precision i.e. 76% compared to the second model ; 
#  but for this dataset this much amount of Precision is not accountable.
#  It would be very risky if we take this model into consideration.


### 2. Used for Recursive partitioning for classification and Regression Trees 
library(rpart) 
cartfit = rpart(chd_13~.,data = training ,method = "class") 
print(cartfit)


### rpart automatically scales and adjusts the displayed tree for the best fit.
library(rpart.plot) 
rpart.plot(cartfit,main= "Classification Tree")
#  In this model, age_13 , famhist_13 , typea_13 , ldl_13 ,tobacco_13 and 
# obesity_13 are the features which are helping in classifying the chd_13 as 0 or 1.


pred3 = predict(cartfit, newdata = testing, type = 'class') 
pred3

tabl1 = table(predicted = pred3,actual = testing[,10]) 
tabl1
#  So 51 and 21 are correctly classified for the model. 

confusionMatrix(tabl1)
precision(tabl1)
#  82% Precision for the decision tree.


### 3. Random Forest
library(randomForest)     ### Used to create Random Forests 
ranfor = randomForest(factor(chd_13)~.,data = training) 
summary(ranfor)


### Used for dotchart variable importance as measured by a Random Forest. 
varImpPlot(ranfor)
getTree(ranfor)


pred4 = predict(ranfor , newdata = testing ,type = 'response') 
pred4

tabl2 = table(pred4,testing[,10]) 
confusionMatrix(tabl2)
precision(tabl2)