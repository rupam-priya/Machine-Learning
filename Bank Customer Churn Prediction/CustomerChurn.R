rm(list=ls())
setwd(".../Case 4")

################# Introduction #################
# Call Libraries:
library(caret)
library(MASS)
library(dplyr)

# Read in Data:
data <- read.csv("Case4.csv", stringsAsFactors = TRUE)

# Convert into Factor datatype
data$Exited <- as.factor(data$Exited)
data$HasCrCard <- as.factor(data$HasCrCard)
data$IsActiveMember <- as.factor(data$IsActiveMember)

# Delete Columns: row number, customer id, surname (not valuable predictors)
data = data[,-c(1, 2, 3)]

summary(data) # get summary statistics for each feature
str(data) # check columns names and its data types

################# Comparing Methods #################

# Divide the data into training and testing group:
set.seed(1)
divideData <- createDataPartition(data$Exited, p = .8, list = FALSE)
train <- data[divideData,]
test <- data[-divideData,]

## Creating Models
# Create a logistic regression model using all the applicable predictor variables:
logisticmodel <- glm(Exited ~ ., family = binomial, data = train)
summary(logisticmodel)

# Center and scale your data and create a LDA model using centered and scaled training/test data:
## Step 1 - Center and Scale:
preprocessing <- train %>% preProcess(method = c("center","scale"))
traintransformed <- preprocessing %>% predict(train)
testtransformed <- preprocessing %>% predict(test)

## Step 2 - LDA Model:
ldamodel <- lda(Exited ~., data = traintransformed)
summary(ldamodel)

# Using that same centered and scaled data, create a QDA model:
qdamodel <- qda(Exited ~., data = traintransformed)
summary(qdamodel)

# Create a knn model that also includes centered and scaled data:
knnmodel <- train(Exited ~., data = data, method = "knn", preProcess = c("center","scale"))

# Plot model accuracy vs different values of k
knnmodel
# we can see accuracy with each k values, out of which k=9 gives us the best result
plot(knnmodel)
# the graph shows accuracy vs knn values, reinstating that k=9 performs the best
knnmodel$bestTune
# hence, the system choses knn=9

## Predictions:
# Provide the accuracy rates of the validation set for each test conducted above:

#### logisticmodel
probs<-predict(logisticmodel, test, type="response")
pred<-ifelse(probs>.5, "1", "0")
class(pred);class(test$Exited) # checking to see of the variables are factors 
pred<-as.factor(pred)

#### ldamodel
ldapred <- ldamodel %>% predict(testtransformed)

#### qdamodel
qdap <- predict(qdamodel, testtransformed, type = "response")
qdapred<-ifelse(qdap$class == 1, "1", "0")
qdapred <- as.factor(qdapred)

#### knnmodel

# Make predictions on the test data
knnclass <- predict(knnmodel, newdata=test)
head(knnclass)

## Accuracy:

#### Logistic
mean(pred == test$Exited) # Accuracy: .8164

#### LDA
mean(ldapred$class == testtransformed$Exited) # The mean for the LDA model is 0.8134

#### QDA
mean(qdapred == testtransformed$Exited)
# The accuracy rate of the QDA model = 0.8364
# indicating the model correctly categorized 83.64% of the customers from the test set as either exiting or not exiting the bank.

#### KNN
# Compute model accuracy rate
mean(knnclass == test$Exited) # 85.993% accuracy rate

# For each model listed above, provided is a summary in comments 
# interpreting the confusion matrix/table object regarding True Positives, True Negatives, False Positives, and False Negatives, specificity and sensitivity. 

## Logisitic
confusionMatrix(pred,test$Exited,positive = "1")
# True positives: 91
# True Negatives: 1541
# False Positives: 51
# False negatives: 316
# Specificity: .96796
# Sensitivity: .22359
# Accuracy: .8164

# The model correctly predicted that 291 customers would exit, and that 5374 customers would not exit. 
# The model produced 200 false positives (predicted exited, actually didn't exit), and 1134 false negatives (predicted not exit, actually exited). 
# The model correctly predicted instances of the non event happening 96.80% of the time (indicated by the specificity of .96796),
# and correctly predicted instances of the event happening 22.35% of the time (indicated by the sensitivity of .22359)

## LDA
confusionMatrix(ldapred$class, testtransformed$Exited, positive = '1')

# True positives: 100
# True Negatives: 1526
# False Positives: 66
# False negatives: 307
# Specificity: .95854
# Sensitivity: .24570
# Accuracy: .8134
## The LDA model correctly classifies customers as exiting or not exiting the bank 81.34% of the time. 
# The true negative prediction rate of the model is 95.85%. This is a very high value. 
# However, the bank is trying to identify who will exit, so this value is not useful. 
# The true positive prediction rate of the model is 24.57%, which is a very low value. 
# This indicates that the model has a very poor ability to classify exiting customers.

## QDA
confusionMatrix(qdapred, testtransformed$Exited, positive = '1')

# True Negative: 1508
# False Negative: 243
# False Positive: 84
# True Positive: 164

## Accuracy: 0.8364
# The QDA prediction accuracy rate indicates that the QDA model accurately classifies whether or not someone will exit 83.64% of the time. 
# However, it is clear that the model is much better at predicting whether or not someone will not exit. 
# Out of the test values, 1508 bank customers were correctly predicted to not exit, with 243 customers incorrectly predicted to not exit. 
# In contrast, only 164 customers were correctly predicted to exit, with 84 incorrectly predicted to exit.

## Specificity: 0.94724
# The specificity here is measuring the true negative rate of all of the negative results. 
# In this case, the true negative rate is 94.72%, indicating that the model accurately categorizes the negative result (customers not exiting) at a very high rate.

## Sensitivity: 0.40295
# The sensitivity here is measuring the true positive rate of all of the positive results in the test. 
# In this case, the true positive rate is 40.03%. This means the model identifies correctly classifies exiting customers less than 50% of the time.

## KNN confusion matrix
confusionMatrix(knnclass,test$Exited,positive = "1")

# Specificity : 96.80% (good at predicting the event of not exiting)
# Sensitivity : 43.73% (not so good at predicting the positive event of exiting)

# True positives 1541
# False positives 51
# True negatives 178
# False negatives of 229

# The model predicts the number of customers who actually did exit correctly for 1541 and incorrectly for 229 people. 
# It also predicted correctly that 178 customers would not exit and also predicted that 51 customers would not exit while they did.
# Notedly, the bank could have lost 229 customers and it depends on the management to decide if this gap in prediction is detrimental to the bank’s business or not.

################# Conclusion #################
# Finally, in comments, select the best model(s) and describe what that means in terms of the shape of the data (linear to non-parametric):

## The model with the best accuracy is the KNN model, with an 85.993% accuracy.
## The model with the best sensitivity is the KNN model, with a 43.735% sensitivity.
## The model with the best specificity was a tie between the logistic model and the KNN model, with a 96.796% specificity.

## In this case, overall accuracy and sensitivity are the most important predictors to determine the usefulness of a model. 
# This is because the bank cares more about correctly predicting if a customer will exit than if they will stay. 
# Overall, the model that best predicts if a customer will exit is the KNN model. This means that the data is non-parametric, indicating that it does not have a linear shape to it. 
# KNN does not make assumptions about the shape of the data, and this allows it to be more flexible when modeling non-linear data. 
# QDA also did better than both LDA and the logistic regression model, confirming that the data is not linear. 
# However, QDA is not as flexible as the KNN model because it does make some assumptions about the shape of the data, which is the reason that it does worse than the KNN model on this data.

# WE CHOSE KNN MODEL
## Out of all of the models, KNN only misclassified 229 exiting customers by predicting that they would not exit. This is in contrast to 316 for the logistic model, 307 for the LDA model, and 243 for the QDA model. 
# The bank is attempting to determine whether or not they can identify customers who will exit so they can take corresponding action, and a model that incorrectly classifies these customers will have a negative impact on the bank's decision making by providing misleading information. 
# The KNN model's sensitivity also indicates that it incorrectly identifies a customer as exiting when they are staying 56.27% of the time. 
# While this is a high value, it is a better predictor than the other models. Overall, the KNN model best classifies the customers so that the bank can most closely predict churn and make business decisions to help curb any negative impact.
