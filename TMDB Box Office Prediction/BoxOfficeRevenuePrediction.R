TMDB Box Office Predictions

'''
# Goal is to predict Revenue -- Regression not classification unless we place into buckets 
# Cannot be a linear model
# Cannot be binary

'''
Proposals -- 
1. Remove belongs_to_collection variable
2. How to deal with 0 values in budget variable
3. Recode genre variable 
4. Disregard Homepage/url variable 
5. imdb_id is not relevant to predicting revenue
6. Original language - not valuable but we can test it
7. Original Title - can see how long the text is 
8. Overview is qualitative 
9. Popularity seems like a good option but is this pre or post realease? 
10. Poster Path - remove variable 
11. Production Companies - Just handle null values (more null than countries)
12. Production Countries - Just handle null values (this may be better) -- check if they are correlated

'''

rm(list=ls())

# load libraries 
library(stringr)
library(stringi)
library(randomForest)
library(gbm)

##### Load datasets 
Train <- read.csv("train.csv", stringsAsFactors = F)
Test <- read.csv("test.csv", stringsAsFactors = F)

Train$revenue <- as.numeric(Train$revenue)
class(Train$revenue)
sum(is.na(Train$revenue))

##### CLEAN DATA 
Train.Clean <- Train[, -c(2, 5, 6, 8, 9, 11, 12, 13, 16, 17, 18, 20, 21, 22)] 
Test.Clean <- Test[, -c(2, 5, 6, 8, 9, 11, 12, 13, 16, 17, 18, 20, 21, 22)] 

# Removed below variables- too long text, urls to movie poster paths, languages were twice (in form of original and spoken) 
  # belongs to collection variable (2)
  # Homepage url link (5)
  # imdb_id (6)
  # Original Title (8)
  # Overview (9)
  # Poster Path (url) (11)
  # Production Company (12)
  # Production Countires (13)
  # Spoken Languages (16)
  # Status (17)
  # Tagline (18)
  # Keywords (20)
  # Cast (21)
  # Crew (22)


# Replace 0's with mean 
sum(is.na(Train.Clean$budget))
sum(is.na(Train.Clean$runtime))

Train.Clean$budget <- ifelse(Train.Clean$budget == 0,mean(Train.Clean$budget),Train.Clean$budget)
Test.Clean$budget <- ifelse(Test.Clean$budget == 0,mean(Test.Clean$budget),Test.Clean$budget)

Train.Clean$runtime <- ifelse(Train.Clean$runtime == 0,mean(Train.Clean$runtime),Train.Clean$runtime)
Test.Clean$runtime <- ifelse(Test.Clean$runtime == 0,mean(Test.Clean$runtime),Test.Clean$runtime)

# Remove ID from production company, string as factor
# Create new column 
# install.packages("stringr")

Test.Clean$num.genres <- str_count(Test.Clean$genres, 'name')
  # counts the number of occurrences of a specified pattern in a character vector or a string
Train.Clean$num.genres <- str_count(Train.Clean$genres, 'name')

Train.Clean$genres <- tolower(Train.Clean$genres)
Test.Clean$genres <- tolower(Test.Clean$genres)

# One-hot-code (14 different genres)
Test.Clean$genre.comedy<-ifelse(stri_detect_fixed(Test.Clean$genres, 'comedy'),1, 0)
Test.Clean$genre.drama<-ifelse(stri_detect_fixed(Test.Clean$genres, 'drama'),1, 0)
Test.Clean$genre.family<-ifelse(stri_detect_fixed(Test.Clean$genres, 'family'),1, 0)
Test.Clean$genre.romance<-ifelse(stri_detect_fixed(Test.Clean$genres, 'romance'),1, 0)
Test.Clean$genre.thriller<-ifelse(stri_detect_fixed(Test.Clean$genres, 'thriller'),1, 0)
Test.Clean$genre.action<-ifelse(stri_detect_fixed(Test.Clean$genres, 'action'),1, 0)
Test.Clean$genre.animation<-ifelse(stri_detect_fixed(Test.Clean$genres, 'animation'),1, 0)
Test.Clean$genre.adventure<-ifelse(stri_detect_fixed(Test.Clean$genres, 'adventure'),1, 0)
Test.Clean$genre.horror<-ifelse(stri_detect_fixed(Test.Clean$genres, 'horror'),1, 0)
Test.Clean$genre.documentary<-ifelse(stri_detect_fixed(Test.Clean$genres, 'documentary'),1, 0)
Test.Clean$genre.music<-ifelse(stri_detect_fixed(Test.Clean$genres, 'music'),1, 0)
Test.Clean$genre.crime<-ifelse(stri_detect_fixed(Test.Clean$genres, 'crime'),1, 0)
Test.Clean$genre.sciencefiction<-ifelse(stri_detect_fixed(Test.Clean$genres, 'science fiction'),1, 0)
Test.Clean$genre.mystery<-ifelse(stri_detect_fixed(Test.Clean$genres, 'mystery'),1, 0)

Train.Clean$genre.comedy<-ifelse(stri_detect_fixed(Train.Clean$genres, 'comedy'),1, 0)
Train.Clean$genre.drama<-ifelse(stri_detect_fixed(Train.Clean$genres, 'drama'),1, 0)
Train.Clean$genre.family<-ifelse(stri_detect_fixed(Train.Clean$genres, 'family'),1, 0)
Train.Clean$genre.romance<-ifelse(stri_detect_fixed(Train.Clean$genres, 'romance'),1, 0)
Train.Clean$genre.thriller<-ifelse(stri_detect_fixed(Train.Clean$genres, 'thriller'),1, 0)
Train.Clean$genre.action<-ifelse(stri_detect_fixed(Train.Clean$genres, 'action'),1, 0)
Train.Clean$genre.animation<-ifelse(stri_detect_fixed(Train.Clean$genres, 'animation'),1, 0)
Train.Clean$genre.adventure<-ifelse(stri_detect_fixed(Train.Clean$genres, 'adventure'),1, 0)
Train.Clean$genre.horror<-ifelse(stri_detect_fixed(Train.Clean$genres, 'horror'),1, 0)
Train.Clean$genre.documentary<-ifelse(stri_detect_fixed(Train.Clean$genres, 'documentary'),1, 0)
Train.Clean$genre.music<-ifelse(stri_detect_fixed(Train.Clean$genres, 'music'),1, 0)
Train.Clean$genre.crime<-ifelse(stri_detect_fixed(Train.Clean$genres, 'crime'),1, 0)
Train.Clean$genre.sciencefiction<-ifelse(stri_detect_fixed(Train.Clean$genres, 'science fiction'),1, 0)
Train.Clean$genre.mystery<-ifelse(stri_detect_fixed(Train.Clean$genres, 'mystery'),1, 0)


# Update genre variables to be factors 
Test.Clean$genre.comedy <-as.factor(Test.Clean$genre.comedy)
Test.Clean$genre.drama <- as.factor(Test.Clean$genre.drama)
Test.Clean$genre.family <- as.factor(Test.Clean$genre.family)
Test.Clean$genre.romance <- as.factor(Test.Clean$genre.romance) 
Test.Clean$genre.thriller <- as.factor(Test.Clean$genre.thriller)
Test.Clean$genre.action <- as.factor(Test.Clean$genre.action)
Test.Clean$genre.animation <- as.factor(Test.Clean$genre.animation)
Test.Clean$genre.adventure <- as.factor(Test.Clean$genre.adventure) 
Test.Clean$genre.horror <- as.factor(Test.Clean$genre.horror)
Test.Clean$genre.documentary <- as.factor(Test.Clean$genre.documentary)
Test.Clean$genre.music <- as.factor(Test.Clean$genre.music)
Test.Clean$genre.crime <- as.factor(Test.Clean$genre.crime)
Test.Clean$genre.sciencefiction <- as.factor(Test.Clean$genre.sciencefiction)
Test.Clean$genre.mystery <-as.factor(Test.Clean$genre.mystery)

Train.Clean$genre.comedy <-as.factor(Train.Clean$genre.comedy)
Train.Clean$genre.drama <- as.factor(Train.Clean$genre.drama)
Train.Clean$genre.family <- as.factor(Train.Clean$genre.family)
Train.Clean$genre.romance <- as.factor(Train.Clean$genre.romance) 
Train.Clean$genre.thriller <- as.factor(Train.Clean$genre.thriller)
Train.Clean$genre.action <- as.factor(Train.Clean$genre.action)
Train.Clean$genre.animation <- as.factor(Train.Clean$genre.animation)
Train.Clean$genre.adventure <- as.factor(Train.Clean$genre.adventure) 
Train.Clean$genre.horror <- as.factor(Train.Clean$genre.horror)
Train.Clean$genre.documentary <- as.factor(Train.Clean$genre.documentary)
Train.Clean$genre.music <- as.factor(Train.Clean$genre.music)
Train.Clean$genre.crime <- as.factor(Train.Clean$genre.crime)
Train.Clean$genre.sciencefiction <- as.factor(Train.Clean$genre.sciencefiction)
Train.Clean$genre.mystery <-as.factor(Train.Clean$genre.mystery)

Test.Clean$genres <- NULL #remove original genre column 
Train.Clean$genres <- NULL #remove original genre column 

# Split Data between month and year 
Test.Clean$Month <- format(as.Date(Test.Clean$release_date, "%m/%d/%y"), "%m")
Test.Clean$Year <- format(as.Date(Test.Clean$release_date, "%m/%d/%y"), "%y")
Test.Clean$Year <- ifelse(as.integer(Test.Clean$Year) <= 20, paste0('20', Test.Clean$Year), paste0('19', Test.Clean$Year))
Train.Clean$Month <- format(as.Date(Train.Clean$release_date, "%m/%d/%y"), "%m")
Train.Clean$Year <- format(as.Date(Train.Clean$release_date, "%m/%d/%y"), "%y")
Train.Clean$Year <- ifelse(as.integer(Train.Clean$Year) <= 20, paste0('20', Train.Clean$Year), paste0('19', Train.Clean$Year))

Test.Clean$Month <- as.factor(Test.Clean$Month)
Test.Clean$Year <- as.Date(paste0(Test.Clean$Year,"-01-01" ))
Train.Clean$Month <- as.factor(Train.Clean$Month)
Train.Clean$Year <- as.Date(paste0(Train.Clean$Year,"-01-01" ))

Test.Clean$release_date <- NULL #remove full date column
Train.Clean$release_date <- NULL #remove full date column

# Change language to be binary (1 = english, else = 0 #foreign)
Test.Clean$original_language <- ifelse(Test.Clean$original_language == 'en', 1, 0) 
Test.Clean$original_language <- as.factor((Test.Clean$original_language))
Train.Clean$original_language <- ifelse(Train.Clean$original_language == 'en', 1, 0) 
Train.Clean$original_language <- as.factor((Train.Clean$original_language))

# Count length of title 
Test.Clean$title<- as.character(Test.Clean$title)
Test.Clean$title <- nchar(Test.Clean$title)
Train.Clean$title<- as.character(Train.Clean$title)
Train.Clean$title <- nchar(Train.Clean$title)


sum(is.na(Train.Clean))# 14 na's will not let model to run
Train.Clean <- na.omit(Train.Clean) # drop rows with NA's


##### ANALYSIS- Non-Linear & Supervised

# Methods:
  # Random Forest:
  # Gradient Boosting:
  # Neural Networks: need more data for NN to be effective
set.seed(1693)

my.df <- Train.Clean

trainIndex <- sample(1:nrow(my.df), nrow(my.df) * 0.8)
testIndex <- (-trainIndex)
trainOFtrain <- my.df[trainIndex, ] # training set from the training data
testOFtrain <- my.df[testIndex, ] # test set sampled from the training data


# RANDOM FOREST

rf.model = randomForest(my.df$revenue ~ . ,
                        data = my.df,
                        subset = trainIndex, # row indices
                        mtry = 10, # number of x features to include
                        ntree = 25, #specify the number of trees if you want
                        importance = TRUE) # Ranks the importance of each feature

rfPrediction <- predict(rf.model, newdata = testOFtrain)


# BOOSTING
set.seed(1693)

my.df$Year <- as.factor(my.df$Year)
y <- my.df$revenue[trainIndex]

gbm.model = gbm(y ~ .,
                data = my.df[trainIndex,],
                distribution = "gaussian", # distribution is Gaussian for Regression
                n.trees = 5000, # how many trees do you want
                interaction.depth = 4, # how many layers does each tree have
                shrinkage = 0.2, # the learning rate (Lambda) for each tree
                verbose = F) # does it output something for each tree

gbPrediction <- predict(gbm.model,newdata = my.df[-trainIndex,], n.trees = 5000)

# COMPARE RMSE's

rfRMSE <- sqrt(mean((rfPrediction - my.df$revenue[-trainIndex])^2))
gbRMSE <- sqrt(mean((gbPrediction - my.df$revenue[-trainIndex])^2)) 
rfRMSE # 69656975
gbRMSE # 8104440

# BOOSTING MODEL WORKS MUCH BETTER


