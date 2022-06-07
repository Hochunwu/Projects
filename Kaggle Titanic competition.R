#Kaggle Titanic Contest


#Import required libraries

library (tidyverse)
library (datasets)
library (visdat)

#Import the datafiles
train = read.csv("C:\\Users\\hochu\\OneDrive\\Documents\\R\\Datasets\\train.csv")

test = read.csv("C:\\Users\\hochu\\OneDrive\\Documents\\R\\Datasets\\test.csv")

#Make a seperate column so we can later on distinguish the two files after we merged them
train$Istrain = TRUE
test$Istrain = FALSE
test$Survived = NA

#Merge files so it's easier to clean the data
merge = rbind(train, test)

#Looking at the data structures, we want to convert Survived, Pclass and Embarked to factors instead of integers

str(merge)

attach(merge)
Survived = as.factor(Survived)
Embarked = as.factor(Embarked)
Pclass = as.factor(Pclass)


#Check to see whether it's successful
str(merge)

#Check missing values in data

vis_dat(merge)
vis_miss(merge)

#Based on the graphs we can see mainly data in age is missing. The distribution of Age seems skewed because of a few outliers

merge %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_boxplot() + theme_bw()

#Therefore, we replace the missing values with the median value of age.

merge[is.na(merge$Age), "Age"] = median(merge$Age, na.rm = TRUE)

#Split the dataset back to train and split
train = merge[merge$Istrain == TRUE,]
test = merge[merge$Istrain == FALSE,]

#Create formula with all the predictor variables
form = as.formula("Survived ~ Age + Embarked + Pclass + Sex + Parch + Fare")

#Train the model
rf_model = randomForest(formula = form, data = train, ntree = 500, mty=6)

#Now that we trained our model, we test it on the test set
Survived = predict(rf_model, newdata=test)

#Check prediction
Survived

#Now we save our output in the requested file format
PassengerId = test$PassengerId

df = as.data.frame(PassengerId)

df$Survived = Survived

write.csv(df, file="baseSubmission.csv", row.names = FALSE)
























train$Istrain = TRUE
test$Istrain = FALSE
test$Survived = NA

vis_dat(df_train)
vis_miss(df_train)
vis_miss(df_test)


df_train %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_boxplot() + theme_bw()








































train_clean = train %>%
  mutate(Age = replace_na(Age, median(Age, na.rm = TRUE)))

test_clean = test %>%
  mutate(Age = replace_na(Age, median(Age, na.rm = TRUE)))

str(df_test_clean)


#Convert to factor
train_clean$Survived = as.factor(train_clean$Survived)
test_clean$Survived = as.factor(test_clean$Survived)
         
vis_miss(merge)
         
str(df_train_clean)
str(df_test_clean)


form = as.formula("Survived ~ Pclass + Sex + Age + SibSp + Patch + Fare + Embarked")

rf_model = randomForest(formula = form, data=train_clean, ntree=500, mtry=3)


