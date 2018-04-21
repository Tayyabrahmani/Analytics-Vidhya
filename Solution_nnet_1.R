#Loading Libraries
library(dplyr)
library(ggplot2)
library(caret)
library(tidyr)

#Loading Train and test dataset
train_men = read.csv("mens_train_file.csv", stringsAsFactors = FALSE)
test_men = read.csv("mens_test_file.csv", stringsAsFactors = FALSE)
full = bind_rows(train_men, test_men)

#Loading train_women and test_women dataset
train_women = read.csv("womens_train_file.csv", stringsAsFactors = FALSE)
test_women = read.csv("womens_test_file.csv", stringsAsFactors = FALSE)
full_women = bind_rows(train_women, test_women)

#Men data
full$serve = factor(full$serve)
full$hitpoint = factor(full$hitpoint)
full$outside.sideline = factor(full$outside.sideline)
full$outside.baseline = factor(full$outside.baseline)
full$same.side = factor(full$same.side)
full$previous.hitpoint = factor(full$previous.hitpoint)
full$server.is.impact.player = factor(full$server.is.impact.player)
full$train  = factor(full$train)
full$outcome  = factor(full$outcome)

#Women data
full_women$serve = factor(full_women$serve)
full_women$hitpoint = factor(full_women$hitpoint)
full_women$outside.sideline = factor(full_women$outside.sideline)
full_women$outside.baseline = factor(full_women$outside.baseline)
full_women$same.side = factor(full_women$same.side)
full_women$previous.hitpoint = factor(full_women$previous.hitpoint)
full_women$server.is.impact.player = factor(full_women$server.is.impact.player)
full_women$train  = factor(full_women$train)
full_women$outcome  = factor(full_women$outcome)

#men data
train_men = full[1:5000,-c(25, 26, 28)]
test_men = full[5001:7000, -c(25, 26,28)]

#Women Data
train_women = full_women[1:5000,-c(25,26, 28)]
test_women = full_women[5001:7000, -c(25,26,28)]

#Modeling men and Women data
model_nnet <- train(factor(outcome) ~ serve + speed + distance.from.sideline + depth + outside.sideline + outside.baseline + player.distance.travelled + player.impact.depth + player.depth + previous.net.clearance + opponent.depth, data = train_men, method='nnet',trControl=trainControl(method='cv')) 
model_nnet_women <- train(factor(outcome) ~ serve + speed + distance.from.sideline + depth + outside.sideline + outside.baseline + player.distance.travelled + player.impact.depth + player.depth + previous.net.clearance + opponent.depth, data = train_women, method='nnet',trControl=trainControl(method='cv')) 

#Predicting results
probs_women <- predict(model_nnet_women, newdata = test_women, type='prob')
probs <- predict(model_nnet, newdata = test_men, type='prob')

#Saving Data
total_prob = bind_rows(probs, probs_women)
write.csv(total_prob, "Final_prob_nnet.csv")