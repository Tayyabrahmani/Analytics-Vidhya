library(dplyr)
library(ggplot2)
library(caret)
library(mice)
library(e1071)

#Importing Files
train = read.csv('Train.csv')
test = read.csv('Test.csv')
combined = bind_rows(train,test)

#Data Exploration
apply(combined, 2,function(x) length(unique(x)))
apply(combined, 2,function(x) sum(is.na(x)))

ggplot(combined, aes(x = log(ApplicantIncome))) + geom_density()
ggplot(combined[combined$ApplicantIncome< 50000,], aes(x = ApplicantIncome, y = LoanAmount, col = Loan_Status)) + geom_point()
ggplot(combined, aes(x = ApplicantIncome, y = LoanAmount, col = Self_Employed)) + geom_point()
ggplot(combined, aes(x = LoanAmount)) + geom_histogram()

table(combined$Gender, combined$Married, combined$Education)
pairs(~ ApplicantIncome + CoapplicantIncome + LoanAmount, data = combined)
table(combined$Property_Area, combined$Loan_Status)
table(combined$Education, combined$Self_Employed)

#Gender
combined$Gender = as.character(combined$Gender)
combined$Gender = ifelse(combined$Gender == "", "Male", combined$Gender) 
combined$Gender = factor(combined$Gender, labels = c("Female", "Male"))

#Dependents
combined$Dependents = ifelse(combined$Dependents == "", NA, combined$Dependents)

combined$Dependents[combined$Dependents == "" & combined$Married == "No"] = "0"

combined_mice = combined[,1:12]
cols = c("Gender", "Dependents", "Education", "Self_Employed", "Property_Area", "Loan_Status")
for(c in cols)
{
  combined_mice[,c] = as.numeric(combined_mice[,c])
}

combined_imputed <- mice(combined_mice, m=5, maxit = 20, method = 'pmm', seed = 100)
combined <- complete(combined_imputed,3)

new_train = combined[1:614, 2:12]
new_test = combined[615:981,2:12]

new_train$Loan_Status = train$Loan_Status

#Model Fitting
rfr = train(Loan_Status ~ ., data = new_train, method = "rf", tune.grid = expand.grid(.cp = seq(0.01,0.5,0.01)), trControl = trainControl(method = "cv", repeats = 3, number = 10))

rfr = train(Loan_Status ~ ., data = new_train, method = "rf", trControl = trainControl(method = "repeatedcv", repeats = 3, number = 10), mtry = 35)
y_pred = predict(rfr, newdata = new_test, type = "prob")
y_actual = y_pred
write.csv(y_pred, "Solution_rfr.csv")