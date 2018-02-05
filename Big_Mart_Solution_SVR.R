library(dplyr)
library(ggplot2)
library(caret)
library(mice)
library(e1071)

#Importing Files
train = read.csv('Train.csv')
test = read.csv('Test.csv')
combined = bind_rows(train,test)

#Searching for Item Weights
x = combined %>% filter(Outlet_Identifier == "OUT027") 
x1 = combined %>% filter(Outlet_Identifier == "OUT019") 

#Filling the missing values in Item_Weights
Median_Weights = combined %>% group_by(Item_Identifier) %>% summarise(avg = median(Item_Weight, na.rm = TRUE))
combined$Item_Weight <- ifelse(is.na(combined$Item_Weight), Median_Weights$avg[match(combined$Item_Identifier, Median_Weights$Item_Identifier)], combined$Item_Weight)

#Trimming Item Identifier
combined$Item_Identifier = strtrim(combined$Item_Identifier, 2)
combined$Item_Identifier = as.factor(combined$Item_Identifier)

#Fat_Content
combined$Item_Fat_Content = as.character(combined$Item_Fat_Content)
for( i in 1:nrow(combined))
{ 
  if(combined$Item_Fat_Content[i] %in% c("LF", "low fat", "Low Fat"))
  {
    combined$Item_Fat_Content[i] = "Low Fat"
  }
  else
    combined$Item_Fat_Content[i] = "Regular"
}
combined[which(combined$Item_Type == "Household"),]$Item_Fat_Content = "None"
combined[which(combined$Item_Type == "Health and Hygiene"),]$Item_Fat_Content = "None"
combined[which(combined$Item_Type == "Others"),]$Item_Fat_Content = "None"
combined$Item_Fat_Content = as.factor(combined$Item_Fat_Content)

#ITEM_MRP
ggplot(combined, aes(x = Item_MRP)) + geom_density(col = "Blue") + geom_vline(xintercept = 69, col = "Red") + geom_vline(xintercept = 136, col = "Red") + geom_vline(xintercept = 210, col = "Red")

#Outlet_Establishment_Year
ggplot(combined, aes(x = Outlet_Establishment_Year)) + geom_density()
combined$Outlet_Establishment_Year = max(combined$Outlet_Establishment_Year) - combined$Outlet_Establishment_Year

#Imputing missing values
combined$Outlet_Size = as.character(combined$Outlet_Size)
for( i in 1:nrow(combined))
  if(combined$Outlet_Size[i] == "")
  {
    combined$Outlet_Size[i] = 'Small'
  }
combined$Outlet_Size = factor(combined$Outlet_Size)

#Item_visibility Imputing with mice packages
combined[combined$Item_Visibility == 0,]$Item_Visibility = NA
combined1 = combined[,1:11]
combined_imputed <- mice(combined1, m=5, maxit = 20, method = 'pmm', seed = 100)
combined <- complete(combined_imputed,2)
Visibility = combined %>% group_by(Outlet_Identifier) %>% summarise(count = n(), sum = sum(Item_Visibility))


#Feature Engineering
combined$Item_MRP2 = combined$Item_MRP ^ 2

new_train = combined[1:8523,]
new_test = combined[8524:14204,]
new_train$Item_Outlet_Sales = train$Item_Outlet_Sales

svr = svm(Item_Outlet_Sales ~ . -Item_MRP, kernel = "radial",scale = TRUE,data = new_train)
test_pred = predict(svr, newdata = new_test)
write.csv(test_pred, file ="Solution_SVR.csv")
