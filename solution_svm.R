#Loading Libraries
library(dplyr)
library(ggplot2)
library(e1071) 
library(tidyr)
library(lubridate)

#Loading Train and test dataset
train = read.csv("train.csv", stringsAsFactors = FALSE)
test = read.csv("test.csv", stringsAsFactors = FALSE)
full = bind_rows(train, test)

full$datetime = ymd_hms(full$datetime)
full = separate(full, col = "datetime", into = c("date", "time"),sep = " ")
full = separate(full, col = "date", into = c("year", "month", "date"),sep = "-")
full = separate(full, col = "time", into = c("hour", "min", "sec"),sep = ":")

full = full[, -c(1,6,7)]

full$year = factor(full$year)
full$month = factor(full$month)
full$date = factor(full$date)
full$hour = factor(full$hour)
full$var2 = factor(full$var2)

ggplot(full, aes(x = temperature, y = electricity_consumption, col = factor(year))) + geom_point()
ggplot(full, aes(x = electricity_consumption, col = factor(year))) + geom_histogram()
ggplot(full, aes(x= hour, y = electricity_consumption, col = factor(year))) + geom_point()

train = full[1:26496, -3]
test = full[26497:35064,1:9]
test = test[, -3]

model_svm = svm(electricity_consumption ~ ., data = train)
y_pred_svm = predict(model_svm, newdata = test)
write.csv(y_pred_svm, "solution_svm.csv")