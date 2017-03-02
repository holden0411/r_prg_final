#匯入資料
titanic <- read.csv("https://storage.googleapis.com/r_rookies/kaggle_titanic_train.csv")
#ㄍ觀察資料外框
str(titanic)
#描述性統計
summary(titanic)
#補充遺漏值
titanic <- titanic[complete.cases(titanic), ]
titanic$Survived <- factor(titanic$Survived)
titanic$Embarked <- as.character(titanic$Embarked)
titanic$Embarked[titanic$Embarked == ""] <- "S"
titanic$Embarked <- factor(titanic$Embarked)
#繪圖前置作業
library(ggplot2)
library(plotly)
# 性別
ggplot_bar_sex <- ggplot(titanic, aes(x = Sex, y = Survived, fill = Sex)) + geom_bar(stat = "identity")
ggplot_bar_sex_plotly <- ggplotly(ggplot_bar_sex)
ggplot_bar_sex_plotly
# Pclass
ggplot_bar_pclass <- ggplot(titanic, aes(x = factor(Pclass), y = Survived, fill = factor(Pclass))) + geom_bar(stat = "identity", width = .7)
ggplot_bar_pclass_plotly <- ggplotly(ggplot_bar_pclass)
ggplot_bar_pclass_plotly
#建立分類模型
# 切分訓練與測試資料
set.seed(87)
n <- nrow(titanic)
shuffled_titanic <- titanic[sample(n), ]
train_indices <- 1:round(0.7 * n)
train <- shuffled_titanic[train_indices, ]
test_indices <- (round(0.7 * n) + 1):n
test <- shuffled_titanic[test_indices, ]

# 建立分類器
library(randomForest)
rf_clf <- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, ntree = 100)

# 計算 accuracy
prediction <- predict(rf_clf, test[, c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")])
confusion_matrix <- table(test$Survived, prediction)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy

#探索沒有答案資料
url <- "https://storage.googleapis.com/py_ds_basic/kaggle_titanic_test.csv"
to_predict <- read.csv(url)
summary(to_predict)

#填補遺漏值
library(dplyr)
library(magrittr)
# Fare
fare_mean <- mean(to_predict$Fare, na.rm = TRUE)
to_predict$Fare[is.na(to_predict$Fare)] <- fare_mean

# Age
mean_age_by_Pclass <- to_predict %>%
  group_by(Pclass) %>%
  summarise(mean_age = round(mean(Age, na.rm = TRUE)))
filter_1 <- is.na(to_predict$Age) & to_predict$Pclass == 1
filter_2 <- is.na(to_predict$Age) & to_predict$Pclass == 2
filter_3 <- is.na(to_predict$Age) & to_predict$Pclass == 3
mean_age_by_Pclass

to_predict[filter_1, ]$Age <- 41
to_predict[filter_2, ]$Age <- 29
to_predict[filter_3, ]$Age <- 24

# Summary after imputation
summary(to_predict)

#準備上傳

predicted <- predict(rf_clf, newdata = to_predict[, c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")])
to_submit <- data.frame(to_predict[, "PassengerId"], predicted)
names(to_submit) <- c("PassengerId", "Survived")
head(to_submit, n = 10)
#輸出
write.csv(to_submit, file = "to_submit.csv", row.names = FALSE)
#查詢位置
getwd()