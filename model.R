library(dplyr)
library(ggplot2)
library(caret)
library(reshape)
df <- read.csv("./heart_2020_cleaned.csv")
str(df)
summary(df)
df$HeartDisease <- as.factor(df$HeartDisease)
df$Smoking <- as.factor(df$Smoking)
df$AlcoholDrinking <- as.factor(df$AlcoholDrinking)
df$Stroke <- as.factor(df$Stroke)
df$DiffWalking <- as.factor(df$DiffWalking)
df$Sex <- as.factor(df$Sex)
df$AgeCategory <- as.factor(df$AgeCategory)
df$Race <- as.factor(df$Race)
df$Diabetic <- as.factor(df$Diabetic)
df$PhysicalActivity <- as.factor(df$PhysicalActivity)
df$GenHealth <- as.factor(df$GenHealth)
df$Asthma <- as.factor(df$Asthma)
df$KidneyDisease <- as.factor(df$KidneyDisease)
df$SkinCancer <- as.factor(df$SkinCancer)
summary(df)
df_clf <-df[1:10000,]
summary(df_clf)
set.seed(42)
train_index_clf <- createDataPartition(
  df_clf$HeartDisease,
  p = 0.8,
  list = FALSE,
  times = 1
)
train_set_clf <- df_clf[train_index_clf,]
test_set_clf <- df_clf[-train_index_clf, ]
table(train_set_clf$HeartDisease)
#Define repeated 10-fold cross validation.
fit_control <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 10
)
#Define a helper function to train model.
train_model_clf <- function(method = "",
                            data = train_set_clf,
                            tuneGrid = NULL,
                            tuneLength = 3) {
  set.seed(42)
  fit <- train(
    HeartDisease ~ .,
    data = data,
    method = method,
    trControl = fit_control,
    preProcess = c("center", "scale"),
    tuneGrid = tuneGrid,
    tuneLength = tuneLength
  )
  print(fit)
  return(fit)
}
#Define a helper function to evaluate model.
evaluate_model_clf <- function(model = NULL, name = "") {
  pred <- predict(model, test_set_clf)
  cm_fail <- confusionMatrix(pred, test_set_clf$HeartDisease, mode = "prec_recall")
  print(cm_fail)
  cm_pass <- confusionMatrix(
    pred,
    test_set_clf$HeartDisease,
    mode = "prec_recall",
    positive = "Yes"
  )
  return(data.frame(
    Model = name,
    Accuracy = round(cm_fail$overall[["Accuracy"]], 3),
    Precision.Fail = round(cm_fail$byClass[["Precision"]], 3),
    Recall.Fail = round(cm_fail$byClass[["Recall"]], 3),
    F1.Fail = round(cm_fail$byClass[["F1"]], 3),
    Precision.Pass = round(cm_pass$byClass[["Precision"]], 3),
    Recall.Pass = round(cm_pass$byClass[["Recall"]], 3),
    F1.Pass = round(cm_pass$byClass[["F1"]], 3)
  ))
}
lr_clf <- train_model_clf("glmnet")
lr_metrics_clf <- evaluate_model_clf(lr_clf, "Logistic Regression")
df_newdf<-df[36:36,]
df_newdf$HeartDisease<-""
df_newdf$Smoking<-"Yes"
df_newdf$AlcoholDrinking<-"Yes"
df_newdf$Stroke<-"Yes"
df_newdf$MentalHealth<-0
df_newdf$KidneyDisease<-"Yes"
test <- predict(lr_clf, newdata = df_newdf)
if(test[1]=="Yes"){
 "hello world"
  
}
token<-gs4_auth(email = gargle::gargle_oob_default())
saveRDS(token,file = "token.rds")
model <- readRDS("token.rds","rb")
gs4_auth(token=model)
