setwd("~/Desktop/Courses_Online/Data_Science_Specialization/08_PracticalMachineLearning/08_august/course_project_github")
rm( list = ls() )

# Read the data from file
training <- read.csv("pml-training.csv", na.strings = c("NA",""))
testing  <- read.csv("pml-testing.csv" , na.strings = c("NA",""))

# Cleaning the data
columns_with_NA <- colSums(is.na(testing))
training <- training[, !columns_with_NA ]
testing  <- testing[ , !columns_with_NA ]
rm("columns_with_NA")
   
# Select variables:
#   roll_belt,     gyros_belt_x  , accel_belt_x
#   roll_arm,      pitch_arm     , yaw_arm
#   roll_forearm,  pitch_forearm , yaw_forearm
#   roll_dumbbell, pitch_dumbbell, yaw_dumbbell
my_vars <- c(8,12,21,22,23,47,48,49, 34, 35, 36)
train_subset <- training[,c(60, my_vars)]
test_subset  <- testing[ ,c(    my_vars)]
rm(my_vars)

# Cross Validation
library(caret)
set.seed(1)
K = 4           # it takes too long in my computer...
cv_subsets <- createFolds( y=train_subset$classe, k = K, list=TRUE, returnTrain=TRUE)
for (k in 1:K) {
  cvk_train <- train_subset[ cv_subsets[[k]],]
  cvk_test  <- train_subset[-cv_subsets[[k]],]
  
  # I use the "gbm" method to train the classifiers
  cvk_model  <- train( classe ~ ., method="gbm", data=cvk_train, verbose=FALSE)
  cvk_classe <- predict(cvk_model, cvk_test)
  cvk_acc    <- sum(cvk_classe == cvk_test$classe)/length(cvk_classe)
  
  if (k==1) {
    cv_models   <- list(cvk_model)
    cv_accuracy <- list(cvk_acc)
  } else {
    cv_models   <- c(cv_models,   list(cvk_model))
    cv_accuracy <- c(cv_accuracy, list(cvk_acc)  )
  }
} 

# out of sample accuracy
plot(y=cv_accuracy,x=1:K)
mean(cv_accuracy)

# Submission information
predicted_classe <- list( predict(cv_models[[k]], test_subset) )

# CP submission
answers = as.character( predicted_classe )
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}
pml_write_files(answers)
