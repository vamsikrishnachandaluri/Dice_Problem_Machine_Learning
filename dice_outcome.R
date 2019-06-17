options(warn = -1)
#importing the libraries required
library(caret)
library(plyr)
library(e1071)
library(rpart.plot)

#Creating the train dataset which is named as dataset_1 Generating the 10000 random numbers and converting into data frame
outcomes <- sample(1:6, 10000, replace = TRUE)
dataset_1 <- data.frame(outcomes)
#set.seed(123)
outcome_list_1 <- c(table(dataset_1$outcomes))


#sorting and labelling based on their frequencies
outcome_conf_1 <- c()
outcome_conf_sorted <- c()
outcome_conf_sorted <- count(dataset_1, 'outcomes')
outcome_conf_sorted_1 <- outcome_conf_sorted[order(outcome_conf_sorted$freq), ]
prob_label_list <- c()
prob_label_list <- c('low_prob', 'low_prob', 'medium_prob', 'medium_prob', 'high_prob', 'high_prob')
#target_label_list <- c(0, 0, 1, 1, 2, 2)
prob_label <- c()
#target_attribute <- c()
for(i in 1:length(outcome_conf_sorted_1$outcomes)){
  outcome_conf_sorted_1$prob_label[i] <- prob_label_list[i]
  #outcome_conf_sorted_1$target_attribute[i] <- target_label_list[i]
}

for(i in 1:length(outcome_list_1)){
  outcome_conf_1[i] <- outcome_list_1[i]/1000
}

for(i in 1:length(dataset_1$outcomes)){
  if(dataset_1$outcomes[i] == 1){
    dataset_1$outcome_confidence[i] <- outcome_conf_1[1] 
    dataset_1$prob_label[i] <- outcome_conf_sorted_1$prob_label[outcome_conf_sorted_1$outcomes == 1]
    #dataset_1$target_attribute[i] <- outcome_conf_sorted_1$target_attribute[outcome_conf_sorted_1$outcomes == 1]
  }
  if(dataset_1$outcomes[i] == 2){
    dataset_1$outcome_confidence[i] <- outcome_conf_1[2] 
    dataset_1$prob_label[i] <- outcome_conf_sorted_1$prob_label[outcome_conf_sorted_1$outcomes == 2]
    #dataset_1$target_attribute[i] <- outcome_conf_sorted_1$target_attribute[outcome_conf_sorted_1$outcomes == 2]
    
  }
  if(dataset_1$outcomes[i] == 3){
    dataset_1$outcome_confidence[i] <- outcome_conf_1[3] 
    dataset_1$prob_label[i] <- outcome_conf_sorted_1$prob_label[outcome_conf_sorted_1$outcomes == 3]
    #dataset_1$target_attribute[i] <- outcome_conf_sorted_1$target_attribute[outcome_conf_sorted_1$outcomes == 3]
    
  }
  if(dataset_1$outcomes[i] == 4){
    dataset_1$outcome_confidence[i] <- outcome_conf_1[4] 
    dataset_1$prob_label[i] <- outcome_conf_sorted_1$prob_label[outcome_conf_sorted_1$outcomes == 4]
    #dataset_1$target_attribute[i] <- outcome_conf_sorted_1$target_attribute[outcome_conf_sorted_1$outcomes == 4]
    
  }
  if(dataset_1$outcomes[i] == 5){
    dataset_1$outcome_confidence[i] <- outcome_conf_1[5] 
    dataset_1$prob_label[i] <- outcome_conf_sorted_1$prob_label[outcome_conf_sorted_1$outcomes == 5]
    #dataset_1$target_attribute[i] <- outcome_conf_sorted_1$target_attribute[outcome_conf_sorted_1$outcomes == 5]
    
  }
  if(dataset_1$outcomes[i] == 6){
    dataset_1$outcome_confidence[i] <- outcome_conf_1[6] 
    dataset_1$prob_label[i] <- outcome_conf_sorted_1$prob_label[outcome_conf_sorted_1$outcomes == 6]
    #dataset_1$target_attribute[i] <- outcome_conf_sorted_1$target_attribute[outcome_conf_sorted_1$outcomes == 6]
    
  }
}

#Creating the test dataset which is named as dataset_2
outcomes <- sample(1:6, 10000, replace = TRUE)
dataset_2 <- data.frame(outcomes)

outcome_list_2 <- c(table(dataset_2$outcomes))

#sorting and labelling based on their frequencies
outcome_conf_2 <- c()
outcome_conf_sorted <- c()
outcome_conf_sorted <- count(dataset_2, 'outcomes')
outcome_conf_sorted_2 <- outcome_conf_sorted[order(outcome_conf_sorted$freq), ]
prob_label_list <- c()
prob_label_list <- c('low_prob', 'low_prob', 'medium_prob', 'medium_prob', 'high_prob', 'high_prob')
#target_label_list <- c(0, 0, 1, 1, 2, 2)
prob_label <- c()
#target_attribute <- c()
for(i in 1:length(outcome_conf_sorted_2$outcomes)){
  outcome_conf_sorted_2$prob_label[i] <- prob_label_list[i]
  #outcome_conf_sorted_1$target_attribute[i] <- target_label_list[i]
}

for(i in 1:length(outcome_list_1)){
  outcome_conf_2[i] <- outcome_list_2[i]/1000
}

for(i in 1:length(dataset_2$outcomes)){
  if(dataset_2$outcomes[i] == 1){
    dataset_2$outcome_confidence[i] <- outcome_conf_2[1] 
    dataset_2$prob_label[i] <- outcome_conf_sorted_2$prob_label[outcome_conf_sorted_2$outcomes == 1]
    #dataset_1$target_attribute[i] <- outcome_conf_sorted_1$target_attribute[outcome_conf_sorted_1$outcomes == 1]
  }
  if(dataset_2$outcomes[i] == 2){
    dataset_2$outcome_confidence[i] <- outcome_conf_2[2] 
    dataset_2$prob_label[i] <- outcome_conf_sorted_2$prob_label[outcome_conf_sorted_2$outcomes == 2]
    #dataset_1$target_attribute[i] <- outcome_conf_sorted_1$target_attribute[outcome_conf_sorted_1$outcomes == 2]
    
  }
  if(dataset_2$outcomes[i] == 3){
    dataset_2$outcome_confidence[i] <- outcome_conf_2[3] 
    dataset_2$prob_label[i] <- outcome_conf_sorted_2$prob_label[outcome_conf_sorted_2$outcomes == 3]
    #dataset_1$target_attribute[i] <- outcome_conf_sorted_1$target_attribute[outcome_conf_sorted_1$outcomes == 3]
    
  }
  if(dataset_2$outcomes[i] == 4){
    dataset_2$outcome_confidence[i] <- outcome_conf_2[4] 
    dataset_2$prob_label[i] <- outcome_conf_sorted_2$prob_label[outcome_conf_sorted_2$outcomes == 4]
    #dataset_1$target_attribute[i] <- outcome_conf_sorted_1$target_attribute[outcome_conf_sorted_1$outcomes == 4]
    
  }
  if(dataset_2$outcomes[i] == 5){
    dataset_2$outcome_confidence[i] <- outcome_conf_2[5] 
    dataset_2$prob_label[i] <- outcome_conf_sorted_2$prob_label[outcome_conf_sorted_2$outcomes == 5]
    #dataset_1$target_attribute[i] <- outcome_conf_sorted_1$target_attribute[outcome_conf_sorted_1$outcomes == 5]
    
  }
  if(dataset_2$outcomes[i] == 6){
    dataset_2$outcome_confidence[i] <- outcome_conf_2[6] 
    dataset_2$prob_label[i] <- outcome_conf_sorted_2$prob_label[outcome_conf_sorted_2$outcomes == 6]
    #dataset_1$target_attribute[i] <- outcome_conf_sorted_1$target_attribute[outcome_conf_sorted_1$outcomes == 6]
    
  }
}


dataset_1$prob_label <- as.factor(dataset_1$prob_label)
dataset_2$prob_label <- as.factor(dataset_2$prob_label)

#set.seed(123)







#using the caret package inbuilt function for modelling the svm classifications.

trctrl_svm <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
#set.seed(123)

svm_Linear <- train(prob_label ~., data = dataset_1, method = "svmLinear",
                    trControl=trctrl_svm,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Linear

test_pred_svm <- predict(svm_Linear, newdata = dataset_2)


#This will show the confusion matrix
confusionMatrix(test_pred_svm, dataset_2$prob_label)


#Visualization
library(ggplot2)
ggplot(outcome_conf_sorted_2) + aes(x = outcomes, y = prob_label, col = prob_label) + geom_point()