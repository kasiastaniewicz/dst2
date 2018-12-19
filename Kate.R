      kddata <- read.csv("kddcup.data_10_percent_corrected")
      kddnames=read.table("kddcup.names",sep=":",skip=1,as.is=T)
      colnames(kddata)=c(kddnames[,1],"normal")

      kddata$effect <- kddata$normal
      levels(kddata$effect)

#Adding the effect column
  levels(kddata$effect)[which(levels(kddata$effect) =='normal.')] <- "nothing"
  levels(kddata$effect)[which(levels(kddata$effect) =='back.'|levels(kddata$effect) =='land.'|levels(kddata$effect) =='neptune.'|levels(kddata$effect) =='smurf.'|levels(kddata$effect) =='pod.')] <- 'slows'
  levels(kddata$effect)[which(levels(kddata$effect) =='teardrop.')] <- "reboots"
  levels(kddata$effect)[which(levels(kddata$effect) =='loadmodule.'|levels(kddata$effect) =='buffer_overflow.'|levels(kddata$effect) =='rootkit.'|levels(kddata$effect) =='perl.')] <- 'root_shell'
  levels(kddata$effect)[which(levels(kddata$effect) =='phf.')] <- "executes_commands_root"
  levels(kddata$effect)[which(levels(kddata$effect) =='guess_passwd.'|levels(kddata$effect) =='warezmaster.'|levels(kddata$effect) =='ftp_write.'|levels(kddata$effect) =='spy.'|levels(kddata$effect) =='warezclient.')]<-'user_access'
  levels(kddata$effect)[which(levels(kddata$effect) =='satan.')] <- "looks_known_vulnerabilities"
  levels(kddata$effect)[which(levels(kddata$effect) =='ipsweep.')] <- "identifies_active_machines"
  levels(kddata$effect)[which(levels(kddata$effect) =='multihop.'|levels(kddata$effect) =='imap.')] <- "root_access"
  levels(kddata$effect)[which(levels(kddata$effect) =='portsweep.'|levels(kddata$effect) =='nmap.')] <- "identifies_active_ports"
  

#set service col to three levels:private=1,http=2,others=3
    levels(kddata$service)[which(levels(kddata$service) != "http" & levels(kddata$service) != "private")] <- "others"
    kddata$service<-as.numeric(factor(kddata$service,levels = c("private","http","others")))

    kddata <- kddata[-21]
    kddata <- kddata[-20]
    kddata$effect = factor(kddata$effect, labels = c(1:10))
    kddata$protocol_type = factor(kddata$protocol_type, levels = c('icmp', 'tcp', 'udp'), labels = c(1, 2, 3))
    kddata$flag = factor(kddata$flag, labels = c(1:11))

#train and test
    datasample<-sample(seq_len(nrow(kddata)),size=floor(.20*nrow(kddata)))
    train<-kddata[datasample,]
    test<-kddata[-datasample,]

# I saved the training and testing that includes the effect column
    write.csv(train,file="trainS.csv")
    write.csv(test,file="testS.csv")

#HERE I USUALLY START
  setwd("~/Documents/Data Science/Week 1")
  trainS <- read.csv("~/Documents/Data Science/Week 1/trainS.csv")
  testS <- read.csv("~/Documents/Data Science/Week 1/testS.csv")

library(tidyr)
library(dplyr)
library(data.table)
library(caTools)
library(randomForest)
library(ggplot2)
library(lattice)
library(e1071)
library(caret)
library(MASS)
library(rpart)

#Here I select only a few columns
  train2<-trainS[,c("effect","dst_bytes","logged_in","dst_host_count","protocol_type")]
  test2<-testS[,c("effect","dst_bytes","logged_in","dst_host_count","protocol_type")]

#I had to take a smaller sample because my computer breaks.
  indexA <- sample(1:nrow(train2), 2000)
  T2<-train2[indexA,]

  indexE <- sample(1:nrow(test2), 2000)
  TE2<-test2[indexE,]
#T2 is the training data and TE2 is the test data

  
#WHOLE DATASET
#Kate2 <-randomForest(effect ~., data=trainS)

  
#Kate1 is the random forest model on less variables
  Kate1 <- randomForest(effect ~., data=T2)

#nice should show the forest
  nice=rpart(effect ~., data=T2)
  plot(nice)

#We show how number of trees devreses the error
  plot(Kate1)


# The expected values are not integres, so i round them
  expe=round(expected,0)
  conf<-table(expe,TE2$effect)
  conf
  accuracy_Test <- sum(diag(conf)) / sum(conf)
  accuracy_Test

#Variable importance
varImpPlot(Kate1,  
           sort = T,
           n.var=10,
           main="Top 10 - Variable Importance")


#I cant do adaboost because it works only on 2 levels of effect and we have 7
#Try it out with Syd 
  library("fastAdaboost")
  ada <- adaboost(effect ~ ., T2, nIter = 5)

#Bagging
  library("rpart")
  
##Training of the bagged model
  n_model=10
  bagged_models=list()

for (i in 1:n_model)
{
  new_sample=sample(indexA,size=1000,replace=T)
  bagged_models=c(bagged_models,list(rpart(effect~.,TE2[new_sample,],control=rpart.control(minsplit=6))))
}

#Since TE2 is the testing data      
  bagging_data=TE2

##Getting estimate from the bagged model
  bagged_result=NULL
  i=0
for (from_bag_model in bagged_models)
{
  if (is.null(bagged_result))
    bagged_result=predict(from_bag_model,bagging_data)
  else
    bagged_result=(i*bagged_result+predict(from_bag_model,bagging_data))/(i+1)
  i=i+1
}

#Round the values
  BB=round(bagged_result,0)
  plot(BB)
  
#Confusion matrix for Bagging
  confBAG<-table(BB,TE2$effect)
  accuracy_Test_BAG <- sum(diag(confBAG)) / sum(confBAG)
  accuracy_Test_BAG



