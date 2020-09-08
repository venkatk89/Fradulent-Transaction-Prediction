library(randomForest)

library(AUC)


a <- read.csv("train.csv")  # train data

test <- read.csv("test.csv")  # test data

b <- read.csv("sampleSubmission.csv")



a[,31]<- as.factor(a[,31])  # for classification




#building a logit model
model <- glm(a[,31] ~ ., family = binomial(link = 'logit'), data = a[,-31])   

plot(roc(predict(model,a[,-31]), a[,31]), main = "ROC") # plotting roc for logistic regression




#building the random forest model
set.seed(1234)                                                                 
rf_2 <- randomForest(x = a[,-31], y = a[,31], importance = TRUE, ntree = 100)   

varImpPlot(rf_2, main = "Variable Importance Plot") # variable importance plot

b[,2] <- predict(rf_2, test)
write.csv(b, "rf.csv", row.names = FALSE)





#Amount
s <- data.frame(a[which(a[,31] == 1),30]) # amount for fradulant transactions 

plot(y = s[,1], x = c(1:330), main = "Amount distribution for  fraudulent transactions", sub = "for Amount < 10", ylim = c(0,10) ,xlab = "Fraudulent Transactions", ylab = "Amount")




#Time
r <- data.frame(a[which(a[,31] == 1),1])  # time for fradulant transactions

r <- cbind(c(1:330), r)

km <- kmeans(r, center=3) # clustering time

plot(r,col=km$cluster,main = "Time distribution for  fraudulent transactions", xlab = "Fraudulent Transactions", ylab = "Time")






#v14
v14 <- data.frame(a[which(a[,31] == 1),15]) # v14 for fradulant transactions

plot(y = v14[,1], x = c(1:330), ylim = c(-20, 12), main = "v14 distribution for  fraudulent transactions", xlab = "Fraudulent Transactions", ylab = "v14")
plot(a[,15], ylim = c(-20, 12),main = "v14 distribution for  all transactions", xlab = "All Transactions", ylab = "v14" )



#v12
v12 <- data.frame(a[which(a[,31] == 1),13]) # v12 for fradulant transactions

plot(y = v12[,1], x = c(1:330), ylim = c(-19, 8), main = "v12 distribution for  fraudulent transactions", xlab = "Fraudulent Transactions", ylab = "v12")
plot(a[,13], ylim = c(-19, 8),main = "v12 distribution for  all transactions", xlab = "All Transactions", ylab = "v12" )



#v17
v17 <- data.frame(a[which(a[,31] == 1),18]) # v17 for fradulant transactions

plot(y = v17[,1], x = c(1:330), ylim = c(-24, 10), main = "v17 distribution for  fraudulent transactions", xlab = "Fraudulent Transactions", ylab = "v17")
plot(a[,18], ylim = c(-24, 10),main = "v17 distribution for  all transactions", xlab = "All Transactions", ylab = "v17" )


