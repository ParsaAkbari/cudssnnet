##### HIGHLIGHT BELOW AND PRESS CTRL+ENTER #######
######### MODIFY THIS #############
# example: First and second layer both have 1 neuron, feel free to increase or add new
# layers
LAYERS <- c(1,1)

######### DONT MODIFY THIS ########
#install.packages("MASS")
#install.packages("neuralnet")
set.seed(500)
library(MASS)
data <- Boston

index <- sample(1:nrow(data),round(0.75*nrow(data)))

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=LAYERS,linear.output=T)
pr.nn <- compute(nn,test_[,1:13])
pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test_$medv)*(max(data$medv)-min(data$medv))+min(data$medv)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test_)
print(paste("Mean Standard Error of Model:", MSE.nn))
###########################################


#### HIGHLIGHT EACH LINE AND PRESS CTRL+ENTER TO RUN

# PLOT REAL VS PREDICTED DATA
plot(test_$medv,pr.nn$net.result,col='red',main='Real vs predicted NN',pch=18,cex=0.7, xlab="Real Data", ylab="Predicted by NNet")
abline(0,1,lwd=2)

# VIEW YOUR NEURAL NETWORK
plot(nn)




