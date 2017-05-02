# DESCALE THE RESULTS SO THAT THEY ARE COMPARABLE WITH REAL LIFE
pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)
medv_ <- test_$medv*(max(data$medv)-min(data$medv))+min(data$medv)
plot(medv_,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
