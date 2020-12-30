##################################################################
##                       Subset Selection                       ##
##################################################################
library(leaps)
# Portu
# Forward Subset
sub.por = regsubsets(G3 ~ ., data=Por, nvmax = 32, method = "forward")
sum.sub.por = summary(sub.por)  

which.max(sum.sub.por$adjr2) # find the max adjusted r2 value index
sum.sub.por$adjr2[29] # index the value
coef(sub.por, 29) # the coefficient values corresponding to that model

plot(sum.sub.por$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsq",
     main = "Forward Stepwise Subset Selection for Portuguese Scores", type = "l")
abline(v=29, h=0.7211149, col="red", lty=2)

# Backward Subset
sub.por = regsubsets(G3 ~ ., data=Por, nvmax = 32, method = "backward")
sum.sub.por = summary(sub.por)  

which.max(sum.sub.por$adjr2) # find the max adjusted r2 value index
sum.sub.por$adjr2[28] # index the value
coef(sub.por, 28) # the coefficient values corresponding to that model

plot(sum.sub.por$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsq",
     main = "Forward Stepwise Subset Selection for Portuguese Scores", type = "l")
abline(v=28, h= 0.7207633, col="red", lty=2)



# Math
# Forward Subset
sub.mat = regsubsets(G3 ~ ., data=Mat, nvmax = 32, method="forward")
sum.mat = summary(sub.mat)  

which.max(sum.mat$adjr2)# find the max adjusted r2 value index
sum.mat$adjr2[30] # index the value
coef(sub.mat, 30) # the coefficient values corresponding to that model

plot(sum.mat$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsq",
     main = "Forward Stepwise Subset Selection for Math Scores", type = "l")
abline(v=30, h=0.7030638, col="red", lty=2)

# Backward Subset
sub.mat = regsubsets(G3 ~ ., data=Mat, nvmax = 32, method="backward")
sum.mat = summary(sub.mat)  

which.max(sum.mat$adjr2)# find the max adjusted r2 value index
sum.mat$adjr2[32] # index the value
coef(sub.mat, 32) # the coefficient values corresponding to that model

plot(sum.mat$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsq",
     main = "Forward Stepwise Subset Selection for Math Scores", type = "l")
abline(v=32, h=0.7019238, col="red", lty=2)
