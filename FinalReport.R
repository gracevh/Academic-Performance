Por = read.csv("student-por.csv", sep = ";")
Mat = read.csv("student-mat.csv", sep = ";")

# Set up the variables 
fac.vars = c("Walc", "Dalc", "health", "goout", "famrel", "freetime", "traveltime", "studytime",
             "failures", "Medu", "Fedu")

Por[,fac.vars] = lapply(Por[,fac.vars], factor) 
Mat[,fac.vars] = lapply(Mat[,fac.vars], factor)

library(tidyverse)
Por = Por %>%
  mutate(G3.bin = as.factor(ifelse(Por$G3 >= 10, "pass", "fail")), 
         G3.comp = as.factor(ifelse(Por$G3 >= 15, "comp", "noncomp")))

Mat = Mat %>%
  mutate(G3.bin = as.factor(ifelse(Mat$G3 >= 10, "pass", "fail")), 
         G3.comp = as.factor(ifelse(Mat$G3 >= 15, "comp", "noncomp")))


# Remove the one highly correlated variable 
Mat = subset(Mat, select = -c(G2))
Por = subset(Por, select = -c(G2))


#################################################
##  Descriptive Statistics and Visualizations  ##
#################################################
library(ggplot2)
library(gridExtra)

G3por = ggplot(Por, aes(G3)) + geom_histogram(fill="turquoise4",color="black") + 
  ggtitle("Final Portuguese Scores, n=649") + ylim(c(0,110)) + xlab("Score") +
  theme_classic()
G3mat = ggplot(Mat, aes(G3)) + geom_histogram(fill="goldenrod3",color="black") +
  ggtitle("Final Math Scores, n=395") + ylim(c(0,110)) + xlab("Score") + 
  theme_classic()
# Plot of distribution of scores
grid.arrange(G3por,G3mat, ncol=2) + ggtitle("Distribution of Final Scores")

# Bar charts for categorical Gs
G3Cpor = ggplot(Por, aes(G3.comp)) + geom_bar(fill="turquoise4") +
  ggtitle("Final Portuguese Scores, n=649") + ylim(c(0,600)) + xlab("Competitiveness") +
  theme_classic()
G3Bpor = ggplot(Por, aes(G3.bin)) + geom_bar(fill="turquoise4") +
  ggtitle("Final Portuguese Scores, n=649") + ylim(c(0,600)) + xlab("Pass/Fail") +
  theme_classic()

G3Cmat = ggplot(Mat, aes(G3.comp)) + geom_bar(fill="goldenrod3") +
  ggtitle("Final Math Scores, n=395") + ylim(c(0,600)) + xlab("Competitiveness") +
  theme_classic()
G3Bmat = ggplot(Mat, aes(x=G3.bin)) + geom_bar(fill="goldenrod3") +
  ggtitle("Final Math Scores, n=395") + ylim(c(0,600)) + xlab("Pass/Fail") +
  theme_classic()

grid.arrange(G3Bpor,G3Cpor, G3Bmat, G3Cmat, ncol=2) + ggtitle("Distribution of Final Scores")

## Summary
out = capture.output(summary(Por))
cat("Portuguese Descriptive Summary", out, sep = "\n", append = T,
    file = "PorSum.txt")

out = capture.output(summary(Mat))
cat("Math Descriptive Summary", out, sep = "\n", append = T,
    file = "MatSum.txt")


library(GGally)
# A correlogram of the three test scores for both datasets 
ggpairs(Por[,31:33], title="Correlogram of Portuguese Test Scores G1 G2 & G3")
ggpairs(Mat[,31:33], title="Correlogram of Math Test Scores G1 G2 & G3")

##################################################################
##                  Multiple Linear Regression                  ##
##################################################################
library(car) # for vif()
full.fit = lm(G3 ~ ., data = Por[,1:32])
summary(full.fit)
vif(full.fit)
plot(full.fit)

reduced.fit = lm(G3 ~ G1 + health + Dalc + goout + failures + reason + Fjob + age,
                 data = Por[, 1:32])
summary(reduced.fit)
vif(reduced.fit)
plot(reduced.fit, which = 1)

select.fit = lm(G3 ~ G1 + studytime + health + Dalc + goout + failures + age + absences + famrel + romantic,
                data = Por[, 1:32])
summary(select.fit)
vif(select.fit)
plot(select.fit, which = 1)


full.fit = lm(G3 ~ ., data = Mat[,1:32])
summary(full.fit)
vif(full.fit)
plot(full.fit)

reduced.fit = lm(G3 ~  G1 +reason + Walc + Dalc + goout + schoolsup + romantic + studytime + age,
                 data = Mat[, 1:32])
summary(reduced.fit)
vif(reduced.fit)
plot(reduced.fit, which = 1)

select.fit = lm(G3 ~ G1 + studytime + health + Dalc + goout + failures + age + absences + famrel + romantic,
                data = Mat[, 1:32])
summary(select.fit)
vif(select.fit)
plot(select.fit, which = 1)


##################################################################
##                       Subset Selection                       ##
##################################################################
library(leaps)
# Portu
sub.por = regsubsets(G3 ~ ., data=Por[,1:32], nvmax = 32, method = "forward")
sum.sub.por = summary(sub.por)  

which.max(sum.sub.por$adjr2)
sum.sub.por$adjr2[27] 
coef(sub.por, 27)
plot(sum.sub.por$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsq",
     main = "Forward Stepwise Subset Selection for Portuguese Scores", type = "l")
abline(v=27, h=0.7201327, col="red", lty=2)


# Math
sub.mat = regsubsets(G3 ~ ., data=Mat[,1:32], nvmax = 32, method="forward")
sum.mat = summary(sub.mat)  

which.max(sum.mat$adjr2)
sum.mat$adjr2[32] 
coef(sub.mat, 32)
plot(sum.mat$adjr2, xlab = "Number of Variables", ylab = "Adjusted Rsq",
     main = "Forward Stepwise Subset Selection for Math Scores", type = "l")
abline(v=32, h=0.7016002, col="red", lty=2)

##############################################
##  Decision Trees for Variable Importance  ##
##############################################
library(tree)
# These sets are interchanged for given test to reduce total environment vars
## Portu ##
set.seed(10)
p.tree = tree(G3 ~ ., data=por.trn[,1:32])
plot(p.tree) # tree plot
text(p.tree)

yphat = predict(p.tree, newdata = por.val[,1:31])
test.G3 = por.val[,32] 
mean((yphat-test.G3)^2) 

cv.ptree = cv.tree(p.tree)
which.min(cv.ptree$dev) # find smallest value by size
cv.ptree$size[1] # find best # of leaves

prune.ptree = prune.tree(p.tree, best = 8) # put in best to prune the tree
plot(prune.ptree)
text(prune.ptree, pretty = 0)

y.prune = predict(prune.ptree, newdata = por.val[,1:31])
mean((y.prune-test.G3)^2) 

# final test
yphat = predict(prune.ptree, newdata = por.tst[,1:31])
test.G3 = por.tst[,32] 
mean((yphat-test.G3)^2)

# For categorical testing
set.seed(10)
p.tree = tree(G3.bin ~ ., data=por.trn[,-c(34,32)])
plot(p.tree)
text(p.tree)

yphat = predict(p.tree, newdata = por.val[,1:31], type = "class")
test.G3 = por.val[,33] 
1-mean(yphat==test.G3) 

cv.cptree = cv.tree(p.tree)
which.min(cv.cptree$dev)
cv.cptree$size[10]

prune.cptree = prune.tree(p.tree, best = 5) 
prune.ycphat = predict(prune.cptree, newdata = por.val[,1:31], type = "class")
1-mean(prune.ycphat==test.G3) 

yphat = predict(prune.cptree, newdata = por.tst[,1:31], type = "class")
test.G3 = por.tst[,33] 
1-mean(yphat==test.G3)

## Math ##
set.seed(10)
m.tree = tree(G3 ~ ., data=mat.trn[,1:32])
plot(m.tree)
text(m.tree)

ymhat = predict(m.tree, newdata = mat.val[,1:31])
test.G3 = mat.val$G3
mean((ymhat-test.G3)^2) 

cv.cmtree = cv.tree(m.tree)
which.min(cv.cmtree$dev)
cv.cmtree$size[8]

prune.mtree = prune.tree(m.tree, best = 4) 

y.prune = predict(prune.mtree, newdata = mat.val[,1:31])
mean((y.prune-test.G3)^2)

ymhat = predict(prune.mtree, newdata = mat.tst[,1:31])
test.G3 = mat.tst$G3
mean((ymhat-test.G3)^2)

# For categorical testing 
set.seed(10)
m.tree = tree(G3.comp ~ ., data=mat.trn[,-c(33,32)])
plot(m.tree)
text(m.tree)

ymhat = predict(m.tree, newdata = mat.val[,1:31], type="class")
test.G3 = mat.val$G3.comp
1-mean(ymhat==test.G3) 

cv.cmtree = cv.tree(m.tree)
which.min(cv.cmtree$dev)
cv.cmtree$size[5]

prune.cmtree = prune.tree(m.tree, best = 4) 
prune.ycmhat = predict(prune.cmtree, newdata = mat.val, type = "class")
1-mean(prune.ycmhat==test.G3) 

ymhat = predict(prune.cmtree, newdata = mat.tst[,1:31], type = "class")
test.G3 = mat.tst$G3.comp
1-mean(ymhat==test.G3)

#
"""
Categorical simple trees produced relatively high error rates
Numerical trees had noted MSEs 
"""

##################################################################
##                         RandomForest                         ##
##################################################################
library(randomForest)
# Por regression; mtry = p/3
set.seed(10)
rf.G3p = randomForest(G3 ~.-G3.bin-G3.comp, data = por.trn, mtry = 10)
yphat.rf = predict(rf.G3p, newdata = por.val)
test.G3 = por.val[,32] 
mean((yphat.rf - test.G3)^2)

opt.error = c() # loop through different m values 
for (i in 8:18){
  set.seed(10)
  rf.G3p = randomForest(G3 ~.-G3.bin-G3.comp, data = por.trn, mtry = i)
  yphat.rf = predict(rf.G3p, newdata = por.val)
  test.G3 = por.val[,32] 
  x = mean((yphat.rf - test.G3)^2)
  opt.error = append(opt.error, x)
}
df = data.frame(cbind(8:18,opt.error))
df$V1[which.min(df$opt.error)] # choose mvalue by smallest error

# Predictions on the test data set
rf.G3p = randomForest(G3 ~.-G3.bin-G3.comp, data = por.trn, mtry = 18)
yphat.rf = predict(rf.G3p, newdata = por.tst)
test.G3 = por.tst[,32] 
mean((yphat.rf - test.G3)^2)

# variable importance plot
varImpPlot(rf.G3p, main = "Portuguese RandomForest Regression on G3 (noG1)")

# Por for categorical bin sqrt(p)
set.seed(10)
rf.G3pbin = randomForest(G3.bin ~.-G3-G3.comp, data = por.trn, mtry = 5)
yphat.rf = predict(rf.G3pbin, newdata = por.val, type = "class")
test.G3 = por.val$G3.bin
1-mean(yphat.rf==test.G3)

# test
opt.error = c()
for (i in 1:12){
  set.seed(10)
  rf.G3pbin = randomForest(G3.bin ~.-G3-G3.comp, data = por.trn, mtry = i)
  yphat.rf = predict(rf.G3pbin, newdata = por.val, type = "class")
  test.G3 = por.val$G3.bin
  x = 1-mean(yphat.rf==test.G3)
  opt.error = append(opt.error, x)
}
df = data.frame(cbind(1:12,opt.error))
df$V1[which.min(df$opt.error)]

set.seed(10)
rf.G3pbin = randomForest(G3.bin ~.-G3-G3.comp, data = por.trn, mtry = 11)
yphat.rf = predict(rf.G3pbin, newdata = por.tst, type = "class")
test.G3 = por.tst$G3.bin 
1-mean(yphat.rf==test.G3)

# variable importance plot
varImpPlot(rf.G3pbin, main = "Portuguese RandomForest Classification on G3.bin")

# Por for categorical comp
set.seed(10)
rf.G3pcomp = randomForest(G3.comp ~., data = por.trn[,-c(32,33)], mtry = 5)
yphat.rf = predict(rf.G3pcomp, newdata = por.val[,1:31], type = "class")
test.G3 = por.val$G3.comp
1-mean(yphat.rf==test.G3)

opt.error = c()
for (i in 1:12){
  set.seed(10)
  rf.G3pcomp = randomForest(G3.comp ~., data = por.trn[,-c(32,33)], mtry = i)
  yphat.rf = predict(rf.G3pcomp, newdata = por.val[,1:31], type = "class")
  test.G3 = por.val$G3.comp
  x = 1-mean(yphat.rf==test.G3)
  opt.error = append(opt.error, x)
}
df = data.frame(cbind(1:12,opt.error))
df$V1[which.min(df$opt.error)]

set.seed(10)
rf.G3pcomp = randomForest(G3.comp ~., data = por.trn[,-c(32,33)], mtry = 12)
yphat.rf = predict(rf.G3pcomp, newdata = por.tst[,1:31], type = "class")
test.G3 = por.tst$G3.comp
1-mean(yphat.rf==test.G3)

# variable importance plot
varImpPlot(rf.G3pcomp, main = "Portuguese RandomForest Classification on G3.comp")


# Math regression; 
set.seed(10)
rf.G3m = randomForest(G3 ~., data = mat.trn[,1:32], mtry = 10)
ymhat.rf = predict(rf.G3m, newdata = mat.val[,1:31])
test.G3 = mat.val$G3
mean((ymhat.rf - test.G3)^2)

opt.error = c()
for (i in 8:18){
  set.seed(10)
  rf.G3m = randomForest(G3 ~., data = mat.trn[,1:32], mtry = 10)
  ymhat.rf = predict(rf.G3m, newdata = mat.val[,1:31])
  test.G3 = mat.val$G3
  x = mean((ymhat.rf - test.G3)^2)
  opt.error = append(opt.error, x)
}
df = data.frame(cbind(8:18,opt.error))
df$V1[which.min(df$opt.error)]

# Predictions on the test data set
set.seed(10)
rf.G3m = randomForest(G3 ~., data = mat.trn[,1:32], mtry = 8)
ymhat.rf = predict(rf.G3m, newdata = mat.tst[,1:31])
test.G3 = mat.tst$G3
mean((ymhat.rf - test.G3)^2)


# variable importance plot
varImpPlot(rf.G3m, main = "Math RandomForest Regression on G3")

# Math for categorical bin
set.seed(10)
rf.G3mbin = randomForest(G3.bin ~., data = mat.trn[,-c(32,34)], mtry = 5)
ymhat.rf = predict(rf.G3mbin, newdata = mat.val[,1:31], type = "class")
test.G3 = mat.val$G3.bin
1-mean(ymhat.rf==test.G3)

opt.error = c()
for (i in 1:12){
  set.seed(10)
  rf.G3mbin = randomForest(G3.bin ~., data = mat.trn[,-c(32,34)], mtry = 5)
  ymhat.rf = predict(rf.G3mbin, newdata = mat.val[,1:31], type = "class")
  test.G3 = mat.val$G3.bin
  x = 1-mean(ymhat.rf==test.G3)
  opt.error = append(opt.error, x)
}
df = data.frame(cbind(1:12,opt.error))
df$V1[which.min(df$opt.error)]

set.seed(10)
rf.G3mbin = randomForest(G3.bin ~., data = mat.trn[,-c(32,34)], mtry = 1)
ymhat.rf = predict(rf.G3mbin, newdata = mat.val[,1:31], type = "class")
test.G3 = mat.val$G3.bin
1-mean(ymhat.rf==test.G3)

# variable importance plot
varImpPlot(rf.G3mbin, main = "Math RandomForest Classification on G3.bin")

# math for categorical comp
set.seed(10)
rf.G3mcomp = randomForest(G3.comp ~., data = mat.trn[,-c(32,33)], mtry = 5)
ymhat.rf = predict(rf.G3mcomp, newdata = mat.val[,1:31], type = "class")
test.G3 = mat.val$G3.comp
1-mean(ymhat.rf==test.G3)

opt.error = c()
for (i in 1:12){
  set.seed(10)
  rf.G3mcomp = randomForest(G3.comp ~., data = mat.trn[,-c(32,33)], mtry = i)
  ymhat.rf = predict(rf.G3mcomp, newdata = mat.val[,1:31], type = "class")
  test.G3 = mat.val$G3.comp
  x = 1-mean(ymhat.rf==test.G3)
  opt.error = append(opt.error, x)
}
df = data.frame(cbind(1:12,opt.error))
df$V1[which.min(df$opt.error)]

set.seed(10)
rf.G3mcomp = randomForest(G3.comp ~., data = mat.trn[,-c(32,33)], mtry = 4)
ymhat.rf = predict(rf.G3mcomp, newdata = mat.val[,1:31], type = "class")
test.G3 = mat.val$G3.comp
1-mean(ymhat.rf==test.G3)

# variable importance plot
varImpPlot(rf.G3mcomp, main = "Math RandomForest Classification on G3.comp")

#################################################################
##                      Shrinkage Methods                      ##
#################################################################
# First create test sets and then turn into matrices for glmnet
set.seed(8)
por.idx = sample(1:nrow(Por[,1:32]), nrow(Por[,1:32])/4)
mat.idx = sample(1:nrow(Mat[,1:32]), nrow(Mat[,1:32])/4)

# train set
por.trn = Por[-por.idx,]
mat.trn = Mat[-mat.idx,]

# the validation and test sets
por.tst = Por[por.idx,]
mat.tst = Mat[mat.idx,]
# turn data into matrix format 
# train set
por.y = por.trn$G3
mat.y = mat.trn$G3
por.x = model.matrix(G3 ~ ., data=por.trn[,1:32])[,-1]
mat.x = model.matrix(G3 ~ ., data=mat.trn[,1:32])[,-1]

# test set
ptest.y = por.tst$G3
mtest.y = mat.tst$G3
ptest.x = model.matrix(G3 ~., data = por.tst[,1:32])[,-1]
mtest.x = model.matrix(G3 ~., data = mat.tst[,1:32])[,-1]
#################################################################
##                            LASSO                            ##
#################################################################
library(glmnet)
set.seed(1)
cv.por = cv.glmnet(por.x, por.y, alpha = 1, nfolds = 10)

pbest.lambda = cv.por$lambda.min
pbest.lambda 
p.coef = coef(cv.por, s = pbest.lambda)
round(p.coef,5) 

pLS.yhat = predict(cv.por, newx = ptest.x, s = pbest.lambda, type = "response")
mean((pLS.yhat-ptest.y)^2)

set.seed(1)
cv.mat = cv.glmnet(mat.x, mat.y, alpha = 1, nfolds = 10)

mbest.lambda = cv.mat$lambda.min
mbest.lambda 
m.coef = coef(cv.mat, s = mbest.lambda)
round(m.coef, 5)

mLS.yhat = predict(cv.mat, newx = mtest.x, s = mbest.lambda, type = "response")
mean((mLS.yhat-mtest.y)^2)


##################################################################
##                       Ridge Regression                       ##
##################################################################


set.seed(1)
rr.por = cv.glmnet(por.x, por.y, alpha=0, nfolds = 10)

rrpbest.lambda = rr.por$lambda.min
rrpbest.lambda 
rrpcoef = coef(rr.por, s = rrpbest.lambda)
round(rrpcoef, 4) 

pLS.yhat = predict(rr.por, newx = ptest.x, s = rrpbest.lambda, type = "response")
mean((pLS.yhat-ptest.y)^2)


set.seed(1)
rr.mat = cv.glmnet(mat.x, mat.y, alpha=0, nfolds = 10)

rrmbest.lambda = rr.mat$lambda.min
rrmbest.lambda 
rrmcoef = coef(rr.mat, s = rrmbest.lambda)
round(rrmcoef, 4)

mLS.yhat = predict(rr.mat, newx = mtest.x, s = rrmbest.lambda, type = "response")
mean((mLS.yhat-mtest.y)^2)

#################################################################
##                         Elastic Net                         ##
#################################################################
##alpha = 0.5
set.seed(1)
en.por = cv.glmnet(por.x, por.y, alpha=.5, nfolds = 10)

enpbest.lambda = en.por$lambda.min
enpbest.lambda 
enp.coef = coef(en.por, s = enpbest.lambda)
round(enp.coef, 4)

pLS.yhat = predict(en.por, newx = ptest.x, s = enpbest.lambda, type = "response")
mean((pLS.yhat-ptest.y)^2)


set.seed(1)
en.mat = cv.glmnet(mat.x, mat.y, alpha=.5, nfolds = 10)

enmbest.lambda = en.mat$lambda.min
enmbest.lambda 
enm.coef = coef(en.mat, s = enmbest.lambda)
round(enm.coef, 4)

mLS.yhat = predict(en.mat, newx = mtest.x, s = enmbest.lambda, type = "response")
mean((mLS.yhat-mtest.y)^2)


######## Summary of the three shrinkage methods #########
allp.coef = cbind(round(rrpcoef, 4), round(enp.coef, 4), round(p.coef, 4))
colnames(allp.coef) = c("Ridge", "Elastic", "LASSO")
# save the output
out = capture.output(allp.coef)
cat("Shrinkage Methods for Por -c(G2)", out, sep = "\n", append = T,
    file = "Por-Shrinkage.txt")

allm.coef = cbind(round(rrmcoef, 4), round(enm.coef, 4), round(m.coef, 4))
colnames(allm.coef) = c("Ridge", "Elastic", "LASSO")
allm.coef
# save the output
out = capture.output(allm.coef)
cat("Shrinkage Methods for Math -c(G2)", out, sep = "\n", append = T,
    file = "Math-Shrinkage.txt")

##################################################################
##           Logistic Regression and Cross Validation           ##
##################################################################

## Por bin
pb.glm = glm(G3.bin ~ Medu + famrel + higher + freetime + failures + absences + 
               goout + Walc  + health + G1, 
             data = por.trn[,-c(32,34)], family = "binomial")


contrasts(por.trn$G3.bin)
pb.prob = predict(pb.glm, type = "response")
pb.pred = rep("fail", dim(por.trn)[1])
pb.pred[pb.prob > 0.5] = "pass"
table(pb.pred, por.trn$G3.bin)
1 - mean(pb.pred == por.trn$G3.bin)

log.error = c()
k = seq(.3, .7, .02)
for (i in k) {
  pbval.prob = predict(pb.glm, newdata = por.val[,1:31], type = "response")
  pbval.yhat = ifelse(pbval.prob[] > i, "pass", "fail")
  table(pbval.yhat, por.val$G3.bin)
  x = 1 - mean(pbval.yhat == por.val$G3.bin)
  log.error = append(log.error, x)
}
df2 = data.frame(cbind(k, log.error)) # combine the values with the errors
p = df2$k[which.min(df2$log.error)] ; p # which min
match.p = df2$k[df2$log.error == df2$log.error[df2$k==p]] ; match.p # how many matched mins
sum(match.p)/length(match.p) # average the min probs  


pbval.prob = predict(pb.glm, newdata = por.tst[,1:31], type = "response")
pbval.yhat = ifelse(pbval.prob[] > 0.53, "pass", "fail")
table(pbval.yhat, por.tst$G3.bin)
1 - mean(pbval.yhat == por.tst$G3.bin)

## Por comp
pc.glm = glm(G3.comp ~ Walc + age  + Fjob + Medu + Mjob +
               goout + absences  + health + Fedu + G1, 
             data = por.trn[,-c(32,33)], family = "binomial")


contrasts(por.trn$G3.comp)
pc.prob = predict(pc.glm, type = "response")
pc.pred = rep("comp", dim(por.trn)[1])
pc.pred[pc.prob > 0.5] = "noncomp"
table(pc.pred, por.trn$G3.comp)
1 - mean(pc.pred == por.trn$G3.comp)

log.error = c()
k = seq(.3, .7, .02)
for (i in k) {
  pcval.prob = predict(pc.glm, newdata = por.val[,1:31], type = "response")
  pcval.yhat = ifelse(pcval.prob[] > i, "noncomp", "comp")
  table(pcval.yhat, por.val$G3.comp)
  x = 1 - mean(pcval.yhat == por.val$G3.comp)
  log.error = append(log.error, x)
}
df2 = data.frame(cbind(k, log.error))
p = df2$k[which.min(df2$log.error)] ; p
match.p = df2$k[df2$log.error == df2$log.error[df2$k==p]] ; match.p
sum(match.p)/length(match.p)

pcval.prob = predict(pc.glm, newdata = por.tst[,1:31], type = "response")
pcval.yhat = ifelse(pcval.prob[] > 0.69, "noncomp", "comp")
table(pcval.yhat, por.tst$G3.comp)
1 - mean(pcval.yhat == por.tst$G3.comp)


## Math bin
mb.glm = glm(G3.bin ~ Walc + age + Fjob + Mjob + Medu + 
               goout + absences  + health + failures + G1, 
             data = mat.trn[,-c(32,34)], family = "binomial")

mb.prob = predict(mb.glm, type = "response")
mb.pred = rep("fail", dim(mat.trn)[1])
mb.pred[mb.prob > .5] = "pass"
table(mb.pred, mat.trn$G3.bin)
1 - mean(mb.pred == mat.trn$G3.bin)

log.error = c()
k = seq(.3, .7, .02)
for (i in k) {
  mbval.prob = predict(mb.glm, newdata = mat.val[,1:31], type = "response")
  mbval.yhat = ifelse(mbval.prob > i, "pass", "fail")
  table(mbval.yhat, mat.val$G3.bin)
  x = 1 - mean(mbval.yhat == mat.val$G3.bin)
  log.error = append(log.error, x)
}
df2 = data.frame(cbind(k, log.error))
p = df2$k[which.min(df2$log.error)] ; p
match.p = df2$k[df2$log.error == df2$log.error[df2$k==p]] ; match.p
sum(match.p)/length(match.p)

mbval.prob = predict(mb.glm, newdata = mat.tst[,1:31], type = "response")
mbval.yhat = ifelse(mbval.prob > .7, "pass", "fail")
table(mbval.yhat, mat.tst$G3.bin)
1 - mean(mbval.yhat == mat.tst$G3.bin)

## Math comp
mc.glm = glm(G3.comp ~ Walc + freetime + Fedu + Mjob + studytime + Medu + 
               goout + absences  + health + G1, 
             data = mat.trn[,-c(32,33)], family = "binomial")

mc.prob = predict(mc.glm, type = "response")
mc.pred = rep("comp", dim(mat.trn)[1])
mc.pred[mc.prob > 0.5] = "noncomp"
table(mc.pred, mat.trn$G3.comp)
1 - mean(mc.pred == mat.trn$G3.comp)

log.error = c()
k = seq(.3, .7, .02)
for (i in k) {
  mcval.prob = predict(mc.glm, newdata = mat.val[,1:31], type = "response")
  mcval.yhat = ifelse(mcval.prob > i, "noncomp", "comp")
  x = 1 - mean(mcval.yhat == mat.val$G3.comp)
  log.error = append(log.error,x)
}
df2 = data.frame(cbind(k, log.error))
p = df2$k[which.min(df2$log.error)] ; p
match.p = df2$k[df2$log.error == df2$log.error[df2$k==p]] ; match.p 
sum(match.p)/length(match.p)

mcval.prob = predict(mc.glm, newdata = mat.tst[,1:31], type = "response")
mcval.yhat = ifelse(mcval.prob > 0.5, "noncomp", "comp")
table(mcval.yhat, mat.tst$G3.comp)
1 - mean(mcval.yhat == mat.tst$G3.comp)




