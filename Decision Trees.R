##############################################
##  Decision Trees for Variable Importance  ##
##############################################
library(tree)
library(magrittr)

## Portuguese ##
set.seed(10)
p.tree = tree(G3 ~ ., data = Por.val$train)
plot(p.tree) # tree plot
text(p.tree) # text corresponding to tree

pred.y = predict(p.tree, newdata = Por.val$val)
mean((pred.y - unlist(Por.val$val.y))^2) # must unlist the y responses

p.tree %>% 
  cv.tree(.) %$% 
  size[which.min(dev)] # index the deviance for the best size of tree based on minimum deviance score

plot(cv.tree(p.tree)$size, cv.tree(p.tree)$dev, type = "l") # graph to visualize trend in deviance

prune.ptree = prune.tree(p.tree, best = 7) # put in best to prune the tree
plot(prune.ptree)
text(prune.ptree, pretty = 0)

prune.y = predict(prune.ptree, newdata = Por.val$val)
mean((prune.y - unlist(Por.val$val.y))^2) # must unlist the y responses
# Refer to graphed values to pick better tree size; recall bias-variance tradeoff

# Final test on pruned tree
pred.y = predict(prune.ptree, newdata = Por.val$test)
mean((pred.y - unlist(Por.val$test.y))^2)

# For categorical testing
# Binary Pass/Fail
set.seed(10)
p.tree = tree(G3 ~ ., data = PorB.val$train)
plot(p.tree)
text(p.tree)

pred.y = predict(p.tree, newdata = PorB.val$val, type = "class")
1 - mean(pred.y == unlist(PorB.val$val.y))

p.tree %>% 
  cv.tree(.) %$% 
  size[which.min(dev)] # index the deviance for the best size of tree based on minimum deviance score

plot(cv.tree(p.tree)$size, cv.tree(p.tree)$dev, type = "l") # graph to visualize trend in deviance

prune.ptree = prune.tree(p.tree, best = 4) 
prune.y = predict(prune.ptree, newdata = PorB.val$val, type = "class")
1 - mean(prune.y == unlist(PorB.val$val.y))
# Refer to graphed values to pick better tree size; recall bias-variance tradeoff

# Final test
pred.y = predict(prune.ptree, newdata = PorB.val$test, type = "class")
1 - mean(pred.y == unlist(PorB.val$test.y))

# Competitive comp/noncomp
set.seed(10)
p.tree = tree(G3 ~ ., data = PorC.val$train)
plot(p.tree)
text(p.tree)

pred.y = predict(p.tree, newdata = PorC.val$val, type = "class")
1 - mean(pred.y == unlist(PorC.val$val.y))

p.tree %>% 
  cv.tree(.) %$% 
  size[which.min(dev)] # index the deviance for the best size of tree based on minimum deviance score

plot(cv.tree(p.tree)$size, cv.tree(p.tree)$dev, type = "l") # graph to visualize trend in deviance

prune.ptree = prune.tree(p.tree, best = 4) 
prune.y = predict(prune.ptree, newdata = PorC.val$val, type = "class")
1 - mean(prune.y == unlist(PorC.val$val.y))
# Refer to graphed values to pick better tree size; recall bias-variance tradeoff

# Final test
pred.y = predict(prune.ptree, newdata = PorC.val$test, type = "class")
1 - mean(pred.y == unlist(PorC.val$test.y))


## Math ##
set.seed(10)
p.tree = tree(G3 ~ ., data = Mat.val$train)
plot(p.tree) # tree plot
text(p.tree) # text corresponding to tree

pred.y = predict(p.tree, newdata = Mat.val$val)
mean((pred.y - unlist(Mat.val$val.y))^2) # must unlist the y responses

p.tree %>% 
  cv.tree(.) %$% 
  size[which.min(dev)] # index the deviance for the best size of tree based on minimum deviance score

plot(cv.tree(p.tree)$size, cv.tree(p.tree)$dev, type = "l") # graph to visualize trend in deviance

prune.ptree = prune.tree(p.tree, best = 8) # put in best to prune the tree
plot(prune.ptree)
text(prune.ptree, pretty = 0)

prune.y = predict(prune.ptree, newdata = Mat.val$val)
mean((prune.y - unlist(Mat.val$val.y))^2) # must unlist the y responses
# Refer to graphed values to pick better tree size; recall bias-variance tradeoff

# Final test
pred.y = predict(prune.ptree, newdata = Mat.val$test)
mean((pred.y - unlist(Mat.val$test.y))^2)

# For categorical testing
# Binary Pass/Fail
set.seed(10)
p.tree = tree(G3 ~ ., data = MatB.val$train)
plot(p.tree)
text(p.tree)

pred.y = predict(p.tree, newdata = MatB.val$val, type = "class")
1 - mean(pred.y == unlist(MatB.val$val.y)) 

p.tree %>% 
  cv.tree(.) %$% 
  size[which.min(dev)] # index the deviance for the best size of tree based on minimum deviance score

plot(cv.tree(p.tree)$size, cv.tree(p.tree)$dev, type = "l") # graph to visualize trend in deviance

prune.ptree = prune.tree(p.tree, best = 4) 
prune.y = predict(prune.ptree, newdata = MatB.val$val, type = "class")
1 - mean(prune.y == unlist(MatB.val$val.y))
# Refer to graphed values to pick better tree size; recall bias-variance tradeoff

# Final test
pred.y = predict(prune.ptree, newdata = MatB.val$test, type = "class")
1 - mean(pred.y == unlist(MatB.val$test.y))

# Competitive comp/noncomp
set.seed(10)
p.tree = tree(G3 ~ ., data = MatC.val$train)
plot(p.tree)
text(p.tree)

pred.y = predict(p.tree, newdata = MatC.val$val, type = "class")
1 - mean(pred.y == unlist(MatC.val$val.y)) 

p.tree %>% 
  cv.tree(.) %$% 
  size[which.min(dev)] # index the deviance for the best size of tree based on minimum deviance score

plot(cv.tree(p.tree)$size, cv.tree(p.tree)$dev, type = "l") # graph to visualize trend in deviance

prune.ptree = prune.tree(p.tree, best = 3) 
prune.y = predict(prune.ptree, newdata = MatC.val$val, type = "class")
1 - mean(prune.y == unlist(MatC.val$val.y))
# Refer to graphed values to pick better tree size; recall bias-variance tradeoff

# Final test
pred.y = predict(prune.ptree, newdata = MatC.val$test, type = "class")
1 - mean(pred.y == unlist(MatC.val$test.y))
