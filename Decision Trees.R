##############################################
##  Decision Trees for Variable Importance  ##
##############################################
library(tree)
library(magrittr)
reg.tree = function(tree, val, val.y, test, test.y, class = F, prune = NULL) {
  if (!class) {
    pred = predict(tree, newdata = val)
    val.error = mean((pred - val.y)^2)
    print(paste("Initial Validation Error:", val.error))
  } else {
    pred = predict(tree, newdata = val, type = "class")
    t = table(pred, unlist(val.y))
    val.error = 1 - mean(pred.y == unlist(val.y))
    print("Confusion Matrix:")
    print(t)
    print(paste("Initial Validation Error:", val.error))
  }
  
  plot(cv.tree(tree)$size, cv.tree(tree)$dev, type = "b") # graph to visualize trend in deviance
    
  if (is.null(prune)) {
      cv.tree = cv.tree(tree)
      min = which.min(cv.tree$dev)
      best.size = cv.tree$size[min]
      prune.tree = prune.tree(tree, best = best.size) # put in best to prune the tree
      print(paste("Prune best size:", best.size))
  } else {
      prune.tree = prune.tree(tree, best = prune) # put in best to prune the tree
  }
  
  if (!class) {
    prune.y = predict(prune.tree, newdata = val)
    prun.error = mean((prune.y - val.y)^2) 
    print(paste("Pruned Validation Error:", prun.error))
  } else {
    prune.y = predict(prune.tree, newdata = val, type = "class")
    t = table(prune.y, unlist(val.y))
    val.error = 1 - mean(prune.y == unlist(val.y))
    print("Confusion Matrix:")
    print(t)
    print(paste("Pruned Validation Error:", val.error))
  }
}

## Portuguese ##
# Decision Tree regression
# Train & Validation
set.seed(10)
tree = tree(G3 ~ ., data = Por.val$train)
reg.tree(tree = tree, val = Por.val$val, val.y = unlist(Por.val$val.y))

# Test
set.seed(10)
pred.y = Por.val$train %>% 
            tree(G3 ~ ., .) %>% 
            prune.tree(., best = 7) %>%
            predict(., newdata = Por.val$test) 

mean((pred.y - unlist(Por.val$test.y))^2)


# For categorical testing
# Binary Pass/Fail
set.seed(10)
tree = tree(G3 ~ ., data = balanced.PorB)
reg.tree(tree = tree, val = PorB.val$val, val.y = unlist(PorB.val$val.y), class = T)

# Test
set.seed(10)
pred.y = balanced.PorB %>% 
  tree(G3 ~ ., .) %>% 
  prune.tree(., best = 3) %>%
  predict(., newdata = Por.val$test, type = "class") 

1 - mean(pred.y == unlist(PorB.val$test.y))
t = table(pred.y, unlist(PorB.val$test.y)) ; t
# Specificity to Failing/Non-competitive students (the underrepresented class)
# Specificity = True/(True + False)
t[1,1]/colSums(t)[1] # for minority class
t[2,2]/colSums(t)[2] # for majority class

# Competitive comp/noncomp
set.seed(10)
tree = tree(G3 ~ ., data = balanced.PorB)
reg.tree(tree = tree, val = PorB.val$val, val.y = unlist(PorB.val$val.y), class = T)

# Test
set.seed(10)
pred.y = balanced.PorB %>% 
  tree(G3 ~ ., .) %>% 
  prune.tree(., best = 4) %>%
  predict(., newdata = Por.val$test, type = "class") 

1 - mean(pred.y == unlist(PorB.val$test.y))

## Math ##
# Decision Tree regression
# Train & Validation
set.seed(10)
tree = tree(G3 ~ ., data = Mat.val$train) 
reg.tree(tree = tree, val = Mat.val$val, val.y = unlist(Mat.val$val.y))

# Test
set.seed(10)
pred.y = Mat.val$train %>% 
  tree(G3 ~ ., .) %>% 
  prune.tree(., best = 7) %>%
  predict(., newdata = Mat.val$test) 

mean((pred.y - unlist(Mat.val$test.y))^2)

# For categorical testing
# Binary Pass/Fail
set.seed(10)
tree = tree(G3 ~ ., data = balanced.PorB)
reg.tree(tree = tree, val = PorB.val$val, val.y = unlist(PorB.val$val.y), class = T)

# Test
set.seed(10)
pred.y = balanced.PorB %>% 
  tree(G3 ~ ., .) %>% 
  prune.tree(., best = 4) %>%
  predict(., newdata = Por.val$test, type = "class") 

1 - mean(pred.y == unlist(PorB.val$test.y))

# Competitive comp/noncomp
set.seed(10)
tree = tree(G3 ~ ., data = balanced.PorB)
reg.tree(tree = tree, val = PorB.val$val, val.y = unlist(PorB.val$val.y), class = T)

# Test
set.seed(10)
pred.y = balanced.PorB %>% 
  tree(G3 ~ ., .) %>% 
  prune.tree(., best = 4) %>%
  predict(., newdata = Por.val$test, type = "class") 

1 - mean(pred.y == unlist(PorB.val$test.y))
