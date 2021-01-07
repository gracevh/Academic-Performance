## Train, Validation, Test split

# This function will be stored as a list that can be indexed for the dataframes and vectors
# they are stored in the order of the function
val.split = function(data, y, train.per, test.per) {
  loc = grep(y, colnames(data)) # locate the index of the response variable by its column name
  index = sample(1:nrow(data), nrow(data)*train.per) # create index for train dataset
  train = data[index,] # dataframe 
  rm = data[-index,] # use remaining index for test and val dataset
  index2 = sample(1:nrow(rm), nrow(rm)*test.per) # test percentage is index
  test = rm[index2, -loc] # remove the response
  test.y = rm[index2, loc] # vector of test responses
  val = rm[-index2, -loc] # remaining index is validation percentage
  val.y = rm[-index2, loc]
  return(list(train = train, test = test, test.y = test.y, val = val, val.y = val.y)) # named lists
}

# This function will be stored as a list and is for simple train-test split data
test.split = function(data, y, train.per) {
  loc = grep(y, colnames(data)) # locate the index of the response variable by its column name
  index = sample(1:nrow(data), nrow(data)*train.per) # create index for train dataset
  train = data[index,] # dataframe 
  test = data[-index, -loc] # use remaining index for test dataset & remove response variable
  test.y = data[-index, loc] # vector of test responses
  return(list(train = train, test = test, test.y = test.y)) # named lists
}

# Use these variables for desired datasets for train test and validation
set.seed(8)
Por.val = val.split(Por, c("G3"), .5, .5) # train validate test datasets
PorB.val = val.split(PorB, c("G3"), .5, .5)
PorC.val = val.split(PorC, c("G3"), .5, .5)

Mat.val = val.split(Mat, c("G3"), .5, .5) 
MatB.val = val.split(MatB, c("G3"), .5, .5) 
MatC.val = val.split(MatC, c("G3"), .5, .5) 


set.seed(8)
Por.tst = test.split(Por, c("G3"), .5) # train test datasets
PorB.tst = test.split(PorB, c("G3"), .5)
PorC.tst = test.split(PorC, c("G3"), .5)

Mat.tst = test.split(Mat, c("G3"), .5)
MatB.tst = test.split(MatB, c("G3"), .5)
MatC.tst = test.split(MatC, c("G3"), .5)

# Synthetic Minority Over-Sampling Technique
# perc.over adds minority samples; perc.under removes majority samples; k is nearest neighbors generation

# Balanced Test train datasets
balanced.PorBt = SMOTE(G3 ~ ., data = as.data.frame(PorB.tst$train), perc.over = 100, k = 5, perc.under = 100)
balanced.PorCt = SMOTE(G3 ~ ., data = as.data.frame(PorC.tst$train), perc.over = 100, k = 5, perc.under = 100)

balanced.MatBt = SMOTE(G3 ~ ., data = as.data.frame(MatB.tst$train), perc.over = 100, k = 5, perc.under = 100)
balanced.MatCt = SMOTE(G3 ~ ., data = as.data.frame(MatC.tst$train), perc.over = 100, k = 5, perc.under = 100)

# Balanced Validation train datasets
balanced.PorB = SMOTE(G3 ~ ., data = as.data.frame(PorB.val$train), perc.over = 100, k = 5, perc.under = 100)
balanced.PorB = SMOTE(G3 ~ ., data = as.data.frame(PorC.val$train), perc.over = 100, k = 5, perc.under = 100)

balanced.MatB = SMOTE(G3 ~ ., data = as.data.frame(MatB.val$train), perc.over = 100, k = 5, perc.under = 100)
balanced.MatC = SMOTE(G3 ~ ., data = as.data.frame(MatC.val$train), perc.over = 100, k = 5, perc.under = 100)


