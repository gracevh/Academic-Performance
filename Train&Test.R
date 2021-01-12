source("Clean&Visualize.R")
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


######################################
##  50-25-25 Train Validation Test  ##
######################################

# Note the unbalanced class distributions after the splitting with table()

set.seed(8)
Por.val = val.split(Por, c("G3"), .5, .5) # train validate test datasets
PorB.val = val.split(PorB, c("G3"), .5, .5)
PorC.val = val.split(PorC, c("G3"), .5, .5)

set.seed(8)
Mat.val = val.split(Mat, c("G3"), .5, .5) 
MatB.val = val.split(MatB, c("G3"), .5, .5) 
MatC.val = val.split(MatC, c("G3"), .5, .5) 


########################
##  50-50 Train Test  ##
########################

set.seed(8)
Por.tst = test.split(Por, c("G3"), .5) 
PorB.tst = test.split(PorB, c("G3"), .5)
PorC.tst = test.split(PorC, c("G3"), .5)

set.seed(8)
Mat.tst = test.split(Mat, c("G3"), .5)
MatB.tst = test.split(MatB, c("G3"), .5)
MatC.tst = test.split(MatC, c("G3"), .5)


