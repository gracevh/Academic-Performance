source("Train&Test.R")
library(DMwR) # SMOTE
library(magrittr) # to use %$%

#############################################
##  Unorder and Reorder Factors for SMOTE  ##
#############################################
# SMOTE will balance the data by generating new observations using kNN method; only needs to be applied to training datasets
# Must unorder factor variables to use SMOTE

# Function to unorder ordered train variables
unorder = function(data) {
 col.names = data %>% 
    as.data.frame() %>%
    select_if(is.factor) %>% #select factors
    colnames()
 temp = as.data.frame(lapply(data[, col.names], factor, ordered = F)) # unorder factors
 data %>% mutate(temp) # replace the df
}

# This function will reorder the numeric factor variables that should be ordered
# SMOTE destroys factor variables and turns all variables to numeric type integers or doubles

ord.list = Por %>% select(where(is.ordered)) %>% names() # first obtain list of original ordered factors

re.order = function(data, list.vars) {
  numeric.vars = data %>% Filter(is.numeric, .) %>% names()

  data %>%
    dplyr::select(!numeric.vars) %>% 
    dplyr::select(where(is.numeric)) %>%
    colnames %>% 
    data[, .] %>% 
    lapply(., ordered) %>%
    as_tibble(.) %>%
    mutate(data, .) 
}

tmp = balanced.PBT
numeric.vars = balanced.PBT %>% Filter(is.numeric, .) %>% names()
class(balanced.PBT)
class(balanced.PBT$age)
is.string(balanced.PBT$sex)
typeof(balanced.PBT$G1)

tmp2 = balanced.PBT %>% as_tibble() 

categorical = tmp %>%
  dplyr::select(!numeric.vars) %>% 
  transform(., 2, as.factor) %>% # use mighty transform() to coerce them back to factors
  as_tibble(.) 


# solution plus rose function below
otr[,numeric.vars] = round(otr[, numeric.vars])
otr[, numeric.vars] = apply(otr[, numeric.vars], 2, as.integer)
otr[, ord.list] = lapply(otr[, ord.list], ordered)







n = as.numeric(levels(balanced.PBT$Medu)[balanced.PBT$Medu])

# Synthetic Minority Over-Sampling Technique
# perc.over adds minority samples; perc.under removes majority samples; k is nearest neighbors generation
# Max of 100% of underrepresented data will be oversampled 
# Max of 2x the new underrepresented sample size sampled from majority unless 2x > N(majority)


#########################################
##  Balanced Portuguese Training Sets  ##
#########################################

# PorB test set
temp = unorder(PorB.tst$train)
otr = ROSE(G3 ~ ., data = as.data.frame(temp), seed = 230)$data %>% as_tibble()
# To parameterize sample size
  SMOTE(G3 ~ ., data = as.data.frame(temp), perc.over = 100, k = 5, perc.under = 400) %$%
  table(G3)
# Imbalanced initial dataset
table(PorB.tst$train$G3)

# Final selectiopn
balanced.PBT = SMOTE(G3 ~ ., data = as.data.frame(temp), perc.over = 100, k = 5, perc.under = 400)
  reorder(balanced.PBT)
# PorC test set
temp = unorder(PorC.tst$train)
# To parameterize sample size
SMOTE(G3 ~ ., data = as.data.frame(temp), perc.over = 100, k = 5, perc.under = 400) %$%
  table(G3)
# Imbalanced initial dataset
table(PorC.tst$train$G3)

# Final selectiopn
balanced.PCT = SMOTE(G3 ~ ., data = as.data.frame(temp), perc.over = 100, k = 5, perc.under = 400) 
  

# PorB validation set
temp = unorder(PorB.val$train)
# To parameterize sample size
SMOTE(G3 ~ ., data = as.data.frame(temp), perc.over = 100, k = 5, perc.under = 400) %$%
  table(G3)
# Imbalanced initial dataset
table(PorB.val$train$G3)

# Final selectiopn
balanced.PBV = SMOTE(G3 ~ ., data = as.data.frame(temp), perc.over = 100, k = 5, perc.under = 400)

# PorC validation set
temp = unorder(PorC.val$train)
# To parameterize sample size
SMOTE(G3 ~ ., data = as.data.frame(temp), perc.over = 100, k = 5, perc.under = 384) %$%
  table(G3)
# Imbalanced initial dataset
table(PorC.val$train$G3)

# Final selectiopn
balanced.PCV = SMOTE(G3 ~ ., data = as.data.frame(temp), perc.over = 100, k = 5, perc.under = 384) 



###################################
##  Balanced Math Training Sets  ##
###################################

# MatB test set
temp = unorder(MatB.tst$train)
# To parameterize sample size
SMOTE(G3 ~ ., data = as.data.frame(temp), perc.over = 100, k = 5, perc.under = 229) %$%
  table(G3)
# Imbalanced initial dataset
table(MatB.tst$train$G3)

# Final selectiopn
balanced.MBT = SMOTE(G3 ~ ., data = as.data.frame(temp), perc.over = 100, k = 5, perc.under = 229)

# PorC test set
temp = unorder(MatC.tst$train)
# To parameterize sample size
SMOTE(G3 ~ ., data = as.data.frame(temp), perc.over = 100, k = 5, perc.under = 400) %$%
  table(G3)
# Imbalanced initial dataset
table(MatC.tst$train$G3)

# Final selectiopn
balanced.MCT = SMOTE(G3 ~ ., data = as.data.frame(temp), perc.over = 100, k = 5, perc.under = 400) 


# PorB validation set
temp = unorder(MatB.val$train)
# To parameterize sample size
SMOTE(G3 ~ ., data = as.data.frame(temp), perc.over = 90, k = 5, perc.under = 210) %$%
  table(G3)
# Imbalanced initial dataset
table(MatB.val$train$G3)

# Final selectiopn
balanced.MBV = SMOTE(G3 ~ ., data = as.data.frame(temp), perc.over = 90, k = 5, perc.under = 210)

# PorC validation set
temp = unorder(MatC.val$train)
# To parameterize sample size
SMOTE(G3 ~ ., data = as.data.frame(temp), perc.over = 100, k = 5, perc.under = 355) %$%
  table(G3)
# Imbalanced initial dataset
table(MatC.val$train$G3)

# Final selectiopn
balanced.MCV = SMOTE(G3 ~ ., data = as.data.frame(temp), perc.over = 100, k = 5, perc.under = 355) 




