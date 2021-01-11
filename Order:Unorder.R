source("Train&Test.R")
library(magrittr)
library(dplyr)
library(DMwR) # SMOTE

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
re.order = function(data) {
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

tmp = balanced.MBT
numeric.vars = balanced.MBT %>% Filter(is.numeric, .) %>% names()

balanced.MBT %>% as.data.frame() %>%
  dplyr::select(!numeric.vars) %>% 
  dplyr::select(where(is_integer)) %>%
  colnames %>% 
  tmp[, .] %>% 
  lapply(., ordered) %>%
  as_tibble(.) %>%
  mutate(tmp, .) 

is_integer(balanced.MBT$Medu) & length(balanced.MBT$Medu) < 2




# Synthetic Minority Over-Sampling Technique
# perc.over adds minority samples; perc.under removes majority samples; k is nearest neighbors generation
# Max of 100% of underrepresented data will be oversampled 
# Max of 2x the new underrepresented sample size sampled from majority unless 2x > N(majority)


#########################################
##  Balanced Portuguese Training Sets  ##
#########################################

# PorB test set
temp = unorder(PorB.tst$train)
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




