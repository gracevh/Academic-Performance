##################################################################
##                  Multiple Linear Regression                  ##
##################################################################
library(magrittr)
library(car) # for vif()
# Function to report model summary, variance inflation factors, and residual plot
sumfit = function(model) {
  print(vif(model))
  print(summary(model))
  plot(model, which = 1)
}

## POR dataset
# Full model
Por %>% lm(G3 ~ ., data = .) %>% sumfit()

# Reduced fit from full model
Por %>% lm(G3 ~ G1 + health + Dalc + famrel + goout + failures + reason + Fjob + age,
                 data = .) %>%
  sumfit()

# Selected fit based on factors of interest to investigator
Por %>% lm(G3 ~ G1 + studytime + health + Dalc + goout + failures + age + absences + famrel + romantic,
                data = .) %>%
  sumfit()


## MAT dataset
# Full model
Mat %>% lm(G3 ~ ., data = .) %>% sumfit()

# Reduced fit from full model
Mat %>% lm(G3 ~  G1 + absences + Walc + goout + schoolsup + romantic + studytime + reason + Medu + age,
                 data = .) %>%
  sumfit()

# Selected fit based on factors of interest to investigator
Mat %>% lm(G3 ~ G1 + studytime + health + Dalc + goout + failures + age + absences + famrel + romantic,
                data = .) %>% 
  sumfit()

