Por = read.csv("student-por.csv", sep = ";")
Mat = read.csv("student-mat.csv", sep = ";")

library(tidyverse)
library(tidyr)
library(dplyr)
Por = Por %>% as_tibble()
Mat = Mat %>% as_tibble()

# only numeric variables are age, absences, G1, G2, G3
# check that it is coded so
names(Filter(is.numeric, Por)) 
names(Filter(is.numeric, Mat))

num.vars = c("age", "absences", "G1", "G2", "G3")

# convert non-numerics to factor variables
# Once converted to factor, a variable with type integer will respond FALSE to boolean is.integer
# Must isolate those variables before factor conversion

# convert the numeric factor variables to ordered factors
Por = Por %>% 
  select(!num.vars) %>% 
  select(where(is.numeric)) %>%
  colnames %>% 
  Por[, .] %>% 
  lapply(., ordered) %>%
  as_tibble(.) %>%
  mutate(Por, .)
  

Mat = Mat %>% 
  select(!num.vars) %>% 
  select(where(is.numeric)) %>%
  colnames %>% 
  Mat[, .] %>% 
  lapply(., ordered) %>%
  as_tibble(.) %>%
  mutate(Mat, .)

# check that it worked 
is.factor(Mat$Medu)
levels(Mat$Medu)
Mat$Medu



library(GGally)
# A correlogram of the three test scores for both datasets 
ggpairs(Por[,31:33], title="Correlogram of Portuguese Test Scores G1 G2 & G3")
ggpairs(Mat[,31:33], title="Correlogram of Math Test Scores G1 G2 & G3")

# Remove one highly correlated variable 
Mat = subset(Mat, select = -c(G2))
Por = subset(Por, select = -c(G2))

# Create new datasets with new binned response variables
PorB = Por %>%
  mutate(G3.bin = as.factor(ifelse(Por$G3 >= 10, "pass", "fail"))) %>%
  subset(., select = -G3) %>%
  rename(., G3 = G3.bin) # rename the new binned var as G3

PorC = Por %>%
  mutate(G3.comp = as.factor(ifelse(Por$G3 >= 15, "comp", "noncomp"))) %>%
  subset(., select = -G3) %>%
  rename(., G3 = G3.comp)


MatB = Mat %>%
  mutate(G3.bin = as.factor(ifelse(Mat$G3 >= 10, "pass", "fail"))) %>%
  subset(., select = -G3) %>%
  rename(., G3 = G3.bin)

MatC = Mat %>%
  mutate(G3.comp = as.factor(ifelse(Mat$G3 >= 15, "comp", "noncomp"))) %>%
  subset(., select = -G3) %>%
  rename(., G3 = G3.comp)


#################################################
##  Descriptive Statistics and Visualizations  ##
#################################################
library(ggplot2)
library(gridExtra)

# Histogram of course scores
G3por = ggplot(Por, aes(G3)) + geom_histogram(fill="turquoise4",color="black") + 
  ggtitle("Final Portuguese Scores, n=649") + ylim(c(0,110)) + xlab("Score") +
  theme_classic()
G3mat = ggplot(Mat, aes(G3)) + geom_histogram(fill="goldenrod3",color="black") +
  ggtitle("Final Math Scores, n=395") + ylim(c(0,110)) + xlab("Score") + 
  theme_classic()

# plotted into grid form
grid.arrange(G3por,G3mat, ncol=2) + ggtitle("Distribution of Final Scores")

# Bar charts for categorical scores
G3Cpor = ggplot(PorC, aes(G3)) + geom_bar(fill="turquoise4") +
  ggtitle("Final Portuguese Scores, n=649") + ylim(c(0,600)) + xlab("Competitiveness") +
  theme_classic()
G3Bpor = ggplot(PorB, aes(G3)) + geom_bar(fill="turquoise4") +
  ggtitle("Final Portuguese Scores, n=649") + ylim(c(0,600)) + xlab("Pass/Fail") +
  theme_classic()

G3Cmat = ggplot(MatC, aes(G3)) + geom_bar(fill="goldenrod3") +
  ggtitle("Final Math Scores, n=395") + ylim(c(0,600)) + xlab("Competitiveness") +
  theme_classic()
G3Bmat = ggplot(MatB, aes(G3)) + geom_bar(fill="goldenrod3") +
  ggtitle("Final Math Scores, n=395") + ylim(c(0,600)) + xlab("Pass/Fail") +
  theme_classic()


grid.arrange(G3Bpor,G3Cpor, G3Bmat, G3Cmat, ncol=2) + ggtitle("Distribution of Final Scores")


## Summary information output as text file
out = capture.output(summary(Por))
cat("Portuguese Descriptive Summary", out, sep = "\n", append = T,
    file = "PorSum.txt")

out = capture.output(summary(Mat))
cat("Math Descriptive Summary", out, sep = "\n", append = T,
    file = "MatSum.txt")

# Some simple plots of relationships between final grade G3 and other variables
fac.vars = c()
vars = subset(Por, select = -G3) %>% names(.)

qwkplt = function(vars, y, data) {
  pdf(file="plots.pdf", width = 5, height = 5) # output to pdf
  d = substitute(data) # calls the name of the dataset as string
  for (i in vars) {
    plot(data[[y]] ~ data[[i]], data = data, xlab = i, ylab = y,
         main = paste0(y, " by ", i, " in (", d, ") dataset")) # paste0() is no separation
  }
  dev.off() # ends pdf
}

qwkplt(vars, y=c("G3"), data=Por)
qwkplt(vars, y=c("G3"), data = Mat)

