# EXERCISE 3
library(tidyverse)
# read data into frame, i think this is what you wanted?
# csv files must be in same directory for it to work however
signif <- read_csv("A3_EX3_signif.csv")
signifnot <- read_csv("A3_EX3_not_signif.csv")
hypothesis <- function(file_name) {
  # initial hypothesis
  cat("Null Hypothesis: H_0: b = 0, which means there is no significant linear relationship between X and Y", "\n")
  cat("Alternative Hypothesis: H_1: b =/= 0, which means there is a significant linear relationship between X and Y", "\n\n")
}
# constructor for class
mylm <- function(...) {
  fit <- list(fit = lm(...))
  class(fit) <- "mylm"
  return(fit)
}
fit <- function(file_name) {
  fit1 = mylm(Y ~ X, data = file_name)
  # get X confidence interval
  conf = confint(fit1$fit, 'X', level=0.95)
  cat("confidence interval from: " , conf[1] , "," , conf[2], "\n")
  # p value and t value extracted from summary at 2,4 and 2,3 coordinates
  cat("B^-value: ", summary(fit1$fit)$coefficients[2,1], "\n")
  cat("p-value: ", summary(fit1$fit)$coefficients[2,4], "\n")
  cat("t-value: ", summary(fit1$fit)$coefficients[2,3], "\n")
  cat("df: ", fit1$fit$df.residual, "\n\n")
  # quick return
  return(fit1)
}
assumptions <- function(file_name) {
  fit <- lm(Y ~ X, data = file_name)
  # par the graphs so we can display all at once
  par(mfrow=c(2,2))
  plot(file_name$X, file_name$Y, main="y vs x",
       xlab="X",
       ylab="Y")
  plot(fit$fitted.values, fit$residuals, main="ei vs y^",
       xlab="y^",
       ylab="ei")
  hist(fit$residuals, main="Histogram of regression residuals", xlab="ei")
  
}
decision.mylm <- function(fit) {
  # calculate confidence interval again
  conf = confint(fit$fit, "X" ,level=0.95)
  # check if 0 is within the 95% confidence interval if yes then retain null hypothesis
  if (0 >= conf[1] & 0 <= conf[2]) {
    cat("RETAIN NULL: NO SIGNIFICANT LINEAR RELATIONSHIP \n\n")
  } else {
    cat("REJECT NULL: SIGNIFICANT LINEAR RELATIONSHIP \n\n")
  }
}
conclusion.mylm <- function(fit) {
  #do a little test to see what conclusion we should print 
  conf = confint(fit$fit, "X" ,level=0.95)
  if (0 >= conf[1] & 0 <= conf[2]) {
    cat(" The conclusion for this specific test is to retain the null hypothesis, which means that there is no evidence for a linear relationship between Y and X. The p-value " , summary(fit$fit)$coefficients[2,4] , " also indicates that the p-value that we should retain the null hypothesis, such as the p-value is greater than 0.05. The greater the p-value is from 0.05 the greater we should retain the null-hypothesis" ,
        "\n")
  } else {
    cat(" The conclusion for this specific test is to reject the null hypothesis, which means that there is for a linear relationship between Y and X. The p-value " , summary(fit$fit)$coefficients[2,4] , " also indicates that the p-value that we should reject the null hypothesis, such as the p-value is less than 0.05. The lesser the p-value is from 0.05 the greater we should reject the null-hypothesis" ,
        "\n")
  }
}
# so we dont need to reference with the '.'
# also the only functions it was mention with .mylm was decision and conclusion 
decision <- function(x, ...) {
  UseMethod("decision")
}
conclusion <- function(x, ...) {
  UseMethod("conclusion")
}
mytest <- function(file_name) {
  # all the functions lined up in mytest
  hypothesis(file_name)
  assumptions(file_name)
  fit1 = fit(file_name)
  decision(fit1)
  conclusion(fit1)
}
# for testing signif
mytest(signif)
# for testing signifnot
mytest(signifnot) 

