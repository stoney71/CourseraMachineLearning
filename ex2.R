setwd("~/RFolder/CourseraMachineLearning")
library(ggplot2)
library(dplyr)

### Load the data

ds1 <- read.csv("./ex2data1.txt", header = FALSE)
colnames(ds1) <- c("Exam1", "Exam2", "Admitted")

### Part 1.1 Visualising the Data

g <- ggplot(ds1, aes(x = Exam1, y = Exam2))
g <- g + geom_point(aes(color = as.factor(ds1$Admitted)))
g <- g + labs(title = "Scatter plot of training data", x = "Exam 1 score", y = 
                      "Exam 2 score")
g <- g+ scale_colour_discrete(name  ="Admitted", breaks=c("1", "0"),
                              labels=c("Yes", "No"))
g

### Part 1.2.1 Sigmoid function

sigmoid <- function(z) {
        g <- 1 / (1 + exp(-1 * z))
        g
}

### Cost Function, Gradient, Learning Parameters
### This is achieved in R by the glm function

model <- glm(Admitted ~ ., data = ds1, family = binomial(link = "logit"))
theta <- summary(model)$coeff[, 1]

# Calculate cost with initial theta (zeros)

init_theta <- rep(0, length(theta))
x_theta <- mapply(function(x1, x2) sum(c(1, x1, x2) * init_theta), ds1$Exam1, ds1$Exam2)
cost <- (sum((-1 * as.integer(ds1$Admitted)) * log(sigmoid(x_theta))) - 
                 sum((1 - as.integer(ds1$Admitted)) * log(1-sigmoid(x_theta)))) / nrow(ds1)

paste("Cost at initial theta (zeros):", cost)

# Calculate cost after logical regression complete

x_theta <- mapply(function(x1, x2) sum(c(1, x1, x2) * theta), ds1$Exam1, ds1$Exam2)
cost <- (sum((-1 * as.integer(ds1$Admitted)) * log(sigmoid(x_theta))) - 
        sum((1 - as.integer(ds1$Admitted)) * log(1-sigmoid(x_theta)))) / nrow(ds1)

paste("Cost at Theta:", cost)

### Part 1.2.4 Evaluating logistic regression

# Plot Figure 2
x1_int <- -1 * theta[1] / theta[2]
x2_int <- -1 * theta[1] / theta[3]

g <- g + geom_abline(intercept = x2_int, slope = -1 * x2_int / x1_int, color = "red")
g

predict <- function(x1, x2) {
        x <- c(1, x1, x2)
        p <- as.integer(sigmoid(sum(x*theta)) >= 0.5)
        p 
}

paste("For a student with scores 45 and 85, we predict an admission probability of",
      sigmoid(sum(c(1, 45, 85)*theta)))

ds1$Predicted <- mapply(predict, ds1$Exam1, ds1$Exam2)

paste("Train Accuracy of:", sum(ds1$Admitted == ds1$Predicted) / nrow(ds1) * 100)

