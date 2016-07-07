setwd("~/RFolder/CourseraMachineLearning")
library(ggplot2)
library(dplyr)

### Load the data

ds2 <- read.csv("./ex2data2.txt", header = FALSE)
colnames(ds2) <- c("Test1", "Test2", "Accepted")

### Add the Sigmoid Function

sigmoid <- function(z) {
        g <- 1 / (1 + exp(-1 * z))
        g
}

### Part 2.1 Visualise the Data
# Plot Figure 3

g <- ggplot(ds2, aes(x = Test1, y = Test2))
g <- g + geom_point(aes(color = as.factor(Accepted)))
g <- g + labs(title = "Plot of training data", x = "Microchip Test 1", y = 
                      "Microchip Test  2")
g <- g + scale_colour_discrete(name  ="Result",breaks=c("1", "0"),
                               labels=c("Accepted", "Rejected"))
g

### Part 2.2 Feature Mapping

ds2 <- mutate(ds2, T12 = Test1^2, T1T2 = Test1*Test2, T22 = Test2^2)

### Cost Function, Gradient, Learning Parameters
### This is achieved in R by the glm function

model <- glm(Accepted ~ ., data = ds2, family = binomial(link = "logit"))
theta <- summary(model)$coeff[, 1]


### Part 3.4 Plot the Decision Boundary

# A function to predict the outcome based on x1, x2 and theta (already calculated)
predict2 <- function(x1, x2) {
        x <- c(1, x1, x2, x1^2, x1*x2, x2^2)
        p <- as.integer(sigmoid(sum(x*theta)) >= 0.5)
        p 
}

# First, create a dataframe dec_boundary which is the predicted outcome for a
# grid of x1 and x2 values.
decB <- function() {
        scl <- seq(-1, 1.5, length = 50)
        x1 <- NULL
        x2 <- rep(scl, 50)
        for (i in 1:length(scl)) {
                x1 <- c(x1, rep(scl[i], 50))
        }
        z <- mapply(function(a, b) {
                x <- c(1, a, b, a^2, a*b, b^2)
                sum(x * theta)
        }, x1, x2)
        df <- data.frame(x1, x2, z)
        colnames(df) <- c("x1", "x2", "z")
        df
}

dec_boundary <- decB()

# Plot Figure 4

g <- g + stat_contour(data = dec_boundary, aes(x = x1, y = x2, z = z),
                      breaks = 0, colour = "green")
g 


# Now calculate predicted values and Accuracy.
ds2$Predicted <- mapply(predict2, ds2$Test1, ds2$Test2)

paste("Train Accuracy:", sum(ds2$Accepted == ds2$Predicted) / nrow(ds2) * 100)
