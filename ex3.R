setwd("~/RFolder/CourseraMachineLearning")

### Load these packages
library(nnet)

### Part 1.1 Load the Dataset

X <- as.matrix(read.table("./ex3_train_X.csv", header=FALSE, sep = ","))
y <- as.matrix(read.table("./ex3_train_y.csv", header=FALSE, sep = ","))


### Part 1.2 Visualising the Data
# randomly select 100 digits to display

sel <- X[sample(1:nrow(X), 100), ]


# Plot 10 x 10 digits.
# Need to transpose and flip each digit matrix vertically.

par(mfrow=c(10,10), mar = c(0,0,0,0))
for (i in 1:100) {
        digit <- matrix(sel[i, ], nrow = 20, ncol = 20, byrow = FALSE)
        image(t(digit)[, ncol(digit):1], col = gray.colors(8), axes = FALSE)
        box()
}

### Part 2 Neural Networks
# The exercise loads a pre-trained set of weights (theta) and uses the 5000 observations
# of X to test against that. Instead, here we will use X and divide it into a train and
# test set. 

df <- data.frame(X, as.factor(y))
names(df)[401] <- "y"

index <- c(sample(1:5000, 4000))
train <- df[index,]
test <- df[-index,]

mod <- nnet(y ~ ., train, maxit = 300, size = 25, MaxNWts = 10500)

pred <- as.integer(predict(mod, test, type = "class"))

paste("Training Set Accuracy:", sum(pred == test$y) / nrow(test) * 100, "%")
