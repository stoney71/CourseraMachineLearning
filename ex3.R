setwd("~/RFolder/CourseraMachineLearning")

### May need these packages later
# library(ggplot2)
# library(dplyr)


### Load the data

X <- as.matrix(read.table("./ex3_train_X.csv", header=FALSE, sep = ","))
y <- as.matrix(read.table("./ex3_train_y.csv", header=FALSE, sep = ","))


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

