## Multinomial regression practical
## Multivariate Statistics @ Utrecht University
## By: Jasper Ginn & Sarah Mod

# PART I: complete linear separation -----

## Load the iris data
rm(list=ls())
data("iris")

# Look at data
summary(iris)

# Naively, we simply model setosa or not? (1/0) without exploring the data
iris[,5] <- ifelse(iris[,5] == "setosa", 1, 0)

# Run glm model
binmodel <- glm(Species ~ ., data=iris, family=binomial(link="logit"))
# Q. What does the warning message mean?
# Hint: look at the summary of the model. What do you notice?
summary(binmodel)

# Let's look at the data
data("iris")
plot(iris[,1:4], col=iris[,5])

# Clearly, the Setosa group is very well separated from the other data on at least three of the four variables.

# To further understand the problem of perfect separation, lets look at the data in a scatterplot.
# We cannot plot four dimensions, but perhaps three dimensions is enough?
library(plotly)
library(ggplot2)
p <- plot_ly(iris, x = ~Sepal.Length, y = ~Petal.Length, z = ~Petal.Width, 
             color = ~Species, colors = c('red', 'blue', 'green')) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Sepal length'),
                      yaxis = list(title = 'Petal length'),
                      zaxis = list(title = 'Petal width')))
p

# Another possibility is to compute the principal components of the data and to plot the first two components.
prc <- princomp(iris[,1:4])
prc <- prc$scores

# Make a new dataset
prcdata <- data.frame("component1" = prc[,1],
                      "component2" = prc[,2],
                      "species" = iris[,5])

ggplot(prcdata, aes(x=component1, y=component2, color=species)) +
  geom_point() + 
  theme_bw() 

# Q. for which research question is perfect separation an issue? Is it an issue if we want to make inferences or if we want to predict?
# Hint: try to separate the algorithm from statistical theory. Which one is impacted by perfect separation of variables?

# PART II: Multinomial logistic regression & making inferences ----

# PART III: Multinomial logistic regression for prediction -----

# Q. What is the research question?
# Q. What do we want from the classifier? i.e. what are we trying to optimize?

# Using a naive model:
#  - model all three classes separately (as binaries). So we are running K models for K classes.
#  - we then compare the probabilities for each model and choose the 'most likely' one
#    e.g. the class which has the highest probability

## Explanation of log likelihood
# The outcomes should look 'likely' given the data. So a point that is classified as class 1 but has a low probability of being selected as such gets penalized by the log-likelihood.
# as an aside, the use of the log function is a computational choice: by the rules of logarithms, products can be summed instead of multiplied when we take the log.

# Now we perform multi-class classification
# First, think about what a 'naive' model would do given K classes?
# Would it:
#  a) guess randomly?
#  b) choose the class with the maximum number of subjects?
#  c) ....

## Load the mnlr library
library(mnlr)

mclr <- mlogistic(iris[,1:4], iris[, 5])
# View probabilities
probs <- as.data.frame(mclr$probabilities)
# Reshape data from wide --> long format
library(reshape2)
probs_long <- melt(probs)

# Plot densities
ggplot(probs_long, aes(x=value, fill=variable)) +
  geom_density(alpha=1) +
  theme_bw() +
  facet_grid(variable ~ .) +
  theme(legend.position = "none") +
  scale_x_continuous(name = "probability")
# Q. What do you learn from this plot? 

# Inspect confusion matrix
pred_classes <- mclr$predictions
caret::confusionMatrix(as.factor(pred_classes), iris[,5])
# Q. What does the confusion matrix tell us?
# Q. Based on the above plots, would you say this is a good result?

# PART III.2: cross-validation ----

## Now let's stop for a moment and think about what we've done here.
## What might be the problem with our current approach?

## We're overfitting, but we have no idea how to tackle this issue because we have no 'unbiased' data to test how much we're overfitting.

## So pretend that we're at the beginning of our classification quest.
rm(list=ls())
data("iris")

## We split the data into train/test
set.seed(8)
index <- runif(nrow(iris))

## We split the data such that approximately 80% is used for training, 10% is used for testing our trained model, and another 10% is used for testing the final model
train <- which(index <= 0.70)
dev <- which(index > 0.7 & index <= 0.85)
test <- which(index > 0.85)

## Subset data
iris_train <- iris[train,]
iris_dev <- iris[dev,]
iris_test <- iris[test,]

## We saw previously that the iris data is well separated, so we will add extra variance to the data by adding some random error. This will simulate overfit on train data.
set.seed(100)
iris_dev[,1:4] <- apply(iris_dev[,1:4], 2, function(x) x + runif(length(x), 0, 0.5))
set.seed(500)
iris_test[,1:4] <- apply(iris_test[,1:4], 2, function(x) x + runif(length(x), 0, 0.5))

## The idea is this: we model on the train data and we keep the test data until the end.
## We never touch the test data until we have a model we feel confident about. Only then do we predict. 

mclr2 <- mlogistic(iris_train[,1:4], iris_train[, 5])

## Predict class
preds <- predict_mlr(mclr2, iris_train[,1:4])
caret::confusionMatrix(as.factor(preds$class), iris_train[, 5])

## Predict class on test set
preds_dev <- predict_mlr(mclr2, iris_dev[,1:4])
caret::confusionMatrix(as.factor(preds_dev$class), iris_dev[, 5])

## Q. What happened to the accuracy of the model? Why is the model underperforming so much on unbiased data?
## Hint: think about the bias-variance trade-off. What did we do to the dev & test set?
## Also: think about the number of data points we have.

# PART III.3: using regularization ----

## Explanation of regularization

## What happens to the cost function with different values of lambda?
grid <- c(0.1, 1, 5, 10, 30, 50, 100)
result <- list()
for(value in grid) {
  
  ## Train model with lambda value
  mod <- mlogistic(iris_train[,1:4], iris_train[, 5], lambda = value)
  
  ## Retrieve costs
  costs <- lapply(mod$models, function(x) x$model_information$costs)
  
  ## Bind into a matrix
  costs_matrix <- do.call(cbind, costs)
  
  ## Calculate mean cost as an approximation
  avg_cost <- apply(costs_matrix, 1, mean)
  
  ## Save results to list
  result[[paste0("lambda", value)]] <- avg_cost
  
}

## Bind the columns together
result_bind <- do.call(cbind, result)
## Reshape the data from wide --> long format
library(reshape2)
result_bind_long <- melt(result_bind)
## Add iteration numbers
result_bind_long$iteration <- rep(1:2000, length(grid))

## Plot
library(ggplot2)
ggplot(result_bind_long, aes(x=iteration, y=value, color=Var2)) +
  geom_line() +
  theme_bw() +
  theme(legend.title = element_text("Lambda")) +
  scale_x_continuous(name = "Iteration") +
  scale_y_continuous(name = "Cost")

## Q. What do we see on this plot? The x-axis contains the iterations of the algorithm and the y-axis is the cost function.

## Q. The coefficients are assumed to be unbiased population parameters. Given the effect of the l2-regularization on the coefficients, can this still be the case? Think of the bias/variance trade-off and how you would test for this. Would confidence intervals help?

mclr3 <- mlogistic(iris_train[,1:4], iris_train[, 5], lambda = 100)

## Predict class on train data
preds <- predict_mlr(mclr3, iris_train[,1:4])
caret::confusionMatrix(as.factor(preds$class), iris_train[, 5])

## Predict on dev set
preds_dev <- predict_mlr(mclr3, iris_dev[,1:4])
caret::confusionMatrix(as.factor(preds_dev$class), iris_dev[, 5])

## Q. Is this a better fit?
## Why/why not?

# Predict on test set
preds_test <- predict_mlr(mclr3, iris_test[,1:4])
caret::confusionMatrix(as.factor(preds_test$class), iris_test[, 5])

# Q. What does this do to interpretability if the coefficients?
# What happened to them?
mclr2$models$setosa$model_information$params # Model without lambda
mclr3$models$setosa$model_information$params # Model with lambda

# Q. When do you think that regularization is a good technique? In what sort of situations would you use it?