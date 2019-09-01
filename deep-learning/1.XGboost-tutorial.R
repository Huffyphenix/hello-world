# https://xgboost.readthedocs.io/en/latest/R-package/xgboostPresentation.html
# xgboost -----------------------------------------------------------------

library(xgboost)

# dividing the dataset into train and test data
data(agaricus.train, package='xgboost')

data(agaricus.test, package='xgboost')

train <- agaricus.train

test <- agaricus.test

str(train)

# 1. boosting tree
############################################### Basic Training using XGBoost
bstSparse <- xgboost(data = train$data, label = train$label, max.depth = 2, 
                     eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")

# generate xgb.DMatrix
dtrain <- xgb.DMatrix(data = train$data, label = train$label)

# set Verbose option to see the training progress
# verbose = 0, no message
# verbose = 1, print evaluation metric
# verbose = 2, also print information about tree
bst <- xgboost(data = dtrain, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, 
               objective = "binary:logistic", verbose = 2)

# Perform the prediction
pred <- predict(bst, test$data)
# size of the prediction vector
print(length(pred))
prediction <- as.numeric(pred > 0.5)
print(head(prediction))

#Measuring model performance
err <- mean(as.numeric(pred > 0.5) != test$label)
print(paste("test-error=", err))


############################################### Advanced features,  xgb.train
# in some way it is similar to what we have done above with the average error. The main difference is that above it was after building the model, and now it is during the construction that we measure errors.
dtrain <- xgb.DMatrix(data = train$data, label=train$label)
dtest <- xgb.DMatrix(data = test$data, label=test$label)
# For the purpose of this example, we use watchlist parameter. It is a list of xgb.DMatrix, each of them tagged with a name.
watchlist <- list(train=dtrain, test=dtest)

bst <- xgb.train(data=dtrain, max.depth=2, eta=1, nthread = 2, nrounds=2, watchlist=watchlist, objective = "binary:logistic")

bst <- xgb.train(data=dtrain, max.depth=2, eta=1, nthread = 2, nrounds=2, watchlist=watchlist, eval.metric = "error", eval.metric = "logloss", objective = "binary:logistic")

# 2. linear boosting
# XGBoost implements a second algorithm, based on linear boosting. The only difference with the previous command is booster = "gblinear" parameter (and removing eta parameter).
bst <- xgb.train(data=dtrain, booster = "gblinear", max.depth=2, nthread = 2, nrounds=2, watchlist=watchlist, eval.metric = "error", eval.metric = "logloss", objective = "binary:logistic")

# 3. Save / Load
# save data 
xgb.DMatrix.save(dtrain, "dtrain.buffer")
## [1] TRUE
# to load it in, simply call xgb.DMatrix
dtrain2 <- xgb.DMatrix("dtrain.buffer")

# save model
xgb.save(bst, "xgboost.model")

# 4. feature importance
importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

# View the trees from a model
xgb.dump(bst, with_stats = T)
xgb.plot.tree(model = bst)
