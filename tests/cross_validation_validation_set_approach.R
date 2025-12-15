### cross validation - validation set approach
dim(Hitters)

# 180: select 180 indices from the 263 indices/observations. 
# train is a vector of indices.
train = sample(1:263 , 180 , replace=FALSE)
# or
train = sample(seq(263) , 180 , replace=FALSE)
train

# glm always try many lamdas and store the corresponding  models.
lasso.tr = glmnet(x[train,], y[train])
lasso.tr

#.   Df  %Dev  Lambda
# 1   0  0.00 246.300
# 2   2  5.48 224.400

# df = 2 means that 2 coefficients are not 0.
# at the largest lamda, all coefficients are 0. as lamda decreases, more features are used/ more coefficients become non-zero.
# % Dev means how much is explained by the model.

# each column corresponds to one lamda.
# each entry in a column corresponds the prediction with that lamda.
# pred is a matrix
pred = predict(lasso.tr , x[-train,])
dim(pred)


squared_matrix = (y[-train]-pred)^2
# mean is the funtion
# 2 means apply the function to columns of the matrix
mean_squared_error = apply(squared_matrix, 2, mean)
rmse = sqrt(mean_squared_error) # root mean squared error

# from left to right, lamda increases 
plot(log(lasso.tr$lambda), rmse, type="b")
# choose the lamda that has the smallest rmse.
lam.best = lasso.tr$lambda[order(rmse)[1]]
lam.best

coef(lasso.tr, s=lam.best)
