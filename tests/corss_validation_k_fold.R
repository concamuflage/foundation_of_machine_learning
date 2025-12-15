#############################################################
# Cross validation
# 10 fold cross validation

folds = sample(rep(1:10 , length = nrow(Hitters)))
folds
table(folds)

cv.errors = matrix(NA , 10, 19)
### Note following returns ERROR, since there is no predict method for regsubsets
for (k in 1:10){
  best.fit = regsubsets(Salary~.,data=Hitters[folds!=k,],nvmax=19,method="forward")
  for (i in 1:19){
    pred = predict(best.fit, Hitters[folds==k], id=i)
    cv.errors[k,i] = mean( (Hitters$Salary[folds==k]-pred)^2)
  }
} 
### ERROR!

for (k in 1:10){
  best.fit = regsubsets(Salary~.,data=Hitters[folds!=k,],nvmax=19,method="forward")
  for (i in 1:19){
    coefi = coef(best.fit , id=i)
    x.test = model.matrix(Salary~. , data=Hitters[folds==k,])
    pred = x.test[,names(coefi)] %*% coefi
    cv.errors[k,i] = mean( (Hitters$Salary[folds==k]-pred)^2)
  }
} 

rmse.cv = sqrt(apply(cv.errors,2,mean))
plot(rmse.cv , pch=19, type="b")