install.packages("ISLR2")
library(ISLR2)
dim(Bikeshare)
attach(Bikeshare)
is.factor(hr)
mod.lm = lm(bikers ~ mnth + hr + workingday + temp + weathersit, data = Bikeshare)
summary(mod.lm)

# use sum coding for coding the groups.
contrasts(Bikeshare$mnth) = contr.sum(24)
contrasts(Bikeshare$mnth) = contr.sum(12)

# uses hour0 and Jan as the base months. coefficients for them are not shown.
mod.lm2 = lm(bikers ~ mnth + hr + workingday + temp + weathersit, data = Bikeshare)
summary(mod.lm)
# predict(mod.lm) is fitted values from this model
# difference is a vector of differences between fitted values from each model.

difference = (predict(mod.lm)-predict(mod.lm2))^2
sum(difference) # this is sum of squares of the differences, which is very small.

all.equal(predict(mod.lm),predict(mod.lm2)) # another way to compare the two models

# ----- Poisson -----------
mod.pois = glm(bikers ~ mnth + hr + workingday + temp + weathersit, data = Bikeshare, family = poisson)
summary(mod.pois)
