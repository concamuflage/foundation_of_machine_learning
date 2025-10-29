
# Lecture 1 - CS555
# Farshid Alizadeh-Shabdiz

# R is a language – it is an interpreted language
# It provides many statistical computing graphics functionalities. 
# According to their site “it is a suite of software facilities for 
#   - Data manipulation
#   - Calculation
#   - Graphical display”
# It is a GNU project – under GNU general public license
#   - It is an open source
#   - It is a free for our use


# Comments

# Base packages
getOption("defaultPackages")    # See all the base packages

# Contributed packages
install.packages("prob")      # Install contributed packages 
install.packages("prob", depends=TRUE) # Install a packages with dependencies
# Need to install a package once!
# but has to get loaded in each every session
library(prob)               # Load a package

library()                   # See list of all the packages in library

####
# Simple expressions
x <- 8
x
x = 18
x

y = "Hello"
y

z <- TRUE
z
z = T
z

####
# Logical operators:
1 + 7 != 7
# &, |, !
# <,>,<=,>=, ==, !=

####
# Variables assignment is done with the <- operator
mynumber <- 483

# typeof() tells use type
typeof(mynumber)

# we can convert between types
myint <- as.integer(mynumber)
typeof(myint)

# a data vector
voted <- c(TRUE, FALSE, TRUE, TRUE)
voted
typeof(voted)
x <- as.numeric(voted)
x
as.logical(x)

#  Functions and exressions
x <- 1:5
sum(x)
length(x)
min(x)
mean(x)
sd(x)     # sample standard deviation

?mean
help(mean)

# Vectors
a = c(10,20,30,40,50)
a[1]

a[1]+1
a = c(10,20,30,40,50,"60")
a[1]+1 #??

length(a)
a[c(3,4)]
a[5] = 55

a

x = c(10,20,30,40,50)
y = c(2,4,6,7,3)

plot(x,y)
plot(x,y, type="l")

# Normal Distribution
dnorm(0.3, mean=0, sd=1) # pdf corresponding to the point: 0.3
pnorm(0.3, mean=0, sd=1) # CDF corresponding to point 0.3
qnorm(0.4, mean=0, sd=1) # Find a point corresponding to CDF point of 0.4
rnorm(12, mean=0, sd=1)  # Generate 12 random samples of the given distribution



# Working directory - Setting/Getting
getwd()
# Set working directory
setwd("~/BostonUniversity/CS555_DataAnalysisVisualization")
getwd()
# You can use the RStudio menus to set your working directory.

####
#Reading Data into R
#Read a Comma-Separated Values (CSV) data file from a text file 
read.csv("usa_daily_avg_temps.csv")   
# First Line is the header, default value for header is True
usTemp = read.csv("usa_daily_avg_temps.csv", header=TRUE) 

summary(usTemp)

length(usTemp$city)

hist(usTemp$month)
hist(usTemp$day)

hist(usTemp$day, freq=FALSE)
bounderies = seq(0,35, by=5)
hist(usTemp$day , breaks = bounderies, freq=FALSE)

####
earning <- c(35, 40, 45, 33, 30, 42, 32, 32, 25)
summary(earning)

mean(earning)
median(earning)
min(earning)
max(earning) 
quantile(earning) 
var(earning)
sd(earning)

boxplot(earning)

earning <- c(35, 40, 145, 33, 30, 42, 32, 32, 25)
boxplot(earning)

