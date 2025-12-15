################################
### lecture ggplot code - CS-555
### Regression
### By: Farshid Alizadeh-Shabdiz
### Reference: modified from "ggplot2 book" by Wicham
#### 

# Introduction
# ggplot2 is for producing statistical, or data, graphics. 
# Unlike most other graphics packages, ggplot2 has an underlying grammar, 
# that allows you to compose graphs by combining independent components. 
# This makes ggplot2 powerful. 
# The idea is learning a grammar and a simple set of core principles. 
# The hard part is to forget all the preconceptions 
# that you bring over from using other graphics tools.

# All plots are composed of the data, the information you want to visualize, 
# and a mapping, the description of how the data’s variables are mapped to 
# aesthetic attributes. 
# These are important mapping components:
#   - A layer is a collection of geometric elements and statistical transformations. 
#   Geometric elements, "geoms" for short, represent what you actually see in the plot: 
#   points, lines, polygons, etc. 
#   Statistical transformations, "stats" for short, summarise the data: for example, 
#   binning and counting observations to create a histogram, or fitting a linear model.
# 
#   - Scales map values in the data space to values in the aesthetic space. 
#   This includes the use of color, shape or size. Scales also draw the legend 
#   and axes, which make it possible to read the original data values from the plot 
#   (an inverse mapping).
# 
#   - A facet specifies how to break up and display subsets of data as small multiples.
#   


# Let’s get started by loading ggplot2 package and loading our dataset.
# Data sets and R Code is available at
# https://github.com/kiat/R-Examples

### Install packages if you haven't installed them yet 
# Typical install:
# install.packages('gpplot2')
# install.packages('dplyr')
### Load packages 
library(ggplot2) 


# Optoin to Load local copy
# library(ggplot2,lib.loc="/path/to/myfolder")
# library(dplyr,lib.loc="/path/to/myfolder")

## Example 1
mpg # Fuel economy of popular car models in 1999 and 2008.

# Different ways to have a quick look at the data
summary(mpg)
View(mpg)
str(mpg)
head(mpg)
tail(mpg)

#Every ggplot2 plot has three key components:
#  - data,
#  - A set of aesthetic mappings between variables in the data and visual properties
#  - At least one layer which describes how to render each observation. 
#  Layers are usually created with a geom function.

# a simple example:
ggplot(mpg, aes(x = displ, y = hwy)) + 
  geom_point()
# or in short x and y are dropped from "aes"
ggplot(mpg, aes(displ, hwy)) +
  geom_point()
# This produces a scatterplot defined by:
#  Data: mpg.
#  Aesthetic mapping: engine size mapped to x position, fuel economy to y position.
#  Layer: points.
# Pay attention to the structure of this function call: data and aesthetic mappings 
#  are supplied in ggplot(), then layers are added on with "+". 
#  This is an important pattern.

# To add additional variables to a plot, we can use other aesthetics like colour, 
# shape, and size 
  # aes(displ, hwy, colour = class)
  # aes(displ, hwy, shape = drv)
  # aes(displ, hwy, size = cyl)

# adding to base wth scale
ggplot(mpg, aes(displ, hwy, colour = class)) + 
  geom_point()
# or by adding to layers individually
ggplot(mpg, aes(displ, hwy)) + geom_point(colour = "blue")

# adding a smoother to a plot
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth() # Note: includes pointwise confidence interval by default

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth(se=FALSE) # Default is local polynomial regression. 
# geom_smooth - default is "Leoss" which is good for small N
# Loess doesn't work well for large N and instead we use "gem"

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth(method = "gam", formula = y ~ x) # Formula is a line 

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth(method = "gam", formula = y ~ s(x)) # s(x) is a polynomial of x

ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth(method = "gam", formula = y ~ s(x , k=2)) # a polynomial of degree 2 

# or instead we use "lm"
ggplot(mpg, aes(displ, hwy)) + 
  geom_point() + 
  geom_smooth(method = "lm")


### Boxplots and jittered points
ggplot(mpg, aes(drv, hwy)) + 
  geom_point()    # Since drv is a catogorical data (f, r, 4wheel_drive) 
                  # the points are on top of each other

# Instead use boxplot or jitter plot
ggplot(mpg, aes(drv, hwy)) + geom_boxplot()

ggplot(mpg, aes(drv, hwy)) + geom_jitter()
ggplot(mpg, aes(drv, hwy)) + geom_violin()

# Histogram presentation of the data
ggplot(mpg, aes(hwy)) + geom_histogram()
ggplot(mpg, aes(hwy)) + geom_freqpoly() # note freqpoly is exactly same as histogram
                                        # but instead of bars, it shows lines 
                                        # connecting top of the bars
# Showing displ
ggplot(mpg, aes(displ)) +
    geom_bar()
ggplot(mpg, aes(displ)) +
  geom_histogram(bandwidth=05)
# also showing displ differently for catogorical data drv
ggplot(mpg, aes(displ, colour = drv)) + 
  geom_freqpoly(binwidth = 0.5)

ggplot(mpg, aes(displ, fill = drv)) + 
  geom_histogram(binwidth = 0.5) + 
  facet_wrap(~drv, ncol = 1)

# bar charts
ggplot(mpg, aes(manufacturer)) + 
  geom_bar()

### Output the model - save the model
base<- ggplot(mpg, aes(cty, hwy)) 
#Note: most geoms have an alpha, which shows points contrast change
# alpha = 0 means tranparant
# alpha = 1 means opague
# default appha is 1

base +
  geom_point(alpha = 1 / 3) + 
  xlab("city driving (mpg)") + 
  ylab("highway driving (mpg)")


ggplot(mpg, aes(drv, hwy)) +
  geom_jitter(width = 0.25)

base +
  geom_jitter(width = 0.25)

ggplot(mpg, aes(drv, hwy)) +
  geom_jitter(width = 0.25) + 
  xlim("f", "r") + 
  ylim(20, 30)

# Save a model
p <- ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
  geom_point()
# Save png to disk
ggsave("plot.png", p, width = 5, height = 5)
# or render it on screen
print(p)

# Can also breifly describe its structure
summary(p)

# the entire plot object can be also saved and retrieved 
# note - rds files store a single R object
saveRDS(p, "plot.rds")
q <- readRDS("plot.rds")



##############################
### Example 2
library(stats) 
library(base) 
library(dplyr)

# setwd("YOUR-WORKING-PATH")
setwd("lectures")
getwd()


# Read data
auto.data <- read.csv("AutoData.csv",
                      header = TRUE) 
auto.data

# tbl_df() isn't necessary here. 
# tbl_df() changes data to Tibble dataframe.
# Tibble is a new type of dataframe. 
# it never changes the type of the inputs (e.g. it never converts strings to factors!), 
# it never changes the names of variables, it only recycles inputs of length 1, 
# and it never creates row.names().
# It helps to display the data more clearly 
auto.data <- tbl_df(auto.data)
auto.data

### Data Exploration
# 􏰑 For a new data set, explore the data first
# 􏰑 Look at the columns of the data and their type
#   􏰑 What types of relationships do you expect to see between variables?
#   􏰑 What is your intuition about the data?
#   􏰑 Do we observe anomalous behavior?

#Run the following to get a quick glimpse of the data
# Find the dimensions
dim(auto.data)
# Look at the structure
str(auto.data)
# Examine the top
head(auto.data)
# Find out about a function
?str

### Scatter Plots
# One simple plot is a scatter plot between two continuous variables.
# qplot is convenient and powerful
qplot(curb.weight, price, data=auto.data)


# Power of ggplot
# The true power of ggplot comes from its ability to easily visualize relationships 
#    between many variables.
# The main ingredients are:
# 1. aesthetics 
# 2. facets
# 3. geoms


# ggplot - Aesthetics
# Aesthetics control many of the plot’s visual properties
# Importantly these visual properties may be mapped directly to variables


# Scatter Plots
# map color to factor/categorical variable
qplot(curb.weight,
      price,
      data=auto.data,
      color=num.of.cylinders)
# map color to continuous variable
qplot(curb.weight,
      price,
      data=auto.data,
      color=bore)


# Aesthetics
# There are many other aesthetics attributes besides color. Some of them are:
#   Not all aesthetics work with both categorical and continuous variables (like color did)
# Also only a certain subset of aesthetics will be available for each plot type (geom)
#  1. color 
#  2. size 
#  3. shape 
#  4. fill

# Aesthetics
# Let us examine aesthetics with a scatter plot. 
# Feel free to change the variables in the scatter plot
qplot(curb.weight,
      price,
      data=auto.data,
      size=horsepower) # size parameter
qplot(curb.weight,
      price,
      data=auto.data,
      shape=drive.wheels) # shape parameter


# Facets
# Facets represent another way of visualizing the effect of 
# factor/categorical variables
# Facets enable us to get a separate plot for each level/category

# Facets Example
# Try out a faceting example:
qplot(curb.weight,
        price,
        data=auto.data) + facet_wrap(~aspiration)

# Facets
# Note facet wrap gives a separate plot for each category
# Also note how we incorporated the behavior of facet wrap: 
#   via the "+" operator
# This is one of the main strengths of ggplot that plots are built in
# intuitive layers

# Facets
# Also available is facet grid for examining the interaction between two categorical variables.

qplot(curb.weight,
        price,
        data=auto.data) +
      facet_grid(drive.wheels~num.of.doors)

# Facets
# Try the following:
qplot(curb.weight,
        price,
        data=auto.data) + facet_grid(.~drive.wheels) # facets as columns
qplot(curb.weight,
      price,
      data=auto.data) + facet_grid(drive.wheels~.) # facets as rows
qplot(curb.weight,
      price,
      data=auto.data,
      color=num.of.doors) + facet_grid(drive.wheels~.)

# geom histogram
# geom_histogram operates with a single continuous variable.
# example: Let's look at price
qplot(price,
      data=auto.data,
      geom='histogram')
# histogram is the default when only one variable
qplot(price,data=auto.data)

# geom histogram
# Note the warning concerning bin-width
# The binwidth can dramatically impact how we visually interpret the distribution
# It’s best to experiment with different values to get a feel of the data 
# We can alter the binwidth by passing the option to qplot
qplot(price,
      data=auto.data,
      geom='histogram',
      binwidth=450)

# Histogram
# Note that our price distribution is slightly skewed
# E.g. if we are not interested in higher priced (≥ 20,000 say) 
#   cars We can limit our plot cars with lower price by setting limits
qplot(price,
      data=auto.data,
      geom='histogram',
      binwidth=450) +
      xlim(4000,20000)

# Histogram
# Just like our point geom, histogram has aesthetics. Try the following
qplot(price,
      data=auto.data,
      color=drive.wheels) # the border of the bar

qplot(price,
      data=auto.data,
      fill=drive.wheels) # the fill of the bar

# Histogram with facets
# The colors help but the figure is a bit busy. We can try faceting instead:
qplot(price,
        data=auto.data,
        fill=drive.wheels) +
        facet_wrap(~drive.wheels)

# Histogram with facets
# This helps us separate out the categorical variables much easier.
# Note the counts vary quite a bit among the different classes, 
#   but yet the count axis is the same for all. 
#   We can change this by modifying the facet wrap call:
qplot(price,
    data=auto.data) +
    facet_wrap(~drive.wheels,
    scales = 'free_y')

# More geoms
# 􏰑 There are many other geoms besides point and histogram. 
#     Try ??geom to see a list.
# 􏰑 Different geoms operate with different (combinations of) 
#     data types (i.e. categorical or continuous).
# 􏰑 As is characteristic of ggplot, geoms can be layered to 
#     create plots of increasing detail/complexity.

# Layering of ggplot, geoms
qplot(price,data=auto.data) +
      geom_density()

qplot(price,data=auto.data,
      geom='density')

qplot(price,data=auto.data) +
      geom_histogram()

qplot(price,
      ..density.., # don't use counts
      data=auto.data,
      geom='histogram')+
  geom_density()

qplot(height,price, data=auto.data)+ geom_density2d()

# Or 
qplot(height,price,
      data=auto.data,
      geom='density2d')


qplot(height,price,
      data=auto.data)+
  geom_density2d()

# geoms boxplot
#   􏰑 Can you guess the geom for creating a boxplot?
#   􏰑 Create a boxplot displaying price for each of the drive.wheels categories

# geoms boxplot
qplot(drive.wheels,
      price,
      data=auto.data,
      geom='boxplot')

# References and Additional Info
# 􏰑 ggplot2 documentation: http://docs.ggplot2.org/current/
#   􏰑 Hadley’s ggplot2 book: http://ggplot2.org/book/
#   􏰑 RStudio ggplot cheatsheet: http://www.rstudio.com/ 
#   wp-content/uploads/2015/03/ggplot2-cheatsheet.png







