#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108,
       87, 90, 94, 113, 112, 98, 80, 97, 95, 111,
       114, 89, 95, 126, 98)

# sample size
length(y)

# sample mean
mean(y)

# sample standard deviation
sd(y)

# 90% confidence interval
t.test(y, conf.level = 0.90)

# one-sided hypothesis test
# H0: mu = 100
# HA: mu > 100
t.test(y, mu = 100, alternative = "greater")
  
#####################
# Problem 2
#####################

# import data
expenditure <- read.table(
  "https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/expenditure.txt",
  header = TRUE
)

# inspect data
head(expenditure)
str(expenditure)
summary(expenditure)

# Question 2a
# relationships among Y, X1, X2, X3
pairs(expenditure[, c("Y", "X1", "X2", "X3")])

# correlations
cor(expenditure[, c("Y", "X1", "X2", "X3")])

# Question 2b
# create labelled region variable
expenditure$RegionFactor <- factor(
  expenditure$Region,
  levels = c(1, 2, 3, 4),
  labels = c("Northeast", "North Central", "South", "West")
)

# plot Y by region
boxplot(
  Y ~ RegionFactor,
  data = expenditure,
  xlab = "Region",
  ylab = "Per Capita Expenditure on Housing Assistance",
  main = "Housing Assistance Expenditure by Region"
)

# average Y by region
aggregate(Y ~ RegionFactor, data = expenditure, FUN = mean)

# Question 2c
# relationship between Y and X1
plot(
  expenditure$X1,
  expenditure$Y,
  xlab = "Per Capita Personal Income (X1)",
  ylab = "Per Capita Housing Assistance Expenditure (Y)",
  main = "Housing Assistance Expenditure and Personal Income"
)

# relationship between Y and X1 by region
plot(
  expenditure$X1,
  expenditure$Y,
  pch = expenditure$Region,
  col = expenditure$Region,
  xlab = "Per Capita Personal Income (X1)",
  ylab = "Per Capita Housing Assistance Expenditure (Y)",
  main = "Housing Assistance Expenditure and Personal Income by Region"
)

legend(
  "topleft",
  legend = c("Northeast", "North Central", "South", "West"),
  pch = 1:4,
  col = 1:4
)
