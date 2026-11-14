# Problem Set 3
# Aisling Casey

# Load dataset
incumbents <- read.csv("incumbents_subset.csv")


# Question 1

# 1.1 Run the regression
model1 <- lm(voteshare ~ difflog, data = incumbents)
summary(model1)

# 1.2 Make a scatterplot and add the regression line
plot(
  incumbents$difflog,
  incumbents$voteshare,
  xlab = "Difference in Log Campaign Spending",
  ylab = "Incumbent Vote Share",
  main = "Incumbent Vote Share and Campaign Spending Difference"
)

abline(model1)

# 1.3 Save the residuals in a separate object
residuals1 <- residuals(model1)
head(residuals1)

# 1.4 Display coefficients
coef(model1)


# Question 2

# 2.1 Run the regression
model2 <- lm(presvote ~ difflog, data = incumbents)
summary(model2)

# 2.2 Make a scatterplot and add the regression line
plot(
  incumbents$difflog,
  incumbents$presvote,
  xlab = "Difference in Log Campaign Spending",
  ylab = "Presidential Vote Share",
  main = "Presidential Vote Share and Campaign Spending Difference"
)

abline(model2)

# 2.3 Save the residuals in a separate object
residuals2 <- residuals(model2)
head(residuals2)

# 2.4 Display coefficients
coef(model2)


# Question 3

# 3.1 Run the regression
model3 <- lm(voteshare ~ presvote, data = incumbents)
summary(model3)

# 3.2 Make a scatterplot and add the regression line
plot(
  incumbents$presvote,
  incumbents$voteshare,
  xlab = "Presidential Vote Share",
  ylab = "Incumbent Vote Share",
  main = "Incumbent Vote Share and Presidential Vote Share"
)

abline(model3)

# 3.3 Display coefficients
coef(model3)


# Question 4

# 4.1 Regress the residuals from Question 1
# on the residuals from Question 2
model4 <- lm(residuals1 ~ residuals2)
summary(model4)

# 4.2 Make a scatterplot and add the regression line
plot(
  residuals2,
  residuals1,
  xlab = "Residuals from Question 2",
  ylab = "Residuals from Question 1",
  main = "Residual Relationship"
)

abline(model4)

# 4.3 Display coefficients
coef(model4)


# Question 5

# 5.1 Run the multiple regression
model5 <- lm(voteshare ~ difflog + presvote, data = incumbents)
summary(model5)

# 5.2 Display coefficients
coef(model5)
