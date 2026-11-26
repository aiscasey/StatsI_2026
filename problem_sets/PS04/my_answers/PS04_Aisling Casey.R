# Problem Set 4
# Aisling Casey

# Load package and data
library(car)
data(Prestige)

# -------------------------
# Question 1(a)
# -------------------------

# Create dummy variable:
# professional = 1
# blue-collar / white-collar = 0

Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

# Check coding
table(Prestige$type, Prestige$professional)


# -------------------------
# Question 1(b)
# -------------------------

# Regression with income, professional status,
# and their interaction

model1 <- lm(prestige ~ income * professional, data = Prestige)

summary(model1)


# -------------------------
# Question 1(c)
# -------------------------

# Display coefficients for prediction equation

coef(model1)


# -------------------------
# Question 1(d)
# -------------------------

# Effect of a $1,000 increase in income
# for non-professional occupations

income_effect_nonprofessional <- coef(model1)["income"] * 1000
income_effect_nonprofessional


# -------------------------
# Question 1(e)
# -------------------------

# Difference between professional and non-professional
# occupations when income = 0

professional_effect_income0 <- coef(model1)["professional"]
professional_effect_income0


# -------------------------
# Question 1(f)
# -------------------------

# Effect of a $1,000 increase in income
# for professional occupations

income_effect_professional <-
  (coef(model1)["income"] +
     coef(model1)["income:professional"]) * 1000

income_effect_professional


# -------------------------
# Question 1(g)
# -------------------------

# Effect of changing from non-professional to
# professional when income = $6,000

professional_effect_6000 <-
  coef(model1)["professional"] +
  coef(model1)["income:professional"] * 6000

professional_effect_6000


# -------------------------
# Question 2(a)
# -------------------------

# Hypothesis test calculation for precincts
# assigned lawn signs

beta_signs <- 0.042
se_signs <- 0.016

t_signs <- beta_signs / se_signs
t_signs


# -------------------------
# Question 2(b)
# -------------------------

# Hypothesis test calculation for precincts
# adjacent to lawn-sign precincts

beta_adjacent <- 0.042
se_adjacent <- 0.013

t_adjacent <- beta_adjacent / se_adjacent
t_adjacent


# -------------------------
# Question 2(c)
# -------------------------

# Constant from supplied regression table

constant <- 0.302
constant


# -------------------------
# Question 2(d)
# -------------------------

# R-squared and unexplained proportion

r_squared <- 0.094

explained_percent <- r_squared * 100
unexplained_percent <- (1 - r_squared) * 100

explained_percent
unexplained_percent