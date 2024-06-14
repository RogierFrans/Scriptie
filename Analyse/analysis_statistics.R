library(readxl)
library(readr)
library(dplyr)
library(stargazer)
library(car)
library(knitr)
library(kableExtra)
library(plm)
library(lmtest)
library(sandwich)
library(ggplot2)

library(tidymodels)
library(tidyverse)
################
# import data ##
################
# read the data
df <- read_xlsx('C:/Users/megar/OneDrive/Documenten/Business Analytics Management/Scriptie/Output/dataset_using_5-6-2024.xlsx')

# remove dubplicate rows in sites
df <- df[!duplicated(df$site),]
original <- df

original <- original %>%
    mutate(Price = as.numeric(Price),
           popularity = as.numeric(popularity),
           hosts = as.numeric(hosts),
           trackers = as.numeric(trackers),
           companies = as.numeric(companies),
           Freechoice = as.numeric(Freechoice),
           Contentpass = as.numeric(Contentpass),
           pass = ifelse(Contentpass == 1 | Freechoice == 1, 1, 0))

original_pass <- original %>% filter(pass == 1)
original_no_pass <- original %>% filter(pass == 0)
original_no_pass %>% filter(category == "E-Commerce") %>% nrow()
# variables to use in the model
colnames_original <- colnames(original)

variables <- colnames_original[c(4,6,7:33)]

formula <- paste("log(Price) ~", paste(variables, collapse = " + "))

model_original <- lm(formula, data = original_no_pass)
summary(model_original)
vif(model_original)
# remove the ones with the highest vif

# I want to remove cookies from variabels
variables <- variables[!variables %in% c("cookies")]
formula2 <- paste("log(Price) ~", paste(variables, collapse = " + "))
model_original2 <- lm(formula2, data = original_no_pass)
summary(model_original2)
vif(model_original2)

variables <- variables[!variables %in% c("trackers")]
formula3 <- paste("log(Price) ~", paste(variables, collapse = " + "))
model_original3 <- lm(formula3, data = original_no_pass)
summary(model_original3)
vif(model_original3)

variables <- variables[!variables %in% c("requests")]
formula4 <- paste("log(Price) ~", paste(variables, collapse = " + "))
model_original4 <- lm(formula4, data = original_no_pass)
summary(model_original4)
vif(model_original4)


formula6 <- paste("log(Price) ~", paste(variables, collapse = " + "))
variables_test <- variables[!variables %in% c("hosts")]
formula7 <- paste("log(Price) ~", paste(variables_test, collapse = " + "))
model_original6 <- lm(formula6, data = original_no_pass)
summary(model_original6)
convif(model_original6)
# test for homoscedasticity
bptest(model_original6)
plot(model_original6)
stargazer(model, log_model, model_original6, keep = c("Intercept","countryeu", "countryfr", "popularity", "referer_leaked", "hosts", "companies"))
# test for normality
shapiro.test(model_original6$residuals)
hist(model_original6$residuals)
# based on the research question and the data I will only use the following columns
# 1) site
# 2) category.x
# 3) populatiry
# 4) price
# 5) country
# 6) trackers
# 7) companies
# 8) freechoice
# 9) contentpass
# 10) hosts

#make a prediction on the model
predict(model_original6, newdata = add[add$site == "europe1.fr",]) %>% exp()

# df <- df %>% select(site, category, popularity, Price, country, hosts, trackers, companies, Freechoice, Contentpass)
df$pass <- 0
df$pass[df$Contentpass == 1] <- 1
df$pass[df$Freechoice == 1] <- 1

df$category <- as.factor(df$category)
df$country <- as.factor(df$country)
df$Freechoice <- as.factor(df$Freechoice)
df$Contentpass <- as.factor(df$Contentpass)
df$pass <- as.factor(df$pass)

# not all the data is complete, I checked the rows with missing prices and only one of them has a paywall of 49 euros
df[df$site == "finanzmarktwelt.de", "Price"] <- 49
# this price can be considered unreasabily high so we would like to remove this entry alongside the entries without data on price
df <- df %>% 
    filter(!is.na(Price)) %>%
    filter(Price < 49)

#subset only the sites that have a pass
df_without_pass <- df %>% filter(pass == 0)
df_pass <- df %>% filter(pass == 1)

#######################
## T-test statistics ##
#######################

# test the assumptions of the t-test
# 1) normality
# 2) homogeneity of variance
# 3) independence

# check the normality of the data
#plot data to check for normality
hist(df_without_pass$popularity)
hist(df_pass$popularity)
hist(df_without_pass$hosts)
hist(df_pass$hosts)
hist(df_without_pass$Price)
hist(df_pass$Price)
hist(df_without_pass$trackers)
hist(df_pass$trackers)

# get the variances
var(df_without_pass$popularity)
var(df_pass$popularity)

var(df_without_pass$Price)
var(df_pass$Price)

var(df_without_pass$hosts)
var(df_pass$hosts)

var(df_without_pass$trackers)
var(df_pass$trackers)

var(df_without_pass$companies)
var(df_pass$companies)

shapiro.test(df_without_pass$popularity)
shapiro.test(df_pass$popularity)
qqnorm(df_without_pass$popularity)
qqline(df_without_pass$popularity)


shapiro.test(df_without_pass$Price)
shapiro.test(df_pass$Price)

shapiro.test(df_without_pass$hosts)
shapiro.test(df_pass$hosts)

shapiro.test(df_without_pass$trackers)
shapiro.test(df_pass$trackers)

shapiro.test(df_without_pass$companies)
shapiro.test(df_pass$companies)

# check the homogeneity of variance
leveneTest(popularity ~ pass, data = df)

leveneTest(Price ~ pass, data = df)

leveneTest(hosts ~ pass, data = df)

leveneTest(trackers ~ pass, data = df)

leveneTest(companies ~ pass, data = df)

# check the independence of the data
cor.test(df_without_pass$popularity, df_without_pass$Price)

# do the mann whitney u test, because the data is not normally distributed

wilcox.test(df_without_pass$popularity, df_pass$popularity)
wilcox.test(df_without_pass$Price, df_pass$Price)
wilcox.test(df_without_pass$hosts, df_pass$hosts)
wilcox.test(df_without_pass$trackers, df_pass$trackers)
wilcox.test(df_without_pass$companies, df_pass$companies)

wilcon_test_results <- data.frame(
  Variable = c("Popularity", "Price", "Hosts", "Trackers", "Companies"),

  `W-value` = c(wilcox.test(df_without_pass$popularity, df_pass$popularity)$statistic, wilcox.test(df_without_pass$Price, df_pass$Price)$statistic, wilcox.test(df_without_pass$hosts, df_pass$hosts)$statistic, wilcox.test(df_without_pass$trackers, df_pass$trackers)$statistic, wilcox.test(df_without_pass$companies, df_pass$companies)$statistic),
  `p-value` = c(wilcox.test(df_without_pass$popularity, df_pass$popularity)$p.value, wilcox.test(df_without_pass$Price, df_pass$Price)$p.value, wilcox.test(df_without_pass$hosts, df_pass$hosts)$p.value, wilcox.test(df_without_pass$trackers, df_pass$trackers)$p.value, wilcox.test(df_without_pass$companies, df_pass$companies)$p.value)

)

wilcon_test_results
# Perform t-tests
t_test_popularity <- t.test(df_without_pass$popularity, df_pass$popularity)
t_test_price <- t.test(df_without_pass$Price, df_pass$Price)
t_test_category1 <- t.test(dummy(df_without_pass$category), df_pass$category)
t_test_hosts <- t.test(df_without_pass$hosts, df_pass$hosts)
t_test_trackers <- t.test(df_without_pass$trackers, df_pass$trackers)
t_test_companies <- t.test(df_without_pass$companies, df_pass$companies)

# Create a data frame for t-test results
t_test_results <- data.frame(
  Variable = c("Popularity", "Price", "Hosts", "Trackers"),
  `t-value` = c(t_test_popularity$statistic, t_test_price$statistic, t_test_hosts$statistic, t_test_trackers$statistic),
  `p-value` = c(t_test_popularity$p.value, t_test_price$p.value, t_test_hosts$p.value, t_test_trackers$p.value),
  `Mean (No Pass)` = c(mean(df_without_pass$popularity, na.rm = TRUE), mean(df_without_pass$Price, na.rm = TRUE), mean(df_without_pass$hosts, na.rm = TRUE), mean(df_without_pass$trackers, na.rm = TRUE)),
  `Mean (Pass)` = c(mean(df_pass$popularity, na.rm = TRUE), mean(df_pass$Price, na.rm = TRUE), mean(df_pass$hosts, na.rm = TRUE), mean(df_pass$trackers, na.rm = TRUE))
)


t_test_results
# Create a table using kableExtra for LaTeX output
latex_table <- t_test_results %>%
  kable("latex", booktabs = TRUE, col.names = c("Variable", "t-value", "p-value", "Mean (No Pass)", "Mean (Pass)")) %>%
  kable_styling(latex_options = c("striped", "hold_position"))

# Print the LaTeX code
latex_table

#################################
## subquestion 2: Model making ##
#################################
# \section{Subquestion 2: Which characteristics determines the price of a cookie paywall within the current cookie paywall landscape?}
# For the second sub-question, data sets (1), (2) and (3) will be combined to see if it is possible to estimate a price from the characteristics of the enriched website. For this analysis of the price only the website that does not make use of a bundle service, like Contentpass of Freechoice. The reason for this, like explained above, is that it is difficult to interpreter the price of website with contentpass or freechoice, because of the bundle deal. 

# The analysis done by running an simple OLS regression.  however, it is important to check for the assumptions. For this regression the log of the price is used as a Independent Variable and the ... is used as the dependent vairable. The reason for the logarithm scale is because this will lead to lower hetereoskedasity. It is hypothesised that these variables would have effect on the amount of which a cookie paywall is priced:

# Need to come back when i have checked the assumptions.

# [Each variable and why it is used in relation to price]

# This will be checked by making the following model:
# \[
# \log(\text{Price}) = \beta_0 + \beta_1 \cdot \text{pass} + \beta_2 \cdot \text{category.x} + \beta_3 \cdot \log(\text{popularity}) + \beta_4 \cdot \text{hosts} + \epsilon
# \]


# An OLS regression is used because it is the easiest model to see which variables effects the price. By using this method as opposed to other methods it will give an easy to interpret results.

# []

# lets make a simple linear regression model

model <- lm(Price ~ country + category + popularity+hosts+ companies, data = df_without_pass)
log_model <- lm(log(Price) ~ country + category + popularity+hosts+companies, data = df_without_pass)

summary(model)
summary(log_model)

# check the assumptions of the model

hist(log10(df_without_pass$Price))
hist(df_without_pass$Price)

# 1) multicollinearity
vif(model)
vif(log_model)
# 2) homoscedasticity
bptest(log_model)
bptest(model)
# linearity
plot(model)
plot(log_model)
# 3) no autocorrelation
dwtest(model)
dwtest(log_model)
# 4) normality of residuals
shapiro.test(log_model$residuals)
shapiro.test(model$residuals)

hist(model$residuals)
hist(log_model$residuals)
# or a qqplot
qqnorm(model$residuals)
qqline(model$residuals)

qqnorm(log_model$residuals)
qqline(log_model$residuals)
# plot the residuals
plot(model)
plot(log_model)
abline(0,0)

plot(density(model$residuals))
#ftest two models
f_test <- anova(model, log_model)
print(f_test)

robust_se <- coeftest(log_model, vcov = vcovHC(log_model, type = "HC1"))
robust_se <- coeftest(model_original6, vcov = vcovHC(model_original6, type = "HC0"))
robust_se
stargazer(model_original6, type = "text")

summary(f_test)
bptest(model_original6)
# make a stargazer table with the robust standard errors
stargazer(model_original6, robust_se, type = "text", title = "Regression models for the price of a cookie paywall", align = TRUE, dep.var.caption = "Dependent variable: Price", dep.var.labels = "Price", dep.var.labels.include = FALSE)
anova(log_model, model_original6)

waldtest(log_model, model_original6, vcov = vcovHC(model_original6, type = "HC0"))

stargazer(model, log_model, type = "text", title = "Regression models for the price of a cookie paywall", align = TRUE, dep.var.caption = "Dependent variable: Price", dep.var.labels = "Price", dep.var.labels.include = FALSE)

