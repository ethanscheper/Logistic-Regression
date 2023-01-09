# Log Reg HW Report 2
library(car)
library(tidyverse)

# Read in the binned data
setwd("/Users/ethanscheper/Documents/AA 502/Logistic Regression/HW 2")
train <- read.csv("insurance_t_bin.csv")
validate <- read.csv("insurance_v_bin.csv")

# Change data to include a missing category for data with missing values
train[is.na(train)] = 'M'

# Check each variable for separation concerns
sep_list <- list()
for (i in 1:length(train)) {
  i_name <- names(train[i])
  freqs <- table(train[[i_name]], train$INS)
  test <- sapply(freqs, function(x) x==0)
  if (TRUE %in% test) {
      sep <- i_name
      sep_list <- append(sep_list, sep)
  }
}
# 2 predictors with quasi-complete separation issues - CASHBK and MMCRED

# Investigate the separation issues further
table(train$CASHBK, train$INS)
table(train$MMCRED, train$INS)

# Collapse the high end categories in CASHBK and MMCRED to adjust for separation

# Collapse the 1 and 2 categories in CASHBK to make just two categories - 
  # 0 for no cash back requests and 1+ for at least 1 cash back request
train$CASHBK[train$CASHBK == 1] = '1+'
train$CASHBK[train$CASHBK == 2] = '1+'
table(train$CASHBK, train$INS)

# Collapse the 3 and 5 categories in MMCRED to make just four total categories - 
  # 3+ for at least 3 money market credits
train$MMCRED[train$MMCRED == 3] = '3+'
train$MMCRED[train$MMCRED == 5] = '3+'
table(train$MMCRED, train$INS)

# Now that all variables are categorized and corrected, make them all factors
train <- as.data.frame(lapply(train, function(x) as.factor(x)))

# Create a main effects only bin log reg model predicting INS
# Use backward selection with p-value criteria (alpha = 0.002)
full.model <- glm(INS ~ ., data = train, family = binomial(link = "logit"))
back.model <- step(full.model, direction = "backward",
                   k = qchisq(0.002, 1, lower.tail = F))
summary(back.model)

# Find out which variable has the perfect multicollinearity with CCM
test_df <- train %>%
  select(DDA, NSF, IRA, INV, ILS, MM, MTG, CC, DDABAL_Bin, CHECKS_Bin, 
         TELLER_Bin, SAVBAL_Bin, ATMAMT_Bin, CDBAL_Bin)

for (i in 1:length(test_df)) {
  i_name <- names(test_df[i])
  if (i_name != "CC") {
    print(i_name)
    print(table(train[[i_name]], train$CC))
  }
}
# INV (missing) and CC (missing) are perfect aliases

# Report final variables of the model ranked by p-value
# First obtain p-values for each variable (categorical, not its individual
# dummies) using LRT
p_values <- car::Anova(back.model, test = 'LR', type = 'III', singular.ok = T)
# Now manipulate the data to be formatted well and order by p-value
p_values$Variable <- rownames(p_values)
rownames(p_values) <- 1:nrow(p_values)
p_values <- p_values %>% 
  select(Variable, `Pr(>Chisq)`) %>%
  arrange(`Pr(>Chisq)`) %>%
  rename(`p value` = `Pr(>Chisq)`)

# Interpret one variable's odds ratio as an example
summary(back.model)
# Customers with an insufficient fund issue are 1.401 times more likely to 
# purchase a variable annuity product than those with 0 insufficient fund
# issues.

# Investigate possible interactions using forward selection
# Use only main effects selected in the previous model

# Do the forward selection
final.model <- step(back.model,
                  scope = . ~ .^2,
                  direction = "forward",
                  k = qchisq(0.002, 1, lower.tail = FALSE))
# DDA and IRA was the only interaction

# Report final logistic regression model's variables by significance
p_values_final <- car::Anova(final.model, test = 'LR', type = 'III', singular.ok = T)
# Now manipulate the data to be formatted well and order by p-value
p_values_final$Variable <- rownames(p_values_final)
rownames(p_values_final) <- 1:nrow(p_values_final)
p_values_final <- p_values_final %>% 
  select(Variable, `Pr(>Chisq)`) %>%
  arrange(`Pr(>Chisq)`) %>%
  rename(`p value` = `Pr(>Chisq)`)

# Write out necessary tables for the report
write.csv(p_values, "main effects.csv")
write.csv(p_values_final, "final model.csv")




