DS1 = read.csv("https://tinyurl.com/ha-dataset1")

library(psych) # for describe	
library(car) # for residualPlots, vif, pairs.panels, ncvTest	
library(lmtest) # bptest	
library(sandwich) # for coeftest vcovHC estimator	
library(boot) # for bootstrapping	
library(lmboot) # for wild bootsrapping	
library(tidyverse) # for tidy code	
library(gridExtra)	


str(DS1)
DS1 = DS1 %>%
  mutate(ID = factor(ID), sex = factor(sex))

#corrections

DS1[88, 2] = 5


which(grepl("4.20", DS1$STAI_trait))

DS1[34, 5] = 42

DS1[1, 12] = 3628

DS1 %>% 
  summary()

describe(DS1)

#checking the new variables

DS1 %>% 	
  mutate(rownum = row.names(DS1)) %>% 	
  ggplot() +	
  aes(x = IQ, y = pain, label = rownum) +	
  geom_label()

DS1 %>% 	
  mutate(rownum = row.names(DS1)) %>% 	
  ggplot() +	
  aes(x = weight, y = pain, label = rownum) +	
  geom_label()

DS1 %>% 	
  mutate(rownum = row.names(DS1)) %>% 	
  ggplot() +	
  aes(x = household_income, y = pain, label = rownum) +	
  geom_label()

#model creation
theory_mod = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = DS1)
summary(theory_mod)

mod_all <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + weight + IQ + household_income, data = DS1)

back_mod_all = step(mod_all, direction = "backward")

summary(back_mod_all)

#checking the model

back_mod_all %>%
  plot(which = 5)

back_mod_all %>% 
  plot(which = 4)

# QQ plot
back_mod_all %>%
  plot(which = 2)

# histogram
residuals_back = enframe(residuals(back_mod_all))
residuals_back %>%
  ggplot() + aes(x = value) + geom_histogram()

# skew and kurtosis
describe(residuals(back_mod_all))

#linearity
back_mod_all %>% 
  residualPlots()

# NCV test
back_mod_all %>% 
  ncvTest()

# Multicollinearity test
back_mod_all %>% 
  vif()

#AIC check
AIC(theory_mod, back_mod_all)

anova(back_mod_all, theory_mod)

# confidence intervals (Zoltan's function)
coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}
coef_table(back_mod_all)

#Importing new data
new_data = read.csv("https://tinyurl.com/87v6emky")

view(new_data)

# calculate predicted values
pred_test_theory <- predict(theory_mod, new_data)
pred_test_back <- predict(back_mod_all, new_data)

summary(pred_test_theory)
summary(pred_test_back)

# sum of squared residuals RSS
RSS_test_theory = sum((new_data[, "pain"] - pred_test_theory)^2)
RSS_test_back = sum((new_data[, "pain"] - pred_test_back)^2)
RSS_test_theory
RSS_test_back


