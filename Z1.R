DS1 = read.csv("https://tinyurl.com/ha-dataset1")

library(psych) # for describe	
library(car) # for residualPlots, vif, pairs.panels, ncvTest	
library(lmtest) # bptest	
library(sandwich) # for coeftest vcovHC estimator	
library(boot) # for bootstrapping	
library(lmboot) # for wild bootsrapping	
library(tidyverse) # for tidy code	
library(gridExtra)	

DS1 %>% 
  summary()
str(DS1)

#Corrections
DS1 = DS1 %>%
  mutate(ID = factor(ID), sex = factor(sex))

DS1[88, 2] = 5

DS1 %>% 
  summary()

which(grepl("4.20", DS1$STAI_trait))

DS1[34, 5] = 42

DS1 %>% 
  summary()

DS1[1, 12] = 3628

DS1 %>% 
  summary()

describe(DS1)
str(DS1)
#model creation

mod1 = lm(pain ~ age + sex, data =  DS1)	
summary(mod1)	

mod2 = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = DS1)
summary(mod2)

#checking the data
DS1 %>% 	
  mutate(rownum = row.names(DS1)) %>% 	
  ggplot() +	
  aes(x = age, y = pain, label = rownum) +	
  geom_label()

DS1 %>% 	
  mutate(rownum = row.names(DS1)) %>% 	
  ggplot() +	
  aes(x = STAI_trait, y = pain, label = rownum) +	
  geom_label()

DS1 %>% 	
  mutate(rownum = row.names(DS1)) %>% 	
  ggplot() +	
  aes(x = pain_cat, y = pain, label = rownum) +	
  geom_label()

DS1 %>% 	
  mutate(rownum = row.names(DS1)) %>% 	
  ggplot() +	
  aes(x = mindfulness, y = pain, label = rownum) +	
  geom_label()

DS1 %>% 	
  ggplot() +	
  aes(x = cortisol_serum, y = pain) +	
  geom_point() +	
  geom_smooth(method = "lm")	



#cook's distance
mod2 %>%
  plot(which = 5)

mod1 %>% 
  plot (which = 5)

mod2 %>% 
  plot(which = 4)

mod1 %>% 
  plot(which = 4)

DS1 %>%
  slice(c(47, 65, 86))

# QQ plot
mod2 %>%
  plot(which = 2)

mod1 %>%
  plot(which = 2)

# histogram
residuals_mod2 = enframe(residuals(mod2))
residuals_mod2 %>%
  ggplot() + aes(x = value) + geom_histogram()

residuals_mod1 = enframe(residuals(mod1))
residuals_mod1 %>%
  ggplot() + aes(x = value) + geom_histogram()

# skew and kurtosis
describe(residuals(mod2))

describe(residuals(mod1))
#linearity
mod2 %>% 
  residualPlots()

# NCV test
mod2 %>% 
  ncvTest()
mod1 %>% 
  ncvTest()

# Multicollinearity test
mod2 %>% 
vif()

mod1 %>% 
  vif()
summary(mod2)

AIC(mod2, mod1)

summary(mod2)
summary(mod1)

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

coef_table(mod1)
coef_table(mod2)

# compare AIC
AIC(mod1, mod2)
anova(mod1, mod2)


#post hoc

mod3 = lm(pain ~ pain_cat + mindfulness + cortisol_serum, data =  DS1)	
summary(mod3)	

DS1 %>% 	
  mutate(rownum = row.names(DS1)) %>% 	
  ggplot() +	
  aes(x = mindfulness, y = pain_cat, label = rownum) +	
  geom_point() +
  geom_smooth(method = "lm")

mod4data <- DS1[c("cortisol_serum", "mindfulness", "pain_cat")]
view(mod4data)

cor(mod4data)
view(DS1)
