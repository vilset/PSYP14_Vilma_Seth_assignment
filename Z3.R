DS3 = read.csv("https://tinyurl.com/b385chpu")
DS4 = read.csv("https://tinyurl.com/4f8thztv")

library(psych) # for describe	
library(car) # for residualPlots, vif, pairs.panels, ncvTest	
library(lmtest) # bptest	
library(sandwich) # for coeftest vcovHC estimator	
library(boot) # for bootstrapping	
library(lmboot) # for wild bootsrapping	
library(cAIC4) # for cAIC\t
library(r2glmm) # for r2beta\t
library(lme4) # for lmer
library(lmerTest) # to get singificance test in lmer
library(MuMIn) # for r.squaredGLMM
library(tidyverse) # for tidy code	
library(gridExtra)	
library(influence.ME)

#custom function
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))}

#checking the data
summary(DS3)
str(DS3)

#correction Factors

DS3 = DS3 %>%
  mutate(ID = factor(ID), sex = factor(sex), hospital = factor(hospital))
str(DS3)

str(DS3)
levels(DS3$hospital)
table(DS3$hospital)

#Correction of negative income, believed typo

which(grepl("-7884", DS3$household_income))
DS3[2, 12] = "7884"

#Checking data set 4

summary(DS4)
str(DS4)

#corrections
DS4 = DS4 %>%
  mutate(ID = factor(ID), sex = factor(sex), hospital = factor(hospital))

# create random intercept model for data set 3
RIM_DS3 = lmer(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1 | hospital), data = DS3)

summary(RIM_DS3)

#Exploring clustering
DS3 %>%
  ggplot() + aes(y = pain, x = age) + 
  geom_point(aes(color = hospital), size = 4) + 
  geom_smooth(method = "lm", se = F)

DS3 %>%
  ggplot() + aes(y = pain, x = sex) + 
  geom_point(aes(color = hospital), size = 4) + 
  geom_smooth(method = "lm", se = F)

DS3 %>%
  ggplot() + aes(y = pain, x = STAI_trait) + 
  geom_point(aes(color = hospital), size = 4) + 
  geom_smooth(method = "lm", se = F)

int_plot = DS3 %>%
  ggplot() + aes(y = pain, x = age + STAI_trait + pain_cat + mindfulness + cortisol_serum, color = hospital) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE)

int_plot
mod_fixed_int = lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + hospital, data = DS3)

mod_fixed_int
summary(mod_fixed_int)

int_plot + xlim(-1, 50) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)

#Checking the assumptions

#Normality !!!!
qqmath(RIM_DS3, id = 0.05)


# QQ plot !!!!!
qqmath(ranef(RIM_DS3))


# histogram
residuals_RIM_DS3 = enframe(residuals(RIM_DS3))
residuals_RIM_DS3 %>%
  ggplot() + aes(x = value) + geom_histogram()


# skew and kurtosis
describe(residuals(RIM_DS3))

#Homoscedasticity !!!!!
plot(RIM_DS3, arg = "pearson")

# Multicollinearity test
RIM_DS3 %>% 
  vif()



# confidence intervals + coefficents

round(confint(RIM_DS3), digits=2)
round(stdCoef.merMod(RIM_DS3), digits=2)
summary(RIM_DS3)

round(confint(mod2), digits=2)
summary(mod2)



# Marginal R2
r2beta(RIM_DS3, method = "nsj", data = DS3)

# marginal and conditional R2 values
round(r.squaredGLMM(RIM_DS3), digits=2)

pred_DS4_RIM_DS3 <- predict(RIM_DS3, DS4, allow.new.levels = TRUE)

summary(DS4)
view(DS4)

summary(pred_DS4_RIM_DS3)

pred_DS4_RIM_DS3
mean(DS4$pain)

#RSS
RSS_pred_DS4_RIM_DS3 = sum((DS4[, "pain"] - pred_DS4_RIM_DS3)^2)
RSS_pred_DS4_RIM_DS3

#TSS
mod_mean_DS4 <- lm(pain ~ 1, data = DS4)	
TSS_pred_DS4_RIM_DS3 = sum((DS4$pain - predict(mod_mean_DS4))^2)	

#R2 of RIM_DS3 on DS4
R2_pred_DS4_RIM_DS3 = 1-(RSS_pred_DS4_RIM_DS3/TSS_pred_DS4_RIM_DS3)	
R2_pred_DS4_RIM_DS3



# Random slope model
RSM_DS3 = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital),
                     data = DS3)

RSM_DS3
# Singular fit warning, what to do??


#Comparing the models

DS3 = DS3 %>%
  mutate(pred_int = predict(RIM_DS3), pred_slope = predict(RSM_DS3))
  

DS3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 4) + geom_line(color = "red",
                                                       aes(y = pred_slope, x = cortisol_serum)) + facet_wrap(~hospital, ncol = 2)

anova(RIM_DS3, RSM_DS3)   

cAIC(RIM_DS3)$caic
cAIC(RSM_DS3)$caic
