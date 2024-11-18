
library(dplyr)
library(tidyr)
library(mixmeta)
library(lmtest)

load("first-level_estimates.RData")

# Correlation matrix:
estimates %>% dplyr::select(-region) %>% cor(use = "na.or.complete") %>% View()

# Null model:
model0 <- mixmeta(coef_temp, Slist, data = estimates, method = "ml")
summary(model0)

# Model with region-level random effects:
model1a <- model0 %>% update(random = ~1|region)
model1b <- model0 %>% update(. ~ as.factor(year), random = ~1|region)

# Model with income variables as metapredictors:
model2a <- model1b %>% update(. ~ . + log(gdp) )
model2b <- model1b %>% update(. ~ . + log(income) )

# Model with control for education:
model3a <- model1b %>% update(. ~ . + hi_education_predicted)
model3b <- model2a %>% update(. ~ . + hi_education_predicted)
model3c <- model2b %>% update(. ~ . + hi_education_predicted)

# Model with control for average temperature:
model4a <- model1b %>% update(. ~ . + summer_t2m + winter_t2m)
model4b <- model1b %>% update(. ~ . + annual_t2m)
model4c <- model2b %>% update(. ~ . + summer_t2m + winter_t2m)
model4d <- model2b %>% update(. ~ . + annual_t2m)
model4e <- model3a %>% update(. ~ . + summer_t2m + winter_t2m)
model4f <- model3a %>% update(. ~ . + annual_t2m)
model4g0 <- model3b %>% update(. ~ . + summer_t2m)
model4g <- model3b %>% update(. ~ . + summer_t2m + winter_t2m)
model4h <- model3b %>% update(. ~ . + annual_t2m)
model4i0 <- model3c %>% update(. ~ . + summer_t2m)
model4i <- model3c %>% update(. ~ . + summer_t2m + winter_t2m)
model4j <- model3c %>% update(. ~ . + annual_t2m)

# LRtests with coefficients dropping:
LRtest1b = drop1(model1b, test = "Chisq")
LRtest2a = drop1(model2a, test = "Chisq")
LRtest2b = drop1(model2b, test = "Chisq")
LRtest3a = drop1(model3a, test = "Chisq")
LRtest3b = drop1(model3b, test = "Chisq")
LRtest3c = drop1(model3c, test = "Chisq")
LRtest4a = drop1(model4a, test = "Chisq")
LRtest4b = drop1(model4b, test = "Chisq")
LRtest4c = drop1(model4c, test = "Chisq")
LRtest4d = drop1(model4d, test = "Chisq")
LRtest4e = drop1(model4e, test = "Chisq")
LRtest4f = drop1(model4f, test = "Chisq")
LRtest4g = drop1(model4g, test = "Chisq")
LRtest4h = drop1(model4h, test = "Chisq")
LRtest4i = drop1(model4i, test = "Chisq")
LRtest4j = drop1(model4j, test = "Chisq")

model_list = list(model0,model1b,model2a,model2b,model3a,model3b,model3c,model4a,model4b,model4c,model4d,model4e,model4f,model4g,model4h,model4i,model4j)

# Model summaries:
Isq_stats = model_list %>% sapply(function(x) {c(as.character(summary(x)$call$formula)[3], summary(x)$i2stat[".all"]) } )
Isq_stats = data.frame(formula = Isq_stats[1,], statistics = as.numeric(Isq_stats[2,]))

Q_stats = model_list %>% sapply(function(x) { c(as.character(summary(x)$call$formula)[3], summary(x)$qstat$Q[".all"], summary(x)$qstat$df[".all"], summary(x)$qstat$pvalue[".all"]) } )
Q_stats = data.frame(formula = Q_stats[1,], statistics = as.numeric(Q_stats[2,]),df = as.numeric(Q_stats[3,]), p = as.numeric(Q_stats[4,]))

AIC_stats = model_list %>% sapply(function(x) { c(as.character(summary(x)$call$formula)[3], summary(x)$AIC, summary(x)$BIC) } )
AIC_stats = data.frame(formula = AIC_stats[1,], AIC = as.numeric(AIC_stats[2,]),BIC = as.numeric(AIC_stats[3,]))

waldtest(model1b,model2a,model3b,model4g0,model4g, test = "Chisq")
waldtest(model1b,model2b,model3c,model4i0,model4i, test = "Chisq")

# Model with variables reflecting housing conditions:
model5a1 <- model1b %>% update(.~.+air_conditioning_per_household)
model5a2 <- model1b %>% update(.~.+air_conditioning_prevalence)
model5a3 <- model1b %>% update(.~.+poor_housing)
model5a4 <- model1b %>% update(.~.+poor_heating)
model5b1 <- model4g %>% update(.~.+air_conditioning_per_household)
model5b2 <- model4g %>% update(.~.+air_conditioning_prevalence)
model5b3 <- model4g %>% update(.~.+poor_housing)
model5b4 <- model4g %>% update(.~.+poor_heating)
model5c1 <- model5b2 %>% update(.~.+poor_housing)
model5c2 <- model5b2 %>% update(.~.+poor_heating)
model5d1 <- model4i %>% update(.~.+air_conditioning_per_household)
model5d2 <- model4i %>% update(.~.+air_conditioning_prevalence)
model5d3 <- model4i %>% update(.~.+poor_housing)
model5d4 <- model4i %>% update(.~.+poor_heating)
model5d5 <- model4i %>% update(.~.+air_conditioning_prevalence+poor_housing)
model5d6 <- model4i %>% update(.~.+air_conditioning_prevalence+poor_heating)
model5e1 <- model4h %>% update(.~.+air_conditioning_per_household)
model5e2 <- model4h %>% update(.~.+air_conditioning_prevalence)
model5e3 <- model4h %>% update(.~.+poor_housing)
model5e4 <- model4h %>% update(.~.+poor_heating)
model5e5 <- model4h %>% update(.~.+air_conditioning_prevalence+poor_housing)
model5e6 <- model4h %>% update(.~.+air_conditioning_prevalence+poor_heating)

lmtest::lrtest(model5a1,model5a2,model5a3,model5a4,
               model5b1,model5b2,model5b3,model5b4,model5c1,model5c2,
               model5d1,model5d2,model5d3,model5d4,model5d5,model5d6,
               model5e1,model5e2,model5e3,model5e4,model5e5,model5e6)

waldtest(model5a2,model5b2,model5c2, test = "Chisq")
waldtest(model5a2,model5d2,model5d6, test = "Chisq")
waldtest(model5a2,model5e2,model5e6, test = "Chisq")

model_list2 = list(model5a1,model5a2,model5a3,model5a4,
                   model5b1,model5b2,model5b3,model5b4,model5c1,model5c2,
                   model5d1,model5d2,model5d3,model5d4,model5d5,model5d6,
                   model5e1,model5e2,model5e3,model5e4,model5e5,model5e6)

# Model summaries:
Isq_stats2 = model_list2 %>% sapply(function(x) {c(as.character(summary(x)$call$formula)[3], summary(x)$i2stat[".all"]) } )
Isq_stats2 = data.frame(formula = Isq_stats2[1,], statistics = as.numeric(Isq_stats2[2,]))
Q_stats2 = model_list2 %>% sapply(function(x) { c(as.character(summary(x)$call$formula)[3], summary(x)$qstat$Q[".all"], summary(x)$qstat$df[".all"], summary(x)$qstat$pvalue[".all"]) } )
Q_stats2 = data.frame(formula = Q_stats2[1,], statistics = as.numeric(Q_stats2[2,]),df = as.numeric(Q_stats2[3,]), p = as.numeric(Q_stats2[4,]))
AIC_stats2 = model_list2 %>% sapply(function(x) { c(as.character(summary(x)$call$formula)[3], summary(x)$AIC, summary(x)$BIC) } )
AIC_stats2 = data.frame(formula = AIC_stats2[1,], AIC = as.numeric(AIC_stats2[2,]),BIC = as.numeric(AIC_stats2[3,]))

save(list = c("estimates", "bvar", "mmt", ls(pattern="model")), file = "second-level_estimates.RData")

