
library(dplyr)
library(tidyr)
library(mixmeta)

load("second-level_estimates.RData")

years_list = c(2004,2008,2012,2016)

pred0 <- predict(model0, vcov=T)
cp0 <- crosspred(bvar, coef=pred0[[1]]$fit, vcov=pred0[[1]]$vcov,
                 model.link="log", cen=mmt)
cp0 = cbind(cp0$allRRfit,cp0$allRRhigh,cp0$allRRlow)
cp0 = cp0 %>% cbind(as.numeric(rownames(cp0)))
cp0 = as.data.frame(cp0)
colnames(cp0) = c("allRRfit","allRRhigh","allRRlow","temp")

pred1 <- predict(model1b, vcov=T)
cp1 <- crosspred(bvar, coef=pred1[[1]]$fit, vcov=pred1[[1]]$vcov,
                 model.link="log", cen=mmt)
cp1 = cbind(cp1$allRRfit,cp1$allRRhigh,cp1$allRRlow)
cp1 = cp1 %>% cbind(as.numeric(rownames(cp1)))
cp1 = as.data.frame(cp1)
colnames(cp1) = c("allRRfit","allRRhigh","allRRlow","temp")

# Scenario predictions by GDP
pred_gdp = predict(model5c2, 
                   newdata = data.frame(gdp = quantile(estimates$gdp,c(0.05,0.5,0.95)),
                                        hi_education_predicted = mean(estimates$hi_education_predicted),
                                        summer_t2m = mean(estimates$summer_t2m),
                                        winter_t2m = mean(estimates$winter_t2m), 
                                        poor_heating = mean(estimates$poor_heating, na.rm = T), 
                                        air_conditioning_prevalence = mean(estimates$air_conditioning_prevalence, na.rm = T), 
                                        year = 2018),
                   vcov=T)
cp_gdp_low <- crosspred(bvar, coef=pred_gdp[[1]]$fit, vcov=pred_gdp[[1]]$vcov, model.link="log", cen=mmt)
cp_gdp_mid <- crosspred(bvar, coef=pred_gdp[[2]]$fit, vcov=pred_gdp[[2]]$vcov, model.link="log", cen=mmt)
cp_gdp_hi <- crosspred(bvar, coef=pred_gdp[[3]]$fit, vcov=pred_gdp[[3]]$vcov, model.link="log", cen=mmt)
cp_gdp = rbind(cbind(cp_gdp_low$allRRfit,cp_gdp_low$allRRhigh,cp_gdp_low$allRRlow),
               cbind(cp_gdp_mid$allRRfit,cp_gdp_mid$allRRhigh,cp_gdp_mid$allRRlow),
               cbind(cp_gdp_hi$allRRfit,cp_gdp_hi$allRRhigh,cp_gdp_hi$allRRlow))
cp_gdp = cp_gdp %>% cbind(c(rep(quantile(estimates$gdp,0.05),length(cp_gdp_hi$allfit)),
                            rep(quantile(estimates$gdp,0.5),length(cp_gdp_hi$allfit)),
                            rep(quantile(estimates$gdp,0.95),length(cp_gdp_hi$allfit))))
cp_gdp = cp_gdp %>% cbind(as.numeric(rownames(cp_gdp)))
cp_gdp = as.data.frame(cp_gdp)
colnames(cp_gdp) = c("allRRfit","allRRhigh","allRRlow","type","temp")

# Scenario predictions by income
pred_income = predict(model5d6, 
                      newdata = data.frame(income = quantile(estimates$income,c(0.05,0.5,0.95)),
                                           hi_education_predicted = mean(estimates$hi_education_predicted),
                                           summer_t2m = mean(estimates$summer_t2m),
                                           winter_t2m = mean(estimates$winter_t2m), 
                                           poor_heating = mean(estimates$poor_heating, na.rm = T), 
                                           air_conditioning_prevalence = mean(estimates$air_conditioning_prevalence, na.rm = T),
                                           year = 2018),
                      vcov=T)
cp_income_low <- crosspred(bvar, coef=pred_income[[1]]$fit, vcov=pred_income[[1]]$vcov, model.link="log", cen=mmt)
cp_income_mid <- crosspred(bvar, coef=pred_income[[2]]$fit, vcov=pred_income[[2]]$vcov, model.link="log", cen=mmt)
cp_income_hi <- crosspred(bvar, coef=pred_income[[3]]$fit, vcov=pred_income[[3]]$vcov, model.link="log", cen=mmt)
cp_income = rbind(cbind(cp_income_low$allRRfit,cp_income_low$allRRhigh,cp_income_low$allRRlow),
                  cbind(cp_income_mid$allRRfit,cp_income_mid$allRRhigh,cp_income_mid$allRRlow),
                  cbind(cp_income_hi$allRRfit,cp_income_hi$allRRhigh,cp_income_hi$allRRlow))
cp_income = cp_income %>% cbind(c(rep(quantile(estimates$income,0.05),length(cp_income_hi$allfit)),
                                  rep(quantile(estimates$income,0.5),length(cp_income_hi$allfit)),
                                  rep(quantile(estimates$income,0.95),length(cp_income_hi$allfit))))
cp_income = cp_income %>% cbind(as.numeric(rownames(cp_income)))
cp_income = as.data.frame(cp_income)
colnames(cp_income) = c("allRRfit","allRRhigh","allRRlow","type","temp")

# Scenario predictions by proportion of individuals with degree
pred_educ = predict(model5c2, 
                    newdata = data.frame(hi_education_predicted = quantile(estimates$hi_education_predicted,c(0.05,0.5,0.95)),
                                         gdp = mean(estimates$gdp),
                                         summer_t2m = mean(estimates$summer_t2m),
                                         winter_t2m = mean(estimates$winter_t2m),
                                         poor_heating = mean(estimates$poor_heating, na.rm = T), 
                                         air_conditioning_prevalence = mean(estimates$air_conditioning_prevalence, na.rm = T), year = 2018), vcov=T)

cp_educ_low <- crosspred(bvar, coef=pred_educ[[1]]$fit, vcov=pred_educ[[1]]$vcov, model.link="log", cen=mmt)
cp_educ_mid <- crosspred(bvar, coef=pred_educ[[2]]$fit, vcov=pred_educ[[2]]$vcov, model.link="log", cen=mmt)
cp_educ_hi <- crosspred(bvar, coef=pred_educ[[3]]$fit, vcov=pred_educ[[3]]$vcov, model.link="log", cen=mmt)
cp_educ = rbind(cbind(cp_educ_low$allRRfit,cp_educ_low$allRRhigh,cp_educ_low$allRRlow),
                cbind(cp_educ_mid$allRRfit,cp_educ_mid$allRRhigh,cp_educ_mid$allRRlow),
                cbind(cp_educ_hi$allRRfit,cp_educ_hi$allRRhigh,cp_educ_hi$allRRlow))
cp_educ = cp_educ %>% cbind(c(rep(quantile(estimates$hi_education_predicted,0.05),length(cp_educ_hi$allfit)),
                              rep(quantile(estimates$hi_education_predicted,0.5),length(cp_educ_hi$allfit)),
                              rep(quantile(estimates$hi_education_predicted,0.95),length(cp_educ_hi$allfit))))
cp_educ = cp_educ %>% cbind(as.numeric(rownames(cp_educ)))
cp_educ = as.data.frame(cp_educ)
colnames(cp_educ) = c("allRRfit","allRRhigh","allRRlow","type","temp")

# Scenario predictions by average summer temperatures
pred_summer = predict(model5c2, 
                      newdata = data.frame(summer_t2m = quantile(estimates$summer_t2m, c(0.05,0.5,0.95)),
                                           gdp = mean(estimates$gdp),
                                           hi_education_predicted = mean(estimates$hi_education_predicted),
                                           poor_heating = mean(estimates$poor_heating, na.rm = T),
                                           air_conditioning_prevalence = mean(estimates$air_conditioning_prevalence, na.rm = T),
                                           winter_t2m = mean(estimates$winter_t2m), year = 2018),
                      vcov=T)
cp_summer_low <- crosspred(bvar, coef=pred_summer[[1]]$fit, vcov=pred_summer[[1]]$vcov, model.link="log", cen=mmt)
cp_summer_mid <- crosspred(bvar, coef=pred_summer[[2]]$fit, vcov=pred_summer[[2]]$vcov, model.link="log", cen=mmt)
cp_summer_hi <- crosspred(bvar, coef=pred_summer[[3]]$fit, vcov=pred_summer[[3]]$vcov, model.link="log", cen=mmt)
cp_summer = rbind(cbind(cp_summer_low$allRRfit,cp_summer_low$allRRhigh,cp_summer_low$allRRlow),
                  cbind(cp_summer_mid$allRRfit,cp_summer_mid$allRRhigh,cp_summer_mid$allRRlow),
                  cbind(cp_summer_hi$allRRfit,cp_summer_hi$allRRhigh,cp_summer_hi$allRRlow))
cp_summer = cp_summer %>% cbind(c(rep(quantile(estimates$summer_t2m,0.05),length(cp_summer_hi$allfit)),
                                  rep(quantile(estimates$summer_t2m,0.5),length(cp_summer_hi$allfit)),
                                  rep(quantile(estimates$summer_t2m,0.95),length(cp_summer_hi$allfit))))
cp_summer = cp_summer %>% cbind(as.numeric(rownames(cp_summer)))
cp_summer = as.data.frame(cp_summer)
colnames(cp_summer) = c("allRRfit","allRRhigh","allRRlow","type","temp")

# Scenario predictions by average winter temperatures
pred_winter = predict(model5c2,newdata = data.frame(winter_t2m = quantile(estimates$winter_t2m, c(0.05,0.5,0.95)),
                                                    gdp = mean(estimates$gdp),
                                                    hi_education_predicted = mean(estimates$hi_education_predicted),
                                                    poor_heating = mean(estimates$poor_heating, na.rm = T),
                                                    air_conditioning_prevalence = mean(estimates$air_conditioning_prevalence, na.rm = T),
                                                    summer_t2m = mean(estimates$summer_t2m), year = 2018),
                      vcov=T)
cp_winter_low <- crosspred(bvar, coef=pred_winter[[1]]$fit, vcov=pred_winter[[1]]$vcov, model.link="log", cen=mmt)
cp_winter_mid <- crosspred(bvar, coef=pred_winter[[2]]$fit, vcov=pred_winter[[2]]$vcov, model.link="log", cen=mmt)
cp_winter_hi <- crosspred(bvar, coef=pred_winter[[3]]$fit, vcov=pred_winter[[3]]$vcov, model.link="log", cen=mmt)
cp_winter = rbind(cbind(cp_winter_low$allRRfit,cp_winter_low$allRRhigh,cp_winter_low$allRRlow),
                  cbind(cp_winter_mid$allRRfit,cp_winter_mid$allRRhigh,cp_winter_mid$allRRlow),
                  cbind(cp_winter_hi$allRRfit,cp_winter_hi$allRRhigh,cp_winter_hi$allRRlow))
cp_winter = cp_winter %>% cbind(c(rep(quantile(estimates$winter_t2m,0.05),length(cp_winter_hi$allfit)),
                                  rep(quantile(estimates$winter_t2m,0.5),length(cp_winter_hi$allfit)),
                                  rep(quantile(estimates$winter_t2m,0.95),length(cp_winter_hi$allfit))))
cp_winter = cp_winter %>% cbind(as.numeric(rownames(cp_winter)))
cp_winter = as.data.frame(cp_winter)
colnames(cp_winter) = c("allRRfit","allRRhigh","allRRlow","type","temp")

# Scenario predictions by average annual temperatures
pred_annual = predict(model5e6,
                      newdata = data.frame(annual_t2m = quantile(estimates$annual_t2m, c(0.05,0.5,0.95)),
                                                    gdp = mean(estimates$gdp),
                                                    hi_education_predicted = mean(estimates$hi_education_predicted),
                                                    poor_heating = mean(estimates$poor_heating, na.rm = T),
                                                    air_conditioning_prevalence = mean(estimates$air_conditioning_prevalence, na.rm = T), year = 2018), vcov=T)

cp_annual_low <- crosspred(bvar, coef=pred_annual[[1]]$fit, vcov=pred_annual[[1]]$vcov, model.link="log", cen=mmt)
cp_annual_mid <- crosspred(bvar, coef=pred_annual[[2]]$fit, vcov=pred_annual[[2]]$vcov, model.link="log", cen=mmt)
cp_annual_hi <- crosspred(bvar, coef=pred_annual[[3]]$fit, vcov=pred_annual[[3]]$vcov, model.link="log", cen=mmt)
cp_annual = rbind(cbind(cp_annual_low$allRRfit,cp_annual_low$allRRhigh,cp_annual_low$allRRlow),
                  cbind(cp_annual_mid$allRRfit,cp_annual_mid$allRRhigh,cp_annual_mid$allRRlow),
                  cbind(cp_annual_hi$allRRfit,cp_annual_hi$allRRhigh,cp_annual_hi$allRRlow))
cp_annual = cp_annual %>% cbind(c(rep(quantile(estimates$annual_t2m,0.05),length(cp_annual_hi$allfit)),
                                  rep(quantile(estimates$annual_t2m,0.5),length(cp_annual_hi$allfit)),
                                  rep(quantile(estimates$annual_t2m,0.95),length(cp_annual_hi$allfit))))
cp_annual = cp_annual %>% cbind(as.numeric(rownames(cp_annual)))
cp_annual = as.data.frame(cp_annual)
colnames(cp_annual) = c("allRRfit","allRRhigh","allRRlow","type","temp")

# Scenario predictions by years
pred_period = predict(model5c2, 
                      newdata = estimates %>% group_by(year) %>% 
                        summarise(gdp = mean(gdp), 
                                  summer_t2m = mean(summer_t2m), winter_t2m = mean(winter_t2m), 
                                  hi_education_predicted = mean(hi_education_predicted), 
                                  poor_heating = mean(estimates$poor_heating, na.rm = T), 
                                  air_conditioning_prevalence = mean(estimates$air_conditioning_prevalence, na.rm = T)), vcov=T)

cp_period_temp = sapply(1:(length(years_list)-1), FUN = function(x) {
  crosspred(bvar, coef=pred_period[[x]]$fit, vcov=pred_period[[x]]$vcov, model.link="log", cen=mmt)[c("allRRfit","allRRhigh","allRRlow")]})

cp_period = data.frame()
for (i in 1:(length(years_list)-1)) {
  cp_period = cp_period %>% rbind(data.frame(allRRfit=cp_period_temp[1,i],allRRhigh=cp_period_temp[2,i],allRRlow=cp_period_temp[3,i],type=paste(years_list[i],(years_list[i+1]-1),sep="-"),temp=seq(-44,32,2)))
}

# Prediction for household-level variables:

# Air conditioning
pred_ac = predict( model5c2, newdata = data.frame(
  air_conditioning_prevalence = c(0, 0.50, 1.00, quantile(estimates$air_conditioning_prevalence, c(0.05, 0.5, 0.95), na.rm = T ) ),
  gdp = mean(estimates$gdp),
  income = mean(estimates$income),
  hi_education_predicted = mean(estimates$hi_education_predicted),
  annual_t2m = mean(estimates$annual_t2m),
  summer_t2m = mean(estimates$summer_t2m),
  winter_t2m = mean(estimates$winter_t2m),
  poor_heating = mean(estimates$poor_heating, na.rm = T),
  year = 2018),  vcov = T)


cp_ac_low_1 <- crosspred(bvar, coef=pred_ac[[1]]$fit, vcov=pred_ac[[1]]$vcov, model.link="log", cen=mmt)
cp_ac_mid_1 <- crosspred(bvar, coef=pred_ac[[2]]$fit, vcov=pred_ac[[2]]$vcov, model.link="log", cen=mmt)
cp_ac_hi_1 <- crosspred(bvar, coef=pred_ac[[3]]$fit, vcov=pred_ac[[3]]$vcov, model.link="log", cen=mmt)
cp_ac_low_2 <- crosspred(bvar, coef=pred_ac[[4]]$fit, vcov=pred_ac[[4]]$vcov, model.link="log", cen=mmt)
cp_ac_mid_2 <- crosspred(bvar, coef=pred_ac[[5]]$fit, vcov=pred_ac[[5]]$vcov, model.link="log", cen=mmt)
cp_ac_hi_2 <- crosspred(bvar, coef=pred_ac[[6]]$fit, vcov=pred_ac[[6]]$vcov, model.link="log", cen=mmt)
cp_ac = rbind(# cbind(cp_ac_low_1$allRRfit,cp_ac_low_1$allRRhigh,cp_ac_low_1$allRRlow),
  # cbind(cp_ac_mid_1$allRRfit,cp_ac_mid_1$allRRhigh,cp_ac_mid_1$allRRlow),
  # cbind(cp_ac_hi_1$allRRfit,cp_ac_hi_1$allRRhigh,cp_ac_hi_1$allRRlow),
  cbind(cp_ac_low_2$allRRfit,cp_ac_low_2$allRRhigh,cp_ac_low_2$allRRlow),
  cbind(cp_ac_mid_2$allRRfit,cp_ac_mid_2$allRRhigh,cp_ac_mid_2$allRRlow),
  cbind(cp_ac_hi_2$allRRfit,cp_ac_hi_2$allRRhigh,cp_ac_hi_2$allRRlow))
cp_ac = cp_ac %>% cbind(c(# rep(0,length(cp_ac_hi$allfit)),
  #                          rep(50,length(cp_ac_hi$allfit)),
  #                          rep(100,length(cp_ac_hi$allfit)),
  rep(quantile(estimates$air_conditioning_prevalence,0.05,na.rm=T),length(cp_ac_hi_2$allfit)),     
  rep(quantile(estimates$air_conditioning_prevalence,0.5,na.rm=T),length(cp_ac_hi_2$allfit)), 
  rep(quantile(estimates$air_conditioning_prevalence,0.95,na.rm=T),length(cp_ac_hi_2$allfit))))

#cp_ac = cp_ac %>% cbind(c(rep(0,length(cp_ac_hi$allfit)*3),
#                          rep(1,length(cp_ac_hi$allfit)*3)))

cp_ac = cp_ac %>% cbind(as.numeric(rownames(cp_ac)))
cp_ac = as.data.frame(cp_ac)
colnames(cp_ac) = c("allRRfit","allRRhigh","allRRlow","type","temp")
cp_ac %>% filter(temp > 30)

# Heating
pred_heating = predict( model5c2,  newdata = data.frame(
  poor_heating = c(0, 0.50, 1.00, quantile(estimates$poor_heating, c(0.05, 0.5, 0.95), na.rm = T ) ),
  gdp = mean(estimates$gdp),
  income = mean(estimates$income),
  hi_education_predicted = mean(estimates$hi_education_predicted),
  annual_t2m = mean(estimates$annual_t2m),
  summer_t2m = mean(estimates$summer_t2m),
  winter_t2m = mean(estimates$winter_t2m),
  air_conditioning_prevalence = mean(estimates$air_conditioning_prevalence, na.rm = T),
  year = 2018),  vcov = T)

cp_heating_low_1=crosspred(bvar,coef=pred_heating[[1]]$fit,vcov=pred_heating[[1]]$vcov,model.link="log",cen=mmt)
cp_heating_mid_1=crosspred(bvar,coef=pred_heating[[2]]$fit,vcov=pred_heating[[2]]$vcov,model.link="log",cen=mmt)
cp_heating_hi_1=crosspred(bvar,coef=pred_heating[[3]]$fit,vcov=pred_heating[[3]]$vcov,model.link="log",cen=mmt)
cp_heating_low_2=crosspred(bvar,coef=pred_heating[[4]]$fit,vcov=pred_heating[[4]]$vcov,model.link="log",cen=mmt)
cp_heating_mid_2=crosspred(bvar,coef=pred_heating[[5]]$fit,vcov=pred_heating[[5]]$vcov,model.link="log",cen=mmt)
cp_heating_hi_2=crosspred(bvar,coef=pred_heating[[6]]$fit,vcov=pred_heating[[6]]$vcov,model.link="log",cen=mmt)
cp_heating = rbind(# cbind(cp_ac_low_1$allRRfit,cp_ac_low_1$allRRhigh,cp_ac_low_1$allRRlow),
  # cbind(cp_ac_mid_1$allRRfit,cp_ac_mid_1$allRRhigh,cp_ac_mid_1$allRRlow),
  # cbind(cp_ac_hi_1$allRRfit,cp_ac_hi_1$allRRhigh,cp_ac_hi_1$allRRlow),
  cbind(cp_heating_low_2$allRRfit,cp_heating_low_2$allRRhigh,cp_heating_low_2$allRRlow),
  cbind(cp_heating_mid_2$allRRfit,cp_heating_mid_2$allRRhigh,cp_heating_mid_2$allRRlow),
  cbind(cp_heating_hi_2$allRRfit,cp_heating_hi_2$allRRhigh,cp_heating_hi_2$allRRlow))
cp_heating = cp_heating %>% cbind(c(# rep(0,length(cp_ac_hi$allfit)),
  #                          rep(50,length(cp_ac_hi$allfit)),
  #                          rep(100,length(cp_ac_hi$allfit)),
  rep(quantile(estimates$poor_heating,0.05,na.rm=T),length(cp_heating_hi_2$allfit)),     
  rep(quantile(estimates$poor_heating,0.5,na.rm=T),length(cp_heating_hi_2$allfit)), 
  rep(quantile(estimates$poor_heating,0.95,na.rm=T),length(cp_heating_hi_2$allfit))))

#cp_ac = cp_ac %>% cbind(c(rep(0,length(cp_ac_hi$allfit)*3),
#                          rep(1,length(cp_ac_hi$allfit)*3)))

cp_heating = cp_heating %>% cbind(as.numeric(rownames(cp_heating)))
cp_heating = as.data.frame(cp_heating)
colnames(cp_heating) = c("allRRfit","allRRhigh","allRRlow","type","temp")
cp_heating %>% filter(temp == -10)

# Housing
pred_housing = predict( model5c1,  newdata = data.frame(
  poor_housing = c(0, 0.50, 1.00, quantile(estimates$poor_housing, c(0.05, 0.5, 0.95), na.rm = T ) ),
  gdp = mean(estimates$gdp),
  income = mean(estimates$income),
  hi_education_predicted = mean(estimates$hi_education_predicted),
  annual_t2m = mean(estimates$annual_t2m),
  summer_t2m = mean(estimates$summer_t2m),
  winter_t2m = mean(estimates$winter_t2m),
  air_conditioning_prevalence = mean(estimates$air_conditioning_prevalence, na.rm = T),
  year = 2018),  vcov = T)

cp_housing_low_1=crosspred(bvar,coef=pred_housing[[1]]$fit,vcov=pred_housing[[1]]$vcov,model.link="log",cen=mmt)
cp_housing_mid_1=crosspred(bvar,coef=pred_housing[[2]]$fit,vcov=pred_housing[[2]]$vcov,model.link="log",cen=mmt)
cp_housing_hi_1=crosspred(bvar,coef=pred_housing[[3]]$fit,vcov=pred_housing[[3]]$vcov,model.link="log",cen=mmt)
cp_housing_low_2=crosspred(bvar,coef=pred_housing[[4]]$fit,vcov=pred_housing[[4]]$vcov,model.link="log",cen=mmt)
cp_housing_mid_2=crosspred(bvar,coef=pred_housing[[5]]$fit,vcov=pred_housing[[5]]$vcov,model.link="log",cen=mmt)
cp_housing_hi_2=crosspred(bvar,coef=pred_housing[[6]]$fit,vcov=pred_housing[[6]]$vcov,model.link="log",cen=mmt)
cp_housing = rbind(# cbind(cp_ac_low_1$allRRfit,cp_ac_low_1$allRRhigh,cp_ac_low_1$allRRlow),
  # cbind(cp_ac_mid_1$allRRfit,cp_ac_mid_1$allRRhigh,cp_ac_mid_1$allRRlow),
  # cbind(cp_ac_hi_1$allRRfit,cp_ac_hi_1$allRRhigh,cp_ac_hi_1$allRRlow),
  cbind(cp_housing_low_2$allRRfit,cp_housing_low_2$allRRhigh,cp_housing_low_2$allRRlow),
  cbind(cp_housing_mid_2$allRRfit,cp_housing_mid_2$allRRhigh,cp_housing_mid_2$allRRlow),
  cbind(cp_housing_hi_2$allRRfit,cp_housing_hi_2$allRRhigh,cp_housing_hi_2$allRRlow))
cp_housing = cp_housing %>% cbind(c(# rep(0,length(cp_ac_hi$allfit)),
  #                          rep(50,length(cp_ac_hi$allfit)),
  #                          rep(100,length(cp_ac_hi$allfit)),
  rep(quantile(estimates$poor_housing,0.05,na.rm=T),length(cp_housing_hi_2$allfit)),     
  rep(quantile(estimates$poor_housing,0.5,na.rm=T),length(cp_housing_hi_2$allfit)), 
  rep(quantile(estimates$poor_housing,0.95,na.rm=T),length(cp_housing_hi_2$allfit))))

#cp_ac = cp_ac %>% cbind(c(rep(0,length(cp_ac_hi$allfit)*3),
#                          rep(1,length(cp_ac_hi$allfit)*3)))

cp_housing = cp_housing %>% cbind(as.numeric(rownames(cp_housing)))
cp_housing = as.data.frame(cp_housing)
colnames(cp_housing) = c("allRRfit","allRRhigh","allRRlow","type","temp")

save(list = ls(pattern="cp_[a-z]+$"), file = "predictions.RData")

