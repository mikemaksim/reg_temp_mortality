
library(dplyr)
library(tidyr)
library(splines)
library(dlnm)
library(mvmeta)
library(metafor)
library(mixmeta)
library(purrr)

model_comparison = data.frame()
model_specification = "6 periods"
no_of_lags = 3
seasonality_spline_df = 7
set_of_knots_percentiles = list(c(0.5),c(0.33,0.66),c(0.25,0.5,0.75),c(0.1,0.9),c(0.1,0.5,0.9),c(0.1,0.75,0.9),c(0.05,0.35,0.65,0.95),c(0.05,0.35,0.5,0.65,0.95))
air_pollutant = "+ pm10 + surface_o3"
air_pollutants = c("","+ pm2_5","+ pm10","+ surface_o3","+ no", "+ pm10 + surface_o3", "+ pm10 + no", "+ pm2_5 + surface_o3", "+ pm2_5 + no")

for (no_of_lags in 3:4) {
  for (seasonality_spline_df in 5:9) {
    for (knots_percentiles in set_of_knots_percentiles) {
      for (air_pollutant in air_pollutants) {
          
          # Structure of dose-response function:
          
          varknots = quantile(df$t2m_cams, knots_percentiles, na.rm = T)
          boundary_knots = c(min(df$t2m_cams, na.rm = T),max(df$t2m_cams, na.rm = T))
          dose_response = list(fun = "ns", knots = varknots, Bound = boundary_knots)
          
          # Structure of lag-response function:
          lag_response = list(fun = "integer")

          # Reference mortality:
          centered = 18
          lag_response_temp = -40
          
          # Initialization of variables for metaanalysis
          regions_list = unique(df$PopCode)
          years_list = c(2004,2008,2012,2016,2020)# c(2004,2008,2012,2016,2020)
          
          coef_temp = matrix(NA,length(regions_list)*(length(years_list)-1), length(varknots)+1 ) 
          coef_lag = matrix(NA,length(regions_list)*(length(years_list)-1), no_of_lags+1) 
          
          Slist <- vector("list",length(regions_list)*(length(years_list)-1))
          Slist_lag <- vector("list",length(regions_list)*no_of_lags )
          
          i = 0
          estimates = data.frame()
          
          for (region in regions_list) {
            # Region-specific temperature distribution
            regional_boundary_knots = range(df[c(df$PopCode == region),]$t2m_cams, na.rm = T)
            varknots = quantile(df[c(df$PopCode == region),]$t2m_cams, knots_percentiles, na.rm = T)
            for (year_num in 1:(length(years_list)-1) ) {
              i = i + 1
              region_mortality = df[c(df$PopCode == region &
                                            df$Year %in% c(years_list[year_num]:(years_list[year_num + 1]-1)) &
                                            df$Week < 53 ), ] %>% arrange(Year, Week) %>%
                mutate(perc = percent_rank(t2m_cams), week = row_number())

              temp_spline = crossbasis(
                region_mortality$t2m_cams,lag = c(0,no_of_lags),
                # argvar = dose_response,
                argvar = list(fun = "ns", knots = varknots, Bound = regional_boundary_knots),
                arglag = lag_response
              )
              m = glm(
                data = region_mortality,
                as.formula(paste("SDR ~ temp_spline + 
                  ns(week, df = seasonality_spline_df * length(unique(region_mortality$Year)) )", air_pollutant, sep = "")),
                family = quasipoisson(),
                offset = log(Pop),
                na.action = na.omit
              )
              
              # If we predict lag-response functions:
              predicted_spline = crossreduce(temp_spline, m, type = "var", value = lag_response_temp, cen = centered)
              coef_lag[i,] = predicted_spline$coefficients
              Slist_lag[[i]] <- predicted_spline$vcov
              
              # If we predict dose-response functions:
              predicted_spline = crossreduce(temp_spline, m, cen = centered) # , lag = 0)
              coef_temp[i, ] <- predicted_spline$coefficients
              
              Slist[[i]] <- predicted_spline$vcov
              
              estimates = rbind(
                estimates,
                data.frame(
                  year = mean(years_list[year_num]:years_list[year_num + 1]),
                  region = region,
                  income = mean(region_mortality$income_ppp_adj),
                  gdp = mean(region_mortality$gdp),
                  hi_education_predicted = mean(region_mortality$hi_education_predicted),
                  air_conditioning_per_household = mean(region_mortality$obdkh_air_conditioning_per_household, na.rm = T),
                  air_conditioning_prevalence = mean(region_mortality$obdkh_air_conditioning_prevalence, na.rm = T),
                  poor_housing = mean(region_mortality$obdkh_poor_housing, na.rm = T),
                  poor_heating = mean(region_mortality$obdkh_poor_heating, na.rm = T),
                  # heat_sustain_predicted = mean(region_mortality$heat_sustain_predicted),
                  # air_conditioning_predicted = mean(region_mortality$air_conditioning_predicted),
                  annual_t2m = mean(region_mortality$annual_t2m),
                  summer_t2m = mean(region_mortality$summer_t2m),
                  winter_t2m = mean(region_mortality$winter_t2m)
                )
              )
            }
          }
          
          estimates$region = as.factor(estimates$region)
          
          # Sensitivity analysis
          model0 <- mixmeta(coef_temp, Slist, data = estimates, method = "ml")
          bvar = do.call("onebasis", c(list(x=seq(boundary_knots[1],boundary_knots[2],by=0.1)), dose_response))
          
          mmt <- seq(boundary_knots[1],boundary_knots[2],by=0.1)[which.min(bvar%*%coef(model0))]
          
          pred0 <- predict(model0, vcov=T)
          cp0 <- crosspred(bvar, coef=pred0[[1]]$fit, vcov=pred0[[1]]$vcov,
                           model.link="log", cen=mmt)
          cp0 = cbind(cp0$allRRfit,cp0$allRRhigh,cp0$allRRlow)
          cp0 = cp0 %>% cbind(as.numeric(rownames(cp0)))
          cp0 = as.data.frame(cp0)
          colnames(cp0) = c("allRRfit","allRRhigh","allRRlow","temp")
          cp0$aic = AIC(model0)
          cp0$bic = BIC(model0)
          
          cp0$knots_placement = paste("Knots: ",
                                      paste0(as.character(knots_percentiles*100),collapse=", "),
                                      "th perc.",sep="")
          cp0$mmt = mmt
          cp0$no_of_lags = no_of_lags
          cp0$seasonality_spline_df = seasonality_spline_df
          cp0$temp_distribution = "total"
          cp0$air_pollutant = air_pollutant
          cp0$scenario = model_specification

          model_comparison = model_comparison %>% rbind(cp0)
    }
  }
}

```