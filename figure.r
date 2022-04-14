library("dplyr")
library(stringr)
library(RCurl)
library(httr)
library("parallel")
library("dplyr")
library("MASS")
library("lme4")
library("glmmTMB")
library("gamm4")

run_date_guess = as.Date("2020-07-31")
date_of_all = format(seq(as.Date("2020-04-18"), as.Date(("2020-07-31")), by = "days"),"%m-%d-%Y")

results_list = mclapply(date_of_all, function(date_of_study){
  pm_data = "P1"
  #date_of_study = "06-08-2020"
  # Historical data
  covid_hist = read.csv(text=getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-30-2020.csv"))
  covid_us_hist = subset(covid_hist, Country_Region == "US" & is.na(FIPS)==F)
  
  # Import outcome data from JHU CSSE
  covid = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",date_of_study,".csv")))
  covid_us = subset(covid,Country_Region == "US")[,1:12]
  covid_us = rbind(covid_us,subset(covid_us_hist, (!(FIPS %in% covid_us$FIPS))  & Confirmed == 0 & Deaths == 0 & is.na(FIPS)==F))
  covid_us$FIPS = str_pad(covid_us$FIPS, 5, pad = "0")
  
  #covid_us$Deaths[covid_us$Admin2=="Jackson" & covid_us$Province_State=="Missouri"] = subset(covid_us,Admin2=="Jackson" & Province_State=="Missouri")$Deaths +
  #                                                                        subset(covid_us,Admin2=="Kansas City" & Province_State=="Missouri")$Deaths 
  #covid_us[covid_us$Admin2=="Bear River",]$FIPS = "06620"
  #covid_us[covid_us$Admin2=="Dukes and Nantucket",]$FIPS = "25007"
  #covid_us[covid_us$Admin2=="Weber-Morgan",]$FIPS = "49057"
  # Import exposure PM2.5 data
  county_pm = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_pm25.csv"))
  # if (pm_data %in% c("P3","P4")){
  #   county_pm = read.csv("/Users/apple/Dropbox/COVID19/covid19/counties/rough_county_ywei/rough_county_pm25.csv")
  # }else if (pm_data %in% c("P1","P2")){
  #   county_pm = read.csv("/Users/apple/Dropbox/COVID19/covid19/counties/rough_county_rm/rough_county_pm25.csv")
  # }
  
  county_temp = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/temp_seasonal_county.csv"))
  # Import census, brfss, testing, mortality, hosptial beds data as potential confounders
  county_census = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/census_county_interpolated.csv"))
  # county_census = read.csv("/Users/apple/Dropbox/COVID19/covid19/census_county_interpolated.csv")
  #county_brfss = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/brfss_county_interpolated.csv"))
  county_brfss<-read.csv(text=getURL("https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2020.csv"),skip = 1)
  # county_brfss<-read.csv("/Users/apple/Dropbox/COVID19/covid19/analytic_data2020.csv",skip=1)
  county_brfss<-county_brfss[,c('fipscode','v011_rawvalue','v009_rawvalue')]
  names(county_brfss)<-c('fips','obese','smoke')
  county_brfss$fips = str_pad(county_brfss$fips, 5, pad = "0")
  #census_population = read.csv("co-est2019-alldata.csv")
  #census_population$fips = paste0(str_pad(census_population$STATE, 2, pad = "0"),str_pad(census_population$COUNTY, 3, pad = "0"))
  #census_population = census_population[,c("fips","POPESTIMATE2019")]
  
  state_test = read.csv(text=getURL("https://api.covidtracking.com/v1/states/daily.csv"))
  state_test = subset(state_test, date ==paste0(substring(str_remove_all(date_of_study, "-"),5,8),substring(str_remove_all(date_of_study, "-"),1,4)))[, - 38]
  statecode = read.csv(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/statecode.csv"))
  
  hospitals = read.csv(text=getURL("https://opendata.arcgis.com/datasets/6ac5e325468c4cb9b905f1728d6fbf0f_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"))
  # hospitals = read.csv("/Users/apple/Dropbox/COVID19/covid19/Hospitals.csv")
  hospitals$BEDS[hospitals$BEDS < 0] = NA
  
  county_base_mortality = read.table(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_base_mortality.txt"), sep = "",header = T)
  county_old_mortality = read.table(text=getURL("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_old_mortality.txt"), sep = "",header = T)
  county_014_mortality = read.table("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_014_mortality.txt", sep = "",header = T)
  county_1544_mortality = read.table("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_1544_mortality.txt", sep = "",header = T)
  county_4564_mortality = read.table("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/county_4564_mortality.txt", sep = "",header = T)
  
  colnames(county_old_mortality)[4] = c("older_Population")
  colnames(county_014_mortality)[4] = c("014_Population")
  colnames(county_1544_mortality)[4] = c("1544_Population")
  colnames(county_4564_mortality)[4] = c("4564_Population")
  
  county_base_mortality = merge(county_base_mortality,county_old_mortality[,c(2,4)] ,by = "County.Code",all.x = T)
  county_base_mortality = merge(county_base_mortality,county_014_mortality[,c(2,4)] ,by = "County.Code",all.x = T)
  county_base_mortality = merge(county_base_mortality,county_1544_mortality[,c(2,4)] ,by = "County.Code",all.x = T)
  county_base_mortality = merge(county_base_mortality,county_4564_mortality[,c(2,4)] ,by = "County.Code",all.x = T)
  
  county_base_mortality$older_pecent = county_base_mortality$older_Population/county_base_mortality$Population
  county_base_mortality$"young_pecent" = county_base_mortality$"014_Population"/county_base_mortality$Population
  county_base_mortality$"prime_pecent" = county_base_mortality$"1544_Population"/county_base_mortality$Population
  county_base_mortality$"mid_pecent" = county_base_mortality$"4564_Population"/county_base_mortality$Population
  county_base_mortality$"older_pecent"[is.na(county_base_mortality$"older_pecent")] = 0
  county_base_mortality$"prime_pecent"[is.na(county_base_mortality$"prime_pecent")] = 0
  county_base_mortality$"mid_pecent"[is.na(county_base_mortality$"mid_pecent")] = 0
  county_base_mortality$"young_pecent"[is.na(county_base_mortality$"young_pecent")] = 0
  
  # Import NCHS Urban-Rural Classification Scheme for Counties
  NCHSURCodes2013 = read.csv("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/NCHSURCodes2013.csv")
  NCHSURCodes2013$FIPS = str_pad(NCHSURCodes2013$FIPS, 5, pad = "0")
  
  # Import FB survey on covid-like sympton data
  #script <- getURL("https://raw.githubusercontent.com/cmu-delphi/delphi-epidata/master/src/client/delphi_epidata.R", ssl.verifypeer = FALSE)
  #eval(parse(text = script))
  
  # merging data
  state_test = merge(state_test,statecode,by.x = "state" ,by.y = "Code" )
  state_policy = read.csv("https://raw.githubusercontent.com/wxwx1993/PM_COVID/master/Data/state_policy0410.csv")
  colnames(state_policy)[6] = "stay_at_home"
  colnames(state_policy)[7] = "relax_stay_at_home"
  colnames(state_policy)[8] = "close_business"
  colnames(state_policy)[9] = "reopen_business"
  
  state_test = merge(state_test,state_policy[,c(1,6,7,8,9)],by = "State")
  state_test$date_since_social = as.numeric(run_date_guess - as.Date((strptime(state_test$stay_at_home, "%m/%d/%y"))))
  state_test$date_since_relax = as.numeric(run_date_guess - as.Date((strptime(state_test$relax_stay_at_home, "%m/%d/%y"))))
  state_test$close_business = as.numeric(run_date_guess - as.Date((strptime(state_test$close_business, "%m/%d/%y"))))
  state_test$reopen_business = as.numeric(run_date_guess - as.Date((strptime(state_test$reopen_business, "%m/%d/%y"))))
  
  state_test[is.na(state_test$date_since_social)==T,]$date_since_social = 0
  state_test[is.na(state_test$date_since_relax)==T,]$date_since_relax = 0
  state_test[is.na(state_test$close_business)==T,]$close_business = 0
  state_test[is.na(state_test$reopen_business)==T,]$reopen_business = 0
  state_test$time_total_social = state_test$date_since_social - state_test$date_since_relax
  state_test$time_total_business = state_test$close_business - state_test$reopen_business
  
  
  if (pm_data %in% c("P1","P3")){
    # pm average over 17 years
    county_pm_aggregated = county_pm %>% 
      group_by(fips) %>% 
      summarise(mean_pm25 = mean(pm25))
  }else if (pm_data %in% c("P2","P4")){
    # pm most recent 2016
    county_pm_aggregated = subset(county_pm , year==2016)
    county_pm_aggregated$mean_pm25 = county_pm_aggregated$pm25
  }
  
  # pm average over 17 years
  county_temp_aggregated = county_temp %>% 
    group_by(fips) %>% 
    summarise(mean_winter_temp= mean(winter_tmmx),
              mean_summer_temp= mean(summer_tmmx),
              mean_winter_rm= mean(winter_rmax),
              mean_summer_rm= mean(summer_rmax))
  
  county_pm_aggregated = merge(county_pm_aggregated,county_temp_aggregated,by="fips",all.x = T)
  
  county_hospitals_aggregated = hospitals %>%
    group_by(COUNTYFIPS) %>%
    summarise(beds = sum(BEDS, na.rm=TRUE))
  county_hospitals_aggregated$COUNTYFIPS = str_pad(county_hospitals_aggregated$COUNTYFIPS, 5, pad = "0")
  
  county_census_aggregated2 = subset(county_census, year==2016)
  
  county_census_aggregated2$q_popdensity = 1
  quantile_popdensity = quantile(county_census_aggregated2$popdensity,c(0.2,0.4,0.6,0.8))
  county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity<=quantile_popdensity[1]] = 1
  county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[1] &
                                           county_census_aggregated2$popdensity<=quantile_popdensity[2]] = 2
  county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[2] &
                                           county_census_aggregated2$popdensity<=quantile_popdensity[3]] = 3
  county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[3] &
                                           county_census_aggregated2$popdensity<=quantile_popdensity[4]] = 4
  county_census_aggregated2$q_popdensity[county_census_aggregated2$popdensity>quantile_popdensity[4]] = 5
  
  county_census_aggregated2$fips = str_pad(county_census_aggregated2$fips, 5, pad = "0")
  county_census_aggregated2 = merge(county_census_aggregated2,county_brfss,
                                    by="fips",all.x=T)
  #county_census_aggregated2 = merge(county_census_aggregated2,census_population,by="fips")
  
  county_pm_aggregated$fips = str_pad(county_pm_aggregated$fips, 5, pad = "0")
  aggregate_pm = merge(county_pm_aggregated,covid_us,by.x="fips",by.y = "FIPS")
  
  aggregate_pm_census = merge(aggregate_pm,county_census_aggregated2,by.x="fips",by.y = "fips")
  
  county_base_mortality$County.Code = str_pad(county_base_mortality$County.Code, 5, pad = "0")
  aggregate_pm_census_cdc = merge(aggregate_pm_census,county_base_mortality[,c(1,4,12:15)],by.x = "fips",by.y = "County.Code",all.x = T)
  
  aggregate_pm_census_cdc = aggregate_pm_census_cdc[is.na(aggregate_pm_census_cdc$fips) ==F,]
  
  aggregate_pm_census_cdc_test = merge(aggregate_pm_census_cdc,
                                       state_test[, !(names(state_test) %in% c("fips"))],
                                       by.x="Province_State",by.y = "State")
  #aggregate_pm_census_cdc_test = aggregate_pm_census_cdc_test %>%
  #  group_by(Province_State) %>%
  #  mutate(population_frac_county = population/sum(population),
  #         totalTestResults_county = population_frac_county*totalTestResults)
  
  aggregate_pm_census_cdc_test_beds = merge(aggregate_pm_census_cdc_test,county_hospitals_aggregated,by.x = "fips",by.y = "COUNTYFIPS",all.x = T)
  aggregate_pm_census_cdc_test_beds$beds[is.na(aggregate_pm_census_cdc_test_beds$beds)] = 0
  
  date_of_all = format(seq(as.Date("2020-03-22"), as.Date(strptime(date_of_study,"%m-%d-%Y")), by = "days"),"%m-%d-%Y")
  
  # Historical data
  # Import outcome data from JHU CSSE
  covid_us_daily_confirmed = lapply(date_of_all,
                                    function(date_of_all){
                                      covid_daily = read.csv(text=getURL(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",date_of_all,".csv")))
                                      covid_daily = covid_daily[!duplicated(covid_daily$FIPS),1:12]
                                      return(subset(covid_daily,Country_Region == "US" & is.na(FIPS)!=T & Confirmed >0 ))
                                    }
  )
  
  covid_us_new_confirmed = list()
  covid_us_new_confirmed[1] = covid_us_daily_confirmed[1]
  covid_us_new_confirmed[[1]]$date_since = length(covid_us_daily_confirmed) 
  
  covid_us_new_confirmed[2:length(date_of_all)] =  lapply(2:(length(covid_us_daily_confirmed)),
                                                          function(i){
                                                            #covid_us_new_confirmed =subset(covid_us_daily_confirmed[[i]],!(FIPS %in% covid_us_daily_confirmed[[i-1]]$FIPS))
                                                            covid_us_new_confirmed =subset(covid_us_daily_confirmed[[i]],!(FIPS %in% unlist(sapply(1:(i-1),function(k)covid_us_daily_confirmed[[k]]$FIPS))))
                                                            if (nrow(covid_us_new_confirmed)>0){
                                                              covid_us_new_confirmed$date_since = length(covid_us_daily_confirmed) - i + 1
                                                              return(covid_us_new_confirmed)
                                                            } else{return(NA)}
                                                          })
  
  covid_us_new_confirmed.df <- do.call("rbind", covid_us_new_confirmed)[,c("FIPS","date_since")]
  covid_us_new_confirmed.df$FIPS = str_pad(covid_us_new_confirmed.df$FIPS, 5, pad = "0")
  aggregate_pm_census_cdc_test_beds = merge(aggregate_pm_census_cdc_test_beds,covid_us_new_confirmed.df,
                                            by.x = "fips",by.y = "FIPS", all.x = T)
  aggregate_pm_census_cdc_test_beds$date_since[is.na(aggregate_pm_census_cdc_test_beds$date_since)] = 0
  
  aggregate_pm_census_cdc_test_beds$q_date_since = 1
  quantile_date_since = quantile(aggregate_pm_census_cdc_test_beds$date_since,c(0.2,0.4,0.6,0.8))
  aggregate_pm_census_cdc_test_beds$q_date_since[aggregate_pm_census_cdc_test_beds$date_since<=quantile_date_since[1]] = 1
  aggregate_pm_census_cdc_test_beds$q_date_since[aggregate_pm_census_cdc_test_beds$date_since>quantile_date_since[1] &
                                                   aggregate_pm_census_cdc_test_beds$date_since<=quantile_date_since[2]] = 2
  aggregate_pm_census_cdc_test_beds$q_date_since[aggregate_pm_census_cdc_test_beds$date_since>quantile_date_since[2] &
                                                   aggregate_pm_census_cdc_test_beds$date_since<=quantile_date_since[3]] = 3
  aggregate_pm_census_cdc_test_beds$q_date_since[aggregate_pm_census_cdc_test_beds$date_since>quantile_date_since[3] &
                                                   aggregate_pm_census_cdc_test_beds$date_since<=quantile_date_since[4]] = 4
  aggregate_pm_census_cdc_test_beds$q_date_since[aggregate_pm_census_cdc_test_beds$date_since>quantile_date_since[4]] = 5
  
  aggregate_pm_census_cdc_test_beds = merge(aggregate_pm_census_cdc_test_beds,NCHSURCodes2013[,c(1,7)],
                                            by.x = "fips",by.y="FIPS", all.x = T)
  
  aggregate_pm_census_cdc_test_beds[aggregate_pm_census_cdc_test_beds$Admin2=="New York City",]$population =
    subset(aggregate_pm_census_cdc_test_beds,Admin2=="New York City"& Province_State=="New York")$population +
    subset(aggregate_pm_census_cdc_test_beds, Admin2=="Bronx"& Province_State=="New York")$population +
    subset(aggregate_pm_census_cdc_test_beds, Admin2=="Kings"& Province_State=="New York")$population +
    subset(aggregate_pm_census_cdc_test_beds, Admin2=="Queens"& Province_State=="New York")$population +
    subset(aggregate_pm_census_cdc_test_beds, Admin2=="Richmond"& Province_State=="New York")$population
  aggregate_pm_census_cdc_test_beds[aggregate_pm_census_cdc_test_beds$Admin2=="New York City",]$beds =
    subset(aggregate_pm_census_cdc_test_beds,Admin2=="New York City"& Province_State=="New York")$beds +
    subset(aggregate_pm_census_cdc_test_beds, Admin2=="Bronx"& Province_State=="New York")$beds +
    subset(aggregate_pm_census_cdc_test_beds, Admin2=="Kings"& Province_State=="New York")$beds +
    subset(aggregate_pm_census_cdc_test_beds, Admin2=="Queens"& Province_State=="New York")$beds +
    subset(aggregate_pm_census_cdc_test_beds, Admin2=="Richmond"& Province_State=="New York")$beds
  
  vars = c("mean_pm25","poverty","medianhousevalue","medhouseholdincome","pct_owner_occ",
           "education","pct_blk","hispanic","older_pecent","prime_pecent","mid_pecent","obese","smoke",
           "mean_summer_temp","mean_summer_rm","mean_winter_temp","mean_winter_rm")
  aggregate_pm_census_cdc_test_beds[aggregate_pm_census_cdc_test_beds$Admin2=="New York City",][,vars] = 
    sapply(vars,function(var){
      (subset(aggregate_pm_census_cdc_test_beds,Admin2=="New York City"& Province_State=="New York")[,var]*subset(aggregate_pm_census_cdc_test_beds,Admin2=="New York City"& Province_State=="New York")$population +
         subset(aggregate_pm_census_cdc_test_beds, Admin2=="Bronx"& Province_State=="New York")[,var]*subset(aggregate_pm_census_cdc_test_beds,Admin2=="Bronx"& Province_State=="New York")$population +
         subset(aggregate_pm_census_cdc_test_beds, Admin2=="Kings"& Province_State=="New York")[,var]*subset(aggregate_pm_census_cdc_test_beds,Admin2=="Kings"& Province_State=="New York")$population +
         subset(aggregate_pm_census_cdc_test_beds, Admin2=="Queens"& Province_State=="New York")[,var]*subset(aggregate_pm_census_cdc_test_beds,Admin2=="Queens"& Province_State=="New York")$population +
         subset(aggregate_pm_census_cdc_test_beds, Admin2=="Richmond"& Province_State=="New York")[,var]*subset(aggregate_pm_census_cdc_test_beds,Admin2=="Richmond"& Province_State=="New York")$population)/(
           subset(aggregate_pm_census_cdc_test_beds,Admin2=="New York City"& Province_State=="New York")$population+subset(aggregate_pm_census_cdc_test_beds,Admin2=="Bronx"& Province_State=="New York")$population+
             subset(aggregate_pm_census_cdc_test_beds, Admin2=="Kings"& Province_State=="New York")$population+ subset(aggregate_pm_census_cdc_test_beds,Admin2=="Queens"& Province_State=="New York")$population +
             subset(aggregate_pm_census_cdc_test_beds,Admin2=="Richmond"& Province_State=="New York")$population)
    })
  aggregate_pm_census_cdc_test_beds = subset(aggregate_pm_census_cdc_test_beds,
                                             !(Admin2=="Bronx"& Province_State=="New York")&
                                               !(Admin2=="Kings"& Province_State=="New York")&
                                               !(Admin2=="Queens"& Province_State=="New York")&
                                               !(Admin2=="Richmond"& Province_State=="New York"))
  
# Main
glmmTMB.off.main = glmmTMB(Deaths ~ mean_pm25 + factor(q_popdensity)
                           + scale(poverty)  + scale(log(medianhousevalue))
                           + scale(log(medhouseholdincome)) + scale(pct_owner_occ) 
                           + scale(education) + scale(pct_blk) + scale(hispanic)
                           + scale(older_pecent) + scale(prime_pecent) + scale(mid_pecent) 
                           + scale(date_since_social) + scale(date_since)
                           + scale(beds/population) 
                           + scale(obese) + scale(smoke)
                           + scale(mean_summer_temp) + scale(mean_winter_temp) + scale(mean_summer_rm) + scale(mean_winter_rm)
                           + offset(log(population)) + (1 | state), data = aggregate_pm_census_cdc_test_beds, 
                           family = nbinom2, ziformula  = ~ 1
)

return(c(exp(summary(glmmTMB.off.main)[6]$coefficients$cond[2,1]),
(exp(summary(glmmTMB.off.main)[6]$coefficients$cond[2,1] - 1.96*summary(glmmTMB.off.main)[6]$coefficients$cond[2,2])),
(exp(summary(glmmTMB.off.main)[6]$coefficients$cond[2,1] + 1.96*summary(glmmTMB.off.main)[6]$coefficients$cond[2,2]))))

#return(c(exp(gamm.off.main$gam$coefficients[2]),
#         exp(gamm.off.main$gam$coefficients[2]-1.96*summary(gamm.off.main$gam)$se[2]),
#         exp(gamm.off.main$gam$coefficients[2]+1.96*summary(gamm.off.main$gam)$se[2])))
}
,mc.cores = 14)


results = do.call("cbind",results_list)
date_of_all = date_of_all
plot(1:length(date_of_all),results[1,1:length(date_of_all)],type="l",ylim=c(0.95,1.2), xaxt="n",xlab="Date",ylab = "Mortality Risk Ratios" ,main = "Daily Mortality Risk Ratios")
axis(1, at = 1:length(date_of_all), lab = sapply(date_of_all,function(date){substring(date,1,5)}))
lines(1:length(date_of_all),results[2,1:length(date_of_all)])
lines(1:length(date_of_all),results[3,1:length(date_of_all)])
abline(h=1, lty = 3)
