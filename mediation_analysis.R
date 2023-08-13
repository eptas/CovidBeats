###############################################################################
# Author: Patrick Blättermann, Fabian Greb                                    #
# Date: 12 Aug 2023                                                           #
#                                                                             #
#                                                                             #
#                                                                             #
# Description: Result summary for Stringency -> SPS -> Danceability           #
#              Mediation                                                      #
#                                                                             #
###############################################################################


###############################################################################
# Install/Load Libraries
###############################################################################

#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("lme4")
#install.packages("lmerTest")
#install.packages("mediation")
#install.packages("boot")

library(tidyverse)
library(dplyr)
library(lme4)
library(lmerTest)
library(mediation)
library(boot)

###############################################################################
# Data Import
###############################################################################

##### Set working directory. Data files (spotify_data_daily.rds,covid_daily.rds,daily_temp_data.rds,
##### nature_selection_daily_agg.rds) must be present in this directory
setwd("C:/data")


###### Spotify data ######
spotify_data_daily = readRDS("spotify_data_daily.rds") %>%
  mutate(Date_char  = Day,
         Date       = Date_char %>% as.Date()) %>%
  dplyr::select(-Day) %>%
  dplyr::select(Country, Date, Date_char, everything())



###### Covid data  ######
covid_data_daily = readRDS("covid_daily.rds") %>%
  mutate(Date_char = Date %>% as.character()) %>%
  dplyr::select(-Date)



###### weather data #######
daily_temp_data = readRDS("daily_temp_data.rds") %>%
  mutate(Date_char = Date %>% as.character()) %>%
  dplyr::select(-Date)



##### Nature stress data ######
nature_data_daily = readRDS("nature_selection_daily_agg.rds") %>%
  mutate(Date_char = date %>%  as.character()) %>%
  dplyr::select(-date)


#### matching table for country legends
country_legends = data.frame(Country = c("de",
                                         "pt",
                                         "fr",
                                         "es",
                                         "it",
                                         "be",
                                         "nl",
                                         "at",
                                         "ch",
                                         "se",
                                         "dk",
                                         "no",
                                         "fi",
                                         "gb",
                                         "br"
                                         
                                         ###...###
),
Country_legend = c("Germany",
                   "Portugal",
                   "France",
                   "Spain",
                   "Italy",
                   "Belgium",
                   "Netherlands",
                   "Austria",
                   "Swiss",
                   "Sweden",
                   "Denmark",
                   "Norway",
                   "Finnland",
                   "Great Britain",
                   "Brasil"
)
)

#### Join data together
data_all = spotify_data_daily %>%
  group_by(Country) %>%
  mutate(Stream_first = first(Streams_total)) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(Stream_index = Streams_total/Stream_first*100) %>%
  ungroup() %>%
  # mutate(Year_week = paste0(Week_end %>%  year(), "-", Week_end %>% week()),
  #        Week_no   = Week_end %>% week()) %>%
  left_join(country_legends, by = "Country") %>%
  left_join(covid_data_daily, by = c("Country", "Date_char" )) %>%
  left_join(daily_temp_data , by = c("Country", "Date_char" )) %>%
  left_join(nature_data_daily, by = c("Country", "Date_char" )) %>%
  ungroup() %>%
  rowwise() %>%
  mutate(Confirmed_pop_percent  = (Confirmed / Population) * 100,
         Weekday                = Date %>% weekdays(),
         Date_Weekday           = paste0(Date,"-", Weekday))


##### Calculate 7-days incidence
data_all$Incidence = 0
for(i in 8:nrow(data_all)){
  if(data_all$Country[i]==data_all$Country[i-7]){
    if(!is.na(data_all$Confirmed[i])&&!is.na(data_all$Confirmed[i-7])&&!is.na(data_all$Population[i])){
      data_all$Incidence[i]<-(data_all$Confirmed[i]-data_all$Confirmed[i-7])/data_all$Population[i]*100000
    }
  }
}

##### Calculate Case Fatality Ratio
data_all$CFR = 0
for(i in 1:nrow(data_all)){
  if(!is.na(data_all$Confirmed[i])&&!is.na(data_all$Deaths[i])&&data_all$Confirmed[i]!=0){
    data_all$CFR[i]<-data_all$Deaths[i]/data_all$Confirmed[i]
  }
}


###############################################################################
# Nature stress data filter/inspect by questionaires
###############################################################################


#### Select Country, Count_nature_data and Date columns to view distribution of number of answered questionaires per day
nature_select<-dplyr::select(nature_data_daily,(c('Country','Count_nature_data',"Date_char")))

#Add filter to selection: Number of questionaires must be >= 50 at least on one day and "Country" must be part of Europe
nature_select_filter<-filter(nature_select, Count_nature_data >= 50 & Country %in% c("de","pt","es","fr","it","be","nl","at","ch","se","dk","no","fi","gb"))

#Spread filtered data to show distribution of answered questionaires per day, which resuts in 11 european countries for further analysis
nature_select_filter_spread<-nature_select_filter%>%spread('Country','Count_nature_data')


###############################################################################
# Filter data for analysis (preprocessing)
###############################################################################

#### Reduce data to european countries and time frame of interest for pre/post analysis
data_all_europe <- data_all %>% 
  dplyr::filter(Country %in% c("de","pt","fr","es","it","be","nl","ch","dk","fi","gb")) %>%
  dplyr::filter((Date >= "2019-03-30" && Date <= "2019-05-30") || (Date >= "2020-03-30" && Date <= "2020-05-30"))


#### Standardize data of subset (european countries)
data_all_europe_st <- data_all_europe

data_all_europe_st$Danceability_mean<-scale(data_all_europe_st$Danceability_mean, center=TRUE, scale=TRUE)
data_all_europe_st$Tempo_mean<-scale(data_all_europe_st$Tempo_mean, center=TRUE, scale=TRUE)
data_all_europe_st$Energy_mean<-scale(data_all_europe_st$Energy_mean, center=TRUE, scale=TRUE)
data_all_europe_st$Valence_mean<-scale(data_all_europe_st$Valence_mean, center=TRUE, scale=TRUE)
data_all_europe_st$Temp_mean<-scale(data_all_europe_st$Temp_mean, center=TRUE, scale=TRUE)
data_all_europe_st$Stream_index<-scale(data_all_europe_st$Stream_index, center=TRUE, scale=TRUE)
data_all_europe_st$CFR<-scale(data_all_europe_st$CFR, center=TRUE, scale=TRUE)
data_all_europe_st$Incidence<-scale(data_all_europe_st$Incidence, center=TRUE, scale=TRUE)
data_all_europe_st$Stringency_index<-scale(data_all_europe_st$Stringency_index, center=TRUE, scale=TRUE)
data_all_europe_st$Stay_home_restrictions<-scale(data_all_europe_st$Stay_home_restrictions, center=TRUE, scale=TRUE)
data_all_europe_st$Cancel_events<-scale(data_all_europe_st$Cancel_events, center=TRUE, scale=TRUE)
data_all_europe_st$Gatherings_restrictions<-scale(data_all_europe_st$Gatherings_restrictions, center=TRUE, scale=TRUE)
data_all_europe_st$Workplace_closing<-scale(data_all_europe_st$Workplace_closing, center=TRUE, scale=TRUE)


#### Reduce data to CovidIStress time frame
data_all_stress <- dplyr::filter(data_all, Date>="2020-03-26" & Date <="2020-05-30")


#### Calculate PSS10 sum score
data_all_stress$Scale_PSS10_UCLA_SUM_mean<-(data_all_stress$Scale_PSS10_UCLA_1_mean+
                                              data_all_stress$Scale_PSS10_UCLA_2_mean+data_all_stress$Scale_PSS10_UCLA_3_mean+
                                              (6-data_all_stress$Scale_PSS10_UCLA_4_mean)+(6-data_all_stress$Scale_PSS10_UCLA_5_mean)+
                                              data_all_stress$Scale_PSS10_UCLA_6_mean+(6-data_all_stress$Scale_PSS10_UCLA_7_mean)+
                                              (6-data_all_stress$Scale_PSS10_UCLA_8_mean)+data_all_stress$Scale_PSS10_UCLA_9_mean+
                                              data_all_stress$Scale_PSS10_UCLA_10_mean)


#### Calculate SLON sum score
data_all_stress$Scale_SLON_SUM_mean<-(data_all_stress$Scale_SLON_1_mean+data_all_stress$Scale_SLON_2_mean+
                                        data_all_stress$Scale_SLON_3_mean)



#### Calculate Corona_concerns (Anxiety) sum score
data_all_stress$Corona_concerns_SUM_mean<-(data_all_stress$Corona_concerns_1_mean+data_all_stress$Corona_concerns_2_mean+
                                             data_all_stress$Corona_concerns_3_mean+data_all_stress$Corona_concerns_4_mean+
                                             data_all_stress$Corona_concerns_5_mean)



#### Calculate Compliance (Anxiety) sum score
data_all_stress$Compliance_SUM_mean<-(data_all_stress$Compliance_1_mean+data_all_stress$Compliance_2_mean+
                                        data_all_stress$Compliance_3_mean+data_all_stress$Compliance_4_mean+
                                        data_all_stress$Compliance_5_mean+data_all_stress$Compliance_6_mean)



#### Calculate SPS sum score
data_all_stress$SPS_SUM_mean<-(data_all_stress$SPS_1_mean+
                                 data_all_stress$SPS_2_mean+data_all_stress$SPS_3_mean+
                                 data_all_stress$SPS_4_mean+data_all_stress$SPS_5_mean+
                                 data_all_stress$SPS_6_mean+data_all_stress$SPS_7_mean+
                                 data_all_stress$SPS_8_mean+data_all_stress$SPS_9_mean+
                                 data_all_stress$SPS_10_mean)


#### Calculate SPS Social Integration score
data_all_stress$SPS_SI_mean<-(data_all_stress$SPS_1_mean+data_all_stress$SPS_2_mean)


#### Calculate SPS Reassurance of worth score
data_all_stress$SPS_ROW_mean<-(data_all_stress$SPS_3_mean+data_all_stress$SPS_6_mean)


#### Calculate SPS Attachment score
data_all_stress$SPS_A_mean<-(data_all_stress$SPS_4_mean+data_all_stress$SPS_5_mean)


#### Calculate SPS Sense of reliable alliance score
data_all_stress$SPS_SORA_mean<-(data_all_stress$SPS_7_mean+data_all_stress$SPS_10_mean)


#### Calculate SPS Guidance score
data_all_stress$SPS_G_mean<-(data_all_stress$SPS_8_mean+data_all_stress$SPS_9_mean)


#### Filter data (CovidIStress data time frame) for european countries
data_all_stress_europe <- dplyr::filter(data_all_stress, Country %in% c("de","pt","fr","es","it","be","nl","ch","dk","fi","gb"))

#### Replace NaN with NA
data_all_stress_europe <- data_all_stress_europe %>% dplyr::mutate_all(~ifelse(is.nan(.), NA, .))

#### Standardize subset (CovidIStress time frame of european countries) ####
data_all_stress_europe_st <- data_all_stress_europe

data_all_stress_europe_st$Danceability_mean<-scale(data_all_stress_europe_st$Danceability_mean, center=TRUE, scale=TRUE)
data_all_stress_europe_st$Tempo_mean<-scale(data_all_stress_europe_st$Tempo_mean, center=TRUE, scale=TRUE)
data_all_stress_europe_st$Energy_mean<-scale(data_all_stress_europe_st$Energy_mean, center=TRUE, scale=TRUE)
data_all_stress_europe_st$Valence_mean<-scale(data_all_stress_europe_st$Valence_mean, center=TRUE, scale=TRUE)
data_all_stress_europe_st$Temp_mean<-scale(data_all_stress_europe_st$Temp_mean, center=TRUE, scale=TRUE)
data_all_stress_europe_st$CFR<-scale(data_all_stress_europe_st$CFR, center=TRUE, scale=TRUE)
data_all_stress_europe_st$Incidence<-scale(data_all_stress_europe_st$Incidence, center=TRUE, scale=TRUE)
data_all_stress_europe_st$Stringency_index<-scale(data_all_stress_europe_st$Stringency_index, center=TRUE, scale=TRUE)
data_all_stress_europe_st$Stay_home_restrictions<-scale(data_all_stress_europe_st$Stay_home_restrictions, center=TRUE, scale=TRUE)
data_all_stress_europe_st$Cancel_events<-scale(data_all_stress_europe_st$Cancel_events, center=TRUE, scale=TRUE)
data_all_stress_europe_st$Gatherings_restrictions<-scale(data_all_stress_europe_st$Gatherings_restrictions, center=TRUE, scale=TRUE)
data_all_stress_europe_st$Workplace_closing<-scale(data_all_stress_europe_st$Workplace_closing, center=TRUE, scale=TRUE)
data_all_stress_europe_st$Scale_PSS10_UCLA_SUM_mean<-scale(data_all_stress_europe_st$Scale_PSS10_UCLA_SUM_mean, center=TRUE, scale=TRUE)
data_all_stress_europe_st$Scale_SLON_SUM_mean<-scale(data_all_stress_europe_st$Scale_SLON_SUM_mean, center=TRUE, scale=TRUE)
data_all_stress_europe_st$Corona_concerns_SUM_mean<-scale(data_all_stress_europe_st$Corona_concerns_SUM_mean, center=TRUE, scale=TRUE)
data_all_stress_europe_st$Compliance_SUM_mean<-scale(data_all_stress_europe_st$Compliance_SUM_mean, center=TRUE, scale=TRUE)
data_all_stress_europe_st$SPS_SUM_mean<-scale(data_all_stress_europe_st$SPS_SUM_mean, center=TRUE, scale=TRUE)
data_all_stress_europe_st$SPS_SI_mean<-scale(data_all_stress_europe_st$SPS_SI_mean, center=TRUE, scale=TRUE)
data_all_stress_europe_st$SPS_ROW_mean<-scale(data_all_stress_europe_st$SPS_ROW_mean, center=TRUE, scale=TRUE)
data_all_stress_europe_st$SPS_A_mean<-scale(data_all_stress_europe_st$SPS_A_mean, center=TRUE, scale=TRUE)
data_all_stress_europe_st$SPS_SORA_mean<-scale(data_all_stress_europe_st$SPS_SORA_mean, center=TRUE, scale=TRUE)
data_all_stress_europe_st$SPS_G_mean<-scale(data_all_stress_europe_st$SPS_G_mean, center=TRUE, scale=TRUE)

###############################################################################
# Calculate Means / SEM
###############################################################################

means <- data_all_stress_europe %>%
            group_by(Country) %>%
            summarise(
              Mean_danceability = mean(Danceability_mean, na.rm=TRUE),
              Mean_stringency_index = mean(Stringency_index, na.rm=TRUE),
              Mean_sps_sum = mean(SPS_SUM_mean, na.rm=TRUE),
              Mean_sps_si = mean(SPS_SI_mean, na.rm=TRUE),
              Mean_sps_row = mean(SPS_ROW_mean, na.rm=TRUE),
              Mean_sps_a = mean(SPS_A_mean, na.rm=TRUE),
              Mean_sps_sora = mean(SPS_SORA_mean, na.rm=TRUE),
              Mean_sps_g = mean(SPS_G_mean, na.rm=TRUE),
              SEM_danceability = sd(Danceability_mean, na.rm=TRUE)/sqrt(sum(!is.na(Danceability_mean))),
              SEM_stringency_index = sd(Stringency_index, na.rm=TRUE)/sqrt(sum(!is.na(Stringency_index))),
              SEM_sps_sum = sd(SPS_SUM_mean, na.rm=TRUE)/sqrt(sum(!is.na(SPS_SUM_mean))),
              SEM_sps_si = sd(SPS_SI_mean, na.rm=TRUE)/sqrt(sum(!is.na(SPS_SI_mean))),
              SEM_sps_row = sd(SPS_ROW_mean, na.rm=TRUE)/sqrt(sum(!is.na(SPS_ROW_mean))),
              SEM_sps_a = sd(SPS_A_mean, na.rm=TRUE)/sqrt(sum(!is.na(SPS_A_mean))),
              SEM_sps_sora = sd(SPS_SORA_mean, na.rm=TRUE)/sqrt(sum(!is.na(SPS_SORA_mean))),
              SEM_sps_g = sd(SPS_G_mean, na.rm=TRUE)/sqrt(sum(!is.na(SPS_G_mean)))
  )

means_st <- data_all_stress_europe_st %>%
            group_by(Country) %>%
            summarise(
              Mean_danceability = mean(Danceability_mean, na.rm=TRUE),
              Mean_stringency_index = mean(Stringency_index, na.rm=TRUE),
              Mean_sps_sum = mean(SPS_SUM_mean, na.rm=TRUE),
              Mean_sps_si = mean(SPS_SI_mean, na.rm=TRUE),
              Mean_sps_row = mean(SPS_ROW_mean, na.rm=TRUE),
              Mean_sps_a = mean(SPS_A_mean, na.rm=TRUE),
              Mean_sps_sora = mean(SPS_SORA_mean, na.rm=TRUE),
              Mean_sps_g = mean(SPS_G_mean, na.rm=TRUE),
              SEM_danceability = sd(Danceability_mean, na.rm=TRUE)/sqrt(sum(!is.na(Danceability_mean))),
              SEM_stringency_index = sd(Stringency_index, na.rm=TRUE)/sqrt(sum(!is.na(Stringency_index))),
              SEM_sps_sum = sd(SPS_SUM_mean, na.rm=TRUE)/sqrt(sum(!is.na(SPS_SUM_mean))),
              SEM_sps_si = sd(SPS_SI_mean, na.rm=TRUE)/sqrt(sum(!is.na(SPS_SI_mean))),
              SEM_sps_row = sd(SPS_ROW_mean, na.rm=TRUE)/sqrt(sum(!is.na(SPS_ROW_mean))),
              SEM_sps_a = sd(SPS_A_mean, na.rm=TRUE)/sqrt(sum(!is.na(SPS_A_mean))),
              SEM_sps_sora = sd(SPS_SORA_mean, na.rm=TRUE)/sqrt(sum(!is.na(SPS_SORA_mean))),
              SEM_sps_g = sd(SPS_G_mean, na.rm=TRUE)/sqrt(sum(!is.na(SPS_G_mean)))
            )


###############################################################################
# Pre-post analysis
###############################################################################

#### Create "Period" covariate for comparsion
data_all_europe_st$Period <- factor(ifelse(data_all_europe_st$Date <= "2020-03-01" , "pre", "post"))

#### Stream_index pre-post model
model_period_stridx_lmtest = lmerTest::lmer(Stream_index ~ relevel(Period, ref="pre")+Temp_mean+(1|Country), data = data_all_europe_st)
model_period_stridx = lme4::lmer(Stream_index ~ relevel(Period, ref="pre")+Temp_mean+(1|Country), data = data_all_europe_st)

model_period_stridx_CI <- confint(model_period_stridx, method="boot", nsim=1000)

summary(model_period_stridx_lmtest)
performance::r2(model_period_stridx_lmtest)
performance::icc(model_period_stridx_lmtest)

##Check regression Assumptions (visual)
plot(model_period_stridx_lmtest)

qqnorm(resid(model_period_stridx_lmtest))
hist(resid(model_period_stridx_lmtest))

#### Danceability pre-post model
model_period_dance_lmtest = lmerTest::lmer(Danceability_mean ~ relevel(Period, ref="pre")+Temp_mean+(1|Country), data = data_all_europe_st)
model_period_dance = lme4::lmer(Danceability_mean ~ relevel(Period, ref="pre")+Temp_mean+(1|Country), data = data_all_europe_st)

model_period_dance_CI = confint(model_period_dance, method="boot", nsim=1000)

summary(model_period_dance_lmtest)
performance::r2(model_period_dance_lmtest)
performance::icc(model_period_dance_lmtest)

##Check regression Assumptions (visual)
plot(model_period_dance_lmtest)

qqnorm(resid(model_period_dance_lmtest))
hist(resid(model_period_dance_lmtest))


#### Boxplots for all countries (stream_index / danceability)
data_all_europe_st %>% 
  ggplot(aes(x = Period, y = Stream_index, fill=Period)) +
  geom_boxplot() +
  ylim(-2,15) +
  stat_summary(fun.y="mean", shape=20, size=1, color="black",fill="black") +
  facet_wrap(~Country, scale="free")

data_all_europe_st %>% 
  ggplot(aes(x = Period, y = Danceability_mean, fill=Period)) +
  geom_boxplot() +
  ylim(-3,2) +
  stat_summary(fun.y="mean", shape=20, size=1, color="black",fill="black") +
  facet_wrap(~Country, scale="free")


###############################################################################
# Mediation
###############################################################################


################################################ Stringency -> SPS SUM Score -> Dance
m1_string_sps_dance <- lme4::lmer(Danceability_mean ~ Stringency_index + SPS_SUM_mean + (1|Country) + Temp_mean, data = data_all_stress_europe_st)
m2_string_sps_dance <- lme4::lmer(SPS_SUM_mean ~ Stringency_index + (1|Country), data = data_all_stress_europe_st)
m3_string_sps_dance <- lme4::lmer(Danceability_mean ~ Stringency_index + (1|Country) + Temp_mean, data = data_all_stress_europe_st)
m4_string_sps_dance <- lme4::lmer(Danceability_mean ~ SPS_SUM_mean + (1|Country) + Temp_mean, data = data_all_stress_europe_st)

set.seed((21))
med_string_sps_dance <- mediation::mediate(m2_string_sps_dance, m1_string_sps_dance, treat="Stringency_index", mediator="SPS_SUM_mean")

summary(med_string_sps_dance)

##Check Models
m1_string_sps_dance_lmtest <-lmerTest::lmer(Danceability_mean ~ Stringency_index + SPS_SUM_mean + (1|Country) + Temp_mean, data = data_all_stress_europe_st)
m2_string_sps_dance_lmtest <-lmerTest::lmer(SPS_SUM_mean ~ Stringency_index + (1|Country), data = data_all_stress_europe_st)
m3_string_sps_dance_lmtest <-lmerTest::lmer(Danceability_mean ~ Stringency_index + (1|Country) + Temp_mean, data = data_all_stress_europe_st)
m4_string_sps_dance_lmtest <-lmerTest::lmer(Danceability_mean ~ SPS_SUM_mean + (1|Country) + Temp_mean, data = data_all_stress_europe_st)

summary(m1_string_sps_dance_lmtest)
performance::r2(m1_string_sps_dance_lmtest)
performance::icc(m1_string_sps_dance_lmtest)

summary(m2_string_sps_dance_lmtest)
performance::r2(m2_string_sps_dance_lmtest)
performance::icc(m2_string_sps_dance_lmtest)

summary(m3_string_sps_dance_lmtest)
performance::r2(m3_string_sps_dance_lmtest)
performance::icc(m3_string_sps_dance_lmtest)

summary(m4_string_sps_dance_lmtest)
performance::r2(m4_string_sps_dance_lmtest)
performance::icc(m4_string_sps_dance_lmtest)

##Check regression assumptions (visual)
plot(m1_string_sps_dance)
plot(m2_string_sps_dance)
plot(m3_string_sps_dance)
plot(m4_string_sps_dance)

qqnorm(resid(m1_string_sps_dance))
qqnorm(resid(m2_string_sps_dance))
qqnorm(resid(m3_string_sps_dance))
qqnorm(resid(m4_string_sps_dance))

hist(resid(m1_string_sps_dance))
hist(resid(m2_string_sps_dance))
hist(resid(m3_string_sps_dance))
hist(resid(m4_string_sps_dance))


################################################ Stringency -> SPS Social Integration -> Dance
m1_string_si_dance <- lme4::lmer(Danceability_mean ~ Stringency_index + SPS_SI_mean + (1|Country) + Temp_mean, data = data_all_stress_europe_st)
m2_string_si_dance <- lme4::lmer(SPS_SI_mean ~ Stringency_index + (1|Country), data = data_all_stress_europe_st)

set.seed((21))
med_string_si_dance <- mediation::mediate(m2_string_si_dance, m1_string_si_dance, treat="Stringency_index", mediator="SPS_SI_mean")

summary(med_string_si_dance)

##Check Models
m1_string_si_dance_lmtest <-lmerTest::lmer(Danceability_mean ~ Stringency_index + SPS_SI_mean + (1|Country) + Temp_mean, data = data_all_stress_europe_st)
m2_string_si_dance_lmtest <-lmerTest::lmer(SPS_SI_mean ~ Stringency_index + (1|Country), data = data_all_stress_europe_st)
m3_string_si_dance_lmtest <-lmerTest::lmer(Danceability_mean ~ Stringency_index + (1|Country) + Temp_mean, data = data_all_stress_europe_st)
m4_string_si_dance_lmtest <-lmerTest::lmer(Danceability_mean ~ SPS_SI_mean + (1|Country) + Temp_mean, data = data_all_stress_europe_st)

summary(m1_string_si_dance_lmtest)
performance::r2(m1_string_si_dance_lmtest)
performance::icc(m1_string_si_dance_lmtest)

summary(m2_string_si_dance_lmtest)
performance::r2(m2_string_si_dance_lmtest)
performance::icc(m2_string_si_dance_lmtest)

summary(m3_string_si_dance_lmtest)
performance::r2(m3_string_si_dance_lmtest)
performance::icc(m3_string_si_dance_lmtest)

summary(m4_string_si_dance_lmtest)
performance::r2(m4_string_si_dance_lmtest)
performance::icc(m4_string_si_dance_lmtest)

##Check regression assumptions (visual)
plot(m1_string_si_dance)
plot(m2_string_si_dance)

qqnorm(resid(m1_string_si_dance))
qqnorm(resid(m2_string_si_dance))
qqnorm(resid(m3_string_si_dance))
qqnorm(resid(m4_string_si_dance))

hist(resid(m1_string_si_dance))
hist(resid(m2_string_si_dance))
hist(resid(m3_string_si_dance))
hist(resid(m4_string_si_dance))


################################################ Stringency -> SPS Reassurance of worth score -> Dance
m1_string_row_dance <- lme4::lmer(Danceability_mean ~ Stringency_index + SPS_ROW_mean + (1|Country) + Temp_mean, data = data_all_stress_europe_st)
m2_string_row_dance <- lme4::lmer(SPS_ROW_mean ~ Stringency_index + (1|Country), data = data_all_stress_europe_st)

set.seed((21))
med_string_row_dance <- mediation::mediate(m2_string_row_dance, m1_string_row_dance, treat="Stringency_index", mediator="SPS_ROW_mean")

summary(med_string_row_dance)

##Check Models
m1_string_row_dance_lmtest <-lmerTest::lmer(Danceability_mean ~ Stringency_index + SPS_ROW_mean + (1|Country) + Temp_mean, data = data_all_stress_europe_st)
m2_string_row_dance_lmtest <-lmerTest::lmer(SPS_ROW_mean ~ Stringency_index + (1|Country), data = data_all_stress_europe_st)
m3_string_row_dance_lmtest <-lmerTest::lmer(Danceability_mean ~ Stringency_index + (1|Country) + Temp_mean, data = data_all_stress_europe_st)
m4_string_row_dance_lmtest <-lmerTest::lmer(Danceability_mean ~ SPS_ROW_mean + (1|Country) + Temp_mean, data = data_all_stress_europe_st)

summary(m1_string_row_dance_lmtest)
performance::r2(m1_string_row_dance_lmtest)
performance::icc(m1_string_row_dance_lmtest)

summary(m2_string_row_dance_lmtest)
performance::r2(m2_string_row_dance_lmtest)
performance::icc(m2_string_row_dance_lmtest)

summary(m3_string_row_dance_lmtest)
performance::r2(m3_string_row_dance_lmtest)
performance::icc(m3_string_row_dance_lmtest)

summary(m4_string_row_dance_lmtest)
performance::r2(m4_string_row_dance_lmtest)
performance::icc(m4_string_row_dance_lmtest)

##Check regression assumptions (visual)
plot(m1_string_row_dance)
plot(m2_string_row_dance)
plot(m3_string_row_dance)
plot(m4_string_row_dance)

qqnorm(resid(m1_string_row_dance))
qqnorm(resid(m2_string_row_dance))
qqnorm(resid(m3_string_row_dance))
qqnorm(resid(m4_string_row_dance))

hist(resid(m1_string_row_dance))
hist(resid(m2_string_row_dance))
hist(resid(m3_string_row_dance))
hist(resid(m4_string_row_dance))


################################################ Stringency -> SPS Attachment -> Dance
m1_string_a_dance <- lme4::lmer(Danceability_mean ~ Stringency_index + SPS_A_mean + (1|Country) + Temp_mean, data = data_all_stress_europe_st)
m2_string_a_dance <- lme4::lmer(SPS_A_mean ~ Stringency_index + (1|Country), data = data_all_stress_europe_st)

set.seed((21))
med_string_a_dance <- mediation::mediate(m2_string_a_dance, m1_string_a_dance, treat="Stringency_index", mediator="SPS_A_mean")

summary(med_string_a_dance)

##Check Models
m1_string_a_dance_lmtest <-lmerTest::lmer(Danceability_mean ~ Stringency_index + SPS_A_mean + (1|Country) + Temp_mean, data = data_all_stress_europe_st)
m2_string_a_dance_lmtest <-lmerTest::lmer(SPS_A_mean ~ Stringency_index + (1|Country), data = data_all_stress_europe_st)
m3_string_a_dance_lmtest <-lmerTest::lmer(Danceability_mean ~ Stringency_index + (1|Country) + Temp_mean, data = data_all_stress_europe_st)
m4_string_a_dance_lmtest <-lmerTest::lmer(Danceability_mean ~ SPS_A_mean + (1|Country) + Temp_mean, data = data_all_stress_europe_st)

summary(m1_string_a_dance_lmtest)
performance::r2(m1_string_a_dance_lmtest)
performance::icc(m1_string_a_dance_lmtest)

summary(m2_string_a_dance_lmtest)
performance::r2(m2_string_a_dance_lmtest)
performance::icc(m2_string_a_dance_lmtest)

summary(m3_string_a_dance_lmtest)
performance::r2(m3_string_a_dance_lmtest)
performance::icc(m3_string_a_dance_lmtest)

summary(m4_string_a_dance_lmtest)
performance::r2(m4_string_a_dance_lmtest)
performance::icc(m4_string_a_dance_lmtest)

##Check regression assumptions (visual)
plot(m1_string_a_dance)
plot(m2_string_a_dance)
plot(m3_string_a_dance)
plot(m4_string_a_dance)

qqnorm(resid(m1_string_a_dance))
qqnorm(resid(m2_string_a_dance))
qqnorm(resid(m3_string_a_dance))
qqnorm(resid(m4_string_a_dance))

hist(resid(m1_string_a_dance))
hist(resid(m2_string_a_dance))
hist(resid(m3_string_a_dance))
hist(resid(m4_string_a_dance))


################################################ Stringency -> SPS Sense of reliable alliance -> Dance
m1_string_sora_dance <- lme4::lmer(Danceability_mean ~ Stringency_index + SPS_SORA_mean + (1|Country) + Temp_mean, data = data_all_stress_europe_st)
m2_string_sora_dance <- lme4::lmer(SPS_SORA_mean ~ Stringency_index + (1|Country), data = data_all_stress_europe_st)

set.seed((21))
med_string_sora_dance <- mediation::mediate(m2_string_sora_dance, m1_string_sora_dance, treat="Stringency_index", mediator="SPS_SORA_mean")

summary(med_string_sora_dance)

##Check Models
m1_string_sora_dance_lmtest <-lmerTest::lmer(Danceability_mean ~ Stringency_index + SPS_SORA_mean + (1|Country) + Temp_mean, data = data_all_stress_europe_st)
m2_string_sora_dance_lmtest <-lmerTest::lmer(SPS_SORA_mean ~ Stringency_index + (1|Country), data = data_all_stress_europe_st)
m3_string_sora_dance_lmtest <-lmerTest::lmer(Danceability_mean ~ Stringency_index +(1|Country) + Temp_mean, data = data_all_stress_europe_st)
m4_string_sora_dance_lmtest <-lmerTest::lmer(Danceability_mean ~ SPS_SORA_mean +(1|Country) + Temp_mean, data = data_all_stress_europe_st)

summary(m1_string_sora_dance_lmtest)
performance::r2(m1_string_sora_dance_lmtest)
performance::icc(m1_string_sora_dance_lmtest)

summary(m2_string_sora_dance_lmtest)
performance::r2(m2_string_sora_dance_lmtest)
performance::icc(m2_string_sora_dance_lmtest)

summary(m3_string_sora_dance_lmtest)
performance::r2(m3_string_sora_dance_lmtest)
performance::icc(m3_string_sora_dance_lmtest)

summary(m4_string_sora_dance_lmtest)
performance::r2(m4_string_sora_dance_lmtest)
performance::icc(m4_string_sora_dance_lmtest)

##Check regression assumptions (visual)
plot(m1_string_sora_dance)
plot(m2_string_sora_dance)
plot(m3_string_sora_dance)
plot(m4_string_sora_dance)

qqnorm(resid(m1_string_sora_dance))
qqnorm(resid(m2_string_sora_dance))
qqnorm(resid(m3_string_sora_dance))
qqnorm(resid(m4_string_sora_dance))

hist(resid(m1_string_sora_dance))
hist(resid(m2_string_sora_dance))
hist(resid(m3_string_sora_dance))
hist(resid(m4_string_sora_dance))


################################################ Stringency -> SPS Guidance -> Dance
m1_string_g_dance <- lme4::lmer(Danceability_mean ~ Stringency_index + SPS_G_mean + (1|Country) + Temp_mean, data = data_all_stress_europe_st)
m2_string_g_dance <- lme4::lmer(SPS_G_mean ~ Stringency_index + (1|Country), data = data_all_stress_europe_st)

set.seed((21))
med_string_g_dance <- mediation::mediate(m2_string_g_dance, m1_string_g_dance, treat="Stringency_index", mediator="SPS_G_mean")

summary(med_string_g_dance)

##Check Models
m1_string_g_dance_lmtest <-lmerTest::lmer(Danceability_mean ~ Stringency_index + SPS_G_mean + (1|Country) + Temp_mean, data = data_all_stress_europe_st)
m2_string_g_dance_lmtest <-lmerTest::lmer(SPS_G_mean ~ Stringency_index + (1|Country), data = data_all_stress_europe_st)
m3_string_g_dance_lmtest <-lmerTest::lmer(Danceability_mean ~ Stringency_index + (1|Country) + Temp_mean, data = data_all_stress_europe_st)
m4_string_g_dance_lmtest <-lmerTest::lmer(Danceability_mean ~ SPS_G_mean + (1|Country) + Temp_mean, data = data_all_stress_europe_st)

summary(m1_string_g_dance_lmtest)
performance::r2(m1_string_g_dance_lmtest)
performance::icc(m1_string_g_dance_lmtest)

summary(m2_string_g_dance_lmtest)
performance::r2(m2_string_g_dance_lmtest)
performance::icc(m2_string_g_dance_lmtest)

summary(m3_string_g_dance_lmtest)
performance::r2(m3_string_g_dance_lmtest)
performance::icc(m3_string_g_dance_lmtest)

summary(m4_string_g_dance_lmtest)
performance::r2(m4_string_g_dance_lmtest)
performance::icc(m4_string_g_dance_lmtest)

##Check regression assumptions (visual)
plot(m1_string_g_dance)
plot(m2_string_g_dance)
plot(m3_string_g_dance)
plot(m4_string_g_dance)

qqnorm(resid(m1_string_g_dance))
qqnorm(resid(m2_string_g_dance))
qqnorm(resid(m3_string_g_dance))
qqnorm(resid(m4_string_g_dance))

hist(resid(m1_string_g_dance))
hist(resid(m2_string_g_dance))
hist(resid(m3_string_g_dance))
hist(resid(m4_string_g_dance))
