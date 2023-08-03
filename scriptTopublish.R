

#####################################################################################
######################################################################################

# Analyses for "Stage-specific demographic effects of hydrologic variation in a stream salamander." American Naturalist

####################################################################################
#####################################################################################

library(dplyr)
library(tidyr)
library(RMark)
library(ggplot2)
library(lubridate)
library(ggprism)
library(mblm)

####################################################################################

# robust design pradel model 

#####################################################################################

# make sure to set working directory
sal.dat<-read.csv("HubbardBrook_SalamanderCaptureRecapture.csv",
                  header=TRUE) # read in full mark-recapture dataset

sal.dat2 <- sal.dat %>%
  filter(Species == "GP") %>% # only include Gyrinophilus porphyriticus 
  mutate(Stage = case_when(Stage == "M" ~ "L",# we want only two life stages (metamorphs go to larvae)
                           Stage == "L" ~ "L",
                           Stage == "A" ~ "A"))%>%
  filter(Remove == "N")%>% # remove individuals marked to remove previously
  filter(Stream != "Canyon")%>% # excluding canyon and cascade as they were only surveyed for 3 yrs
  filter(Stream != "Cascade")%>%
  mutate(LocRound = case_when(RawLongLoc < 100 ~ 50, # rounding meters to nearest 100m
                              RawLongLoc < 200 ~ 150,
                              RawLongLoc < 300 ~ 250,
                              RawLongLoc < 400 ~ 350,
                              RawLongLoc <= 500 ~ 450))%>%
  mutate(Site=paste0(Reach,"",Stream,"",LocRound))%>%
  filter(Primary != 1)%>% # excluding first 3 surveys every summer 
  filter(Primary != 4)%>% 
  filter(Primary != 7)%>% 
  filter(Primary != 10)%>% 
  filter(Primary != 13)%>% 
  filter(Primary != 16)%>% 
  filter(Primary != 19)%>% 
  filter(Primary != 22)


# need to add SurvNum = 39 for lower bear, upper bear and lower zigzag (their wasn't a 6th survey done in 2018)
sal.dat.extra <- sal.dat %>%
  filter(Species == "GP") %>% # only include Gyro
  mutate(Stage = case_when(Stage == "M" ~ "L",
                           Stage == "L" ~ "L",
                           Stage == "A" ~ "A"))%>%
  mutate(Site=paste0(Reach,"",Stream,""))%>%
  filter(Site == "LowerZigzag" | Site == "LowerBear" |
           Site == "UpperBear")%>% 
  mutate(LocRound = case_when(RawLongLoc < 100 ~ 50,
                              RawLongLoc < 200 ~ 150,
                              RawLongLoc < 300 ~ 250,
                              RawLongLoc < 400 ~ 350,
                              RawLongLoc <= 500 ~ 450))%>%
  mutate(Site=paste0(Site,"",LocRound))%>%
  filter(SurNum == 39)%>%
  mutate(SurNum = case_when(SurNum == 39 ~ 42))%>% # change SurNum
  mutate(Primary = case_when(Primary == 13 ~ 14)) # change primary number

# combine two datasets
sal.dat2<-rbind(sal.dat2,sal.dat.extra)


sal.dat2 <- sal.dat2 %>%
  drop_na(SurNum)%>% # get rid of captures if not within a SurNum 
  drop_na(FinalID)%>% # get rid of all NAs in ID data
  mutate(detect = 1)%>% 
  dplyr::select(FinalID, Primary, SurNum,  Site, Stream, Reach, detect) %>%
  mutate(Site = case_when(Site == "UpperCascade50" ~ "ucas50",
                          Site == "UpperCascade150" ~ "ucas150",
                          Site == "UpperCascade250" ~ "ucas250",
                          Site == "UpperCascade350" ~ "ucas350",
                          Site == "UpperCascade450" ~ "ucas450",
                          
                          Site == "UpperCanyon50" ~ "ucan50",
                          Site == "UpperCanyon150" ~ "ucan150",
                          Site == "UpperCanyon250" ~ "ucan250",
                          Site == "UpperCanyon350" ~ "ucan350",
                          Site == "UpperCanyon450" ~ "ucan450",
                          
                          Site == "UpperZigzag50" ~ "uzz50",
                          Site == "UpperZigzag150" ~ "uzz150",
                          Site == "UpperZigzag250" ~ "uzz250",
                          Site == "UpperZigzag350" ~ "uzz350",
                          Site == "UpperZigzag450" ~ "uzz450",
                          
                          Site == "UpperBear50" ~ "ub50",
                          Site == "UpperBear150" ~ "ub150",
                          Site == "UpperBear250" ~ "ub250",
                          Site == "UpperBear350" ~ "ub350",
                          Site == "UpperBear450" ~ "ub450",
                          
                          Site == "UpperParadise50" ~ "upar50",
                          Site == "UpperParadise150" ~ "upar150",
                          Site == "UpperParadise250" ~ "upar250",
                          Site == "UpperParadise350" ~ "upar350",
                          Site == "UpperParadise450" ~ "upar450",
                          
                          Site == "LowerCascade50" ~ "lcas50",
                          Site == "LowerCascade150" ~ "lcas150",
                          Site == "LowerCascade250" ~ "lcas250",
                          Site == "LowerCascade350" ~ "lcas350",
                          Site == "LowerCascade450" ~ "lcas450",
                          
                          Site == "LowerCanyon50" ~ "lcan50",
                          Site == "LowerCanyon150" ~ "lcan150",
                          Site == "LowerCanyon250" ~ "lcan250",
                          Site == "LowerCanyon350" ~ "lcan350",
                          Site == "LowerCanyon450" ~ "lcan450",
                          
                          Site == "LowerZigzag50" ~ "lzz50",
                          Site == "LowerZigzag150" ~ "lzz150",
                          Site == "LowerZigzag250" ~ "lzz250",
                          Site == "LowerZigzag350" ~ "lzz350",
                          Site == "LowerZigzag450" ~ "lzz450",
                          
                          Site == "LowerBear50" ~ "lb50",
                          Site == "LowerBear150" ~ "lb150",
                          Site == "LowerBear250" ~ "lb250",
                          Site == "LowerBear350" ~ "lb350",
                          Site == "LowerBear450" ~ "lb450",
                          
                          Site == "LowerParadise50" ~ "lpar50",
                          Site == "LowerParadise150" ~ "lpar150",
                          Site == "LowerParadise250" ~ "lpar250",
                          Site == "LowerParadise350" ~ "lpar350",
                          Site == "LowerParadise450" ~ "lpar450",
                          
                          Site == "Cushman" ~ "cushman",
                          Site == "Steep" ~ "steep",
                          Site == "WBZZ" ~ "wbzz",
                          Site == "Paradise" ~ "paradise",
                          Site == "Weir4" ~ "w4",
                          Site == "LowerCanyon" ~ "lowcanyon",
                          Site == "UpperCanyon" ~ "upcanyon",
                          Site == "Bagley" ~ "bagley"))%>%
  drop_na(Site)


# bring in watershed area data
area<-read.csv("data/WatershedArea_for_SalamanderSurveys.csv",
               header=TRUE)

# join watershed area to capture data
sal.dat3<-left_join(sal.dat2, area, by=c("Site","Stream"))


# create encounter history 
cap.hist <- sal.dat3 %>%
  dplyr::select(FinalID, SurNum, Stream, Reach, Site, Area_km2, Fish, detect)%>%
  mutate(scale_area = scale(Area_km2))%>%
  group_by(FinalID)%>%
  spread(SurNum,detect,fill=0)

cap.his1<-cap.hist[,8:55] # remove id, all covariates to make inp format
covariates<-data.frame(cap.hist[,2:7])

#function to read the matrix and create the capture history strings
pasty<-function(x) 
{
  k<-ncol(x)
  n<-nrow(x)
  out<-array(dim=n)
  for (i in 1:n)
  {
    y<-(x[i,]>0)*1
    out[i]<-paste(y[1],y[2],y[3],y[4],y[5],y[6],y[7],y[8],y[9],y[10],
                  y[11],y[12],y[13],y[14],y[15],y[16],y[17],y[18],y[19],y[20],
                  y[21],y[22],y[23],y[24],y[25],y[26],y[27],y[28],y[29],y[30],
                  y[31],y[32],y[33],y[34],y[35],y[36],y[37],y[38],y[39],y[40],
                  y[41],y[42],y[43],y[44],y[45],y[46],y[47],y[48],
                  sep="")
  }
  return(out)
}

#capture history data frame for RMark  
capt.hist<-data.frame(ch=pasty(cap.his1))

# add back in co-variates to inp ch
capt.hist<-data.frame(capt.hist,Area=covariates$scale_area, Fish=covariates$Fish, 
                      Stream=covariates$Stream)


# set up time intervals for 8 primary occasions each with 6 secondary occasions
# these values are the interval lengths between occasions
# e.g, 0 time between occ 1 - 5, 1 year between 5 & 6
time.intervals <- c(0, 0, 0 ,0 ,0,  # 2012
                    1,0, 0, 0 ,0 ,0 ,  # 2013
                    1,0, 0, 0 ,0 ,0 ,  # 2014
                    1,0, 0, 0 ,0 ,0 ,  # 2015
                    3,0, 0, 0 ,0 ,0 ,  # 2018
                    1,0, 0, 0 ,0 ,0 ,  # 2019
                    1,0, 0, 0 ,0 ,0 ,  # 2020
                    1,0, 0, 0 ,0 ,0)    # 2021

# Robust design Pradel model with Huggins closed estimator for abundance
sal.process=process.data(capt.hist,
                         model="RDPdfClosed", 
                         time.intervals=time.intervals,
                         groups=c("Stream","Fish")) # have to include factors for stream and reach (fish)
names(sal.process)
head(sal.process$data) # take a look at the data

# make design data
sal.ddl=make.design.data(sal.process)
sal.ddl
# check the number of primary occasions 
sal.process$nocc # s/b 8

# check the number of secondary occasions 
sal.process$nocc.secondary # s/b 6




#########################################################################################
# calculating high and low flows across all years leading up to surveys 
# daily discharge data (mm/day)

CMR_Q_all<-read.csv(file="HBEF_DailyStreamflow_1956-2022.csv")

# change time to POSIXct
CMR_Q_all$DATE <- as.POSIXct(CMR_Q_all$DATE, 
                             tz="America/New_York", format="%m/%d/%Y")

# calculating 7-day minimum flow for weir 3 / paradise brook
CMR_Q1_3 <- CMR_Q_all %>%
  filter(WS == 3)%>%
  mutate(Month = month(DATE))%>%
  mutate(jday = yday(DATE))%>%
  mutate(Year = year(DATE))%>%
  mutate(surv_yr = case_when(Month < 7 ~ Year -1,
                             Month >= 7 ~ Year)) # need "year" to go from july 1st to following june 30

Qmovingavg <- zoo::rollapplyr(CMR_Q1_3$Streamflow,  7, mean,  align='right') # 7day low flow from 6 days prior
# need to add 6 NAs to Qmovingavg
Qmovingavg <- c( rep(NA,6),Qmovingavg)
CMR_Q1_3$Qmovingavg <- Qmovingavg

CMR_Q1_3 <- CMR_Q1_3 %>%
  filter(surv_yr > 1957)%>% # 1958 is first complete year
  filter(surv_yr < 2022)


# weir 6 / bear brook
CMR_Q1_6 <- CMR_Q_all %>%
  filter(WS == 6)%>%
  mutate(Month = month(DATE))%>%
  mutate(jday = yday(DATE))%>%
  mutate(Year = year(DATE))%>%
  mutate(surv_yr = case_when(Month < 7 ~ Year -1,
                             Month >= 7 ~ Year)) # need "year" to go from july 1st to following june 30


Qmovingavg6 <- zoo::rollapplyr(CMR_Q1_6$Streamflow,  7, mean,  align='right') # 7day low flow from 6 days prior
# need to add 6 NAs to Qmovingavg
Qmovingavg6 <- c( rep(NA,6),Qmovingavg6)
CMR_Q1_6$Qmovingavg <- Qmovingavg6

CMR_Q1_6 <- CMR_Q1_6 %>%
  filter(surv_yr > 1962)%>%# 1963 is first complete year
  filter(surv_yr < 2022)

# weir 7 / zigzag brook
CMR_Q1_7 <- CMR_Q_all %>%
  filter(WS == 7)%>%
  mutate(Month = month(DATE))%>%
  mutate(jday = yday(DATE))%>%
  mutate(Year = year(DATE))%>%
  mutate(surv_yr = case_when(Month < 7 ~ Year -1,
                             Month >= 7 ~ Year)) # need "year" to go from july 1st to following june 30

Qmovingavg7 <- zoo::rollapplyr(CMR_Q1_7$Streamflow,  7, mean,  align='right') # 7day low flow from 6 days prior
# need to add 6 NAs to Qmovingavg
Qmovingavg7 <- c( rep(NA,6),Qmovingavg7)
CMR_Q1_7$Qmovingavg <- Qmovingavg7

CMR_Q1_7 <- CMR_Q1_7 %>%
  filter(surv_yr > 1964)%>%# 1965 is first complete year
  filter(surv_yr < 2022)

CMR_Q1<-full_join(CMR_Q1_3, CMR_Q1_6)
CMR_Q1<-full_join(CMR_Q1, CMR_Q1_7)


## calculate recurrence intervals for low flow events for each year
# for weir 3 first
CMR_Q1_weir3<-CMR_Q1_trend%>%
  filter(WS == 3)%>%
  arrange(Q1)%>% # arrange by ascending order
  mutate(rank = seq(1,64,1))%>% # create rank by order
  mutate(RI = 65/rank)
# recurrence interval 
# T = (n+1)/m where recurrence interval (T), n  = number of yrs, m = magnitude ranking


# low flow recurrence intervals for weir 6
CMR_Q1_weir6<-CMR_Q1_trend%>%
  filter(WS == 6)%>%
  arrange(Q1)%>% # arrange by ascending order
  mutate(rank = seq(1,59,1))%>% # create rank by order
  mutate(RI = 60/rank)

# low flow recurrence intervals for weir 7
CMR_Q1_weir7<-CMR_Q1_trend%>%
  filter(WS == 7)%>%
  arrange(Q1)%>% # arrange by ascending order
  mutate(rank = seq(1,57,1))%>% # create rank by order
  mutate(RI = 58/rank)
# 2016 was dry (6 yr recurrence interval) for 0.026000000 mm/day
# 2020 also fairly dry (4 yr recurrence interval) for 0.029571429 mm/day




CMR_Q1_all_summary<-CMR_Q1 %>%
  ungroup()%>%
  group_by(WS,surv_yr)%>%
  summarise(Q1 = min(Qmovingavg))%>%
  filter(surv_yr > 2011)%>%
  filter(surv_yr < 2021) # only want from 2011 surv yr to 2020 surv yr
CMR_Q1_all_summary # record starts in year 1958
# reminder that surv_yr represents the correct year in summer/fall and the "incorrect" yr for the winter leading up to surveys
# calculating survival and recruitment only occurs across the first open period btw yr 1 and 2 surveys

### choosing the lowest of the 2015 - 2017 7-day low flows (b/c no salamander surveys occurred in 2016 and 2017)
CMR_Q1_all_summary <- CMR_Q1_all_summary[-4,] # removing weir 3, 2015
CMR_Q1_all_summary <- CMR_Q1_all_summary[-4,] # removing weir 3, 2016
CMR_Q1_all_summary <- CMR_Q1_all_summary[-11,] # removing weir 6, 2015
CMR_Q1_all_summary <- CMR_Q1_all_summary[-11,] # removing weir 6, 2016
CMR_Q1_all_summary <- CMR_Q1_all_summary[-18,] # removing weir 7, 2015
CMR_Q1_all_summary <- CMR_Q1_all_summary[-19,] # removing weir 7, 2017


yrs_Q1 <- CMR_Q1_all_summary %>%
  mutate(surv_yr = case_when(surv_yr == 2012 ~ 1, # 12-13 # need to lump 2 missing yrs with 4
                             surv_yr == 2013 ~ 2, # 13-14
                             surv_yr == 2014 ~ 3, # 14-15
                             surv_yr == 2015 ~ 4, # 15-16
                             surv_yr == 2016 ~ 4, # 16-17
                             surv_yr == 2017 ~ 4, # 17-18
                             surv_yr == 2018 ~ 7, # 18-19
                             surv_yr == 2019 ~ 8, # 19-20
                             surv_yr == 2020 ~ 9))%>% # 20-21
  ungroup()%>%
  mutate(Q1_scale = scale(Q1))
yrs_Q1
# smaller values (negative if scaled) are worse periods of drought

# 
capt.hist.bear <- capt.hist %>%
  filter(Stream == "Bear")
capt.hist.paradise <- capt.hist %>%
  filter(Stream == "Paradise")
capt.hist.zigzag <- capt.hist %>%
  filter(Stream == "Zigzag")

capt.hist.bear$lflow1=rep(yrs_Q1$Q1_scale[1],1220) # 1220 is the number of individuals in bear
capt.hist.bear$lflow2=rep(yrs_Q1$Q1_scale[2],1220)
capt.hist.bear$lflow3=rep(yrs_Q1$Q1_scale[3],1220)
capt.hist.bear$lflow4=rep(yrs_Q1$Q1_scale[4],1220)
capt.hist.bear$lflow7=rep(yrs_Q1$Q1_scale[5],1220)
capt.hist.bear$lflow8=rep(yrs_Q1$Q1_scale[6],1220)
capt.hist.bear$lflow9=rep(yrs_Q1$Q1_scale[7],1220)

capt.hist.paradise$lflow1=rep(yrs_Q1$Q1_scale[1],1214) # 1214 is the number of individuals in paradise
capt.hist.paradise$lflow2=rep(yrs_Q1$Q1_scale[2],1214)
capt.hist.paradise$lflow3=rep(yrs_Q1$Q1_scale[3],1214)
capt.hist.paradise$lflow4=rep(yrs_Q1$Q1_scale[4],1214)
capt.hist.paradise$lflow7=rep(yrs_Q1$Q1_scale[5],1214)
capt.hist.paradise$lflow8=rep(yrs_Q1$Q1_scale[6],1214)
capt.hist.paradise$lflow9=rep(yrs_Q1$Q1_scale[7],1214)

capt.hist.zigzag$lflow1=rep(yrs_Q1$Q1_scale[1],873) # 873 is the number of individuals
capt.hist.zigzag$lflow2=rep(yrs_Q1$Q1_scale[2],873)
capt.hist.zigzag$lflow3=rep(yrs_Q1$Q1_scale[3],873)
capt.hist.zigzag$lflow4=rep(yrs_Q1$Q1_scale[4],873)
capt.hist.zigzag$lflow7=rep(yrs_Q1$Q1_scale[5],873)
capt.hist.zigzag$lflow8=rep(yrs_Q1$Q1_scale[6],873)
capt.hist.zigzag$lflow9=rep(yrs_Q1$Q1_scale[7],873)

# combine capture histories
capt.hist<-full_join(capt.hist.bear,capt.hist.paradise)
capt.hist<-full_join(capt.hist,capt.hist.zigzag)

# process data
sal.process=process.data(capt.hist,
                         model="RDPdfClosed", 
                         time.intervals=time.intervals,
                         groups=c("Stream","Fish")) # have to include factors
names(sal.process)
head(sal.process$data) # take a look at the data


# add lowflow environmental covariate to design data for Phi; this matches lowflow 1 to lowflow 9
sal.ddl$Phi$lowflow = c(yrs_Q1$Q1_scale,yrs_Q1$Q1_scale)
sal.ddl$f$lowflow = c(yrs_Q1$Q1_scale,yrs_Q1$Q1_scale)







#########################################################
# calculating years with high flow (floods)
CMR_Q99_3 <- CMR_Q_all %>% # weir 3
  filter(WS == 3)%>%
  mutate(Month = month(DATE))%>%
  mutate(jday = yday(DATE))%>%
  mutate(Year = year(DATE))%>%
  mutate(surv_yr = case_when(Month < 7 ~ Year -1,
                             Month >= 7 ~ Year))%>% # need "year" to go from july 1st to following june 30
  filter(surv_yr > 1957)%>%
  filter(surv_yr < 2022)

CMR_Q99_6 <- CMR_Q_all %>% # weir 6
  filter(WS == 6)%>%
  mutate(Month = month(DATE))%>%
  mutate(jday = yday(DATE))%>%
  mutate(Year = year(DATE))%>%
  mutate(surv_yr = case_when(Month < 7 ~ Year -1,
                             Month >= 7 ~ Year))%>% # need "year" to go from july 1st to following june 30
  filter(surv_yr > 1962)%>%
  filter(surv_yr < 2022)


CMR_Q99_7 <- CMR_Q_all %>% # weir 7
  filter(WS == 7)%>%
  mutate(Month = month(DATE))%>%
  mutate(jday = yday(DATE))%>%
  mutate(Year = year(DATE))%>%
  mutate(surv_yr = case_when(Month < 7 ~ Year -1,
                             Month >= 7 ~ Year))%>% # need "year" to go from july 1st to following june 30
  filter(surv_yr > 1964)%>%
  filter(surv_yr < 2022)


CMR_Q99<-full_join(CMR_Q99_3, CMR_Q99_6) # join all three 3 streams data together
CMR_Q99<-full_join(CMR_Q99, CMR_Q99_7)

### look at trends and recurrence intervals over time
CMR_Q99_trends<-CMR_Q99 %>%
  ungroup()%>%
  group_by(WS,surv_yr)%>%
  summarise(Q99 = max(Streamflow))

# flood recurrence interval calculations for weir 3 
CMR_Q99_trends_w3<-CMR_Q99_trends%>%
  filter(WS == 3)%>%
  arrange(desc(Q99))%>% # arrange by descending order
  mutate(rank = seq(1,64,1))%>% # create rank by order
  mutate(RI = 65/rank)
# recurrence interval 
# T = (n+1)/m where recurrence interval (T), n  = number of yrs, m = magnitude ranking



# flood recurrence interval calculations for weir 6
CMR_Q99_trends_w6<-CMR_Q99_trends%>%
  filter(WS == 6)%>%
  arrange(desc(Q99))%>% # arrange by descending order
  mutate(rank = seq(1,59,1))%>% # create rank by order
  mutate(RI = 60/rank)
# recurrence interval 
# T = (n+1)/m where recurrence interval (T), n  = number of yrs, m = magnitude ranking

# flood recurrence intervals calculations for weir 7
CMR_Q99_trends_w7<-CMR_Q99_trends%>%
  filter(WS == 7)%>%
  arrange(desc(Q99))%>% # arrange by descending order
  mutate(rank = seq(1,57,1))%>% # create rank by order
  mutate(RI = 58/rank)
# recurrence interval 
# T = (n+1)/m where recurrence interval (T), n  = number of yrs, m = magnitude ranking


CMR_Q99_all_summary<-CMR_Q99 %>%
  ungroup()%>%
  group_by(WS,surv_yr)%>%
  summarise(Q99 = max(Streamflow))%>%
  filter(surv_yr > 2011)%>%
  filter(surv_yr < 2021) # only want from 2011 surv yr to 2020 surv yr
CMR_Q99_all_summary # record starts in year 1958
# reminder that surv_yr represents the correct year in summer/fall and the "incorrect" yr for the winter leading up to surveys
# calculating survival and recruitment only occurs across the first open period btw yr 1 and 2 surveys

### choosing the highest of the 2015 - 2017 high flows
CMR_Q99_all_summary <- CMR_Q99_all_summary[-4,] # removing weir 3, 2015
CMR_Q99_all_summary <- CMR_Q99_all_summary[-4,] # removing weir 3, 2016
CMR_Q99_all_summary <- CMR_Q99_all_summary[-11,] # removing weir 6, 2015
CMR_Q99_all_summary <- CMR_Q99_all_summary[-11,] # removing weir 6, 2016
CMR_Q99_all_summary <- CMR_Q99_all_summary[-18,] # removing weir 7, 2015
CMR_Q99_all_summary <- CMR_Q99_all_summary[-18,] # removing weir 7, 2015


yrs_Q99 <- CMR_Q99_all_summary %>%
  mutate(surv_yr = case_when(surv_yr == 2012 ~ 1, # 12-13 # need to lump 2 missing yrs with 4
                             surv_yr == 2013 ~ 2, # 13-14
                             surv_yr == 2014 ~ 3, # 14-15
                             surv_yr == 2015 ~ 4, # 15-16
                             surv_yr == 2016 ~ 4, # 16-17
                             surv_yr == 2017 ~ 4, # 17-18
                             surv_yr == 2018 ~ 7, # 18-19
                             surv_yr == 2019 ~ 8, # 19-20
                             surv_yr == 2020 ~ 9))%>% # 20-21
  ungroup()%>%
  mutate(Q99_scale = scale(Q99))
yrs_Q99
# larger values = bigger flood



capt.hist.bear$hflow1=rep(yrs_Q99$Q99_scale[1],1220) # 1220 is the number of sallies
capt.hist.bear$hflow2=rep(yrs_Q99$Q99_scale[2],1220)
capt.hist.bear$hflow3=rep(yrs_Q99$Q99_scale[3],1220)
capt.hist.bear$hflow4=rep(yrs_Q99$Q99_scale[4],1220)
capt.hist.bear$hflow7=rep(yrs_Q99$Q99_scale[5],1220)
capt.hist.bear$hflow8=rep(yrs_Q99$Q99_scale[6],1220)
capt.hist.bear$hflow9=rep(yrs_Q99$Q99_scale[7],1220)

capt.hist.paradise$hflow1=rep(yrs_Q99$Q99_scale[1],1214) # 1214 is the number of sallies
capt.hist.paradise$hflow2=rep(yrs_Q99$Q99_scale[2],1214)
capt.hist.paradise$hflow3=rep(yrs_Q99$Q99_scale[3],1214)
capt.hist.paradise$hflow4=rep(yrs_Q99$Q99_scale[4],1214)
capt.hist.paradise$hflow7=rep(yrs_Q99$Q99_scale[5],1214)
capt.hist.paradise$hflow8=rep(yrs_Q99$Q99_scale[6],1214)
capt.hist.paradise$hflow9=rep(yrs_Q99$Q99_scale[7],1214)

capt.hist.zigzag$hflow1=rep(yrs_Q99$Q99_scale[1],873) # 873 is the number of sallies
capt.hist.zigzag$hflow2=rep(yrs_Q99$Q99_scale[2],873)
capt.hist.zigzag$hflow3=rep(yrs_Q99$Q99_scale[3],873)
capt.hist.zigzag$hflow4=rep(yrs_Q99$Q99_scale[4],873)
capt.hist.zigzag$hflow7=rep(yrs_Q99$Q99_scale[5],873)
capt.hist.zigzag$hflow8=rep(yrs_Q99$Q99_scale[6],873)
capt.hist.zigzag$hflow9=rep(yrs_Q99$Q99_scale[7],873)

# combine capture histories
capt.hist<-full_join(capt.hist.bear,capt.hist.paradise)
capt.hist<-full_join(capt.hist,capt.hist.zigzag)

# process data
sal.process=process.data(capt.hist,
                         model="RDPdfClosed", 
                         time.intervals=time.intervals,
                         groups=c("Stream","Fish")) # have to include factors
names(sal.process)
head(sal.process$data) # take a look at the data


# add highflow environmental covariate to design data for Phi; this matches highflow 1 to highflow 9
sal.ddl$Phi$highflow = c(yrs_Q99$Q99_scale,yrs_Q99$Q99_scale)
sal.ddl$f$highflow = c(yrs_Q99$Q99_scale,yrs_Q99$Q99_scale)


### looking at correlation between high and low flows 
flow_metrics <- data.frame(yrs_Q1$WS, yrs_Q1$Q1_scale, yrs_Q99$Q99_scale)
head(flow_metrics)

plot(flow_metrics$yrs_Q1.Q1_scale, flow_metrics$yrs_Q99.Q99_scale)
cor.test(flow_metrics$yrs_Q1.Q1_scale, flow_metrics$yrs_Q99.Q99_scale)









###################################################################################
## fitting best recapture model structure first 

## Capture and Recapture rates
sal.ddl$p

# p=c i.e., capture = recapture due to use of "share=TRUE"
p.dot = list(formula =  ~ 1, share = TRUE) # null model

# fish variable
p.fish = list(formula = ~ Fish, share = TRUE)

# session = varies by primary, ie. time
p.time = list(formula =  ~ session, share = TRUE)

# p and c varies by stream - already accounted for with fixed group
p.stream = list(formula =  ~ Stream, share = TRUE)


p.fish.time = list(formula = ~ Fish + session, share = TRUE) # model w fish and time
p.fish.stream = list(formula = ~ Fish + Stream, share = TRUE) # model w fish and stream
p.time.stream = list(formula = ~ session + Stream, share = TRUE) # model with time and stream


# p and c vary by stream and session (e.g. time)
p.fish.time.stream = list(formula =  ~ Fish + session + Stream, share = TRUE) 


# start with just time varying phi and f to select for best capture model structure
Phi.time = list(formula =  ~ time, share = TRUE) 
f.time = list(formula =  ~ time, share = TRUE)


# Run all pairings of models
sal.model.list.for.ponly=create.model.list("RDPdfClosed")

sal.results.forponly=mark.wrapper(sal.model.list.for.ponly,
                                  data=sal.process,
                                  ddl=sal.ddl)

# get results
sal.results.forponly
names(sal.results.forponly)



############ RUNNING THE PRADEL MODEL ###############################

### once best recapture model structure is found then run a model with co-variates on survival and recruitment


# final recapture structure (make sure the other co-variate model structures from above aren't loaded into the console)
p.fish.stream = list(formula =  ~ Fish + Stream, share = TRUE)

### survival structure 
# null - always going to include stream and area 
Phi.dot = list(formula = ~ Stream + Area)

# one covariate 
Phi.lowflow = list(formula = ~ Stream + Area + lflow)
Phi.highflow = list(formula = ~ Stream + Area + hflow, link="logit")

# 2 covariates 
Phi.lowflow.highflow = list(formula = ~Stream + Area + lflow + hflow)
# not including fish because it is correlated with watershed area 



### recruitment structure 
# null - always going to include stream and area 
f.dot = list(formula = ~ Stream + Area)

# one covariate 
f.lowflow = list(formula = ~ Stream + Area + lflow)
f.highflow = list(formula = ~ Stream + Area + hflow)

# 2 covariates 
f.lowflow.highflow = list(formula = ~Stream + Area + lflow + hflow, link="logit")



# Run all pairings of models
sal.model.list=create.model.list("RDPdfClosed")

sal.results=mark.wrapper(sal.model.list,
                         data=sal.process,
                         ddl=sal.ddl)

# get results
options(width = 160)
sal.results


# look at output from top model
names(sal.results)
top<-sal.results$Phi.highflow.f.lowflow.p.fish.stream # save top model
top2<-sal.results$Phi.highflow.f.lowflow.highflow.p.fish.stream
top3<-sal.results$Phi.lowflow.highflow.f.lowflow.p.fish.stream

summary(top, showall = FALSE)

top$results$beta # top model beta results


# mean survival, recruitment, and recapture rates for top model 
mean(top$results$real$estimate[1:21])# phi 
sd(top$results$real$estimate[1:21])


mean(top$results$real$estimate[22:42])# f
sd(top$results$real$estimate[22:42]) 


mean(top$results$real$estimate[43:48])# p
sd(top$results$real$estimate[43:48])


# arithmetic mean
mean(top$results$derived$`Lambda Population Change`$estimate)
sd(top$results$derived$`Lambda Population Change`$estimate)
min(top$results$derived$`Lambda Population Change`$estimate)
max(top$results$derived$`Lambda Population Change`$estimate)


# geometric mean
# geometric mean is the appropriate average for stochastic population growth (Cooch & White 2002) 
exp(mean(log(top$results$derived$`Lambda Population Change`$estimate)))



top$results$real

top$results$derived
min(top$results$derived$`Lambda Population Change`$estimate) #  
max(top$results$derived$`Lambda Population Change`$estimate) #   



##############################################
## likelihood ratio test btw nested models
#############################################

### btw top model and second best
# pay attn to order of models (btw simple and more complex) - top is simpler in our case 
1-pchisq((top$results$lnl-top2$results$lnl),df=top2$results$npar-top$results$npar)
# p-value

top$results$lnl-top2$results$lnl
# chi-square value





###################################################
### view results given different hydrologic conditions 


# stream drying on recruitment
# going to do 3 separate lines for 3 streams
min.lowflow = min(yrs_Q1$Q1_scale)
max.lowflow = max(yrs_Q1$Q1_scale)
lowflow.values = seq(from = min.lowflow, to = max.lowflow, length = 1000)


prediction.top.bear <- covariate.predictions(top, 
                                             data = data.frame(lflow1 = lowflow.values),
                                             indices=c(43)) # indices should match stream and parameter
prediction.top.bear <- prediction.top.bear$estimates

prediction.top.par <- covariate.predictions(top, 
                                            data = data.frame(lflow1 = lowflow.values),
                                            indices=c(50)) 
prediction.top.par <- prediction.top.par$estimates

prediction.top.zz <- covariate.predictions(top, 
                                           data = data.frame(lflow1 = lowflow.values),
                                           indices=c(57)) 
prediction.top.zz <- prediction.top.zz$estimates

# combine streams into one df
prediction.top.zz$Stream <- rep("Zigzag",1000)
prediction.top.par$Stream <- rep("Paradise",1000)
prediction.top.bear$Stream <- rep("Bear",1000)

pred.top2<-full_join(prediction.top.bear, prediction.top.par)
pred.top2<-full_join(pred.top2,prediction.top.zz)


# transform discharge back to Q1 to interpret 
# scale subtracts the mean and divides by the sd
# so multifply by sd then add mean to transform back
mlow<-mean(yrs_Q1$Q1)
slow<-sd(yrs_Q1$Q1)

full.prediction.drought.f <- pred.top2 %>%
  mutate(step1 = covdata *slow)%>%
  mutate(step2 = step1+mlow)%>%
  rename(Q1 = step2)


# effect size of change in drought on overall survival 
minfQ1<-min(prediction.top.bear$estimate) # at smallest Q1
maxfQ1<-max(prediction.top.bear$estimate) # at largest Q1


# percent decrease = low starting value goes first 
# ((start value (low value) - final value )/ |final value| )*100
((minfQ1-maxfQ1)/abs(maxfQ1))*100 

### confidence intervals
minfQ1.lcl<-min(prediction.top.bear$lcl) # at smallest Q1
maxfQ1.lcl<-max(prediction.top.bear$lcl) # at largest Q1

# percent decrease = low starting value goes first 
# ((start value (low value) - final value )/ |final value| )*100
((minfQ1.lcl-maxfQ1.lcl)/abs(maxfQ1.lcl))*100 

minfQ1.ucl<-min(prediction.top.bear$ucl) # at smallest Q1
maxfQ1.ucl<-max(prediction.top.bear$ucl) # at largest Q1

# percent decrease = low starting value goes first 
# ((start value (low value) - final value )/ |final value| )*100
((minfQ1.ucl-maxfQ1.ucl)/abs(maxfQ1.ucl))*100 



### the effect of flooding on survival 
### min and max flooding
min.highflow = min(yrs_Q99$Q99_scale)
max.highflow = max(yrs_Q99$Q99_scale)
highflow.values = seq(from = min.highflow, to = max.highflow, length = 1000)



prediction.top.flood.phi <- covariate.predictions(top, 
                                                  data = data.frame(hflow1 = highflow.values),
                                                  indices=c(1)) 
pred.top <- prediction.top.flood.phi$estimates

# transform discharge back to Q99 to interpret 
# scale subtracts the mean and divides by the sd
# so multiply by sd then add mean to transform back
mhigh<-mean(yrs_Q99$Q99)
shigh<-sd(yrs_Q99$Q99)

full.prediction.flood.phi <- pred.top %>%
  mutate(step1 = covdata *shigh)%>%
  mutate(step2 = step1+mhigh)%>%
  rename(Q99 = step2)


# effect size of change in flooding on overall survival 
minphiQ99<-min(full.prediction.flood.phi$estimate) # at smallest Q99
maxphiQ99<-max(full.prediction.flood.phi$estimate) # at largest Q99

# to calculate percent decrease
# ((start value (low value) - final value )/ |final value| )*100
((minphiQ99 - maxphiQ99)/abs(maxphiQ99))*100 

# confidence intervals
minphiQ99.lcl<-min(full.prediction.flood.phi$lcl) # at smallest Q99
maxphiQ99.lcl<-max(full.prediction.flood.phi$lcl) # at largest Q99

# to calculate percent decrease
# ((start value (low value) - final value )/ |final value| )*100
((minphiQ99.lcl - maxphiQ99.lcl)/abs(maxphiQ99.lcl))*100 

minphiQ99.ucl<-min(full.prediction.flood.phi$ucl) # at smallest Q99
maxphiQ99.ucl<-max(full.prediction.flood.phi$ucl) # at largest Q99

# to calculate percent decrease
# ((start value (low value) - final value )/ |final value| )*100
((minphiQ99.ucl - maxphiQ99.ucl)/abs(maxphiQ99.ucl))*100 







######## deriving estimates of lambda given different beta (flow values)
summary(top,se=TRUE,showall=FALSE)
betas=summary(top)$beta

# 95% confidence intervals for beta estimates
beta.lcl=betas$estimate-1.96*betas$se
beta.ucl=betas$estimate+1.96*betas$se

# 95% confidence intervals for real parameter estimates
# inverse logit (which is the chosen link function for survival and recruitment) of the lower and upper limits on the betas
exp(beta.lcl)/(1+exp(beta.lcl))
exp(beta.ucl)/(1+exp(beta.ucl))

# need to use delta method to accurately estimate the se of derived estimates
### see appendix b - the delta method in mark book for details!!
deltamethod<- function (g, mean, cov, ses = TRUE)
{
  cov <- as.matrix(cov)
  n <- length(mean)
  if (!is.list(g))
    g <- list(g)
  if ((dim(cov)[1] != n) || (dim(cov)[2] != n))
    stop(paste("Covariances should be a ", n, " by ", n,
               " matrix"))
  syms <- paste("x", 1:n, sep = "")
  for (i in 1:n) assign(syms[i], mean[i])
  gdashmu <- t(sapply(g, function(form) {
    as.numeric(attr(eval(deriv(form, syms)), "gradient"))
  }))
  new.covar <- gdashmu %*% cov %*% t(gdashmu)
  if (ses) {
    new.se <- sqrt(diag(new.covar))
    new.se
  }
  else new.covar
}

# individually compute the standard error for the real paramters with calls 
# to deltamethod using the inverse logit function 
deltamethod(~exp(x1)/(1+exp(x1)),mean=betas$estimate[1],cov=betas$se[1]^2)





# get mean estimates of the effect of flooding on lambda (only significant on phi not f)
# high flow effect on survival
b1<-(betas$estimate[1] + betas$estimate[5]*-1) # low high flow 
exp(b1)/(1+exp(b1)) 

b2<-(betas$estimate[1] + betas$estimate[5]*0) # average high flow
exp(b2)/(1+exp(b2)) 

b3<-(betas$estimate[1] + betas$estimate[5]*1) # high high flow
exp(b3)/(1+exp(b3)) 



# low flow effect on survival
b4<-(betas$estimate[6] + betas$estimate[10]*-1) # low low flow, ie bad drought
exp(b4)/(1+exp(b4))

b5<-(betas$estimate[6] + betas$estimate[10]*0) # average low flow
exp(b5)/(1+exp(b5)) 

b6<-(betas$estimate[6] + betas$estimate[10]*1) # high low flow
exp(b6)/(1+exp(b6))



## ok let's automate this process in a df 
flood.values<-full.prediction.flood.phi %>%
  dplyr::select(covdata,Q99)%>%
  rename(Q99_scale=covdata)%>%
  mutate(mean_lambda = 
           (exp(betas$estimate[1] + betas$estimate[5]*Q99_scale) / 
              ( 1 + exp(betas$estimate[1] + betas$estimate[5]*Q99_scale))) +  # phi
           ((exp(betas$estimate[6]) / (1 + exp(betas$estimate[6])))) )%>% # f
  
  
  # do delta method by hand
  mutate(x=(exp(b1 + b2*1) / ( 1 + exp(b1 + b2*1))) -  
           ((exp(b1 + b2*1)^2) / ( 1 + exp(b1 + b2*1))^2))%>% 
  
  mutate(y=(1*(exp(b1 + b2*Q99_scale)) / ( 1 + exp(b1 + b2*Q99_scale))) -  
           ((1*(exp(b1 + b2*Q99_scale)^2)) / ( 1 + exp(b1 + b2*Q99_scale))^2))%>%
  
  mutate(z=(exp(b3) / ( 1 + exp(b3))) -  ((exp(b3)^2) / ( 1 + exp(b3))^2))

# covariance estimates for b1, b2, b3
top$results$beta.vcv
# want a matrix with rows and columns that read phi intercept, phi flood, and f intercept
# so vcv matrix will be these combos from the original
# row 1 = [1,1],[1,5],[1,6]
# row 2 = [5,1],[5,5],[5,6]
# row 5 = [10,1],[10,5],[10,6]

# need variance/covariance matrix to multiply by beta vector 2x to get se estimate of derived parameter lambda
variance.covariance.matrix<-matrix(c(top$results$beta.vcv[1,1],top$results$beta.vcv[1,5],top$results$beta.vcv[1,6],
                                     top$results$beta.vcv[5,1],top$results$beta.vcv[5,5],top$results$beta.vcv[5,6],
                                     top$results$beta.vcv[10,1],top$results$beta.vcv[10,5],top$results$beta.vcv[10,6]),nrow=3,ncol=3) 
class(variance.covariance.matrix)
variance.covariance.matrix

# switching to a function to start multiplying vectors and matrices
se_fnct<- function(x,y,z, variance.covariance.matrix){
  DDt<-c(x,y,z)
  v<-DDt*variance.covariance.matrix*DDt
  se1<-sum(v)
  se<-sqrt(se1)
  
  return(se)
}

se_1<-se_fnct(flood.values[1,4], flood.values[1,5], flood.values[1,6], variance.covariance.matrix)
se_50<-se_fnct(flood.values[50,4], flood.values[50,5], flood.values[50,6], variance.covariance.matrix)

se_100<-se_fnct(flood.values[100,4], flood.values[100,5], flood.values[100,6], variance.covariance.matrix)
se_150<-se_fnct(flood.values[150,4], flood.values[150,5], flood.values[150,6], variance.covariance.matrix)

se_200<-se_fnct(flood.values[200,4], flood.values[200,5], flood.values[200,6], variance.covariance.matrix)
se_250<-se_fnct(flood.values[250,4], flood.values[250,5], flood.values[250,6], variance.covariance.matrix)

se_300<-se_fnct(flood.values[300,4], flood.values[300,5], flood.values[300,6], variance.covariance.matrix)
se_350<-se_fnct(flood.values[350,4], flood.values[350,5], flood.values[350,6], variance.covariance.matrix)

se_400<-se_fnct(flood.values[400,4], flood.values[400,5], flood.values[400,6], variance.covariance.matrix)
se_450<-se_fnct(flood.values[450,4], flood.values[450,5], flood.values[450,6], variance.covariance.matrix)

se_500<-se_fnct(flood.values[500,4], flood.values[500,5], flood.values[500,6], variance.covariance.matrix)
se_550<-se_fnct(flood.values[550,4], flood.values[550,5], flood.values[550,6], variance.covariance.matrix)

se_600<-se_fnct(flood.values[600,4], flood.values[600,5], flood.values[600,6], variance.covariance.matrix)
se_650<-se_fnct(flood.values[650,4], flood.values[650,5], flood.values[650,6], variance.covariance.matrix)

se_700<-se_fnct(flood.values[700,4], flood.values[700,5], flood.values[700,6], variance.covariance.matrix)
se_750<-se_fnct(flood.values[750,4], flood.values[750,5], flood.values[750,6], variance.covariance.matrix)

se_800<-se_fnct(flood.values[800,4], flood.values[800,5], flood.values[800,6], variance.covariance.matrix)
se_850<-se_fnct(flood.values[850,4], flood.values[850,5], flood.values[850,6], variance.covariance.matrix)

se_900<-se_fnct(flood.values[900,4], flood.values[900,5], flood.values[900,6], variance.covariance.matrix)
se_950<-se_fnct(flood.values[950,4], flood.values[950,5], flood.values[950,6], variance.covariance.matrix)

se_1000<-se_fnct(flood.values[1000,4], flood.values[1000,5], flood.values[1000,6], variance.covariance.matrix)

seers<-c(se_1,se_50,se_100,se_150,se_200, se_250,se_500,se_550,se_400, se_450,se_500, 
         se_550,se_600,se_650,se_700, se_750,se_800,se_850,se_900, se_950,se_1000)
Q99_scalers<-c(flood.values[1,1], flood.values[50,1], flood.values[100,1], flood.values[150,1], flood.values[200,1],
               flood.values[250,1], flood.values[300,1], flood.values[350,1], flood.values[400,1], flood.values[450,1],
               flood.values[500,1], flood.values[550,1], flood.values[600,1], flood.values[650,1], flood.values[700,1],
               flood.values[750,1], flood.values[800,1], flood.values[850,1], flood.values[900,1], flood.values[950,1],
               flood.values[1000,1])
Q99_real<-c(flood.values[1,2], flood.values[50,2], flood.values[100,2], flood.values[150,2], flood.values[200,2],
            flood.values[250,2], flood.values[300,2], flood.values[350,2], flood.values[400,2], flood.values[450,2],
            flood.values[500,2], flood.values[550,2], flood.values[600,2], flood.values[650,2], flood.values[700,2],
            flood.values[750,2], flood.values[800,2], flood.values[850,2], flood.values[900,2], flood.values[950,2],
            flood.values[1000,2])
meanLambda<-c(flood.values[1,3], flood.values[50,3], flood.values[100,3], flood.values[150,3], flood.values[200,3],
              flood.values[250,3], flood.values[300,3], flood.values[350,3], flood.values[400,3], flood.values[450,3],
              flood.values[500,3], flood.values[550,3], flood.values[600,3], flood.values[650,3], flood.values[700,3],
              flood.values[750,3], flood.values[800,3], flood.values[850,3], flood.values[900,3], flood.values[950,3],
              flood.values[1000,3])
lambda_estimates<-data.frame(meanLambda,seers,Q99_scalers,Q99_real)


lambda_estimates_bear <- lambda_estimates %>%
  mutate(lambda_ucl = meanLambda+1.96*seers)%>%
  mutate(lambda_lcl = meanLambda+-1.96*seers)%>%
  mutate(Stream = "Bear")



# repeat for other streams - zigzag now
flood.values<-full.prediction.flood.phi %>%
  dplyr::select(covdata,Q99)%>%
  rename(Q99_scale=covdata)%>%
  mutate(mean_lambda = 
           (exp(betas$estimate[1] + betas$estimate[3]*1 + betas$estimate[5]*Q99_scale) / 
              ( 1 + exp(betas$estimate[1] + betas$estimate[3]*1 + betas$estimate[5]*Q99_scale))) +  # phi
           ((exp(betas$estimate[6] + betas$estimate[8]*1) / (1 + exp(betas$estimate[6] + betas$estimate[8]*1)))) )%>% # f 
  
  # do delta method by hand
  mutate(x=(exp(b1 + b2*1) / ( 1 + exp(b1 + b2*1))) -  
           ((exp(b1 + b2*1)^2) / ( 1 + exp(b1 + b2*1))^2))%>% 
  
  mutate(y=(1*(exp(b1 + b2*Q99_scale)) / ( 1 + exp(b1 + b2*Q99_scale))) -  
           ((1*(exp(b1 + b2*Q99_scale)^2)) / ( 1 + exp(b1 + b2*Q99_scale))^2))%>%
  
  mutate(z=(exp(b3) / ( 1 + exp(b3))) -  ((exp(b3)^2) / ( 1 + exp(b3))^2))

meanLambda<-c(flood.values[1,3], flood.values[50,3], flood.values[100,3], flood.values[150,3], flood.values[200,3],
              flood.values[250,3], flood.values[300,3], flood.values[350,3], flood.values[400,3], flood.values[450,3],
              flood.values[500,3], flood.values[550,3], flood.values[600,3], flood.values[650,3], flood.values[700,3],
              flood.values[750,3], flood.values[800,3], flood.values[850,3], flood.values[900,3], flood.values[950,3],
              flood.values[1000,3])
lambda_estimates<-data.frame(meanLambda,seers,Q99_scalers,Q99_real)


lambda_estimates_zz <- lambda_estimates %>%
  mutate(lambda_ucl = meanLambda+1.96*seers)%>%
  mutate(lambda_lcl = meanLambda+-1.96*seers)%>%
  mutate(Stream = "Zigzag")




# paradise now 
flood.values<-full.prediction.flood.phi %>%
  dplyr::select(covdata,Q99)%>%
  rename(Q99_scale=covdata)%>%
  mutate(mean_lambda = 
           (exp(betas$estimate[1] + betas$estimate[2]*1 + betas$estimate[5]*Q99_scale) / 
              ( 1 + exp(betas$estimate[1] + betas$estimate[2]*1 + betas$estimate[5]*Q99_scale))) +  # phi
           ((exp(betas$estimate[6] + betas$estimate[7]*1) / (1 + exp(betas$estimate[6] + betas$estimate[7]*1)))) )%>% # f 
  
  # do delta method by hand
  mutate(x=(exp(b1 + b2*1) / ( 1 + exp(b1 + b2*1))) -  
           ((exp(b1 + b2*1)^2) / ( 1 + exp(b1 + b2*1))^2))%>% 
  
  mutate(y=(1*(exp(b1 + b2*Q99_scale)) / ( 1 + exp(b1 + b2*Q99_scale))) -  
           ((1*(exp(b1 + b2*Q99_scale)^2)) / ( 1 + exp(b1 + b2*Q99_scale))^2))%>%
  
  mutate(z=(exp(b3) / ( 1 + exp(b3))) -  ((exp(b3)^2) / ( 1 + exp(b3))^2))

meanLambda<-c(flood.values[1,3], flood.values[50,3], flood.values[100,3], flood.values[150,3], flood.values[200,3],
              flood.values[250,3], flood.values[300,3], flood.values[350,3], flood.values[400,3], flood.values[450,3],
              flood.values[500,3], flood.values[550,3], flood.values[600,3], flood.values[650,3], flood.values[700,3],
              flood.values[750,3], flood.values[800,3], flood.values[850,3], flood.values[900,3], flood.values[950,3],
              flood.values[1000,3])
lambda_estimates<-data.frame(meanLambda,seers,Q99_scalers,Q99_real)


lambda_estimates_par <- lambda_estimates %>%
  mutate(lambda_ucl = meanLambda+1.96*seers)%>%
  mutate(lambda_lcl = meanLambda+-1.96*seers)%>%
  mutate(Stream = "Paradise")

# combine 
lambda_estimates<-full_join(lambda_estimates_bear, lambda_estimates_par)
lambda_estimates<-full_join(lambda_estimates, lambda_estimates_zz)





### calculating the effect of drought on lambda
# covariance estimates for b1, b3, b4
top$results$beta.vcv
# want a matrix with rows and columns that read phi intercept, f intercept, f drought
# so vcv matrix will be these combos from the original
# row 1 = [1,1],[1,6],[1,10]
# row 2 = [5,1],[5,6],[5,10]
# row 5 = [10,1],[10,6],[10,10]

# need variance/covariance matrix to multiply by beta vector 2x to get se estimate of derived parameter lambda
variance.covariance.matrix.d<-matrix(c(top$results$beta.vcv[1,1],top$results$beta.vcv[1,6],top$results$beta.vcv[1,10],
                                       top$results$beta.vcv[5,1],top$results$beta.vcv[5,6],top$results$beta.vcv[5,10],
                                       top$results$beta.vcv[10,1],top$results$beta.vcv[10,6],top$results$beta.vcv[10,10]),nrow=3,ncol=3) 
class(variance.covariance.matrix.d)
variance.covariance.matrix.d

drought.values<-full.prediction.drought.f %>%
  dplyr::select(covdata,Q1)%>%
  rename(Q1_scale=covdata)%>%
  mutate(mean_lambda = 
           (exp(betas$estimate[6] + betas$estimate[10]*Q1_scale) / ( 1 + exp(betas$estimate[6] + 
                                                                               betas$estimate[10]*Q1_scale))) +  # f
           ((exp(betas$estimate[1]) / (1 + exp(betas$estimate[1])))) )%>% # phi
  
  # do delta method by hand
  mutate(x=(exp(b1) / ( 1 + exp(b1 ))) -  
           ((exp(b1 )^2) / ( 1 + exp(b1))^2))%>% 
  
  mutate(y=(exp(b3 + b4*1) / ( 1 + exp(b3 + b4*1))) -  
           ((exp(b3 + b4*1)^2) / ( 1 + exp(b3 + b4*1))^2))%>% 
  
  mutate(z=(1*(exp(b3 + b4*Q1_scale)) / ( 1 + exp(b3 + b4*Q1_scale))) -  
           ((1*(exp(b3 + b4*Q1_scale)^2)) / ( 1 + exp(b3 + b4*Q1_scale))^2))

variance.covariance.matrix.d


# switching to a function to start multiplying vectors and matrices
sed_1<-se_fnct(drought.values[1,4], drought.values[1,5], drought.values[1,6], variance.covariance.matrix.d)
sed_50<-se_fnct(drought.values[50,4], drought.values[50,5], drought.values[50,6], variance.covariance.matrix.d)

sed_100<-se_fnct(drought.values[100,4], drought.values[100,5], drought.values[100,6], variance.covariance.matrix.d)
sed_150<-se_fnct(drought.values[150,4], drought.values[150,5], drought.values[150,6], variance.covariance.matrix.d)

sed_200<-se_fnct(drought.values[200,4], drought.values[200,5], drought.values[200,6], variance.covariance.matrix.d)
sed_250<-se_fnct(drought.values[250,4], drought.values[250,5], drought.values[250,6], variance.covariance.matrix.d)

sed_300<-se_fnct(drought.values[300,4], drought.values[300,5], drought.values[300,6], variance.covariance.matrix.d)
sed_350<-se_fnct(drought.values[350,4], drought.values[350,5], drought.values[350,6], variance.covariance.matrix.d)

sed_400<-se_fnct(drought.values[400,4], drought.values[400,5], drought.values[400,6], variance.covariance.matrix.d)
sed_450<-se_fnct(drought.values[450,4], drought.values[450,5], drought.values[450,6], variance.covariance.matrix.d)

sed_500<-se_fnct(drought.values[500,4], drought.values[500,5], drought.values[500,6], variance.covariance.matrix.d)
sed_550<-se_fnct(drought.values[550,4], drought.values[550,5], drought.values[550,6], variance.covariance.matrix.d)

sed_600<-se_fnct(drought.values[600,4], drought.values[600,5], drought.values[600,6], variance.covariance.matrix.d)
sed_650<-se_fnct(drought.values[650,4], drought.values[650,5], drought.values[650,6], variance.covariance.matrix.d)

sed_700<-se_fnct(drought.values[700,4], drought.values[700,5], drought.values[700,6], variance.covariance.matrix.d)
sed_750<-se_fnct(drought.values[750,4], drought.values[750,5], drought.values[750,6], variance.covariance.matrix.d)

sed_800<-se_fnct(drought.values[800,4], drought.values[800,5], drought.values[800,6], variance.covariance.matrix.d)
sed_850<-se_fnct(drought.values[850,4], drought.values[850,5], drought.values[850,6], variance.covariance.matrix.d)

sed_900<-se_fnct(drought.values[900,4], drought.values[900,5], drought.values[900,6], variance.covariance.matrix.d)
sed_950<-se_fnct(drought.values[950,4], drought.values[950,5], drought.values[950,6], variance.covariance.matrix.d)

sed_1000<-se_fnct(drought.values[1000,4], drought.values[1000,5], drought.values[1000,6], variance.covariance.matrix.d)

seders<-c(sed_1,sed_50,sed_100,sed_150,sed_200, sed_250,sed_500,sed_550,sed_400, sed_450,sed_500, 
          sed_550,sed_600,sed_650,sed_700, sed_750,sed_800,sed_850,sed_900, sed_950,sed_1000)
Q1_scalers<-c(drought.values[1,1], drought.values[50,1], drought.values[100,1], drought.values[150,1], drought.values[200,1],
              drought.values[250,1], drought.values[300,1], drought.values[350,1], drought.values[400,1], drought.values[450,1],
              drought.values[500,1], drought.values[550,1], drought.values[600,1], drought.values[650,1], drought.values[700,1],
              drought.values[750,1], drought.values[800,1], drought.values[850,1], drought.values[900,1], drought.values[950,1],
              drought.values[1000,1])
Q1_real<-c(drought.values[1,2], drought.values[50,2], drought.values[100,2], drought.values[150,2], drought.values[200,2],
           drought.values[250,2], drought.values[300,2], drought.values[350,2], drought.values[400,2], drought.values[450,2],
           drought.values[500,2], drought.values[550,2], drought.values[600,2], drought.values[650,2], drought.values[700,2],
           drought.values[750,2], drought.values[800,2], drought.values[850,2], drought.values[900,2], drought.values[950,2],
           drought.values[1000,2])
meanLambda<-c(drought.values[1,3], drought.values[50,3], drought.values[100,3], drought.values[150,3], drought.values[200,3],
              drought.values[250,3], drought.values[300,3], drought.values[350,3], drought.values[400,3], drought.values[450,3],
              drought.values[500,3], drought.values[550,3], drought.values[600,3], drought.values[650,3], drought.values[700,3],
              drought.values[750,3], drought.values[800,3], drought.values[850,3], drought.values[900,3], drought.values[950,3],
              drought.values[1000,3])
lambda_estimates_d<-data.frame(meanLambda,seders,Q1_scalers,Q1_real)


lambda_estimates_d_bear <- lambda_estimates_d %>%
  mutate(lambda_ucl = meanLambda+1.96*seders)%>%
  mutate(lambda_lcl = meanLambda+-1.96*seders)%>%
  mutate(Stream = "Bear")


### paradise now
drought.values<-full.prediction.drought.f %>%
  dplyr::select(covdata,Q1)%>%
  rename(Q1_scale=covdata)%>%
  mutate(mean_lambda = 
           (exp(betas$estimate[6] + betas$estimate[7]*1 + betas$estimate[10]*Q1_scale) / 
              ( 1 + exp(betas$estimate[6] + betas$estimate[7]*1 + betas$estimate[10]*Q1_scale))) +  # f
           ((exp(betas$estimate[1] +betas$estimate[2]*1) / (1 + exp(betas$estimate[1]+betas$estimate[2]*1)))) )%>% # phi
  
  # do delta method by hand
  mutate(x=(exp(b1) / ( 1 + exp(b1 ))) -  
           ((exp(b1 )^2) / ( 1 + exp(b1))^2))%>% 
  
  mutate(y=(exp(b3 + b4*1) / ( 1 + exp(b3 + b4*1))) -  
           ((exp(b3 + b4*1)^2) / ( 1 + exp(b3 + b4*1))^2))%>% 
  
  mutate(z=(1*(exp(b3 + b4*Q1_scale)) / ( 1 + exp(b3 + b4*Q1_scale))) -  
           ((1*(exp(b3 + b4*Q1_scale)^2)) / ( 1 + exp(b3 + b4*Q1_scale))^2))


meanLambda<-c(drought.values[1,3], drought.values[50,3], drought.values[100,3], drought.values[150,3], drought.values[200,3],
              drought.values[250,3], drought.values[300,3], drought.values[350,3], drought.values[400,3], drought.values[450,3],
              drought.values[500,3], drought.values[550,3], drought.values[600,3], drought.values[650,3], drought.values[700,3],
              drought.values[750,3], drought.values[800,3], drought.values[850,3], drought.values[900,3], drought.values[950,3],
              drought.values[1000,3])
lambda_estimates_d<-data.frame(meanLambda,seders,Q1_scalers,Q1_real)


lambda_estimates_d_par <- lambda_estimates_d %>%
  mutate(lambda_ucl = meanLambda+1.96*seders)%>%
  mutate(lambda_lcl = meanLambda+-1.96*seders)%>%
  mutate(Stream = "Paradise")


### zz now
drought.values<-full.prediction.drought.f %>%
  dplyr::select(covdata,Q1)%>%
  rename(Q1_scale=covdata)%>%
  mutate(mean_lambda = 
           (exp(betas$estimate[6] + betas$estimate[8]*1 + betas$estimate[10]*Q1_scale) / 
              ( 1 + exp(betas$estimate[6] + betas$estimate[8]*1 + betas$estimate[10]*Q1_scale))) +  # f
           ((exp(betas$estimate[1] +betas$estimate[3]*1) / (1 + exp(betas$estimate[1]+betas$estimate[3]*1)))) )%>% # phi
  
  # do delta method by hand
  mutate(x=(exp(b1) / ( 1 + exp(b1 ))) -  
           ((exp(b1 )^2) / ( 1 + exp(b1))^2))%>% 
  
  mutate(y=(exp(b3 + b4*1) / ( 1 + exp(b3 + b4*1))) -  
           ((exp(b3 + b4*1)^2) / ( 1 + exp(b3 + b4*1))^2))%>% 
  
  mutate(z=(1*(exp(b3 + b4*Q1_scale)) / ( 1 + exp(b3 + b4*Q1_scale))) -  
           ((1*(exp(b3 + b4*Q1_scale)^2)) / ( 1 + exp(b3 + b4*Q1_scale))^2))


meanLambda<-c(drought.values[1,3], drought.values[50,3], drought.values[100,3], drought.values[150,3], drought.values[200,3],
              drought.values[250,3], drought.values[300,3], drought.values[350,3], drought.values[400,3], drought.values[450,3],
              drought.values[500,3], drought.values[550,3], drought.values[600,3], drought.values[650,3], drought.values[700,3],
              drought.values[750,3], drought.values[800,3], drought.values[850,3], drought.values[900,3], drought.values[950,3],
              drought.values[1000,3])
lambda_estimates_d<-data.frame(meanLambda,seders,Q1_scalers,Q1_real)


lambda_estimates_d_zz <- lambda_estimates_d %>%
  mutate(lambda_ucl = meanLambda+1.96*seders)%>%
  mutate(lambda_lcl = meanLambda+-1.96*seders)%>%
  mutate(Stream = "Zigzag")

# combine 
lambda_estimates_d<-full_join(lambda_estimates_d_bear, lambda_estimates_d_par)
lambda_estimates_d<-full_join(lambda_estimates_d, lambda_estimates_d_zz)







##### adding data to figures 
# mean transition probability
pradel.result<-data.frame(top$results$real)

parameter<-c(rep("Survival",21),rep("LarvalRecruit",21),rep("Capture",6),rep("PopSize",8))
pradel.result$parameter<-parameter

result.larvalrecruit <- pradel.result %>%
  filter(parameter == "LarvalRecruit")

stream <- c(rep("Bear",7),rep("Paradise",7), rep("Zigzag",7))
surv_yr <-c(1,2,3,4,7,8,9,1,2,3,4,7,8,9,1,2,3,4,7,8,9  )

result.larvalrecruit$stream <- stream
result.larvalrecruit$surv_yr <- surv_yr

result.larvalrecruit <- result.larvalrecruit%>%
  mutate(WS = case_when(stream=="Bear" ~ 6,
                        stream=="Paradise" ~ 3,
                        stream=="Zigzag" ~ 7))

result.larvalrecruit.wq1<-left_join(result.larvalrecruit,yrs_Q1)




# plot effect of low and high flow on larval recruitment with data

# color blind palette
cbp2 <- c("#000000", "#E69F00", "#56B4E9")


lowflow.f.wdat <-ggplot(full.prediction.drought.f, aes(x = Q1, y = estimate,color=Stream)) +
  
  geom_ribbon(aes(ymin = lcl, ymax = ucl,fill=Stream), alpha = 0.15, colour = NA) +
  geom_point(data=result.larvalrecruit.wq1, aes(x=Q1, y=estimate,color=stream),size=4.5,alpha=0.7)+
  geom_errorbar(data=result.larvalrecruit.wq1, aes(x=Q1, ymin=lcl, ymax=ucl,color=stream),alpha=0.7)+
  geom_line(size = 2,alpha=0.8) +
  
  xlab(bquote('Lowest Discharge '('mm day' ^-1)))+
  ylab(expression(paste("Larval Recruitment  " , ( italic(f)))))+
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_blank(),
        legend.position = c(.3,.9),
        legend.background=element_blank(),# making legend fill go away
        plot.margin = margin(1,1,1,1, "cm"))+
  scale_x_continuous(limits=c(-0.009,0.2),expand=c(0,0)) +
  guides(x = "prism_minor",y="prism_minor") +
  scale_colour_manual(values=cbp2)+
  scale_fill_manual(values=cbp2)

lowflow.f.wdat






results4 <- pradel.result %>%
  filter(parameter == "Survival")

stream <- c(rep("Bear",7),rep("Paradise",7), rep("Zigzag",7))
surv_yr <-c(1,2,3,4,7,8,9, 1,2,3,4,7,8,9, 1,2,3,4,7,8,9)

results4$stream <- stream
results4$surv_yr <- surv_yr

results4 <- results4%>%
  mutate(WS = case_when(stream=="Bear" ~ 6,
                        stream=="Paradise" ~ 3,
                        stream=="Zigzag" ~ 7))


results5<-left_join(results4,yrs_Q99)
results5<-left_join(results5,yrs_Q1)


# need to combine f and phi results to create actual data points
result.larvalrecruit.wq1<-left_join(result.larvalrecruit.wq1,yrs_Q99)

# this is recruitment results
recruit.dat <- result.larvalrecruit.wq1 %>%
  dplyr::select(estimate,se,lcl,ucl,Q1,Q1_scale,surv_yr,stream )%>% 
  rename(recruitment = estimate)%>%
  rename(recruitment.lcl = lcl)%>%
  rename(recruitment.ucl = ucl)

# results5 is survival (only flooding is significant )
survival.dat <- results5 %>%
  dplyr::select(estimate,se,lcl,ucl,Q99,Q99_scale,surv_yr,stream )%>%
  rename(survival = estimate)%>%
  rename(survival.lcl = lcl)%>%
  rename(survival.ucl = ucl)


lambda.dat<-left_join(recruit.dat,survival.dat,by=c("surv_yr","stream"))
head(lambda.dat)
lambda.dat <- lambda.dat %>%
  mutate(lambda = recruitment + survival)%>%
  mutate(lambda.lcl = recruitment.lcl + survival.lcl)%>% # won't be using for data points
  mutate(lambda.ucl = recruitment.ucl + survival.ucl)# won't be using for data points





### plotting lambda with data (high flow first)
### actual plot with high flow and lambda for paper
highflow.lambda<-
  ggplot(lambda_estimates, aes(x = Q99_real, y = meanLambda,color=Stream)) +
  
  geom_ribbon(aes(ymin = lambda_lcl, ymax = lambda_ucl,fill=Stream), alpha = 0.15,colour=NA) +
  geom_point(data=lambda.dat, aes(x=Q99, y=lambda,color=stream), size=4.5, alpha=0.7)+
  geom_errorbar(data=lambda.dat, aes(x=Q99, ymin=lambda.lcl, ymax=lambda.ucl,color=stream))+
  geom_line(size = 2, alpha=0.8) +
  
  xlab(bquote('Peak Discharge '('mm day' ^-1))) + 
  ylab(expression(paste("Population Growth  ", (lambda)))) +
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_blank(),
        legend.position = c(.8,.8),
        legend.background =element_blank(),
        plot.margin =margin(1,1,1,1,"cm"))+
  scale_x_continuous(limits=c(30,100), expand = c(0, 0)) + 
  guides(x = "prism_minor",y="prism_minor")+
  scale_fill_grey(start = 0.6, end = 0.6) +
  scale_colour_manual(values=cbp2)+
  scale_fill_manual(values=cbp2)
highflow.lambda





lowflow.lambda<-
  ggplot(lambda_estimates_d, aes(x = Q1_real, y = meanLambda,color=Stream)) +
  geom_ribbon(aes(ymin = lambda_lcl, ymax = lambda_ucl,fill=Stream), alpha = 0.15,colour=NA) +
  geom_point(data=lambda.dat, aes(x=Q1, y=lambda,color=stream), size=4.5, alpha=0.7)+
  geom_errorbar(data=lambda.dat, aes(x=Q1, ymin=lambda.lcl, ymax=lambda.ucl,color=stream))+
  geom_line(size = 2, alpha=0.8) +
  xlab(bquote('Lowest Discharge '('mm day' ^-1))) + 
  ylab(expression(paste("Population Growth  ", (lambda)))) +
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_blank(),
        legend.position = "none",
        plot.margin =margin(1,1,1,1,"cm"))+
  scale_x_continuous(limits=c(-0.004,0.2), expand = c(0, 0)) + 
  
  guides(x = "prism_minor",y="prism_minor")+
  scale_colour_manual(values=cbp2)+
  scale_fill_manual(values=cbp2)
lowflow.lambda



duplot<-cowplot::plot_grid(highflow.lambda +
                             theme(),
                           
                           lowflow.lambda +
                             theme(axis.title.y = element_blank()),
                           
                           nrow = 1, labels = "auto", align = "v")
duplot




### effect size of differences in lambda for peak and low discharge

# (start (low value)-final value / |start value| )* 100
minLambda.q1<-min(lambda_estimates_d_bear$meanLambda)
minLambda.q1 # at worst drought
maxLambda.q1<-max(lambda_estimates_d_bear$meanLambda)
maxLambda.q1 #  at "best" drought

((minLambda.q1-maxLambda.q1)/abs(minLambda.q1))*100 # 

# confidence intervals 
minLambda.q1.lcl<-min(lambda_estimates_d_bear$lambda_lcl )
maxLambda.q1.lcl<-max(lambda_estimates_d_bear$lambda_lcl )

((minLambda.q1.lcl-maxLambda.q1.lcl)/abs(minLambda.q1.lcl))*100 # 

minLambda.q1.ucl<-min(lambda_estimates_d_bear$lambda_ucl )
maxLambda.q1.ucl<-max(lambda_estimates_d_bear$lambda_ucl )

((minLambda.q1.ucl-maxLambda.q1.ucl)/abs(minLambda.q1.ucl))*100 #



# flood effect on lambda
# (start (low value)-final value / |start value| )* 100
head(lambda_estimates_bear)
minLambda.q99<-min(lambda_estimates_bear$meanLambda)
minLambda.q99 #  at worst flood
maxLambda.q99<-max(lambda_estimates_bear$meanLambda)
maxLambda.q99 #  at "best" flood

((minLambda.q99-maxLambda.q99)/abs(minLambda.q99))*100 # 

# confidence intervals 
minLambda.q99.lcl<-min(lambda_estimates_bear$lambda_lcl)
maxLambda.q99.lcl<-max(lambda_estimates_bear$lambda_lcl)

((minLambda.q99.lcl-maxLambda.q99.lcl)/abs(minLambda.q99.lcl))*100 # 

minLambda.q99.ucl<-min(lambda_estimates_bear$lambda_ucl)
maxLambda.q99.ucl<-max(lambda_estimates_bear$lambda_ucl)

((minLambda.q99.ucl-maxLambda.q99.ucl)/abs(minLambda.q99.ucl))*100 # 


### goodness of fit
release.gof(sal.process)


# c^  - needs to be time dependent though
16.7103/43 # 0.389
# A value of chat = 1 was assumed when the calculated chat was<1





#### create figure to look at lambda over time 
sal.ddl$Phi
# bear0 by year then paradise0 by year then zigzag0 by year, then bear1 by year, etc

top$results$derived$`Lambda Population Change` # 42 rows

lambda<-top$results$derived$`Lambda Population Change`$estimate
lcl<-top$results$derived$`Lambda Population Change`$lcl
ucl<-top$results$derived$`Lambda Population Change`$ucl
Stream <- c(rep("Bear",7),rep("Paradise",7), rep("Zigzag",7),
            rep("Bear",7),rep("Paradise",7), rep("Zigzag",7))
surv_yr <-c(1,2,3,4,7,8,9, 1,2,3,4,7,8,9, 1,2,3,4,7,8,9,
            1,2,3,4,7,8,9, 1,2,3,4,7,8,9, 1,2,3,4,7,8,9)
year<-(c(13,14,15,18,19,20,21, 13,14,15,18,19,20,21, 13,14,15,18,19,20,21,
         13,14,15,18,19,20,21, 13,14,15,18,19,20,21, 13,14,15,18,19,20,21))
reach<- c(rep("Upper",21),rep("Lower",21))

lam.results<-data.frame(lambda,lcl,ucl,Stream,reach,surv_yr,year)
lam.results<-lam.results[1:21,]

lambda.time<-ggplot(data=lam.results, aes(x=year,y=lambda))+
  geom_pointrange(mapping = aes(x=year,y=lambda,ymin=lcl,ymax=ucl,color=Stream),
                  position = position_dodge(width = 0.4),size=1.5,alpha=0.7)+
  scale_x_continuous(breaks=seq(13,21,1))+
  xlab("Year") + 
  ylab(expression(paste("Population Growth  ", (lambda)))) +
  scale_colour_manual(values=cbp2)+
  theme_classic()+
  theme(legend.position = "right")+
  geom_hline(yintercept=1.0, linetype="dotted", color="grey", size=0.5)
lambda.time



#### Theil-Sens slopes ####
bear.lam.results<-lam.results%>%filter(Stream=="Bear")
par.lam.results<-lam.results%>%filter(Stream=="Paradise")
zz.lam.results<-lam.results%>%filter(Stream=="Zigzag")



fit.bear<-mblm(lambda~year, dataframe = bear.lam.results, repeated = FALSE)
summary(fit.bear) 

fit.par<-mblm(lambda~year, dataframe = par.lam.results, repeated = FALSE)
summary(fit.par)

fit.zz<-mblm(lambda~year, dataframe = zz.lam.results, repeated = FALSE)
summary(fit.zz)




lam.neg<-ggplot(data=lam.results, aes(x=year,y=lambda))+
  geom_pointrange(mapping = aes(x=year,y=lambda,ymin=lcl,ymax=ucl,color=Stream),
                  position = position_dodge(width = 0.4),size=1.5,alpha=0.7)+
  scale_x_continuous(breaks=seq(13,21,1))+
  xlab("Year") + 
  ylab(expression(paste("Population Growth  ", (lambda)))) +
  scale_colour_manual(values=cbp2)+
  theme_classic()+
  theme(legend.position = "right")+
  geom_hline(yintercept=1.0, linetype="dotted", color="grey", size=0.5)+
  geom_abline(intercept = coef(fit.zz)[1], slope = coef(fit.zz)[2], color="#56B4E9",alpha=0.8)+
  geom_abline(intercept = coef(fit.par)[1], slope = coef(fit.zz)[2], color="#E69F00",alpha=0.8)+
  geom_abline(intercept = coef(fit.bear)[1], slope = coef(fit.zz)[2], color="#000000",alpha=0.8)
lam.neg  













#####################################################################################

# robust design multi-stage CJS model 

#####################################################################################


sal.dat<-read.csv("HB_Salamander_robustCJS_edit.csv",
                  header=TRUE) # this is a secondary mark-recapture dataset 
# 7 individuals (see below) had stages changed so as to match the model assumptions (no stage changes w/in secondary ocassions)

sal.dat2 <- sal.dat %>%
  filter(Species == "GP") %>% # only include G. porphyriticus
  mutate(Stage = case_when(Stage == "M" ~ "L",# we want only two life stages (metamorphs go to larvae)
                           Stage == "L" ~ "L",
                           Stage == "A" ~ "A"))%>%
  filter(Remove == "N")%>% # remove individuals marked to remove previously
  filter(Stream != "Canyon")%>%
  filter(Stream != "Cascade")%>%
  mutate(LocRound = case_when(RawLongLoc < 100 ~ 50,
                              RawLongLoc < 200 ~ 150,
                              RawLongLoc < 300 ~ 250,
                              RawLongLoc < 400 ~ 350,
                              RawLongLoc <= 500 ~ 450))%>%
  mutate(Site=paste0(Reach,"",Stream,"",LocRound))%>%
  filter(Primary != 1)%>% # excluding first 3 surveys every summer 
  filter(Primary != 4)%>% 
  filter(Primary != 7)%>% 
  filter(Primary != 10)%>% 
  filter(Primary != 13)%>% 
  filter(Primary != 16)%>% 
  filter(Primary != 19)%>% 
  filter(Primary != 22)

# need to add SurvNum = 39 for lower bear, upper bear and lower zigzag (their wasn't a 6th survey done in 2018)
sal.dat.extra <- sal.dat %>%
  filter(Species == "GP") %>% # only include Gyro
  mutate(Stage = case_when(Stage == "M" ~ "L",
                           Stage == "L" ~ "L",
                           Stage == "A" ~ "A"))%>%
  mutate(Site=paste0(Reach,"",Stream,""))%>%
  filter(Site == "LowerZigzag" | Site == "LowerBear" |
           Site == "UpperBear")%>% 
  mutate(LocRound = case_when(RawLongLoc < 100 ~ 50,
                              RawLongLoc < 200 ~ 150,
                              RawLongLoc < 300 ~ 250,
                              RawLongLoc < 400 ~ 350,
                              RawLongLoc <= 500 ~ 450))%>%
  mutate(Site=paste0(Site,"",LocRound))%>%
  filter(SurNum == 39)%>%
  mutate(SurNum = case_when(SurNum == 39 ~ 42))%>% # change SurNum
  mutate(Primary = case_when(Primary == 13 ~ 14)) # change primary #

# combine two datasets
sal.dat2<-rbind(sal.dat2,sal.dat.extra)

sal.dat2 <- sal.dat2 %>%
  drop_na(SurNum)%>% # get rid of captures if not within a SurNum 
  drop_na(FinalID)%>% # get rid of all NAs in ID data
  dplyr::select(FinalID, Primary, SurNum,  Site, Stream, Reach, Stage) %>%
  mutate(Site = case_when(Site == "UpperCascade50" ~ "ucas50",
                          Site == "UpperCascade150" ~ "ucas150",
                          Site == "UpperCascade250" ~ "ucas250",
                          Site == "UpperCascade350" ~ "ucas350",
                          Site == "UpperCascade450" ~ "ucas450",
                          
                          Site == "UpperCanyon50" ~ "ucan50",
                          Site == "UpperCanyon150" ~ "ucan150",
                          Site == "UpperCanyon250" ~ "ucan250",
                          Site == "UpperCanyon350" ~ "ucan350",
                          Site == "UpperCanyon450" ~ "ucan450",
                          
                          Site == "UpperZigzag50" ~ "uzz50",
                          Site == "UpperZigzag150" ~ "uzz150",
                          Site == "UpperZigzag250" ~ "uzz250",
                          Site == "UpperZigzag350" ~ "uzz350",
                          Site == "UpperZigzag450" ~ "uzz450",
                          
                          Site == "UpperBear50" ~ "ub50",
                          Site == "UpperBear150" ~ "ub150",
                          Site == "UpperBear250" ~ "ub250",
                          Site == "UpperBear350" ~ "ub350",
                          Site == "UpperBear450" ~ "ub450",
                          
                          Site == "UpperParadise50" ~ "upar50",
                          Site == "UpperParadise150" ~ "upar150",
                          Site == "UpperParadise250" ~ "upar250",
                          Site == "UpperParadise350" ~ "upar350",
                          Site == "UpperParadise450" ~ "upar450",
                          
                          Site == "LowerCascade50" ~ "lcas50",
                          Site == "LowerCascade150" ~ "lcas150",
                          Site == "LowerCascade250" ~ "lcas250",
                          Site == "LowerCascade350" ~ "lcas350",
                          Site == "LowerCascade450" ~ "lcas450",
                          
                          Site == "LowerCanyon50" ~ "lcan50",
                          Site == "LowerCanyon150" ~ "lcan150",
                          Site == "LowerCanyon250" ~ "lcan250",
                          Site == "LowerCanyon350" ~ "lcan350",
                          Site == "LowerCanyon450" ~ "lcan450",
                          
                          Site == "LowerZigzag50" ~ "lzz50",
                          Site == "LowerZigzag150" ~ "lzz150",
                          Site == "LowerZigzag250" ~ "lzz250",
                          Site == "LowerZigzag350" ~ "lzz350",
                          Site == "LowerZigzag450" ~ "lzz450",
                          
                          Site == "LowerBear50" ~ "lb50",
                          Site == "LowerBear150" ~ "lb150",
                          Site == "LowerBear250" ~ "lb250",
                          Site == "LowerBear350" ~ "lb350",
                          Site == "LowerBear450" ~ "lb450",
                          
                          Site == "LowerParadise50" ~ "lpar50",
                          Site == "LowerParadise150" ~ "lpar150",
                          Site == "LowerParadise250" ~ "lpar250",
                          Site == "LowerParadise350" ~ "lpar350",
                          Site == "LowerParadise450" ~ "lpar450",
                          
                          Site == "Cushman" ~ "cushman",
                          Site == "Steep" ~ "steep",
                          Site == "WBZZ" ~ "wbzz",
                          Site == "Paradise" ~ "paradise",
                          Site == "Weir4" ~ "w4",
                          Site == "LowerCanyon" ~ "lowcanyon",
                          Site == "UpperCanyon" ~ "upcanyon",
                          Site == "Bagley" ~ "bagley"))%>%
  drop_na(Site)


# check to see if salamanders are changing stages within a primary session
chk<-sal.dat2 %>%
  group_by(FinalID)%>%
  arrange(SurNum)

# add capture number as id column
chk$cap_occ <- data.table::rowid(chk$FinalID)

chk <- chk %>%
  mutate(maxCaps = max(cap_occ))%>%
  filter(maxCaps > 1)%>%
  group_by(FinalID,Primary)%>%
  count(Stage)%>%
  ungroup()%>%
  mutate(lagID = dplyr::lag(FinalID))%>%
  mutate(lagStage = dplyr::lag(Stage))%>%
  mutate(lagPrimary = dplyr::lag(Primary))%>%
  mutate(chk = ifelse(FinalID == lagID,1,0))%>%
  filter(chk == 1)%>%
  mutate(chk2 = ifelse(lagStage == Stage,1,0))%>%
  filter(chk2 == 0)%>%
  mutate(chkfinal = ifelse(Primary == lagPrimary,1,0))%>%
  filter(chkfinal == 1)

head(chk) # s/b 0 now b/c individual stages have already been edited for these salamanaders:

# GP9002150000822424 Changing M to A

#  GPR3-6-7 Changing from M TO A 

# GPR1-2Y4 Changing from M TO A 

# GPG1-8P3 Changing from M TO A 

# GBR1-2-6 Changing to A

# GPO2-6B1 Changing from M TO A 

# GBY2-3R7 Changing from L TO A 




# join watershed area to capture data
sal.dat3<-left_join(sal.dat2, area, by=c("Site","Stream"))




# create encounter history 
cap.hist <- sal.dat3 %>%
  dplyr::select(FinalID, SurNum, Stream, Reach, Site, Area_km2, Fish, Stage)%>%
  mutate(scale_area = scale(Area_km2))%>%
  group_by(FinalID)%>%
  spread(SurNum,Stage,fill=0)


cap.his1<-cap.hist[,8:55] # remove id, all covariates to make inp format
covariates<-data.frame(cap.hist[,2:7])


# format encounter history for rmark
ch<-cap.his1 %>%
  mutate(ch = paste0(`4`,"",`5`,"",`6`,"",`7`,"",`8`,"",`9`,"",`13`,"",
                     `14`,"",`15`,"",`16`,"",`17`,"",`18`,"",`22`,"",`23`,
                     `24`,"",`25`,"",`26`,"",`27`,"",`31`,"",`32`,"",`33`,
                     `34`,"",`35`,"",`36`,"",`40`,"",`41`,"",`42`,"",`43`,
                     `44`,"",`45`,"",`49`,"",`50`,"",`51`,"",`52`,"",`53`,
                     `54`,"",`58`,"",`59`,"",`60`,"",`61`,"",`62`,"",`63`,
                     `67`,"",`68`,"",`69`,"",`70`,"",`71`,"",`72`)) %>%
  dplyr::select(ch)


# add back in co-variates to inp ch
capt.hist<-data.frame(ch,Area=covariates$scale_area, Fish=covariates$Fish, 
                      Stream=covariates$Stream)


# set up time intervals for 8 primary occasions each with 6 secondary occasions
# these values are the interval lengths between occasions
# e.g, 0 time between occ 1 - 5, 1 year between 5 & 6
time.intervals <- c(0, 0, 0 ,0 ,0 ,1,  # 2012
                    0, 0, 0 ,0 ,0 ,1,  # 2013
                    0, 0, 0 ,0 ,0 ,1,  # 2014
                    0, 0, 0 ,0 ,0 ,3,  # 2015
                    0, 0, 0 ,0 ,0 ,1,  # 2018
                    0, 0, 0 ,0 ,0 ,1,  # 2019
                    0, 0, 0 ,0 ,0 ,1,  # 2020
                    0, 0, 0 ,0 ,0)     # 2021

# PROCESS DATA FOR ROBUST DESIGN 
sal.process=process.data(capt.hist,
                         model="CRDMS", 
                         time.intervals=time.intervals,
                         groups=c("Stream","Fish")) # have to include factors
names(sal.process)
head(sal.process$data) # take a look at the data

# make design data
sal.ddl=make.design.data(sal.process)
sal.ddl
# check the number of primary occasions 
sal.process$nocc # s/b 8

# check the number of secondary occasions 
sal.process$nocc.secondary # s/b 6





#########################################################################################
#########################################################
# adding high and low flows to capture data 

capt.hist.bear <- capt.hist %>%
  filter(Stream == "Bear")
capt.hist.paradise <- capt.hist %>%
  filter(Stream == "Paradise")
capt.hist.zigzag <- capt.hist %>%
  filter(Stream == "Zigzag")

capt.hist.bear$lflow1=rep(yrs_Q1$Q1_scale[1],1220) # 1220 is the number of sallies
capt.hist.bear$lflow2=rep(yrs_Q1$Q1_scale[2],1220)
capt.hist.bear$lflow3=rep(yrs_Q1$Q1_scale[3],1220)
capt.hist.bear$lflow4=rep(yrs_Q1$Q1_scale[4],1220)
capt.hist.bear$lflow7=rep(yrs_Q1$Q1_scale[5],1220)
capt.hist.bear$lflow8=rep(yrs_Q1$Q1_scale[6],1220)
capt.hist.bear$lflow9=rep(yrs_Q1$Q1_scale[7],1220)

capt.hist.paradise$lflow1=rep(yrs_Q1$Q1_scale[1],1214) # 1214 is the number of sallies
capt.hist.paradise$lflow2=rep(yrs_Q1$Q1_scale[2],1214)
capt.hist.paradise$lflow3=rep(yrs_Q1$Q1_scale[3],1214)
capt.hist.paradise$lflow4=rep(yrs_Q1$Q1_scale[4],1214)
capt.hist.paradise$lflow7=rep(yrs_Q1$Q1_scale[5],1214)
capt.hist.paradise$lflow8=rep(yrs_Q1$Q1_scale[6],1214)
capt.hist.paradise$lflow9=rep(yrs_Q1$Q1_scale[7],1214)

capt.hist.zigzag$lflow1=rep(yrs_Q1$Q1_scale[1],873) # 873 is the number of sallies
capt.hist.zigzag$lflow2=rep(yrs_Q1$Q1_scale[2],873)
capt.hist.zigzag$lflow3=rep(yrs_Q1$Q1_scale[3],873)
capt.hist.zigzag$lflow4=rep(yrs_Q1$Q1_scale[4],873)
capt.hist.zigzag$lflow7=rep(yrs_Q1$Q1_scale[5],873)
capt.hist.zigzag$lflow8=rep(yrs_Q1$Q1_scale[6],873)
capt.hist.zigzag$lflow9=rep(yrs_Q1$Q1_scale[7],873)

# combine capture histories
capt.hist<-full_join(capt.hist.bear,capt.hist.paradise)
capt.hist<-full_join(capt.hist,capt.hist.zigzag)



# process data
sal.process=process.data(capt.hist,
                         model="CRDMS", 
                         time.intervals=time.intervals,
                         groups=c("Stream","Fish")) # have to include factors
names(sal.process)
head(sal.process$data) # take a look at the data


# add lowflow environmental co-variate to design data for S; this matches lowflow 1 to lowflow 9
options(max.print=1000000)
sal.ddl$S
# 28 rows for adult bear0, 28 rows for larvae bear0, 28 rows for adult paradise0, 28 rows for larvae paradise 0, 
# 28 for adult zigzag0, 28 for larvae zigzag0, then 28 for adult bear0, ...
# time goes 1,2,3,4,7,8,9, then 2,3,4,7,8,9 then 3,4,7,8,9, ... (to get to 28)
yrs_Q1


w3_scaled<-c(yrs_Q1$Q1_scale[1:7], yrs_Q1$Q1_scale[2:7],yrs_Q1$Q1_scale[3:7], yrs_Q1$Q1_scale[4:7],
             yrs_Q1$Q1_scale[5:7], yrs_Q1$Q1_scale[6:7],yrs_Q1$Q1_scale[7])
w6_scaled<-c(yrs_Q1$Q1_scale[8:14], yrs_Q1$Q1_scale[9:14],yrs_Q1$Q1_scale[10:14], yrs_Q1$Q1_scale[11:14],
             yrs_Q1$Q1_scale[12:14], yrs_Q1$Q1_scale[13:14],yrs_Q1$Q1_scale[14])
w7_scaled<-c(yrs_Q1$Q1_scale[15:21], yrs_Q1$Q1_scale[16:21],yrs_Q1$Q1_scale[17:21], yrs_Q1$Q1_scale[18:21],
             yrs_Q1$Q1_scale[19:21], yrs_Q1$Q1_scale[20:21],yrs_Q1$Q1_scale[21])

sal.ddl$S$lowflow = c(w6_scaled,w6_scaled,w3_scaled, w3_scaled,w7_scaled,w7_scaled )

sal.ddl$Psi
# same as S
sal.ddl$Psi$lowflow = c(w6_scaled,w6_scaled,w3_scaled, w3_scaled,w7_scaled,w7_scaled )
sal.ddl$Psi # should see lowflow columns



#########################################################

capt.hist.bear$hflow1=rep(yrs_Q99$Q99_scale[1],1220) # 1220 is the number of sallies
capt.hist.bear$hflow2=rep(yrs_Q99$Q99_scale[2],1220)
capt.hist.bear$hflow3=rep(yrs_Q99$Q99_scale[3],1220)
capt.hist.bear$hflow4=rep(yrs_Q99$Q99_scale[4],1220)
capt.hist.bear$hflow7=rep(yrs_Q99$Q99_scale[5],1220)
capt.hist.bear$hflow8=rep(yrs_Q99$Q99_scale[6],1220)
capt.hist.bear$hflow9=rep(yrs_Q99$Q99_scale[7],1220)

capt.hist.paradise$hflow1=rep(yrs_Q99$Q99_scale[1],1214) # 1214 is the number of sallies
capt.hist.paradise$hflow2=rep(yrs_Q99$Q99_scale[2],1214)
capt.hist.paradise$hflow3=rep(yrs_Q99$Q99_scale[3],1214)
capt.hist.paradise$hflow4=rep(yrs_Q99$Q99_scale[4],1214)
capt.hist.paradise$hflow7=rep(yrs_Q99$Q99_scale[5],1214)
capt.hist.paradise$hflow8=rep(yrs_Q99$Q99_scale[6],1214)
capt.hist.paradise$hflow9=rep(yrs_Q99$Q99_scale[7],1214)

capt.hist.zigzag$hflow1=rep(yrs_Q99$Q99_scale[1],873) # 873 is the number of sallies
capt.hist.zigzag$hflow2=rep(yrs_Q99$Q99_scale[2],873)
capt.hist.zigzag$hflow3=rep(yrs_Q99$Q99_scale[3],873)
capt.hist.zigzag$hflow4=rep(yrs_Q99$Q99_scale[4],873)
capt.hist.zigzag$hflow7=rep(yrs_Q99$Q99_scale[5],873)
capt.hist.zigzag$hflow8=rep(yrs_Q99$Q99_scale[6],873)
capt.hist.zigzag$hflow9=rep(yrs_Q99$Q99_scale[7],873)

# combine capture histories
capt.hist<-full_join(capt.hist.bear,capt.hist.paradise)
capt.hist<-full_join(capt.hist,capt.hist.zigzag)

# process data
sal.process=process.data(capt.hist,
                         model="CRDMS", 
                         time.intervals=time.intervals,
                         groups=c("Stream","Fish")) # have to include factors
names(sal.process)
head(sal.process$data) # take a look at the data



# add highflow environmental covariate to design data for Phi; this matches highflow 1 to highflow 9

w3_scaled99<-c(yrs_Q99$Q99_scale[1:7], yrs_Q99$Q99_scale[2:7],yrs_Q99$Q99_scale[3:7], yrs_Q99$Q99_scale[4:7],
               yrs_Q99$Q99_scale[5:7], yrs_Q99$Q99_scale[6:7],yrs_Q99$Q99_scale[7])
w6_scaled99<-c(yrs_Q99$Q99_scale[8:14], yrs_Q99$Q99_scale[9:14],yrs_Q99$Q99_scale[10:14], yrs_Q99$Q99_scale[11:14],
               yrs_Q99$Q99_scale[12:14], yrs_Q99$Q99_scale[13:14],yrs_Q99$Q99_scale[14])
w7_scaled99<-c(yrs_Q99$Q99_scale[15:21], yrs_Q99$Q99_scale[16:21],yrs_Q99$Q99_scale[17:21], yrs_Q99$Q99_scale[18:21],
               yrs_Q99$Q99_scale[19:21], yrs_Q99$Q99_scale[20:21],yrs_Q99$Q99_scale[21])

sal.ddl$S$highflow = c(w6_scaled99,w6_scaled99,w3_scaled99, w3_scaled99,w7_scaled99,w7_scaled99)

sal.ddl$Psi
# same as S
sal.ddl$Psi$highflow = c(w6_scaled99,w6_scaled99,w3_scaled99, w3_scaled99,w7_scaled99,w7_scaled99)
sal.ddl$Psi # should see highflow columns



###################################
# setting Psi L -> A to 0
sal.ddl$Psi[,1:18]

# which indices go A to L
PsiLA.indices=as.numeric(row.names(sal.ddl$Psi[sal.ddl$Psi$tostratum =="L",]))
PsiLA.indices

# count how many values need to be set to 0
PsiLA.values=rep(0,length(PsiLA.indices))
PsiLA.values




#####################################################################
###################################################################################
## FITTING BEST Capture and Recapture rates
sal.ddl$p

# using same structure as from final pradel (stream and fish)
p.fish.stream.stratum = list(formula =  ~  Fish + Stream + stratum, share = TRUE)
p.fish.stream = list(formula =  ~  Fish + Stream , share = TRUE)



# wanted to start with just time varying psi and s to select for best capture model structure
Psi.time = list(formula =  ~ time, fixed=list(index=c(PsiLA.indices),
                                              value=c(PsiLA.values))) # need to fix L->A at 0
S.time = list(formula =  ~ time, fixed=list(index=c(S.A.indices),
                                            value=c(S.A.values))) # need to fix S(L) -> 1



# Run all pairings of models
sal.model.list=create.model.list("CRDMS")

sal.results=mark.wrapper(sal.model.list,
                         data=sal.process,
                         ddl=sal.ddl)

sal.results
# best model with time varying S and Phi includes stream, fish, and stratum 

names(sal.results)
sal.results$p.fish.stream.stratum$results$real







###################################################################################
######################## START HERE TO FIT BEST MODEL FOR PSI, S ###################
######################################################################################
# BEST MODEL FOR P
p.fish.stream.stratum = list(formula =  ~  Fish + Stream + stratum, share = TRUE)

# always going to include stream and area in survival and recruitment 
# included stratum in survival structure when not setting larval survival to 1 (interactive terms with lflow,hflow,fish)

# survival covariates 
sal.ddl$S

S.dot = list(formula = ~ Stream + Area)

S.stage = list(formula = ~ Stream + Area + stratum)

S.lowflow = list(formula = ~ Stream + Area + lflow*stratum)

S.highflow = list(formula = ~ Stream + Area + hflow*stratum)

S.lowflow.highflow = list(formula = ~ Stream + Area + lflow*stratum+ hflow*stratum)




# transition covariates 
sal.ddl$Psi

Psi.dot = list(formula = ~ Stream + Area, fixed=list(index=c(PsiLA.indices),
                                                     value=c(PsiLA.values)))

Psi.lowflow = list(formula = ~ Stream + Area + lflow, fixed=list(index=c(PsiLA.indices),
                                                                 value=c(PsiLA.values)))

Psi.highflow = list(formula = ~ Stream + Area + hflow, fixed=list(index=c(PsiLA.indices),
                                                                  value=c(PsiLA.values)))

Psi.lowflow.highflow = list(formula = ~Stream + Area + lflow + hflow, fixed=list(index=c(PsiLA.indices),
                                                                                 value=c(PsiLA.values)))




#####################################################
# Run all pairings of models
combo.model.list=create.model.list("CRDMS")

sal.results.combo=mark.wrapper(combo.model.list,
                               data=sal.process,
                               ddl=sal.ddl)

sal.results.combo




# TOP MODELS 
names(sal.results.combo)
top<-sal.results.combo$S.highflow.Psi.lowflow.highflow.p.fish.stream.stratum # top model 
top2<-sal.results.combo$S.highflow.Psi.lowflow.p.fish.stream.stratum # second best model 
top3<-sal.results.combo$S.lowflow.highflow.Psi.lowflow.highflow.p.fish.stream.stratum # third best model 

summary(top, showall = FALSE)

# results from top model 
top$results$beta
top2$results$beta
top3$results$beta
top$results$derived # population sizes 

options(max.print=1000000)
top$results$real

# for original model 
mean(top$results$real$estimate[1:42]) # mean annual survival  

# adult survival 
mean(c(top$results$real$estimate[1:7],top$results$real$estimate[15:21], top$results$real$estimate[29:35]))
sd(c(top$results$real$estimate[1:7],top$results$real$estimate[15:21], top$results$real$estimate[29:35]))

# larval survival 
mean(c(top$results$real$estimate[8:14],top$results$real$estimate[22:28], top$results$real$estimate[36:42]))
sd(c(top$results$real$estimate[8:14],top$results$real$estimate[22:28], top$results$real$estimate[36:42]))


mean(top$results$real$estimate[43:54]) # mean recapture rate  
sd(top$results$real$estimate[43:54])


# mean transition probability
top$results$real$estimate

cjs.result<-data.frame(top$results$real)

parameter<-c(rep("Survival",42),rep("Capture",12),rep("PopSize",8),rep("TransitionProb",169))
cjs.result$parameter<-parameter

transition.result <- cjs.result %>%
  filter(parameter == "TransitionProb")

transition.result <- transition.result%>%
  filter(fixed != "Fixed")

#transition.result<-transition.result[1:84,] # fish and no fish reaches are the same 

stream <- c(rep("Bear",28),rep("Paradise",28), rep("Zigzag",28))
surv_yr <-c(1,2,3,4,7,8,9,  2,3,4,7,8,9,  3,4,7,8,9,  4,7,8,9,   7,8,9,  8,9,  9,    
            1,2,3,4,7,8,9,  2,3,4,7,8,9,  3,4,7,8,9,  4,7,8,9,   7,8,9,  8,9,  9, 
            1,2,3,4,7,8,9,  2,3,4,7,8,9,  3,4,7,8,9,  4,7,8,9,   7,8,9,  8,9,  9)

transition.result$stream <- stream
transition.result$surv_yr <- surv_yr

transition.result <- transition.result%>%
  mutate(WS = case_when(stream=="Bear" ~ 6,
                        stream=="Paradise" ~ 3,
                        stream=="Zigzag" ~ 7))

# get rid of duplicates 
transition.result <- transition.result%>%
  unique()
head(transition.result)

mean(transition.result$estimate) # 0.29
sd(transition.result$estimate) # 0.12






##############################################
## likelihood ratio test btw nested models
#############################################

### btw top model and second best
# pay attn to order of models (btw simple and more complex) - top2 is simpler in our case 
1-pchisq((top2$results$lnl-top$results$lnl),df=top$results$npar-top2$results$npar)
# p-value 

top2$results$lnl-top$results$lnl
# chi-square value 




############ view results #############

# make predictions for rows of 'sal.ddl' associated with
# survival prediction first
# create values of length to use for predictions
min.highflow = min(yrs_Q99$Q99_scale)
max.highflow = max(yrs_Q99$Q99_scale)
highflow.values = seq(from = min.highflow, to = max.highflow, length = 1000)

mhigh<-mean(yrs_Q99$Q99)
shigh<-sd(yrs_Q99$Q99)

min.lowflow = min(yrs_Q1$Q1_scale)
max.lowflow = max(yrs_Q1$Q1_scale)
lowflow.values = seq(from = min.lowflow, to = max.lowflow, length = 1000)

mlow<-mean(yrs_Q1$Q1)
slow<-sd(yrs_Q1$Q1)



### high flow effect on adult survival
sal.ddl$S

survival.adult.bear <- covariate.predictions(top, 
                                             data = data.frame(hflow1 = highflow.values),
                                             indices=c(1)) # adult survival 
survival.adult.bear <- survival.adult.bear$estimates

survival.adult.par <- covariate.predictions(top, 
                                            data = data.frame(hflow1 = highflow.values),
                                            indices=c(57)) # adult survival 
survival.adult.par <- survival.adult.par$estimates

survival.adult.zz <- covariate.predictions(top, 
                                           data = data.frame(hflow1 = highflow.values),
                                           indices=c(113)) # adult survival 
survival.adult.zz <- survival.adult.zz$estimates

survival.adult.zz$Stream <- rep("Zigzag",1000)
survival.adult.par$Stream <- rep("Paradise",1000)
survival.adult.bear$Stream <- rep("Bear",1000)


survival.flood.adult<-full_join(survival.adult.bear, survival.adult.par)
survival.flood.adult<-full_join(survival.flood.adult, survival.adult.zz)

# transform discharge back to Q99 to interpret 
# scale subtracts the mean and divides by the sd
# so multifply by sd then add mean to transform back

head(survival.flood.adult)
survival.flood.adult <- survival.flood.adult %>%
  mutate(step1 = covdata *shigh)%>%
  mutate(step2 = step1+mhigh)%>%
  rename(Q99 = step2)


# effect size of change in flooding on overall ADULT survival 
head(survival.adult.bear)
minphiAQ99<-min(survival.adult.bear$estimate) # at largest Q99
minphiAQ99
maxphiAQ99<-max(survival.adult.bear$estimate) # at smallest Q99
maxphiAQ99

# to calculate percent decrease
# ((final value (lower one) - start value )/ |start value| )*100
((minphiAQ99 - maxphiAQ99)/abs(maxphiAQ99))*100


# confidence interval
minphiAQ99.lcl<-min(survival.adult.bear$lcl) # at largest Q99
maxphiAQ99.lcl<-max(survival.adult.bear$lcl) # at smallest Q99

((minphiAQ99.lcl - maxphiAQ99.lcl)/abs(maxphiAQ99.lcl))*100 

minphiAQ99.ucl<-min(survival.adult.bear$ucl) # at largest Q99
maxphiAQ99.ucl<-max(survival.adult.bear$ucl) # at smallest Q99

((minphiAQ99.ucl - maxphiAQ99.ucl)/abs(maxphiAQ99.ucl))*100



# high flow effect on larval survival
sal.ddl$S

survival.larvae.bear <- covariate.predictions(top, 
                                              data = data.frame(hflow1 = highflow.values),
                                              indices=c(29)) # larval survival 
survival.larvae.bear <- survival.larvae.bear$estimates

survival.larvae.par <- covariate.predictions(top, 
                                             data = data.frame(hflow1 = highflow.values),
                                             indices=c(85)) # larval survival 
survival.larvae.par <- survival.larvae.par$estimates

survival.larvae.zz <- covariate.predictions(top, 
                                            data = data.frame(hflow1 = highflow.values),
                                            indices=c(141)) # larval survival 
survival.larvae.zz <- survival.larvae.zz$estimates


survival.larvae.zz$Stream <- rep("Zigzag",1000)
survival.larvae.par$Stream <- rep("Paradise",1000)
survival.larvae.bear$Stream <- rep("Bear",1000)


survival.flood.larvae<-full_join(survival.larvae.bear, survival.larvae.par)
survival.flood.larvae<-full_join(survival.flood.larvae, survival.larvae.zz)


# transform discharge back to Q99 to interpret 
# scale subtracts the mean and divides by the sd
# so multifply by sd then add mean to transform back

survival.flood.larvae <- survival.flood.larvae %>%
  mutate(step1 = covdata *shigh)%>%
  mutate(step2 = step1+mhigh)%>%
  rename(Q99 = step2)

# effect size of change in flooding on overall larval survival 
minphiLQ99<-min(survival.larvae.bear$estimate) # at largest Q99
minphiLQ99
maxphiLQ99<-max(survival.larvae.bear$estimate) # at smallest Q99
maxphiLQ99

# to calculate percent decrease
# ((final value (lower one) - start value )/ |start value| )*100
((minphiLQ99 - maxphiLQ99)/abs(maxphiLQ99))*100


# confidence intervals 
minphiLQ99.ucl<-min(survival.larvae.bear$ucl) # at largest Q99
maxphiLQ99.ucl<-max(survival.larvae.bear$ucl) # at smallest Q99

((minphiLQ99.ucl - maxphiLQ99.ucl)/abs(maxphiLQ99.ucl))*100 

minphiLQ99.lcl<-min(survival.larvae.bear$lcl) # at largest Q99
maxphiLQ99.lcl<-max(survival.larvae.bear$lcl) # at smallest Q99

((minphiLQ99.lcl - maxphiLQ99.lcl)/abs(maxphiLQ99.lcl))*100 




########################################################################
# adding data to figure 
survival.result <- cjs.result %>%
  filter(parameter == "Survival")

stage <- c(rep("Adult",7),rep("Larvae",7),rep("Adult",7),rep("Larvae",7),rep("Adult",7),rep("Larvae",7))
stream <- c(rep("Bear",14),rep("Paradise",14), rep("Zigzag",14))
surv_yr <-c(1,2,3,4,7,8,9,1,2,3,4,7,8,9,1,2,3,4,7,8,9,1,2,3,4,7,8,9,1,2,3,4,7,8,9,1,2,3,4,7,8,9)

survival.result$stage <- stage
survival.result$stream <- stream
survival.result$surv_yr <- surv_yr

survival.result <- survival.result%>%
  mutate(WS = case_when(stream=="Bear" ~ 6,
                        stream=="Paradise" ~ 3,
                        stream=="Zigzag" ~ 7))


survival.result2<-left_join(survival.result,yrs_Q99,by=c("WS","surv_yr"))
survival.result2<-left_join(survival.result2,yrs_Q1,by=c("WS","surv_yr"))

adult.survival.result <- survival.result2 %>%
  filter(stage == "Adult")
larvae.survival.result <- survival.result2 %>%
  filter(stage == "Larvae")




### plotting with data the effect of high flow on larval survival
# color blind palette
cbp2 <- c("#000000", "#E69F00", "#56B4E9")

hflow.l.surv<-ggplot(survival.flood.larvae, aes(x = Q99, y = estimate, color=Stream)) +
  geom_point(data=larvae.survival.result, aes(x=Q99, y=estimate,color=stream), size=4.5, alpha=0.7)+
  geom_errorbar(data=larvae.survival.result, aes(x=Q99, ymin=lcl, ymax=ucl,color=stream),alpha=0.7)+
  geom_ribbon(aes(ymin = lcl, ymax = ucl, fill=Stream), alpha = 0.15,colour=NA) +
  geom_line(size = 2, alpha=0.8) +
  xlab(bquote('Peak Discharge '('mm day' ^-1))) + 
  ylab(expression(paste("Larval Survival  ", (phi[L])))) +
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_blank(),
        legend.position = c(.8,.8),
        legend.background =element_blank(),
        plot.margin =margin(1,1,1,1,"cm"))+
  scale_x_continuous(limits = c(31,100),expand = c(0, 0)) + # forcing axes to start at 0
  scale_y_continuous(expand = c(0, 0),limits=c(0.1,0.8))  +
  guides(x = "prism_minor",y="prism_minor")+
  scale_colour_manual(values=cbp2)+
  scale_fill_manual(values=cbp2)
hflow.l.surv






# high flow and adult survival
hflow.a.surv<-ggplot(survival.flood.adult, aes(x = Q99, y = estimate, color=Stream)) +
  geom_point(data=adult.survival.result, aes(x=Q99, y=estimate,color=stream), size=4.5, alpha=0.7)+
  geom_errorbar(data=adult.survival.result, aes(x=Q99, ymin=lcl, ymax=ucl,color=stream),alpha=0.7)+
  geom_ribbon(aes(ymin = lcl, ymax = ucl,fill=Stream), alpha = 0.15,colour=NA) +
  geom_line(size = 2, alpha=0.8) +
  xlab(bquote('Peak Discharge '('mm day' ^-1))) + 
  ylab(expression(paste("Adult Survival  ", (phi[A])))) +
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.title=element_blank(),
        legend.text=element_text(size=12),
        legend.position = "none",
        plot.margin =margin(1,1,1,1,"cm"))+
  scale_x_continuous(limits = c(31,100),expand = c(0, 0)) + # forcing axes to start at 0
  scale_y_continuous(expand = c(0, 0),limits=c(0.1,0.8))  +
  guides(x = "prism_minor",y="prism_minor")+
  scale_colour_manual(values=cbp2)+
  scale_fill_manual(values=cbp2)
hflow.a.surv


duplot<-cowplot::plot_grid(hflow.l.surv +
                             theme(axis.title.x = element_blank()),
                           
                           hflow.a.surv +
                             theme(axis.title.x = element_blank()),
                           
                           nrow = 1, labels = "auto", align = "v")

duplot


duplot2 <-cowplot::add_sub(duplot, expression(paste("Peak Discharge (mm ", day^-1,")"),
                                              x=0.5, y=0.5,fontface="plain", size=12))

cowplot::ggdraw(duplot2)




########## looking at adult recruitment results / figures
# DROUGHT ON PSI
sal.ddl$Psi
sal.ddl$Psi[29,] # from L to A (for Bear0) - model.index is 365
sal.ddl$Psi[85,] # from L to A (for Paradise0) - model index is 421
sal.ddl$Psi[141,] # from L to A (for Zigzag0) - model index is 477

# using best model with lrt (top2)
prediction.top.bear <- covariate.predictions(top, 
                                             data = data.frame(lflow1 = lowflow.values),
                                             indices=c(365)) # from L to A 
prediction.top.bear <- prediction.top.bear$estimates

prediction.top.par <- covariate.predictions(top, 
                                            data = data.frame(lflow1 = lowflow.values),
                                            indices=c(421)) # from L to A 
prediction.top.par <- prediction.top.par$estimates

prediction.top.zz <- covariate.predictions(top, 
                                           data = data.frame(lflow1 = lowflow.values),
                                           indices=c(477)) # from L to A 
prediction.top.zz <- prediction.top.zz$estimates

# combine streams into one df
prediction.top.zz$Stream <- rep("Zigzag",1000)
prediction.top.par$Stream <- rep("Paradise",1000)
prediction.top.bear$Stream <- rep("Bear",1000)

pred.top3<-full_join(prediction.top.bear, prediction.top.par) 
pred.top3<-full_join(pred.top3,prediction.top.zz)

head(pred.top3)


# transform discharge back to Q99 to interpret 
# scale subtracts the mean and divides by the sd
# so multifply by sd then add mean to transform back
mlow<-mean(yrs_Q1$Q1)
slow<-sd(yrs_Q1$Q1)

psi.drought.prediction <- pred.top3 %>%
  mutate(step1 = covdata*slow)%>%
  mutate(step2 = step1+mlow)%>%
  rename(Q1 = step2)


# build and store the plot in object 'p'
lowflow.psi <- ggplot(psi.drought.prediction, aes(x = Q1, y = estimate,color=Stream)) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin = lcl, ymax = ucl), alpha = 0.2) +
  xlab(expression(paste("Low Flow " , (mm/day)))) + 
  ylab("Annual Adult Recruitment") +
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_text(size=12))+
  scale_x_continuous(expand = c(0, 0), labels = scales::comma) + # forcing axes to start at 0 and to not be scientific 
  scale_y_continuous(expand = c(0, 0)) 
# print the plot
lowflow.psi


# effect size of change in DROUGHT on adult recruitment
head(prediction.top.bear)
minpsiQ1<-min(prediction.top.bear$estimate) # at worst drought
minpsiQ1 
maxpsiQ1<-max(prediction.top.bear$estimate) # at "best" drought
maxpsiQ1 

# to calculate percent increase
# ((final value (big value) - start value )/ |start value| )*100
((maxpsiQ1 - minpsiQ1)/abs(minpsiQ1))*100 

# confidence intervals
minpsiQ1.lcl<-min(prediction.top.bear$lcl) # at worst drought
maxpsiQ1.lcl<-max(prediction.top.bear$lcl) # at "best" drought

((maxpsiQ1.lcl - minpsiQ1.lcl)/abs(minpsiQ1.lcl))*100 

minpsiQ1.ucl<-min(prediction.top.bear$ucl) # at worst drought
maxpsiQ1.ucl<-max(prediction.top.bear$ucl) # at "best" drought

((maxpsiQ1.ucl - minpsiQ1.ucl)/abs(minpsiQ1.ucl))*100




# FLOODING ON PSI --- MARGINALLY SIGNIFICANT 
min.highflow = min(yrs_Q99$Q99_scale)
max.highflow = max(yrs_Q99$Q99_scale)
highflow.values = seq(from = min.highflow, to = max.highflow, length = 1000)

sal.ddl$Psi

# using best model 
prediction.top.bear2 <- covariate.predictions(top, 
                                              data = data.frame(hflow1 = highflow.values),
                                              indices=c(365)) # from L to A 
prediction.top.bear2 <- prediction.top.bear2$estimates

prediction.top.par2 <- covariate.predictions(top, 
                                             data = data.frame(hflow1 = highflow.values),
                                             indices=c(421)) # from L to A 
prediction.top.par2 <- prediction.top.par2$estimates

prediction.top.zz2 <- covariate.predictions(top, 
                                            data = data.frame(hflow1 = highflow.values),
                                            indices=c(477)) # from L to A 
prediction.top.zz2 <- prediction.top.zz2$estimates

# combine streams into one df
prediction.top.zz2$Stream <- rep("Zigzag",1000)
prediction.top.par2$Stream <- rep("Paradise",1000)
prediction.top.bear2$Stream <- rep("Bear",1000)

pred.top.2<-full_join(prediction.top.bear2, prediction.top.par2)
pred.top.2<-full_join(pred.top.2,prediction.top.zz2)



# transform discharge back to Q99 to interpret 
# scale subtracts the mean and divides by the sd
# so multifply by sd then add mean to transform back
mhigh<-mean(yrs_Q99$Q99)
shigh<-sd(yrs_Q99$Q99)

psi.flood.prediction <- pred.top.2 %>%
  mutate(step1 = covdata*shigh)%>%
  mutate(step2 = step1+mhigh)%>%
  rename(Q99 = step2)
head(psi.flood.prediction)


# effect size of change in DROUGHT on adult recruitment
head(prediction.top.bear2)
minpsiQ99<-min(prediction.top.bear2$estimate) # at worst flood 
minpsiQ99
maxpsiQ99<-max(prediction.top.bear2$estimate) # at best flood 
maxpsiQ99

# to calculate percent decrease
# ((start value (low value) - final value )/ |final value| )*100
((minpsiQ99 - maxpsiQ99)/abs(maxpsiQ99))*100 


# confidence intervals
minpsiQ99.lcl<-min(prediction.top.bear2$lcl) # 
maxpsiQ99.lcl<-max(prediction.top.bear2$lcl) # 

((minpsiQ99.lcl - maxpsiQ99.lcl)/abs(maxpsiQ99.lcl))*100 # 

minpsiQ99.ucl<-min(prediction.top.bear2$ucl) # 
maxpsiQ99.ucl<-max(prediction.top.bear2$ucl) # 

((minpsiQ99.ucl - maxpsiQ99.ucl)/abs(maxpsiQ99.ucl))*100 # 




### plot graphs with data 
transition.result.wq1<-left_join(transition.result,yrs_Q1)


lowflow.psi.wdat <- ggplot(psi.drought.prediction, aes(x = Q1, y = estimate,color=Stream)) +
  
  geom_ribbon(aes(ymin = lcl, ymax = ucl,fill=Stream), alpha = 0.15,colour=NA) +
  geom_point(data=transition.result.wq1, aes(x=Q1, y=estimate,color=stream),size=4.5,alpha=0.7)+
  geom_errorbar(data=transition.result.wq1, aes(x=Q1, ymin=lcl, ymax=ucl,color=stream),alpha=0.7)+
  geom_line(size = 2, alpha=0.7) +
  xlab(bquote('Lowest Discharge '('mm day' ^-1)))+
  ylab(expression(paste("Adult Recruitment  ", (Psi[LA])))) +
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_blank(),
        legend.position = c(.8,.8),
        legend.background=element_blank(),# making legend fill go away
        plot.margin = margin(1,1,1,1, "cm"))+
  scale_x_continuous(limits=c(-0.009,0.2),expand=c(0,0)) + 
  guides(x = "prism_minor",y="prism_minor")+
  scale_colour_manual(values=cbp2)+
  scale_fill_manual(values=cbp2)
# print the plot
lowflow.psi.wdat



### combing larval and adult recruitment into one figure 
duplot2<-cowplot::plot_grid(lowflow.f.wdat.dubs +
                              theme(axis.title.x = element_blank()),
                            
                            lowflow.psi.wdat.nolegend +
                              theme(axis.title.x = element_blank()),
                            
                            
                            nrow = 1, labels = "auto", align = "v")
duplot2 <- cowplot::add_sub(duplot2, expression(paste("Lowest Discharge (mm ", day^-1,")"),
                                                hjust = 0.25, fontface="plain", size=12))
cowplot::ggdraw(duplot2)







# flood effect on adult recruitment with data
highflow.psi.wdat <- ggplot(psi.flood.prediction, aes(x = Q99, y = estimate,color=Stream)) +
  
  geom_ribbon(aes(ymin = lcl, ymax = ucl,fill=Stream), alpha = 0.15,colour=NA) +
  geom_point(data=transition.result.wq99, aes(x=Q99, y=estimate,color=stream),size=4.5,alpha=0.7)+
  geom_errorbar(data=transition.result.wq99, aes(x=Q99, ymin=lcl, ymax=ucl,color=stream),alpha=0.7)+
  geom_line(size = 2, alpha=0.7) +
  xlab(bquote('Peak Discharge '('mm day' ^-1)))+
  ylab(expression(paste("Adult Recruitment  ", (Psi[LA])))) +
  theme_classic()+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text=element_text(size=12),
        legend.title=element_blank(),
        legend.position = c(.8,.8),
        legend.background=element_blank(),# making legend fill go away
        plot.margin = margin(1,1,1,1, "cm"))+
  scale_x_continuous(limits=c(29,100),expand=c(0,0)) + 
  scale_y_continuous(limits=c(0,0.7),expand = c(0, 0)) +
  guides(x = "prism_minor",y="prism_minor")+
  scale_colour_manual(values=cbp2)+
  scale_fill_manual(values=cbp2)
# print the plot
highflow.psi.wdat



### combing the effect of high and low flow on adult recruitment into one figure 
duplot.ar<-cowplot::plot_grid(highflow.psi.wdat +
                                theme(),
                              
                              lowflow.psi.wdat.nolegend +
                                theme(axis.title.y = element_blank()),
                              
                              
                              nrow = 1, labels = "auto", align = "v")
duplot.ar



# effect size of change in FLOOD on adult recruitment
head(prediction.top.bear2)
minpsiQ99<-min(prediction.top.bear2$estimate) # at worst flood
minpsiQ99 #
maxpsiQ99<-max(prediction.top.bear2$estimate) # at "best" flood
maxpsiQ99 #

# to calculate percent increase
# ((final value (big value) - start value )/ |start value| )*100
((maxpsiQ99 - minpsiQ99)/abs(minpsiQ99))*100 #

# confidence intervals
minpsiQ199.lcl<-min(prediction.top.bear2$lcl) # at worst flood
maxpsiQ99.lcl<-max(prediction.top.bear2$lcl) # at "best" flood

((maxpsiQ99.lcl - minpsiQ199.lcl)/abs(minpsiQ199.lcl))*100 # 

minpsiQ99.ucl<-min(prediction.top.bear2$ucl) # at worst flood
maxpsiQ99.ucl<-max(prediction.top.bear2$ucl) # at "best" flood

((maxpsiQ99.ucl - minpsiQ99.ucl)/abs(minpsiQ99.ucl))*100 # 












