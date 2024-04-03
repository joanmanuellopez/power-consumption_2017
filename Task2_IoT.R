#### Load data into R ####
library(readr)
powerConsumptionMasterData <- read_delim("household_power_consumption.txt", 
                                         ";", escape_double = FALSE, locale = locale(tz = "GMT"), 
                                         na = "NA", trim_ws = TRUE)
View(powerConsumptionMasterData)

# We need to use locale = locale(tz = "GMT") --> to skip issues with daylight saving times

#### Create new column for date/time together and place it in the right place ####
powerConsumptionMasterData <-cbind(powerConsumptionMasterData,paste(powerConsumptionMasterData$Date,powerConsumptionMasterData$Time), stringsAsFactors=FALSE)
colnames(powerConsumptionMasterData)[10] <-"DateTime"
powerConsumptionMasterData <- powerConsumptionMasterData[,c(ncol(powerConsumptionMasterData), 1:(ncol(powerConsumptionMasterData)-1))]
head(powerConsumptionMasterData)

# Check duplicates -- there are no duplicates since to we have used tz=GMT
which(duplicated(powerConsumptionMasterData$DateTime))

# Converting date and Time format
powerConsumptionMasterData$DateTime <- strptime(powerConsumptionMasterData$DateTime, "%d/%m/%Y %H:%M:%S")
powerConsumptionMasterData$Date <- as.Date(powerConsumptionMasterData$Date, "%d/%m/%Y")

powerConsumptionMasterData$DateTime <- as.POSIXct(powerConsumptionMasterData$DateTime)

#### Tidy Master Dataset ####
# Step 1. Change Column Names
names(powerConsumptionMasterData)[names(powerConsumptionMasterData) == 'Global_active_power'] <- 'TotalActive'
names(powerConsumptionMasterData)[names(powerConsumptionMasterData) == 'Sub_metering_1'] <- 'Kitchen'
names(powerConsumptionMasterData)[names(powerConsumptionMasterData) == 'Sub_metering_2'] <- 'Laundry'
names(powerConsumptionMasterData)[names(powerConsumptionMasterData) == 'Sub_metering_3'] <- 'HVAC'

#Step 2. Change Units of Total Active column, from kW to Wh  (Value x 1000 / 60)
powerConsumptionMasterData$TotalActive <- powerConsumptionMasterData$TotalActive*1000/60

#Step 3. Create new column for the energy not sub-metered
library("dplyr")
library("tidyr")
powerConsumptionMasterData <- powerConsumptionMasterData %>% mutate(Others = TotalActive - Kitchen - Laundry - HVAC)



#### Discovering NAs - Remember to dismiss them from the dataset ####
library("ggplot2")
pcd_na <- powerConsumptionMasterData[rowSums(is.na(powerConsumptionMasterData)) > 0,]
ggplot(pcd_na,aes(x=Date))+geom_bar()
ggplot(subset(pcd_na,Date >= "2007-04-01" & Date < "2007-05-01"),aes(x=Date,y = (..count..)/60))+geom_bar()+ylab("Total Hours")


#### Create a new subset that only shows data hourly ####
# library("dplyr")
# library("tidyr")
library("lubridate")

pcd_hourly <- mutate(powerConsumptionMasterData, hourly_tag = format(powerConsumptionMasterData$DateTime, "%d/%m/%Y %H"))

pcd_hourly$DateTime <- NULL
pcd_hourly$Date <- NULL
pcd_hourly$Time <- NULL
pcd_hourly$Global_reactive_power <- NULL
pcd_hourly$Voltage <- NULL
pcd_hourly$Global_intensity <- NULL

pcd_hourly_num <- pcd_hourly[-6]
pcd_hourly_agg <- aggregate(pcd_hourly_num, by=list(pcd_hourly$hourly_tag), FUN=sum, na.rm=TRUE)

colnames(pcd_hourly_agg)[1] <- "DateTime"
pcd_hourly_agg$DateTime <- strptime(pcd_hourly_agg$DateTime, "%d/%m/%Y %H")

pcd_hourly_agg <- pcd_hourly_agg[order(pcd_hourly_agg$DateTime), ]
pcd_hourly_agg$DateTime <- as.POSIXct(pcd_hourly_agg$DateTime)

# Extra work just to have only one DataFrame
pcd_hourly <- pcd_hourly_agg
remove(pcd_hourly_num)
remove(pcd_hourly_agg)

# Export dataframe to a csv file
write.csv(pcd_hourly,"power_consumption_hourly.csv",row.names = FALSE)

# Finally, let's have some initial insights...
pcd_hourly %>% group_by(month=floor_date(DateTime, "month")) %>% summarize(TotalPower=sum(TotalActive/1000,na.rm=TRUE)) %>% ggplot(aes(x=month,y=TotalPower))+geom_bar(stat="identity",fill="#16a085")+xlab("Month")+ylab("Total Energy (kWh)")
pcd_hourly %>% group_by(month=floor_date(DateTime, "month")) %>% summarize(TotalPower=sum(TotalActive/1000,na.rm=TRUE)) %>% ggplot(aes(x=month,y=TotalPower,fill=ifelse(month < "2009-01-01","Red","blue")))+geom_bar(stat="identity")+xlab("Month")+ylab("Total Energy (kWh)")+guides(fill=FALSE)

#### Average for 4 years for each submeter ####
pcdHourlyGath <- pcd_hourly %>% gather(Metered,Energy_Wh,TotalActive:HVAC)
pcdHourlyGath %>% group_by(mesos=floor_date(DateTime, "month"),Metered) %>% summarise(Energy_mean = mean(Energy_Wh,na.rm=TRUE)) %>% ggplot(aes(x=mesos,y=Energy_mean,group=Metered)) + geom_line(aes(color=Metered)) + ylab("Energy Daily Mean per Month [Wh]")

#### plot with ggjoy - experimental issues ####
ggplot(pcdHourlyGath, aes(x = DateTime, y = Metered)) + geom_density_ridges(scale = 0.9)
pcdHourlyGath %>% group_by(mesos=floor_date(DateTime, "month"),Metered) %>% summarise(Energy_mean = mean(Energy_Wh,na.rm=TRUE)) %>% ggplot(aes(x=mesos,y=Metered,group=Metered)) + geom_density_ridges() + ylab("Energy Mean [Wh]")

#### Subsetting by year - Seasonal behavior of consumption - The percentage stackplot ####
# First thing... subset by year
#pcd_hourly_test <- pcd_hourly
pcd_hourly_test <- pcd_hourly %>% filter(DateTime >= "2008-01-01" & DateTime < "2009-01-01")
pcd_hourly_test <- pcd_hourly_test %>% mutate(Kitchen100 = Kitchen/TotalActive * 100)

#All about this in just one line
pcd_test_100 <- pcd_hourly %>% filter(DateTime >= "2007-01-01" & DateTime < "2008-01-01") %>% mutate(Kitchen100=Kitchen/TotalActive * 100, Laundry100 = Laundry/TotalActive * 100,HVAC100 = HVAC/TotalActive * 100,Others100 = Others/TotalActive * 100) %>% select(-Kitchen:-Others)

names(pcd_test_100)[names(pcd_test_100) == 'TotalActive100'] <- 'TotalActive'
names(pcd_test_100)[names(pcd_test_100) == 'Kitchen100'] <- 'Kitchen'
names(pcd_test_100)[names(pcd_test_100) == 'Laundry100'] <- 'Laundry'
names(pcd_test_100)[names(pcd_test_100) == 'HVAC100'] <- 'HVAC'
names(pcd_test_100)[names(pcd_test_100) == 'Others100'] <- 'Others'

#pctgPalette <- c("#ffec26","#865667","#c6a15b","#9fcc2e")
pctgPalette <- c("#db6855","#b12308","#da4d0b","#612205")

pcd_test_100 %>% gather(Measure,Percentage,Kitchen:Others) %>% group_by(mes=floor_date(DateTime,"month"),Measure) %>% summarize(PourCent = mean(Percentage,na.rm=TRUE)) %>% ggplot(aes(x=mes,y=PourCent,fill=Measure))+geom_bar(stat="identity")+xlab("Month")+ylab("Usage of Energy (%)") + scale_fill_manual(values = pctgPalette)
#pcd_test_100 %>% gather(Measure,Percentage,Kitchen100:Others100) %>% group_by(mes=floor_date(DateTime,"month"),Measure) %>% summarize(PourCent = mean(Percentage,na.rm=TRUE)) %>% ggplot(aes(x=mes,y=PourCent,fill=Measure))+geom_bar(stat="identity")+xlab("Month")+ylab("Usage of Energy (%)")
#pcd_test_100 %>% gather(Measure,Percentage,Kitchen100:Others100) %>% group_by(mes=floor_date(DateTime,"month"),Measure) %>% summarize(PourCent = mean(Percentage,na.rm=TRUE)) %>% ggplot(aes(x=mes,y=PourCent,fill=Measure))+geom_bar(stat="identity")+xlab("Month")+ylab("Usage of Energy (%)")+scale_x_datetime(labels = date_format("%b%Y",tz="GMT"))

#see how does it vary during a year... Not only percentages, also totals
pcd_test_tot <- pcd_hourly %>% filter(DateTime >= "2007-01-01" & DateTime < "2008-01-01")
pcd_test_tot <- mutate(pcd_test_tot,trimes = as.factor(quarter(DateTime,with_year = TRUE)))
pcd_test_tot %>% gather(Measure,Power,TotalActive:Others) %>% group_by(mes=floor_date(DateTime,"month"),Measure) %>% summarize(TotalPower = sum(Power,na.rm=TRUE)) %>% ggplot(aes(x=mes,y=TotalPower,group=Measure))+geom_line(aes(color=Measure))+xlab("Month")+ylab("Total Energy (Wh)")

#### Seasonal performance during 1 year for different submeters ####
# Create the new dataset and add new columns
pcd_seasonal <- pcd_hourly %>% filter(DateTime >= "2007-01-01" & DateTime < "2008-01-01") %>% mutate(Season=as.factor(quarter(DateTime)),SeasonDay=difftime(floor_date(DateTime,"day"),floor_date(DateTime,"quarter"),tz="GMT",units="days")) #%>% select(-Kitchen:-Others)
pcd_seasonal$SeasonDay <- as.integer(pcd_seasonal$SeasonDay)
pcd_seasonal$SeasonDay <- as.factor(pcd_seasonal$SeasonDay + 1)

# Modify Values of Quarters just to have season names -- remember to load the "plyr" package and detach after use!!!
library("plyr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.0")
pcd_seasonal$Season <- revalue(pcd_seasonal$Season,c("1"="Winter","2"="Spring","3"="Summer","4"="Autumn"))
detach("package:plyr", unload=TRUE)

# Dismiss the DateTime column and rearrange the rest of columns (It is ok to do it twice, duplicated!!)
pcd_seasonal$DateTime <- NULL
pcd_seasonal <- pcd_seasonal[,c(ncol(pcd_seasonal), 1:(ncol(pcd_seasonal)-1))]
pcd_seasonal <- pcd_seasonal[,c(ncol(pcd_seasonal), 1:(ncol(pcd_seasonal)-1))]

# And now, plot what do you want
pcd_seas_summarized <- pcd_seasonal %>% group_by(Season,SeasonDay) %>% summarize(KitchenCons = mean(Kitchen,na.rm=TRUE),HVACCons = mean(HVAC,na.rm=TRUE)) 
pcd_seas_summarized %>% ggplot(aes(x=SeasonDay,y=KitchenCons,group=Season))+geom_line(aes(color=Season))+xlab("Month")+ylab("Total Energy (Wh)")


#### Seasonal performance during the whole period for different submeters ####
# Create the new dataset and add new columns
pcd_seasonal_whole <- pcd_hourly %>% filter(DateTime >= "2007-01-01") %>% mutate(Season=as.factor(quarter(DateTime))) %>% mutate(day_tag = format(DateTime, "%Y/%m/%d"))

levels(pcd_seasonal_whole$Season) <- c("Winter", "Spring", "Summer", "Autumn")

# Dismiss the DateTime column and rearrange the rest of columns (It is ok to do it twice, duplicated!!)
pcd_seasonal_whole$DateTime <- NULL
pcd_seasonal_whole$Others <- NULL
pcd_seasonal_whole <- pcd_seasonal_whole[,c(ncol(pcd_seasonal_whole), 1:(ncol(pcd_seasonal_whole)-1))]
pcd_seasonal_whole <- pcd_seasonal_whole[,c(ncol(pcd_seasonal_whole), 1:(ncol(pcd_seasonal_whole)-1))]

# And now, plot what do you want
pcd_swh_summarized <- pcd_seasonal_whole %>% group_by(day_tag,Season) %>% summarize(TotalDailyCons = sum(TotalActive,na.rm=TRUE),KitchenDailyCons = sum(Kitchen,na.rm=TRUE),LaundryDailyCons = sum(Laundry,na.rm=TRUE),HVACDailyCons = sum(HVAC,na.rm=TRUE)) 
pcd_swh_summarized$day_tag <- NULL
pcd_swh_summarized$TotalDailyCons <- NULL

pcd_swh_summarized <- pcd_swh_summarized %>% group_by(Season) %>% summarize(KitchenMeanSeas = mean(KitchenDailyCons,na.rm = TRUE)/1000,LaundryMeanSeas = mean(LaundryDailyCons,na.rm = TRUE)/1000,HVACMeanSeas = mean(HVACDailyCons,na.rm = TRUE)/1000)
pcd_swh_summarized <- pcd_swh_summarized %>% gather(Submeter,AvgDayPower,KitchenMeanSeas:HVACMeanSeas)
pcd_swh_summarized$Submeter <- as.factor(pcd_swh_summarized$Submeter)
levels(pcd_swh_summarized$Submeter) <- c("HVAC", "Kitchen", "Laundry")

#seasonsPalette <- c("#db6855","#b12308","#da4d0b","#612205")
seasonsPalette <- c("#29a3a3","#00b300","#e6b800","#b32200")

pcd_swh_summarized %>% ggplot(aes(fill=Season, y=AvgDayPower, x=Submeter)) + 
  geom_bar(position="dodge", stat="identity") + scale_fill_manual(values = seasonsPalette) +
  ylab("Energy Consumption Daily Average [kWh]") +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank())

#Let's try to do something awesome!!!
pcd_swh_summ_ring <- pcd_swh_summarized

pcd_swh_summ_ring %>% group_by(Submeter,Season) %>% mutate()

#### Weekly performance, by submeter and total, each day of the week. ####
# Create the new dataset and add new columns
pcd_weekly_whole <- pcd_hourly %>% filter(DateTime >= "2007-01-01") %>% mutate(DayWeek=as.factor(wday(DateTime))) %>% mutate(day_tag = format(DateTime, "%Y/%m/%d"))
#levels(pcd_weekly_whole$DayWeek) <- levels(pcd_weekly_whole$DayWeek)[c(2:7,1)]
levels(pcd_weekly_whole$DayWeek) <- c("Sun","Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
pcd_weekly_whole$DayWeek <- ordered(pcd_weekly_whole$DayWeek, levels = c("Mon","Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

# Dismiss the DateTime column and rearrange the rest of columns (It is ok to do it twice, duplicated!!)
pcd_weekly_whole$DateTime <- NULL
pcd_weekly_whole$Others <- NULL
pcd_weekly_whole <- pcd_weekly_whole[,c(ncol(pcd_weekly_whole), 1:(ncol(pcd_weekly_whole)-1))]
pcd_weekly_whole <- pcd_weekly_whole[,c(ncol(pcd_weekly_whole), 1:(ncol(pcd_weekly_whole)-1))]

pcd_weekly_whole$TotalActive <- NULL

# And now, plot what do you want
pcd_wewh_summarized <- pcd_weekly_whole %>% group_by(day_tag,DayWeek) %>% summarize(KitchenDailyCons = sum(Kitchen,na.rm=TRUE),LaundryDailyCons = sum(Laundry,na.rm=TRUE),HVACDailyCons = sum(HVAC,na.rm=TRUE)) 
pcd_wewh_summarized$day_tag <- NULL

pcd_wewh_summarized <- pcd_wewh_summarized %>% group_by(DayWeek) %>% summarize(KitchenMeanSeas = mean(KitchenDailyCons,na.rm = TRUE),LaundryMeanSeas = mean(LaundryDailyCons,na.rm = TRUE),HVACMeanSeas = mean(HVACDailyCons,na.rm = TRUE))
pcd_wewh_summarized <- pcd_wewh_summarized %>% gather(Submeter,AvgDayPower,KitchenMeanSeas:HVACMeanSeas)
pcd_wewh_summarized$Submeter <- as.factor(pcd_wewh_summarized$Submeter)
levels(pcd_wewh_summarized$Submeter) <- c("HVAC", "Kitchen", "Laundry")

#seasonsPalette <- c("#29a3a3","#00b300","#e6b800","#b32200")
weekPalette <- c("#e4b425","#e46524","#c2440a","#792b06","#755b0a","#374706","#042f1a")

pcd_wewh_summarized %>% ggplot(aes(fill=DayWeek, y=AvgDayPower, x=Submeter)) + 
  geom_bar(position="dodge", stat="identity") + scale_fill_manual(values = weekPalette) +
  ylab("Energy Consumption Daily Average [Wh]") +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank())

#### Performance during a couple of weeks. Take a sample week -- May 2007 ####
pcd_may_07 <- pcd_hourly %>% filter(DateTime >= "2007-05-01" & DateTime < "2007-06-01") %>% select(-Others)

pcd_may_07_barplot <- pcd_may_07 %>% select(-Kitchen:-HVAC) 
pcd_may_07_barplot <- mutate(pcd_may_07_barplot,dayWeek = as.factor(wday(DateTime)))
pcd_may_07_barplot <- pcd_may_07_barplot %>% group_by(day=floor_date(DateTime,"day"),dayWeek) %>% summarise(TotalActivePower = sum(TotalActive,na.rm=TRUE))

pcd_may_07_Plot <- pcd_may_07 %>% select(-TotalActive) %>% gather(Submeter,Power,Kitchen:HVAC) %>% group_by(day=floor_date(DateTime,"day"),Submeter) %>% summarise(TotalSubmeter = sum(Power,na.rm=TRUE))

#Plot both in a same graph
#ggplot(pcd_may_07_Plot,aes(x=day,y=TotalSubmeter,group=Submeter))+geom_line(aes(color=Submeter))+xlab("Date")+ylab("Total Daily Energy (Wh)")
ggplot() + geom_bar(data = pcd_may_07_barplot, aes(x = day, y = TotalActivePower, fill=wday(day)), stat="identity") + geom_line(data=pcd_may_07_Plot,aes(x=day,y=TotalSubmeter,group=Submeter))+xlab("Date")+ylab("Total Daily Energy (Wh)")

#### Plot consumption, divided by submeter, in a given day ####
pcd_oneDay <- pcd_hourly %>% filter(DateTime >= "2009-05-22" & DateTime < "2009-05-23")
pcd_oneDay <- pcd_hourly %>% filter(DateTime >= "2009-11-11" & DateTime < "2009-11-12")
#pcd_oneDay <- pcd_hourly %>% filter(as.Date(DateTime,tz="UTC") == "2007-01-01")

pcd_oneDay$TotalActive <- NULL
pcd_oneDay$Others <- NULL

pcd_oneDay %>% gather(Submeter,TotalHour,Kitchen:HVAC) %>%
  ggplot(aes(x=DateTime,y=TotalHour)) + geom_line(aes(color=Submeter))

#### Daily consumption per hour averaged in the whole period ####
pcd_daily_avg <- pcd_hourly %>% mutate(dayWeek = as.factor(wday(DateTime)), timeHour = as.numeric(as.character(hour(DateTime))))
#pcd_daily_avg <- pcd_hourly %>% mutate(dayWeek = as.factor(wday(DateTime)), timeHour = as.numeric(hour(DateTime)))

#Rename factors of days of the week and reorder
levels(pcd_daily_avg$dayWeek) <- c("Sun","Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
pcd_daily_avg$dayWeek <- ordered(pcd_daily_avg$dayWeek, levels = c("Mon","Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
#Remove DateTime, TotalActive and Others columns. Reorder dayWeek and timeHour
pcd_daily_avg$DateTime <- NULL
pcd_daily_avg$TotalActive <- NULL
pcd_daily_avg$Others <- NULL
pcd_daily_avg <- pcd_daily_avg[,c(ncol(pcd_daily_avg), 1:(ncol(pcd_daily_avg)-1))]
pcd_daily_avg <- pcd_daily_avg[,c(ncol(pcd_daily_avg), 1:(ncol(pcd_daily_avg)-1))]

#Daily Hour average for each submeter
pcd_daily_summ <- pcd_daily_avg %>% group_by(dayWeek,timeHour) %>% summarise(Kitchen = mean(Kitchen,na.rm = TRUE),Laundry = mean(Laundry,na.rm = TRUE),HVAC = mean(HVAC,na.rm = TRUE))
#pcd_daily_summ$timeHour <- as.POSIXct(strptime(pcd_daily_summ$timeHour,format = "%H:%M"))
#Plot them!!!
pcd_daily_summ_gt <- pcd_daily_summ %>% gather(Submeter,TotalHour,Kitchen:HVAC)
dailyPalette <- c("#4d4dff","#ff0000","#009933")

#pcd_daily_summ_gt <- filter(pcd_daily_summ_gt,dayWeek == "Thu")
ggplot(pcd_daily_summ_gt,aes(x=timeHour,y=TotalHour)) + geom_line(aes(color=Submeter)) +
  facet_grid(.~dayWeek) + scale_colour_manual(values = dailyPalette) +
  theme(axis.title.x = element_blank(),legend.title = element_blank(),axis.text = element_blank(),axis.ticks.x = element_blank()) +
  ylab("Average hourly consumption [Wh]")

#### Analysis of three different TIME SERIES - yearly, weekly and daily ####
# Create appropriate datasets

# Create the timeseries objects
# Plot them!

# Forecasting time series - getting trend and forecast

# Decomposing time series into seasonal, random and trend components

#### First dataset :: yearly by month, considering average daily consumption, from 1st jan 2007 ####
# Step 1: group by day to get the daily totals
pcd_ts_yearly <- powerConsumptionMasterData %>% filter(DateTime >= "2007-01-01") %>% mutate(day_tag = format(DateTime, "%d/%m/%Y"))
pcd_ts_yearly <- select(pcd_ts_yearly,-DateTime:-Time,-Global_reactive_power:-Global_intensity)
pcd_ts_yearly_tmp <- pcd_ts_yearly[-6]

pcd_ts_yearly_agg <- aggregate(pcd_ts_yearly_tmp, by=list(pcd_ts_yearly$day_tag), FUN=sum, na.rm=TRUE)
colnames(pcd_ts_yearly_agg)[1] <- "DateTime"
pcd_ts_yearly_agg$DateTime <- strptime(pcd_ts_yearly_agg$DateTime, "%d/%m/%Y")

pcd_ts_yearly_agg <- pcd_ts_yearly_agg[order(pcd_ts_yearly_agg$DateTime), ]
pcd_ts_yearly_agg$DateTime <- as.POSIXct(pcd_ts_yearly_agg$DateTime)

#Step 2: From the Daily ordered dataset, group by months and get the averages
pcd_ts_yearly_agg <- pcd_ts_yearly_agg %>% mutate(month_tag = format(DateTime, "%Y/%m")) %>% select(-DateTime)
pcd_ts_yearly_tmp <- pcd_ts_yearly_agg[-6]

pcd_ts_yearly_agg2 <- aggregate(pcd_ts_yearly_tmp, by=list(pcd_ts_yearly_agg$month_tag), FUN=mean, na.rm=TRUE)
colnames(pcd_ts_yearly_agg2)[1] <- "DateTime"
#pcd_ts_yearly_agg2$DateTime <- strptime(pcd_ts_yearly_agg2$DateTime, "%m/%Y")

#Step 3: Dismiss auxiliar variables
pcd_ts_yearly <- pcd_ts_yearly_agg2
remove(pcd_ts_yearly_agg,pcd_ts_yearly_agg2,pcd_ts_yearly_tmp)

#### Second dataset :: Weekly trends, take a period of 6 weeks and consider daily total ####
# Period from 5 may 2008 to 15 june 2008
pcd_ts_weekly <- powerConsumptionMasterData %>% filter(DateTime >= "2008-05-05" & DateTime < "2008-06-16") %>% mutate(day_tag = format(DateTime, "%Y/%m/%d"))
pcd_ts_weekly <- select(pcd_ts_weekly,-DateTime:-Time,-Global_reactive_power:-Global_intensity)
pcd_ts_weekly_tmp <- pcd_ts_weekly[-6]

pcd_ts_weekly_agg <- aggregate(pcd_ts_weekly_tmp, by=list(pcd_ts_weekly$day_tag), FUN=sum, na.rm=TRUE)
colnames(pcd_ts_weekly_agg)[1] <- "DateTime"

pcd_ts_weekly <- pcd_ts_weekly_agg
remove(pcd_ts_weekly_agg,pcd_ts_weekly_tmp)

#### Third dataset :: Daily trends, take a period of 5 days and consider hourly total ####
# Period from 2 to 20 of november 2009
pcd_ts_daily <- powerConsumptionMasterData %>% filter(DateTime >= "2009-11-02" & DateTime < "2009-11-21") %>% mutate(dayweek = wday(DateTime),hour_tag = format(DateTime, "%Y/%m/%d %H"))
pcd_ts_daily <- filter(pcd_ts_daily,dayweek %in% 2:6)
pcd_ts_daily <- select(pcd_ts_daily,-DateTime:-Time,-Global_reactive_power:-Global_intensity,-dayweek)
pcd_ts_daily_tmp <- pcd_ts_daily[-6]

pcd_ts_daily_agg <- aggregate(pcd_ts_daily_tmp, by=list(pcd_ts_daily$hour_tag), FUN=sum, na.rm=TRUE)
colnames(pcd_ts_daily_agg)[1] <- "DateTime"

pcd_ts_daily <- pcd_ts_daily_agg
remove(pcd_ts_daily_agg,pcd_ts_daily_tmp)

#### Finally, create the timeseries objects & Plot them! ####
yearlyTimeSerie <- ts(pcd_ts_yearly$TotalActive,frequency=12,start=c(2007))
plot(yearlyTimeSerie)

weeklyTimeSerie <- ts(pcd_ts_weekly$TotalActive,frequency=7,start=c(1))
#weeklyTimeSerie <- ts(pcd_ts_weekly$TotalActive,frequency=365,start=c(2008,126))
#weeklyTimeSerie <- ts(pcd_ts_weekly$TotalActive,frequency=365,start=c(2008+(126/365)))
#weeklyTimeSerie <- ts(pcd_ts_weekly$TotalActive,frequency=365,start=c(decimal_date(ymd("2008-05-05"))))
plot(weeklyTimeSerie)

dailyTimeSerie <- ts(pcd_ts_daily$TotalActive,frequency=24,start=c(0))
#dailyTimeSerie <- ts(pcd_ts_daily$TotalActive,frequency=120,start=c(0)) #For weekly period instead od daily
plot(dailyTimeSerie)

#### Forecasting time series - getting trend and forecast ####
yearlyLM <- tslm(yearlyTimeSerie ~ trend + season)
yearlyForecast <- forecast(yearlyLM, h=12)
plot(yearlyForecast) #h=12 involves a period of 1 year (12 months)

weeklyLM <- tslm(weeklyTimeSerie ~ trend + season)
weeklyForecast <- forecast(weeklyLM, h=7)
plot(weeklyForecast) #h=7 involves a period of 1 week (7 days)

dailyLM <- tslm(dailyTimeSerie ~ trend + season)
dailyForecast <- forecast(dailyLM, h=24)
#dailyForecast <- forecast(dailyLM, h=120) #To predict a weekly period, 24h*5[workdays]
plot(dailyForecast) #h=24 involves a period of 1 day (24 hours)

#### Decomposing time series into seasonal, random and trend components ####
yearlySTL = stl(yearlyTimeSerie, s.window="periodic")
yearlySTL = stl(yearlyTimeSerie, s.window=9, t.window=21)
plot(yearlySTL, col = "red", main="Yearly Consumption (2007 - 2010)")
ggplot(yearlySTL)
#plot(yearlySTL[,"trend"])

weeklySTL = stl(weeklyTimeSerie, s.window="periodic")
weeklySTL = stl(weeklyTimeSerie, s.window=9, t.window=21)
plot(weeklySTL, col = "red", main="Weekly Consumption (May-June 2008)")

dailySTL = stl(dailyTimeSerie, s.window="periodic")
dailySTL = stl(dailyTimeSerie, s.window=13,t.window = 27)
plot(dailySTL, col = "red", main="Daily Consumption on working days (November 2009)")
dailyDecom = decompose(dailyTimeSerie)
plot(dailyDecom)

#### Experimenting Holt-Winters modelling ####
# First step, daily timeseries - play with {alpha, beta, gamma} params in function
#test_hw <- HoltWinters(dailyTimeSerie)
#test_hw <- HoltWinters(dailyTimeSerie,alpha = 0.4,beta = 0.001,gamma = 0.4) #Close to optimal...
# USE ts NON PERIODIC!!!
test_hw_y <- HoltWinters(yearlyTimeSerie)
plot(forecast(test_hw_y,h=24),col="#a82424",main="Holt-Winters Prediction for 2 years")
test_hw_w <- HoltWinters(weeklyTimeSerie)
plot(forecast(test_hw_w,h=14),col="#a82424",main="Holt-Winters Prediction for 2 weeks")
test_hw_d <- HoltWinters(dailyTimeSerie)
plot(forecast(test_hw_d,h=48),col="#a82424",main="Holt-Winters Prediction for 2 workdays")
