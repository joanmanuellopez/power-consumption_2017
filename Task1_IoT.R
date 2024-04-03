#### Load data into R ####
powerConsumptionDataset <- read.csv("household_power_consumption.txt", sep=";", dec = ".")
View(powerConsumptionDataset)

#### Create new column for date/time together and place it in the right place ####
powerConsumptionDataset <-cbind(powerConsumptionDataset,paste(powerConsumptionDataset$Date,powerConsumptionDataset$Time), stringsAsFactors=FALSE)
colnames(powerConsumptionDataset)[10] <-"DateTime"
powerConsumptionDataset <- powerConsumptionDataset[,c(ncol(powerConsumptionDataset), 1:(ncol(powerConsumptionDataset)-1))]
head(powerConsumptionDataset)

#### Converting date and Time format ####
powerConsumptionDataset$DateTime <- strptime(powerConsumptionDataset$DateTime, "%d/%m/%Y %H:%M:%S")
powerConsumptionDataset$Date <- as.Date(powerConsumptionDataset$Date, "%d/%m/%Y")

powerConsumptionDataset$DateTime <- as.POSIXct(powerConsumptionDataset$DateTime)

#### Keep a backup copy of the dataset ####
pcdBackup <- powerConsumptionDataset

#### Discovering NAs ####
pcd_na <- powerConsumptionDataset[rowSums(is.na(powerConsumptionDataset)) > 0,]
ggplot(pcd_na,aes(x=Date))+geom_bar()
ggplot(subset(pcd_na,Date < "2007-05-01" & Date >= "2007-04-01"),aes(x=Date,y = (..count..)/60))+geom_bar()+ylab("Total Hours")

#### Tidy Dataset ####
# Step 1. Change Column Names
names(powerConsumptionDataset)[names(powerConsumptionDataset) == 'Sub_metering_1'] <- 'Kitchen'
names(powerConsumptionDataset)[names(powerConsumptionDataset) == 'Sub_metering_2'] <- 'Laundry'
names(powerConsumptionDataset)[names(powerConsumptionDataset) == 'Sub_metering_3'] <- 'HeaterCooler'

#Step 2. Create new column for energy not sub-metered
powerConsumptionDataset <- powerConsumptionDataset %>% mutate(Others = Global_active_power*1000/60 - Kitchen - Laundry - HeaterCooler)

#Step 3. Create new dataset with gathered info according submeter -- it keeps the powerConsumptionDataset
#Dismiss this step for the global dataset since it would be too large...
#pcdGathered <- powerConsumptionDataset %>% gather(Submeter,Energy_Wh,Kitchen:Others)
#long_DF <- DF %>% gather(Quarter, Revenue, Qtr.1:Qtr.4)

#### Analyse total energy consumption per month, on the whole 47 months period ####
# Load library lubridate to deal with dates
library("lubridate", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.0")

#Create a new dataset for this work
pcdTotal4y <- powerConsumptionDataset

#Summarise the new dataset to get the total monthly consumption
pcdTotal4y %>% group_by(month=floor_date(Date, "month")) %>% summarize(TotalActive=sum(Global_active_power*1000/60,na.rm=TRUE))

#Summarise and plot in one instruction
pcdTotal4y %>% group_by(month=floor_date(Date, "month")) %>% summarize(TotalActive=sum(Global_active_power/60,na.rm=TRUE)) %>% ggplot(aes(x=month,y=TotalActive))+geom_bar(stat="identity")+xlab("Month")+ylab("Total Energy (kWh)")


#### Analyse weekly behaviour during 3 months ####
# Analysing march, april, may 2008
pcd_av08 <- subset(powerConsumptionDataset,Date < "2008-06-01" & Date >= "2008-03-01")
pcd_av08 %>% group_by(day=floor_date(Date, "day")) %>% summarize(TotalActive=sum(Global_active_power/60,na.rm=TRUE)) %>% ggplot(aes(x=day,y=TotalActive,fill=wday(day)))+geom_bar(stat="identity")+xlab("Day")+ylab("Total Energy (kWh)")

#### Analyse averaged daily behaviour during 4 years, grouping by weekday and time (i.e Monday 00:00 Monday 00:01 ... Sunday 23:59) ####
#Create a new copy of the dataset and transform units of Global_active_power attribute to [Wh]
pcdWeekly <- powerConsumptionDataset
pcdWeekly$Global_active_power <- pcdWeekly$Global_active_power * 100 / 6

#Create a new column that joins dayweek + daytime. New library needed to perform padding
#Pay attention to dayweeks!! 1=Sunday 2=Monday and so on
#pcdWeekly <- pcdWeekly %>% mutate(dwHour = paste(wday(Date),str_pad(Time,5,pad = "0"),sep = "_"))
pcdWeekly <- pcdWeekly %>% mutate(dayWeek = wday(Date))

#Dismiss unnecessary attributes in order to save memory space
pcdWeekly$Global_reactive_power <- NULL
pcdWeekly$Voltage <- NULL
pcdWeekly$Global_intensity <- NULL

#Change column name, for easiness of use
names(pcdWeekly)[names(pcdWeekly) == 'Global_active_power'] <- 'TotalActive'

#Gather submetering columns into single column
pcdWeeklyGath <- pcdWeekly %>% gather(Metered,Energy_Wh,TotalActive:Others)

#Create interesting information for our analysis
pcdWeekly_grouped <- group_by(pcdWeeklyGath, dayWeek, Time, Metered)
pcdWeeklyAvg <- summarise(pcdWeekly_grouped,Energy_mean = mean(Energy_Wh,na.rm=TRUE))

#Plot information!! (just only for one day, {1=sunday  2=monday  ...})
ggplot(pcdWeeklyAvg[c(which(pcdWeeklyAvg$dayWeek==1)),],aes(x=Time,y=Energy_mean,group=Metered)) + geom_line(aes(color=Metered)) + ylab("Energy Mean [Wh]")
