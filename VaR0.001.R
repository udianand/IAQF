#VAR model - 0.001

#setwd("C:/Users/XingBill/Desktop/IAQFfolder/IAQF")


#load workspace
#load(file="VaR0.0001.RData") #Udit: Do Not Uncomment.

##Load Data from the GUI. or use command below

#CPI = read.csv("<FileName>.txt",header = TRUE)
#FFR = read.csv("<FileName>.txt",header = TRUE)
#UNEMP = read.csv("<FileName>.txt",header = TRUE)
#All data is monthly

##Data Definition
#CPI: All Urban Consumers - Seasonally Unadjusted USA All items  - Non Adj. CPI\n  - CPI
#FFR: Federal Funds Effective Rate USA  - %"
#UNEMP: Unemployment Rate Non-Seasonally Adjusted USA  - %

CPI = CPI_All_items 
FFR = FFR_Monthly #(Note: you might have to change the '_' to '-' depending upon your method
                  #of loading data)
UNEMP = Unemployment_rate

##Data Cleaning
#------------#
#------------#

#CPI Data
#--------#
CPI = CPI[-1,] #The first row contained text varibales
names(CPI)
names(CPI)[2]<-"CpiSeaAdj" #Change colnames
CPI = as.data.frame(CPI) #Convert the list into a dataframe.

##Convert any NA data to median and Infinity to 0
CPI$CPISeaAdj [is.na(CPI$CPISeaAdj)] = median(CPI$CPISeaAdj,na.rm = "True")
CPI$CPISeaAdj[is.infinite(CPI$CPISeaAdj)] = 0

##Start the data from 199001
CPI = CPI[order(CPI$Time),]

head(CPI)
summary(CPI)

#FFR Data
#--------#
names(FFR)
names(FFR)[2]<-"InterestRates" #Change colnames
FFR = as.data.frame(FFR) #Convert the list into a dataframe.

##Convert any NA data to median and Infinity to 0
FFR$InterestRates [is.na(FFR$InterestRates)] = median(FFR$InterestRates,na.rm = "True")
FFR$InterestRates[is.infinite(FFR$InterestRates)] = 0

##Start the data from 199001
FFR = FFR[order(FFR$Time),]

head(FFR)
summary(FFR)

#UNEMP Data
#--------#
names(UNEMP)
names(UNEMP)[2]<-"UnempRate" #Change colnames
UNEMP = as.data.frame(UNEMP) #Convert the list into a dataframe.

##Convert any NA data to median and Infinity to 0
UNEMP$UnempRate [is.na(UNEMP$UnempRate)] = median(UNEMP$UnempRate,na.rm = "True")
UNEMP$UnempRate[is.infinite(UNEMP$UnempRate)] = 0

##Start the data from 199001
UNEMP = UNEMP[order(UNEMP$Time),]

head(UNEMP)
summary(UNEMP)

#save.image(file="VaR0.0001.RData") Udit: Do not Uncomment
##*********************************************************##

##Convert into a Time Series.
#---------------------------#

#install.packages("quantmod")
#install.packages("zoo")

library(quantmod)
library(zoo)

#UNEMP
UNEMP$Time = as.yearmon(as.character(UNEMP$Time), "%Y%m")
UNEMP$Time = as.Date(UNEMP$Time)
head(UNEMP)

UNEMP.xts<-xts(UNEMP$UnempRate, order.by=UNEMP$Time) 
chartSeries(UNEMP.xts)

#FFR
FFR$Time <- as.yearmon(as.character(FFR$Time), "%Y%m")
FFR$Time = as.Date(FFR$Time)
head(FFR)

FFR.xts<-xts(FFR$InterestRates, order.by=FFR$TS) 
chartSeries(FFR.xts)

#CPI
CPI$TS = as.yearmon(as.character(CPI$Time), "%Y%m")
CPI$TS = as.Date(CPI$TS)
head(CPI)

CPI.xts<-xts(CPI$CpiSeaAdj, order.by=CPI$TS) 
chartSeries(CPI.xts)

##Check for Stationarity
#-----------------------#
##Since a prereq for VAR is stationarity. We run the Dickie Fuller test for stationarity.

#Perform the Dickie Fuller Test for Unit Roots
adf.test()
adf.test(y.DGS10.weekly)
adf.test(y.DGS10.monthly)

#Since the p-values do not fall below  the standard critical values 0.01 or 0.05
#WE CANNOT REJECT THE NULL HYPOTHESIS

#The p-value is the probability (assuming the null hypothesis is true) of realizing
#a test statistic as extreme as that computed for the input series. Smaller
#values (i.e., lower probabilities) provide stronger evidence against the null hyptohesis.
#The p-value decreases as the periodicity of the data shortens. This suggests
#that the time-series structure in the series DGS10 may be stronger at higher
#frequencies.


