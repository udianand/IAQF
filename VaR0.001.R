#VAR model - 0.001

#load workspace
#load(file="VaR0.0001.RData") #Udit: Do Not Uncomment.

##Load Data from the GUI. or use command below

#CPI = read.csv("<FileName>.txt",header = TRUE)
#FFR = read.csv("<FileName>.txt",header = TRUE)
#UNEMP = read.csv("<FileName>.txt",header = TRUE)
#All data is monthly

##Data Definition
##Notice: Not all the data are available since 1990, detailed timing will be illustrated in the file

#CPI: All Urban Consumers - Seasonally Unadjusted USA All items  - Non Adj. CPI\n  - CPI
#FFR: Federal Funds Effective Rate USA  - %"
#UNEMP: Unemployment Rate Non-Seasonally Adjusted USA  - %
#Crude_Oil: Crude Oil Price (Light Sweet Crude) USA  - Price - $/barrel
#Diffusion_Index: Diffusion Index 1 USA  - DI1 (50 is a benchmark)
#Dollar_Index: 1997=100
#Food_Index: Commodity Prices International Agr: Food  - 2010=100 nominal$
#Gold: Gold Fixing (Evening) International  - $/oz
#M1: Money Stock Measures M1 (Seas Adj) USA  - M1 - $
#M2: M2 - Seasonally Adjusted USA  - M2 - $
#Platium: Platinum Fixing (Evening) International  - $/oz
#Purchased_Manager_Index: Purchasing Managers' Index (Markit) United States  - Index
#S_P500: S&P 500 Index (US) - Close International  - Close
#Silver: Silver Fixing (Evening) International  - - $/oz
#TotalReserve: Total Reserves United States  - Current US$
#VIX: Fear Index (VIX) USA  - Close - #

CPI = CPI_All_items 
FFR = FFR_Monthly
UNEMP = Unemployment_rate
Crude_Oil = Crude_Oil_Price
Industrial_Production = Diffusion_Index
Dollar_Index = Dollar_Index_1997_100_
Food_index = Food_Index_Nominal_2010_100_
Gold = Gold_Fixing_Evening_
M1 = Money_Stock_M1
M2 = Money_Stock_M2
Platium = Platinum_Fixing_Evening_
Purchase_Manager_Index = PMI
S_P500 = S_P500
Silver = Silver_Fixing
TotalReserve = Total_Reserves
VIX = VIX

#Data Cleaning
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

#Crude Oil Data
#-------------#
Crude_Oil = Crude_Oil[-1, ] #The first row contained text varibales
names(Crude_Oil)
names(CPI)[2]<-"Price" #Change colnames
Crude_Oil = as.data.frame(CPI) #Convert the list into a dataframe.

##Convert any NA data to median and Infinity to 0
Crude_Oil$Price[is.na(Crude_Oil$Price)] = median(Crude_Oil$Price,na.rm = "True")
Crude_Oil$Price[is.infinite(Crude_Oil$Price)] = 0

##Start the data from 199001
Crude_Oil$Time = as.integer(Crude_Oil$Time) ##convert the character to int
Crude_Oil = Crude_Oil[order(Crude_Oil$Time),]

head(Crude_Oil)
summary(Crude_Oil)


#save.image(file="VaR0.0001.RData") Udit: Do not Uncomment
##*********************************************************##

#Convert into a Time Series.
#-------------------------#
#--------------------------#
library(quantmod)
library(zoo)

#UNEMP#
#------#
UNEMP$Time = as.yearmon(as.character(UNEMP$Time), "%Y%m")
UNEMP$Time = as.Date(UNEMP$Time)
head(UNEMP)

UNEMP.xts<-xts(UNEMP$UnempRate, order.by=UNEMP$Time) 
chartSeries(UNEMP.xts)

#FFR#
#----#
FFR$Time <- as.yearmon(as.character(FFR$Time), "%Y%m")
FFR$Time = as.Date(FFR$Time)
head(FFR)

FFR.xts<-xts(FFR$InterestRates, order.by=FFR$Time) 
chartSeries(FFR.xts)

#CPI
#---#
CPI$Time = as.yearmon(as.character(CPI$Time), "%Y%m")
CPI$Time = as.Date(CPI$Time)
head(CPI)

CPI.xts<-xts(CPI$CpiSeaAdj, order.by=CPI$Time) 
chartSeries(CPI.xts)


#source("DataCleaning.R")

##Stationarity Test
#*******************#
#*******************#
library(tseries)

dim(CPI.xts)
dim(FFR.xts)
dim(UNEMP.xts)

#Perform the Dickie Fuller Test for Unit Roots
adf.test(CPI.xts) #p-value = 0.546 (CANNOT REJECT NULL HYPOTHESIS)
adf.test(FFR.xts) #p-value = 0.01708 (CANNOT NULL HYPOTHESIS)
adf.test(UNEMP.xts) #p-value = 0.8873 (CANNOT REJECT NULL HYPOTHESIS)


##Difference the time series
adf.test(na.omit(diff(CPI.xts))) #p-value < 0.01 (REJECT NULL HYPOTHESIS)
adf.test(na.omit(diff(UNEMP.xts))) #p-value < 0.01 (REJECT NULL HYPOTHESIS)


CPI.xts.diff = na.omit(diff(CPI.xts))
UNEMP.xts.diff = na.omit(diff(UNEMP.xts))

#Difference twice
FFR.xts.diff  = na.omit(diff(FFR.xts))
FFR.xts.diff  = na.omit(diff(FFR.xts.diff))

##Plot the ACF and PACF
par(mfcol=c(2,1))
acf(CPI.xts.diff)
acf(CPI.xts.diff,type="partial")

par(mfcol=c(2,1))
acf(FFR.xts.diff)
acf(FFR.xts.diff,type="partial")

par(mfcol=c(2,1))
acf(UNEMP.xts.diff)
acf(UNEMP.xts.diff,type="partial")

##VAR MODEL
#----------#
#-----------#
library(vars)

#Consider focusing on 3 variables
varmod.001 = merge(UNEMP.xts.diff, FFR.xts.diff, CPI.xts.diff)

varmod.001 = na.omit(varmod.001)
#TODO: Check how to deal with 'NA' Data
#Currently, only two NA values in all of the data so we can omit

#Auto-Select the VAR Model, Ignoring the NA values
varmod.001.VAR.const<-VARselect(varmod.001, lag.max=12, type="const")

#Print out the VAR order identified by different information criteria
varmod.001.VAR.const$selection

#Fit the VAR model corresponding to the Schwarz Criterion (SC) which is the BIC
varmod.001.VAR.const.0 = VAR(varmod.001, p=varmod.001.VAR.const$selection[3],type="const")
options(show.signif.stars=FALSE)
summary(varmod.001.VAR.const.0)

##Plot the Impulse Function
#--------------------------#
#--------------------------#

#When FFR rises:
plot(irf(varmod.001.VAR.const.0, impulse="FFR.xts.diff"))


#QUARTERLY DATA#
#--------------#
#--------------#

##Convert to Quarterly data and taking the closing prices
CPI.xts.q = to.quarterly(CPI.xts)
CPI.xts.q = CPI.xts.q$CPI.xts.Close

FFR.xts.q = to.quarterly(FFR.xts)
FFR.xts.q = FFR.xts.q$FFR.xts.Close

UNEMP.xts.q = to.quarterly(UNEMP.xts)
UNEMP.xts.q = UNEMP.xts.q$UNEMP.xts.Close

##Check for Stationarity
adf.test(CPI.xts.q)
adf.test(UNEMP.xts.q)
adf.test(FFR.xts.q)

##Difference the time series
adf.test(na.omit(diff(CPI.xts.q))) #p-value < 0.01 (REJECT NULL HYPOTHESIS)
adf.test(na.omit(diff(UNEMP.xts.q))) #p-value < 0.01 (REJECT NULL HYPOTHESIS)

CPI.xts.q.diff = na.omit(diff(CPI.xts.q))
UNEMP.xts.q.diff = na.omit(diff(UNEMP.xts.q))

#Difference UNEMP Twice
adf.test(na.omit(diff(UNEMP.xts.q.diff)))
UNEMP.xts.q.diff = na.omit(diff(UNEMP.xts.q))

##VAR MODEL FOR QUARTERLY
#----------#
#-----------#
library(vars)

#Consider focusing on 3 variables
varmod.001.q = merge(UNEMP.xts.q.diff, FFR.xts.q, CPI.xts.q.diff)

varmod.001.q = na.omit(varmod.001.q)
#TODO: Check how to deal with 'NA' Data

#Auto-Select the VAR Model, Ignoring the NA values
varmod.001.q.VAR.const<-VARselect(varmod.001.q, lag.max=12, type="const")

#Print out the VAR order identified by different information criteria
varmod.001.q.VAR.const$selection

#Fit the VAR model corresponding to the Schwarz Criterion (SC) which is the BIC
varmod.001.q.VAR.const.0 = VAR(varmod.001.q, p=varmod.001.q.VAR.const$selection[3],type="const")
options(show.signif.stars=FALSE)
summary(varmod.001.q.VAR.const.0)

##Plot the Impulse Function
#--------------------------#
#--------------------------#

#When FFR rises:
plot(irf(varmod.001.q.VAR.const.0, impulse="FFR.xts.Close"))
