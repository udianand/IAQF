#VAR model - 0.001

setwd("C:/Users/XingBill/Desktop/IAQFfolder/IAQF")


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
#Oil: Crude Oil Price (Light Sweet Crude) USA  - Price - $/barrel
#Industrial Production: Diffusion Index 1 USA  - DI1
#Dollar Index: 1997 = 100
#Food Price_index = Commodity Prices International Agr: Food  - 2010=100 nominal$
#Gold: Gold Fixing (Evening) International- $/oz
#M1: Money Stock Measures M1 (Seas Adj) USA  - M - $
#M2: M2 - Seasonally Adjusted USA  - M2 - $
#Platinum: Platinum Fixing (Evening) International  - $/oz
#PMI: Purchasing Managers' Index (Markit) United States  - Index
#S&P500: S&P 500 Index (US) - Close International  - Close - $


CPI = CPI_All_items 
FFR = FFR_Monthly #(Note: you might have to change the '_' to '-' depending upon your method
                  #of loading data)
UNEMP = Unemployment_rate
Oil = Crude_Oil_Price
Industrial = Diffusion_Index
Dollar_Index = Dollar_Index_1997_100_
Food = Food_Index_Nominal_2010_100_
Gold = Gold_Fixing_Evening_
M1 = Money_Stock_M1
M2 = Money_Stock_M2
Platinum = Platinum_Fixing_Evening_
PMI = PMI
S_P500 = S_P500
Silver = Silver_Fixing
Reserves = Total_Reserves
VIX = VIX

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
FFR = FFR[-1,]
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
#--------#
names(Oil)
Oil = Oil[-1,]
names(Oil)[2]<-"CrudeOilPrice" #Change colnames
Oil = as.data.frame(Oil) #Convert the list into a dataframe.
Oil$Time = as.integer(Oil$Time) #Convert the time from character to integer

##Convert any NA data to median and Infinity to 0
Oil$CrudeOilPrice [is.na(FFR$InterestRates)] = median(FFR$InterestRates,na.rm = "True")
Oil$CrudeOilPrice [is.infinite(FFR$InterestRates)] = 0

##Start the data from 199001
Oil = Oil[order(Oil$Time),]

head(Oil)
summary(Oil)

#Industrial Production Data
#--------#
names(Industrial)
Industrial = Industrial[-1, ]
names(Industrial)[2]<-"Industrial_Production_Index" #Change colnames
Industrial = as.data.frame(Industrial) #Convert the list into a dataframe.

##Convert any NA data to median and Infinity to 0
Industrial$Industrial_Production_Index [is.na(Industrial$Industrial_Production_Index)] = median(Industrial$Industrial_Production_Index,na.rm = "True")
Industrial$Industrial_Production_Index[is.infinite(Industrial$Industrial_Production_Index)] = 0

##Start the data from 199001
Industrial$Time = as.integer(Industrial$Time) #convert the character to integer
Industrial = Industrial[order(Industrial$Time),]

head(Industrial)
summary(Industrial)

#Dollar Index
#--------#
names(Dollar_Index)
Dollar_Index = Dollar_Index[-1, ]
names(Dollar_Index)[2]<-"Dollar_Index" #Change colnames
Dollar_Index = as.data.frame(Dollar_Index) #Convert the list into a dataframe.

##Convert any NA data to median and Infinity to 0
Dollar_Index$Dollar_Index [is.na(Dollar_Index$Dollar_Index)] = median(Dollar_Index$Dollar_Index,na.rm = "True")
Dollar_Index$Dollar_Index [is.infinite(Industrial$Industrial_Production_Index)] = 0

##Start the data from 199501, Daily data
Dollar_Index = Dollar_Index[order(Dollar_Index$DATE),]

head(Dollar_Index)
summary(Dollar_Index)

#Food Price Data
#--------#
names(Food)
Food = Food [-1, ]
names(Food)[2]<-"Food_priceindex" #Change colnames
Food = as.data.frame(Food) #Convert the list into a dataframe.

##Convert any NA data to median and Infinity to 0
Food$Food_priceindex [is.na(Food$Food_priceindex)] = median(Food$Food_priceindex,na.rm = "True")
Food$Food_priceindex[is.infinite(Food$Food_priceindex)] = 0

##Start the data from 199001
Food$Time = as.integer(Food$Time) #convert the character to integer
Food = Food[order(Food$Time),]

head(Food)
summary(Food)

#Gold price Data Daily
#--------#
names(Gold)
Gold = Gold[-1, ]
names(Gold)[2]<-"Gold_price" #Change colnames
Gold = as.data.frame(Gold) #Convert the list into a dataframe.

##Convert any NA data to median and Infinity to 0
Gold$Gold_price [is.na(Gold$Gold_price)] = median(Gold$Gold_price,na.rm = "True")
Gold$Gold_price[is.infinite(Gold$Gold_price)] = 0

##Start the data from 20000102 Daily Data
Gold$Gold_price = as.integer(Gold$Time) #convert the character to integer
Gold = Gold[order(Gold$Time),]

head(Gold)
summary(Gold)

#M1 Data
#--------#
names(M1)
M1 = M1 [-1, ]
names(M1)[2]<-"M1_moneysupply" #Change colnames
M1 = as.data.frame(M1) #Convert the list into a dataframe.

##Convert any NA data to median and Infinity to 0
M1$M1_moneysupply [is.na(M1$M1_moneysupply)] = median(M1$M1_moneysupply,na.rm = "True")
M1$M1_moneysupply[is.infinite(M1$M1_moneysupply)] = 0

##Start the data from 199001
class(M1$M1_moneysupply) #if it is character, convert it into integer
M1$Time = as.integer(Food$Time) #convert the character to integer
M1 = M1[order(M1$Time),]

head(M1)
summary(M1)

#M2 Data
#--------#
names(M2)
M2 = M2 [-1, ]
names(M2)[2]<-"M2_moneysupply" #Change colnames
M2 = as.data.frame(M2) #Convert the list into a dataframe.

##Convert any NA data to median and Infinity to 0
M2$M2_moneysupply [is.na(M2$M2_moneysupply)] = median(M2$M2_moneysupply,na.rm = "True")
M2$M2_moneysupply[is.infinite(M2$M2_moneysupply)] = 0

##Start the data from 199001
class(M2$Time) #if it is character, convert it into integer
M2$Time = as.integer(M2$Time) #convert the character to integer
M2 = M2[order(M2$Time),]

head(M2)
summary(M2)

#Platinum Data
#--------#
names(Platinum)
Platinum = Platinum [-1, ]
names(Platinum) [2]<-"Platinum" #Change colnames
Platinum = as.data.frame(Platinum) #Convert the list into a dataframe.

##Convert any NA data to median and Infinity to 0
Platinum$Platinum [is.na(Platinum$Platinum)] = median(Platinum$Platinum,na.rm = "True")
Platinum$Platinum[is.infinite(Platinum$Platinum)] = 0

##Start the data from 20000104
class(Platinum$Time) #if it is character, convert it into integer
Platinum$Time = as.integer(Platinum$Time)
Platinum = Platinum[order(Platinum$Time),]

head(Platinum)
summary(Platinum)


#Platinum Data 
#--------#
names(PMI)
PMI = PMI [-1, ]
names(PMI) [2]<-"PMI" #Change colnames
PMI = as.data.frame(PMI) #Convert the list into a dataframe.

##Convert any NA data to median and Infinity to 0
PMI$PMI [is.na(PMI$PMI)] = median(PMI$PMI,na.rm = "True")
PMI$PMI[is.infinite(PMI$PMI)] = 0

##Start the data from 201101
class(PMI$Time) #if it is character, convert it into integer
##Platinum$Time = as.integer(Platinum$Time)
PMI = PMI[order(PMI$Time),]

head(PMI)
summary(PMI)

#S&P 500 Data
#--------#
names(S_P500)
S_P500 = S_P500 [-1, ]
names(S_P500)[2]<-"S&P500" #Change colnames
S_P500 = as.data.frame(S_P500) #Convert the list into a dataframe.

##Convert any NA data to median and Infinity to 0
S_P500$`S&P500` [is.na(S_P500$`S&P500`)] = median(S_P500$`S&P500`, na.rm = "True")
S_P500$`S&P500`[is.infinite(S_P500$`S&P500`)] = 0

##Start the data from 199001
class(S_P500$Time) #if it is character, convert it into integer
S_P500$Time = as.integer(S_P500$Time) #convert the character to integer
S_P500 = S_P500[order(S_P500$Time),]

head(S_P500)
summary(S_P500)



#save.image(file="VaR0.0001.RData") Udit: Do not Uncomment
##*********************************************************##

##Convert into a Time Series.
#---------------------------#

#install.packages("quantmod")
#install.packages("zoo")

library(xts)
library(TTR)
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


