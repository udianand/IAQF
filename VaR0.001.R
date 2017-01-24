#VAR model - 0.001


#load workspace
load(file="VaR0.0001.RData")

##Load Data from the GUI. or use command below

#CPI = read.csv("<FileName>.txt",header = TRUE)
#FFR = read.csv("<FileName>.txt",header = TRUE)
#UNEMP = read.csv("<FileName>.txt",header = TRUE)
#All data is monthly

##Data Definition
#CPI: All Urban Consumers - Seasonally Unadjusted USA All items  - Non Adj. CPI\n  - CPI
#FFR: Interest Rates Prices Production or Labor United States 60B..ZF... FEDERAL 
      #FUNDS RATE - Average UNITED STATES  - \n  - Percent per annum
#UNEMP: Unemployment Rate Non-Seasonally Adjusted USA  - %

CPI = CPI_All_items 
FFR = Federal_Funds_Rate_Average
UNEMP = Unemployment_rate

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
FFR = FFR[-1,] #The first row contained text varibales
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

save.image(file="VaR0.0001.RData")
