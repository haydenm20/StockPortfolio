#Load Required Libraries
library(tidyr)
library(tidyverse)
library(ggplot2)
library(reshape2)

#Read in S&P500 data
SP500 <- read.csv("DataFiles/SP500.csv")

#Hold Dates
CalDates <- data.frame(Date = as.Date(SP500$Date, "%m/%d/%Y"))

#Read in Aviation stock data
AAL <- read.csv("DataFiles/Aviation/AAL.csv")
ALGT <- read.csv("DataFiles/Aviation/ALGT.csv")
ALK <- read.csv("DataFiles/Aviation/ALK.csv")
DAL <- read.csv("DataFiles/Aviation/DAL.csv")
HA <- read.csv("DataFiles/Aviation/HA.csv")
LUV <- read.csv("DataFiles/Aviation/LUV.csv")

#Read in Finance Stock Data
BCS <- read.csv("DataFiles/Finance/BCS.csv")
CS <- read.csv("DataFiles/Finance/CS.csv")
DB <- read.csv("DataFiles/Finance/DB.csv")
GS <- read.csv("DataFiles/Finance/GS.csv")
MS <- read.csv("DataFiles/Finance/MS.csv")
WFC <- read.csv("DataFiles/Finance/WFC.csv")

#Read in Pharma/Healthcare stock data
BHC <- read.csv("DataFiles/Pharma_Healthcare/BHC.csv")
JNJ <- read.csv("DataFiles/Pharma_Healthcare/JNJ.csv")
MRK <- read.csv("DataFiles/Pharma_Healthcare/MRK.csv")
PFE <- read.csv("DataFiles/Pharma_Healthcare/PFE.csv")
RHHBY <- read.csv("DataFiles/Pharma_Healthcare/RHHBY.csv")
UNH <- read.csv("DataFiles/Pharma_Healthcare/UNH.csv")

#Read in Technology stock data
AAPL <- read.csv("DataFiles/Technology/AAPL.csv")
AMZN <- read.csv("DataFiles/Technology/AMZN.csv")
FB <- read.csv("DataFiles/Technology/FB.csv")
GOOG <- read.csv("DataFiles/Technology/GOOG.csv")
IBM <- read.csv("DataFiles/Technology/IBM.csv")
MSFT <- read.csv("DataFiles/Technology/MSFT.csv")

#Read in Fama-French variable data
FFfactors <- read.csv("DataFiles/F-F_Data_Factors.csv")

#Get only required date range
FFfactors <- FFfactors[which(FFfactors$Date >= 20160408),]
FFfactors[,2:5] <- FFfactors[,2:5]*.01

#Get the end date of FFfactor data
EndDate <- max(FFfactors$Date)

#Function to alter Date in stock data files
DateConvertor <- function(df) {
  df <- separate(data = df,col = "Date", into = c("Month", "Day", "Year"))
  df <- as.data.frame(apply(df, 2, as.numeric))
  df$Date <- df$Year*10000 + df$Month*100 + df$Day
  return(data.frame(df))
}

#Convert Dates for Stock files to same format as Fama-French data
SP500 <- DateConvertor(SP500)
AAL <- DateConvertor(AAL)
ALGT <- DateConvertor(ALGT)
ALK <- DateConvertor(ALK)
DAL <- DateConvertor(DAL)
HA <- DateConvertor(HA)
LUV <- DateConvertor(LUV)
BCS <-DateConvertor(BCS)
CS <- DateConvertor(CS)
DB <- DateConvertor(DB)
GS <- DateConvertor(GS)
MS <- DateConvertor(MS)
WFC <- DateConvertor(WFC)
BHC <- DateConvertor(BHC)
JNJ <- DateConvertor(JNJ)
MRK <- DateConvertor(MRK)
PFE <- DateConvertor(PFE)
RHHBY <- DateConvertor(RHHBY)
UNH <- DateConvertor(UNH)
AAPL <- DateConvertor(AAPL)
AMZN <- DateConvertor(AMZN)
FB <- DateConvertor(FB)
GOOG <- DateConvertor(GOOG)
IBM <- DateConvertor(IBM)
MSFT <- DateConvertor(MSFT)

#Get same date range of data as available FFfactors data
SP500 <- SP500[which(SP500$Date <= EndDate),]
AAL <- AAL[which(AAL$Date <= EndDate),]
ALGT <- ALGT[which(ALGT$Date <= EndDate),]
ALK <- ALK[which(ALK$Date <= EndDate),]
DAL <- DAL[which(DAL$Date <= EndDate),]
HA <- HA[which(HA$Date <= EndDate),]
LUV <- LUV[which(LUV$Date <= EndDate),]
BCS <- BCS[which(BCS$Date <= EndDate),]
CS <- CS[which(CS$Date <= EndDate),]
DB <- DB[which(DB$Date <= EndDate),]
GS <- GS[which(GS$Date <= EndDate),]
MS <- MS[which(MS$Date <= EndDate),]
WFC <- WFC[which(WFC$Date <= EndDate),]
BHC <- BHC[which(BHC$Date <= EndDate),]
JNJ <- JNJ[which(JNJ$Date <= EndDate),]
MRK <- MRK[which(MRK$Date <= EndDate),]
PFE <- PFE[which(PFE$Date <= EndDate),]
RHHBY <- RHHBY[which(RHHBY$Date <= EndDate),]
UNH <- UNH[which(UNH$Date <= EndDate),]
AAPL <- AAPL[which(AAPL$Date <= EndDate),]
AMZN <- AMZN[which(AMZN$Date <= EndDate),]
FB <- FB[which(FB$Date <= EndDate),]
GOOG <- GOOG[which(GOOG$Date <= EndDate),]
IBM <- IBM[which(IBM$Date <= EndDate),]
MSFT <- MSFT[which(MSFT$Date <= EndDate),]
CalDates <- as.data.frame(CalDates[which(CalDates$Date <= "2021-02-26"),])
names(CalDates) <- "Date"

#Put all adjusted close prices in a data file
ACprices <- data.frame(Date = CalDates$Date, AAL = AAL$Adj.Close, ALGT = ALGT$Adj.Close, ALK = ALK$Adj.Close,
                       DAL = DAL$Adj.Close, HA = HA$Adj.Close, LUV = LUV$Adj.Close, BCS = BCS$Adj.Close, 
                       CS = CS$Adj.Close, DB = DB$Adj.Close, GS = GS$Adj.Close, MS = MS$Adj.Close, WFC = WFC$Adj.Close, 
                       BHC = BHC$Adj.Close, JNJ = JNJ$Adj.Close, MRK = MRK$Adj.Close, PFE = PFE$Adj.Close, 
                       RHHBY = RHHBY$Adj.Close, UNH = UNH$Adj.Close, AAPL = AAPL$Adj.Close, AMZN = AMZN$Adj.Close, 
                       FB = FB$Adj.Close, GOOG = GOOG$Adj.Close, IBM = IBM$Adj.Close, MSFT = MSFT$Adj.Close, 
                       SP500 = SP500$Adj.Close)

#Organize prices by Sector
AviationPrices <- data.frame(Date = CalDates$Date, AAL = AAL$Adj.Close, ALGT = ALGT$Adj.Close, ALK = ALK$Adj.Close,
                             DAL = DAL$Adj.Close, HA = HA$Adj.Close, LUV = LUV$Adj.Close)
FinancePrices <- data.frame(Date = CalDates$Date, BCS = BCS$Adj.Close,CS = CS$Adj.Close, DB = DB$Adj.Close, 
                            GS = GS$Adj.Close, MS = MS$Adj.Close, WFC = WFC$Adj.Close)
PharmHealthPrices <- data.frame(Date = CalDates$Date, BHC = BHC$Adj.Close, JNJ = JNJ$Adj.Close, MRK = MRK$Adj.Close, 
                                PFE = PFE$Adj.Close, RHHBY = RHHBY$Adj.Close, UNH = UNH$Adj.Close)
TechnologyPrices <- data.frame(Date = CalDates$Date, AAPL = AAPL$Adj.Close, AMZN = AMZN$Adj.Close, FB = FB$Adj.Close, 
                               GOOG = GOOG$Adj.Close, IBM = IBM$Adj.Close, MSFT = MSFT$Adj.Close)

#Function to normalize data
normal <- function(df) {
  minimum = vector("numeric", length(df))
  maximum = vector("numeric", length(df))
  
  minimum[1] = 0
  maximum[1] = 0
  
  for(i in 2:length(df)) {
    minimum[i] <- min(df[,i]) 
    maximum[i] <- max(df[,i])
  }
  
  for(j in 2:length(df)) {
    for(i in 1:length(df[,1])){
      df[i,j] <- (df[i,j] - minimum[j])/(maximum[j]-minimum[j])
    }
  }
  return(df)
}

#Normalize data
NormalACprices <- normal(ACprices)
NormalAviationPrices <- normal(AviationPrices)
NormalFinancePrices <- normal(FinancePrices)
NormalPharmHealthPrices <- normal(PharmHealthPrices)
NormalTechnologyPrices <- normal(TechnologyPrices)

#All Stock Plot
AllStockPrices <- melt(ACprices, id.vars = 'Date', variable.name = 'Stock')
AllPrice.Plot <- ggplot(AllStockPrices, aes(x=Date, y = value, col = Stock)) + geom_line()
AllPrice.Plot

#Normalized All Stock Plot
NormalAllStockPrices <- melt(NormalACprices, id.vars = 'Date', variable.name = 'Stock')
names(NormalAllStockPrices)[3] <- "Price"
NormalAllPrice.Plot <- ggplot(NormalAllStockPrices, aes(x=Date, y = Price, col = Stock)) + geom_line()
NormalAllPrice.Plot

#Aviation Stock Plot
AviationStocks <- melt(AviationPrices, id.vars = 'Date', variable.name = 'Stock')
names(AviationStocks)[3] <- "Price"
AviationPrice.Plot <- ggplot(AviationStocks, aes(x=Date, y = Price, col = Stock)) + geom_line() + ggtitle("Aviation Stock Prices")
AviationPrice.Plot

#Normalized Aviation Stock Plot
NormalAviationStocks <- melt(NormalAviationPrices, id.vars = 'Date', variable.name = 'Stock')
names(NormalAviationStocks)[3] <- "Price"
NormalAviationPrice.Plot <- ggplot(NormalAviationStocks, aes(x=Date, y = Price, col = Stock)) + geom_line()+ ggtitle("Normalized Aviation Stock Prices")
NormalAviationPrice.Plot

#Finance Stock Plot
FinanceStocks <- melt(FinancePrices, id.vars = 'Date', variable.name = 'Stock')
names(FinanceStocks)[3] <- "Price"
FinancePrice.Plot <- ggplot(FinanceStocks, aes(x=Date, y = Price, col = Stock)) + geom_line() + ggtitle("Finance Stock Prices")
FinancePrice.Plot

#Normalized Finance Stock Plot
NormalFinanceStocks <- melt(NormalFinancePrices, id.vars = 'Date', variable.name = 'Stock')
names(NormalFinanceStocks)[3] <- "Price"
NormalFinancePrice.Plot <- ggplot(NormalFinanceStocks, aes(x=Date, y = Price, col = Stock)) + geom_line() + ggtitle("Normalized Finance Stock Prices")
NormalFinancePrice.Plot

#Pharma/Health Stock Plot
PharmHealthStocks <- melt(PharmHealthPrices, id.vars = 'Date', variable.name = 'Stock')
names(PharmHealthStocks)[3] <- "Price"
PharmHealthPrice.Plot <- ggplot(PharmHealthStocks, aes(x=Date, y = Price, col = Stock)) + geom_line() + ggtitle("Pharma/Healthcare Stock Prices")
PharmHealthPrice.Plot

#Normalized Pharm/Health Stock Plot
NormalPharmHealthStocks <- melt(NormalPharmHealthPrices, id.vars = 'Date', variable.name = 'Stock')
names(NormalPharmHealthStocks)[3] <- "Price"
NormalPharmHealthPrice.Plot <- ggplot(NormalPharmHealthStocks, aes(x=Date, y = Price, col = Stock)) + geom_line() + ggtitle("Normalized Pharma/Healthcare Stock Prices")
NormalPharmHealthPrice.Plot

#Technology Stock Plot
TechnologyStocks <- melt(TechnologyPrices, id.vars = 'Date', variable.name = 'Stock')
names(TechnologyStocks)[3] <- "Price"
TechnologyPrice.Plot <- ggplot(TechnologyStocks, aes(x=Date, y = Price, col = Stock)) + geom_line() + ggtitle("Technology Stock Prices")
TechnologyPrice.Plot

#Normalized Technology Stock Plot
NormalTechnologyStocks <- melt(NormalTechnologyPrices, id.vars = 'Date', variable.name = 'Stock')
names(NormalTechnologyStocks)[3] <- "Price"
NormalTechnologyPrice.Plot <- ggplot(NormalTechnologyStocks, aes(x=Date, y = Price, col = Stock)) + geom_line() + ggtitle("Normalized Technology Stock Prices")
NormalTechnologyPrice.Plot



#Calculate returns 
Returns <- ACprices[1,] - ACprices[1,]
for(i in 2:length(ACprices$Date)){
  Returns[i,] = (ACprices[i,] - ACprices[i-1,])/ACprices[i-1,]
}
Returns$Date <- CalDates$Date


#Put all model data into a data frame
ModelData <- Returns
ModelData$Mkt.RF <- FFfactors$Mkt.RF
ModelData$SMB <- FFfactors$SMB
ModelData$HML <- FFfactors$HML
ModelData$RF <- FFfactors$RF






#Data for CAPM model
CAPM <- ModelData[,c(1:26, 30)]

#Change values using RF
for(i in 2:(length(CAPM)-1)) {
  for(j in 1:length(CAPM[,1])) {
    CAPM[j, i] <- CAPM[j,i] - CAPM[j, 27]
  }
}

#Remove initial value
CAPM <- CAPM[2:length(CAPM$Date),]

#Averages

#Function for CAPM model coefficients alpha and beta
CAPM.model <- function(df) {
  alpha <- list(length(df)-3)
  beta <- list(length(df)-3)
  
  for(i in 2:(length(df)-2)) {
    model <- lm(df[,i] ~ SP500, data = df)
    alpha[i-1] <- coefficients(model)[1]
    beta[i-1] <- coefficients(model)[2]
  }
  
  ab <- data.frame()
  for(i in 1:length(alpha)) {
    ab[1, i] <- alpha[i]
    ab[2, i] <- beta[i]
    names(ab)[i] <- names(df)[i+1]
  }
  
  row.names(ab) <- c("alpha", "beta")
  return(ab)
}

#Set values for rm, rf, and sigmas
rf <- mean(ModelData$RF)
rm <- mean(ModelData$SP500)
sigma <- ModelData[1, 2:25]

for(i in 1:24) {
  sigma[i] <- sd(ModelData[,i+1])
}

#Get alphas and betas for all the stocks
CAPM.AlphaBeta <- CAPM.model(CAPM)

#Expected returns from CAPM model
ER.CAPM <- CAPM[1,2:25]
row.names(ER.CAPM) <- c("ER")

for(i in 1:24) {
  ER.CAPM[i] <- rf + (CAPM.AlphaBeta[2,i]*(rm - rf))
}

#Sharpe Ration for CAPM model
Sharpe.CAPM <- ER.CAPM
row.names(Sharpe.CAPM) <- c("Sharpe")

for(i in 1:24) {
  Sharpe.CAPM[i] <- 100*(ER.CAPM[i]-rf)/sigma[i]
}






#Data for Fama-French model
FFdata <- ModelData

#Change values using RF
for(i in 2:(length(FFdata)-4)) {
  for(j in 1:length(FFdata[,1])) {
    FFdata[j, i] <- FFdata[j, i] - FFdata[j, 30]
  }
}

#Function for Fama-French 3 factor model coefficients alpha, beta1, beta2, beta3
FF.model <- function(df) {
  alpha <- list(length(df)-6)
  beta1 <- list(length(df)-6)
  beta2 <- list(length(df)-6)
  beta3 <- list(length(df)-6)
  
  for(i in 2:(length(df)-5)) {
    model <- lm(df[,i] ~ SP500 + SMB + HML, data = df)
    alpha[i-1] <- coefficients(model)[1]
    beta1[i-1] <- coefficients(model)[2]
    beta2[i-1] <- coefficients(model)[3]
    beta3[i-1] <- coefficients(model)[4]
  }
  
  ab <- data.frame()
  for(i in 1:length(alpha)) {
    ab[1, i] <- alpha[i]
    ab[2, i] <- beta1[i]
    ab[3, i] <- beta2[i]
    ab[4, i] <- beta3[i]
    names(ab)[i] <- names(df)[i+1]
  }
  
  row.names(ab) <- c("alpha", "beta1", "beta2", "beta3")
  return(ab)
}

AAL.FF <- lm(AAL~SP500+SMB+HML, data = FFdata)
summary(AAL.FF)
  
AALrf+coefficients(AAL.FF)[2]*(rm-rf)+coefficients(AAL.FF)[3]*SMB+coefficients(AAL.FF)[3]*HML
FF.AlphaBeta <- FF.model(FFdata)
FF.AlphaBeta

#Set values for SMB and HML
SMB <- mean(FFdata$SMB)
HML <- mean(FFdata$HML)

#Expected returns from CAPM model
ER.FF <- FFdata[1,2:25]
row.names(ER.FF) <- c("ER")

for(i in 1:24) {
  ER.FF[i] <- rf + (FF.AlphaBeta[2,i]*(rm - rf)) + (FF.AlphaBeta[3,i]*SMB) + (FF.AlphaBeta[4, i]*HML)
}
ER.FF*25200
rm*25200


#Sharpe Ration for Fama French model
Sharpe.FF <- ER.FF
row.names(ER.CAPM) <- c("Sharpe")

for(i in 1:24) {
  Sharpe.FF[i] <- 100*(ER.FF[i]-rf)/sigma[i]
}

