
# Investment strategies 
# Data is from  2/3/2020 to 2/1/2021

# install.packages("ggpubr")
load(file='SPData.RData')
library(dplyr)
library("ggpubr")
library('GGally')
setwd("C:/Users/seanh/Desktop/SeanPrograms/StockMarket-Analysis")

# Read CSVs
{
qqq <- read.csv("QQQ.csv")
vig1 <- read.csv("VIG.csv")
djd <-read.csv("DJD.csv")

tsla <-read.csv("TSLA.csv")
wmt <-read.csv("WMT.csv")
amzn <-read.csv("AMZN.csv")
}

# Parse CSVs into nums
{
# Hint: To parse CSVs with "$" and " " in them, just copy and past a set of the 3 lines below and replace the variable name (ie. wmt, tsla, amzn) with the variable name that you set when reading the CSVs (line 15+)
tsla <- tsla %>%  mutate(Open = gsub("\\$",'', Open), Close.Last = gsub("\\$",'', Close.Last), High = gsub("\\$",'', High), Low = gsub("\\$",'', Low))
tsla <- tsla %>%  mutate(Open = gsub(" ",'', Open), Close.Last = gsub(" ",'', Close.Last), High = gsub(" ",'', High), Low = gsub(" ",'', Low))
tsla <- tsla %>% mutate_at(vars(2:6),as.double)

wmt <- wmt %>%  mutate(Open = gsub("\\$",'', Open), Close.Last = gsub("\\$",'', Close.Last), High = gsub("\\$",'', High), Low = gsub("\\$",'', Low))
wmt <- wmt %>%  mutate(Open = gsub(" ",'', Open), Close.Last = gsub(" ",'', Close.Last), High = gsub(" ",'', High), Low = gsub(" ",'', Low))
wmt <- wmt %>% mutate_at(vars(2:6),as.double)

amzn <- amzn %>%  mutate(Open = gsub("\\$",'', Open), Close.Last = gsub("\\$",'', Close.Last), High = gsub("\\$",'', High), Low = gsub("\\$",'', Low))
amzn <- amzn %>%  mutate(Open = gsub(" ",'', Open), Close.Last = gsub(" ",'', Close.Last), High = gsub(" ",'', High), Low = gsub(" ",'', Low))
amzn <- amzn %>% mutate_at(vars(2:6),as.double)
}

# Convert dates of dataframes to Date datatype
{
  dateIt <- function(df){
    df$Date <- as.Date(df$Date, '%m/%d/%Y')
    return(df)
  }
  
  amzn <- dateIt(amzn)
  djd <- dateIt(djd)
  qqq <- dateIt(qqq)
  tsla <- dateIt(tsla)
  vig1 <- dateIt(vig1)
  wmt <- dateIt(wmt)
}

# Check stock price from Feb 2020
{
# Function that checks Open prices of all securities for the given date, date must be between 2/3/2020 and 2/1/2021
checkOpenPrices <- function(date){
  date <- as.Date(date, '%m/%d/%Y')
  
  print(qqq[qqq$Date == date,]$Open)
  print(djd[djd$Date == date,]$Open)
  print(vig1[vig1$Date == date,]$Open)
  
  print(wmt[wmt$Date == date,]$Open)
  print(amzn[amzn$Date == date,]$Open)
  print(tsla[tsla$Date == date,]$Open)
  
}
  checkOpenPrices("02/03/2020")
}

# Master function (analysis)
analyzeSecurity <- function(df, quarter, analysisType, opt.a, opt.b){
  analysisType <- tolower(analysisType)
# Divide dataframe by quarters
{
  q1 <- df[df$Date >= as.Date("02/03/2020", '%m/%d/%Y') & df$Date <= as.Date("02/03/2020", '%m/%d/%Y') + 365/4,]
  q2 <- df[df$Date >= as.Date("02/03/2020", '%m/%d/%Y') + 365/4 & df$Date <= as.Date("02/03/2020", '%m/%d/%Y') + 2*(365/4),]
  q3 <- df[df$Date >= as.Date("02/03/2020", '%m/%d/%Y') + 2*(365/4) & df$Date <= as.Date("02/03/2020", '%m/%d/%Y') + 3*(365/4),]
  q4 <- df[df$Date >= as.Date("02/03/2020", '%m/%d/%Y') + 3*(365/4) & df$Date <= as.Date("02/03/2020", '%m/%d/%Y') + 4*(365/4),]
  
  }

  
  
# Quarter identification (activeInterval)
{
  
  activeInterval <- switch(quarter, q1, q2, q3, q4, df);
  # Error message for passing a non-existant quarter
  
  
  
  if(quarter < 1 | quarter > 5){
    return('Please type in a quarter between 1 and 5, 5 is to select all quarters!')
  }
}


# Analysis: Percentage Gains Losses
# Result: Checks for gains and losses each day based on specified percentage increase/decrease
getPercentageGainsLosses <- function(percentage){
    plusCounter  <- 0;
    positiveDays <- data.frame(Date=as.Date(character(), ),Close.Last = numeric(),Volume = numeric(),Open = numeric(),High = numeric(), Low = numeric())
    minusCounter <- 0;
    negativeDays <- data.frame(Date=as.Date(character(),),Close.Last = numeric(),Volume = numeric(),Open = numeric(),High = numeric(), Low = numeric())
    
    # If an option is not designated, then it will default to 2%
    if(missing(percentage)){percentage <- 2}
    
    
    for(i in 1:nrow(activeInterval)){

      # Prints all the days the security price increased by at least X%
      if(activeInterval[i,]$Close.Last / activeInterval[i,]$Open >= 1 + percentage/100){
        # print(df[i,])
        plusCounter <- plusCounter + 1
        positiveDays <- rbind(positiveDays, df[i,])
      # Prints all the days the security price decreases by at least X%
      } else if(activeInterval[i,]$Close.Last / activeInterval[i,]$Open <= 1 - percentage/100){
        # print(df[i,])
        minusCounter <- minusCounter + 1
        negativeDays <- rbind(negativeDays, df[i,])
      }
      
      
    }
    
    print(paste('Number of days with', percentage, '% increases on the day in quarter', quarter,': ',plusCounter))
    print(positiveDays)
    print(paste('Number of days with', percentage,'% decreases on the day in quarter', quarter,': ',minusCounter))
    print(negativeDays)
    
}


# Analysis: Get Quarter
# Result: Lists all the days of the quarter requested with prices and volume info
getQuarter <- function(){
  return(activeInterval)
}


# Analysis: Get green days vs red days
# Result: All the days in the quarter that the stock price went up or down
getGreenVsRed <- function(greenOrRed){
  greenDays <- activeInterval[activeInterval$Open < activeInterval$Close.Last,]
  redDays <- activeInterval[activeInterval$Open > activeInterval$Close.Last,]
  greenRedList <- list(Green_Days = greenDays, Red_Days = redDays)
  
  if(greenOrRed == 0){return(greenRedList[[1]])}else if(greenOrRed == 1) {return(greenRedList[[2]])}else if(greenOrRed == 2) {return(greenRedList)}else {return(greenRedList)}
  return('There was an error in getGreenVsRed')
  
  
  
}


# Not working yet
# Analysis: Check price swings.  Create a ratio --> days to price difference.  Show the top 3 price swings.
# Result: Show the top 3 price swings based on the day range inputted and max swing $ amount (this will be inputted and processed as a percentage)
getPriceSwing <- function(dRange, max){
  #Create variables for the largest swing and a data frame containing all the rows of the largest swing
  grt <- 0
  dfx <- data.frame()
  for(i in 1:nrow(activeInterval)-dRange){
    # temp variable
    
    swing <- abs(activeInterval[i,]$Close.Last-activeInterval[i+dRange,]$Close.Last)

    # if a max option is not 0, then it will be sure to limit the max swing
    if(!max == 0){
      # if swing is greater than the previous largest swing recorded, and the swing is less than the max still, then make the new largest swing the current swing and the dataframe the corresponding day range
      if(swing > grt & swing < max){
        grt <- swing
        dfx <- activeInterval[i:i+dRange,]
      } 
    # if a max option is zero, then there will be no limit on the max swing
    } else {
        if(swing > grt){
        grt <- swing
        dfx <- activeInterval[i:i+dRange,]
        }
    }
    
  }
  print(paste("The largest swing for quarter", quarter, "is", grt))
  return(dfx)
  
}

getCorrelation <- function(bComp){
  # Separate the inputted company into quarters
  {
    q1b <- bComp[bComp$Date >= as.Date("02/03/2020", '%m/%d/%Y') & bComp$Date <= as.Date("02/03/2020", '%m/%d/%Y') + 365/4,]
    q2b <- bComp[bComp$Date >= as.Date("02/03/2020", '%m/%d/%Y') + 365/4 & bComp$Date <= as.Date("02/03/2020", '%m/%d/%Y') + 2*(365/4),]
    q3b <- bComp[bComp$Date >= as.Date("02/03/2020", '%m/%d/%Y') + 2*(365/4) & bComp$Date <= as.Date("02/03/2020", '%m/%d/%Y') + 3*(365/4),]
    q4b <- bComp[bComp$Date >= as.Date("02/03/2020", '%m/%d/%Y') + 3*(365/4) & bComp$Date <= as.Date("02/03/2020", '%m/%d/%Y') + 4*(365/4),]
    
  
  activeIntervalB <- switch(quarter, q1b, q2b, q3b, q4b, bComp);
  }
  
  x <- activeInterval$Close.Last
  y <- activeIntervalB$Close.Last
  return(cor(x,y))
}

getCorrelationCharts <- function(){
  dataset <- data.frame(matrix(NA, nrow = 251, ncol = 1))
  dataset$amzn <- amzn$Close.Last
  dataset$wmt <- wmt$Close.Last
  dataset$tsla <- tsla$Close.Last
  dataset$qqq <- qqq$Close.Last
  dataset$djd <- djd$Close.Last
  dataset$vig1 <- vig1$Close.Last
  ggpairs(dataset, title='The Matrix')
}




# Decides what type of analysis is to be run
{
  if(analysisType == 'gainslosses') {return(getPercentageGainsLosses(opt.a))}
  if(analysisType == 'quarter') {return(getQuarter())}
  if(analysisType == 'greenvsred'){return(getGreenVsRed(opt.a))}
  if(analysisType == 'swing'){return(getPriceSwing(opt.a, opt.b))}
  if(analysisType == 'correlation'){return(getCorrelation(opt.a))}
  
}

}

# Format: stock, quarter, analysis, option(s)
# analyzeSecurity(tsla, 5 , 'gainslosses',7)
# analyzeSecurity(tsla, 5 , 'greenvsred',0)
# analyzeSecurity(tsla, 1, 'correlation', amzn)
# analyzeSecurity(tsla, 5 , 'swing',10,0) <-- not working yet
 getCorrelationCharts()

# Finds all entries for specified quarter
# analyzeSecurity(amzn, 1, 'quarter')

# Finds average closing price in quarter 1a
# mean(analyzeSecurity(amzn, 3, 'quarter')$Close.Last)


save.image(file='SPData.RData')















