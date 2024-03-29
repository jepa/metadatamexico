ts_subset<- function(Data,Start_Year,End_Year,Category="NA"){
  
  library(dplyr)  #<- For data wrangling
  library(dygraphs)
  
  Data <- Data %>%
    filter(Research_Field %in% Category)
  
  tdat= na.omit(Data[,c("Start_Year","End_Year")]) #Subset columns with 
  # tdat <- tdat %>%
  #   filter(Start_Year >= Start_Year,
  #          End_Year <= End_Year)
  
  tdat$Start_Year<- as.numeric(as.character(tdat$Start_Year))
  tdat$End_Year<- as.numeric(as.character(tdat$End_Year))
  
  
  #mean(tdat$Start_Year) #just checking they are number
  #mean(tdat$End_Year) #just checking they are number
  
  #start and end year data
  years= Start_Year:End_Year #Limit time span considered
  
  x <- tdat$Start_Year[1]:tdat$End_Year[1]
  
  ts= cbind(years, !is.na(match(years, x))==1 ) #Flag years in time series
  
  for(i in 2:dim(tdat)[1]) #Repeat for all data
  {
    x <- tdat$Start_Year[i]:tdat$End_Year[i]
    ts= cbind(ts, !is.na(match(years, x))==1)
  }
  tssum= cbind(ts[,1], rowSums(ts[,2:dim(ts)[2]])) #Sum over years
  
  J <- data.frame(tssum) #<- Converts to data.frame
  
  return(J)
  
}