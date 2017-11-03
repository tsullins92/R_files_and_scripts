rm(list=ls())
getwd()
setwd(Hobo_Docs)
#starting with bluetooth data files
df1303Blue <- read.csv("171016_13-03.csv")
df1307Blue <- read.csv("171016_13-07.csv")
df1308Blue <- read.csv("171016_13-08.csv")
df1608Blue <- read.csv("171016_16-08.csv")
dfnetwork <- read.csv("171011_1608.csv")
colnames(df1303Blue) <- c("Date_Time","1303_Temp_Bluetooth","1303_RH_Bluetooth","1303_DewPoint_Bluetooth")
colnames(df1308Blue) <- c("Date_Time","1308_Temp_Bluetooth","1308_RH_Bluetooth","1308_DewPoint_Bluetooth")
colnames(df1608Blue) <- c("Date_Time","1608_Temp_Bluetooth","1608_RH_Bluetooth","1608_DewPoint_Bluetooth")
#df1308$Date_Time <- gsub("/", "-", df1308$Date_Time)                          #1308 had '/' instead of '-'
#convert bluetooth temps to farenheit
df1303Blue$`1303_Temp_Bluetooth` <- (df1303Blue$`1303_Temp_Bluetooth`-32)*(5/8)
df1308Blue$`1308_Temp_Bluetooth` <- (df1308Blue$`1308_Temp_Bluetooth`-32)*(5/8)   
df1608Blue$`1608_Temp_Bluetooth` <- (df1608Blue$`1608_Temp_Bluetooth`-32)*(5/8)
df1307Blue$`1307_Temp_Bluetooth` <- (df1307Blue$`1307_Temp_Bluetooth`-32)*(5/8)
#all three files' Date_Time had to be converted to character arrays so that they could be edited
df1308Blue$Date_Time <- as.character(df1308Blue$Date_Time)                           
df1303Blue$Date_Time <- as.character(df1303Blue$Date_Time)
df1608Blue$Date_Time <- as.character(df1608Blue$Date_Time)
df1307Blue$Date_Time <- as.character(df1307Blue$Date_Time)    
#df1303 and df1608 needed to lose the seconds on their Date_Time to work with eachother
#df1303$Date_Time = substr(df1303$Date_Time,1,nchar(df1303$Date_Time)-3)
#df1608$Date_Time = substr(df1608$Date_Time,1,nchar(df1608$Date_Time)-3)
#$Date_Time had to be converted to date-time because each dataframe had a different format
df1303Blue$Date_Time <- strptime(df1303Blue$Date_Time, "%m/%d/%Y %H:%M")
df1308Blue$Date_Time <- strptime(df1308Blue$Date_Time, "%m/%d/%Y %H:%M")
df1608Blue$Date_Time <- strptime(df1608Blue$Date_Time, "%m/%d/%Y %H:%M")
df1307Blue$Date_Time <- strptime(df1307Blue$Date_Time, "%m/%d/%Y %H:%M")
#Merge the bluetooth files together
bluetoothMerged <- merge(df1303Blue,df1308Blue,by="Date_Time",all=T)
bluetoothMerged <- merge(bluetoothMerged,df1608Blue,by="Date_Time",all=T)


#Now for network nodes
dfnetwork <- read.csv("Node_Dataset.csv")
colnames(dfnetwork) <- c("Count","Date_Time","1608_Temp_Network","1608_RH_Network","1303_Temp_Network","1303_RH_Network","1308_Temp_Network","1308_RH_Network","1307_Temp_Network","1307_RH_Network")
dfnetwork$Date_Time <- as.character(dfnetwork$Date_Time) 
dfnetwork$Date_Time <- strptime(dfnetwork$Date_Time, "%m/%d/%Y %H:%M")
#Divide data into dataframes for each percival
df1303Net <- dfnetwork[,c("Date_Time","1303_Temp_Network","1303_RH_Network")]
df1308Net <- dfnetwork[,c("Date_Time","1308_Temp_Network","1308_RH_Network")]
df1608Net <- dfnetwork[,c("Date_Time","1608_Temp_Network","1608_RH_Network")]
#merge bluetooth and network data for each percival
dfmerged <- merge(dfnetwork,df1307Blue,by="Date_Time",all=T)
dfmerged <- merge(dfnetwork,df1303Blue,by="Date_Time",all=T)
dfmerged <- merge(dfmerged,df1307Blue,by="Date_Time",all=T)
dfmerged <- merge(dfmerged,df1308Blue,by="Date_Time",all=T)
dfmerged <- merge(dfmerged,df1608Blue,by="Date_Time",all=T)
#Create Variables for plots
y=na.omit(dfmerged$X13.03.Temp)
y1=na.omit(dfmerged$X1303_Bluetooth_Temp...C.)
x=na.omit(dfmerged$Date_Time)
#create plots
plot(x,y,col="green",type="l",main="1303 temps", xlab="Date-Time",ylab="Centigrade",ylim = c(21.5,24.0))
lines(x,y1,col="red")
text(locator(),labels=c("Network","Bluetooth"),col=c("green","red"))

#repeat, but remove NAs and match data lengths
y=na.omit(dfmerged$X13.07.Temp)
y1=na.omit(dfmerged$X1307_Bluetooth_Temp...C.)
x=na.omit(dfmerged$Date_Time)
x=tail(x,-(5435-4357))
y=tail(y,-(5435-4357))


y=na.omit(dfmerged$X13.08.Temp)
y1=na.omit(dfmerged$X1308_Bluetooth_Temp...C.)
x=na.omit(dfmerged$Date_Time)
x=tail(x,-(5435-5431))
y=tail(y,-(5435-5431))

df <- read.csv("171024_Node_Dataset.csv")
df$Date_Time <- as.character(df$Date_Time)     
df$Date_Time<- strptime(df$Date_Time, "%m/%d/%Y %H:%M")
ggplot(df, aes(Date_Time)) + 
       geom_line(aes(y = df$X1303NetworkTemp, colour = "Network Temperature"))+ 
       geom_line(aes(y = df$X1303BlueTemp, colour = "Bluetooth Temperature"))+
       #ylim(21,24.5)+
       ggtitle("10/20/2017 13-03 Temps")+
       xlab("Date-Time")+
       ylab("Degrees C")

ggplot(df, aes(Date_Time)) + 
       geom_line(aes(y = df$X1307NetworkTemp, colour = "Network Temperature"))+ 
       geom_line(aes(y = df$X1307BlueTemp, colour = "Bluetooth Temperature"))+
       #ylim(21,24.5)+
       ggtitle("10/20/2017 13-07 Temps")+
       xlab("Date-Time")+
       ylab("Degrees C")

ggplot(df, aes(Date_Time)) + 
       geom_line(aes(y = df$X1308NetworkTemp, colour = "Network Temperature"))+ 
       geom_line(aes(y = df$X1308BlueTemp, colour = "Bluetooth Temperature"))+
       #ylim(21,24.5)+
       ggtitle("10/20/2017 13-08 Temps")+
       xlab("Date-Time")+
       ylab("Degrees C")

ggplot(df, aes(Date_Time)) + 
       geom_line(aes(y = df$X1608NetworkTemp, colour = "Network Temperature"))+ 
       geom_line(aes(y = df$X1608BlueTemp, colour = "Bluetooth Temperature"))+
       ylim(18,20.5)+
       ggtitle("10/20/2017 16-08 Temps")+
       xlab("Date-Time")+
       ylab("Degrees C")






rm(list=ls())
