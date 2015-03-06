#This R session will explore cell phone usage data for this month 
#and determining the data usage while I was in Canada
getwd()

#Read in CSV files downloaded from Verizon
raw.data<-read.csv("UnbilledData.csv")
raw.voice <-read.csv("UnbilledVoice.csv")
raw.msg <-read.csv("UnbilledMessaging.csv")

#preview data
head(raw.data)
head(raw.voice)
head(raw.msg)

#Can merge files, but might want to wait until later?
raw.all <- merge(raw.data,raw.voice, by=c("Date", "Time"), all=TRUE)
raw.all <- merge(raw.all, raw.msg, by=c("Date","Time"), all=TRUE)


#truncate data to only include relevant columns
celldata <- raw.data[,c(1,2,4)]

#convert date and time columns to class Date
celldata$Date <- paste(celldata$Date, celldata$Time)
celldata$Date<-strptime(celldata$Date, "%m/%d/%y %I:%M %p")
celldata <-celldata[order(celldata$Date),]

#change col names to be shorter
colnames(celldata) <- c("Date", "Time", "Data.in.MB")


#find out how much data I used while in Canada (somewhere feb20 around 4pm - to feb 23 around 630pm)
library("ggplot2")
#plot this
p1<- ggplot()
p1<-p1 + geom_point(data=celldata, aes(x=Date, y=Data.in.MB),size=3, color="blue")
p1

#approximate the data I used while in Canada
start<- as.POSIXct("2015-02-20 16:15:00") #I looked up approximate dates/times when I crossed the border
end <- as.POSIXct("2015-02-23 18:30:00")

#sum data usage
intl.data <-celldata[celldata$Date >= start & celldata$Date <= end, ]
total.data<-sum(intl.data$Data.in.MB)
total.price <- total.data * 2.05
total.price #9.55 (not bad...)

