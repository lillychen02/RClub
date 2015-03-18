#This R session will explore cell phone usage data for ~1 month
#   (1) Determine the amount of data I used while in Canada
#   (2) Determine the extra charges I'll incur

#Concepts
#   (1) Reading in data from CSV files
#   (2) Subsetting Data and Reformatting/Cleaning Data
#   (3) Visualizing Data with ggplot2 
#   (4) Merge, Functions, Reduce, For Loops [[likely in the next session, more advanced and automates a lot of the processes here]]

#Before you code, think through the steps needed to solve the problem.
#   Read CSV files/ clean data
#   Structure data in long format with data, messages, calls together (add field "type.of.data")
#   Visualize

#----------------------DATA CLEANING / RESTRUCTURING DATA----------------------------------------------
#ensure working directory is correct
getwd()
rm(list=ls())
#Where did you put those CSV files? Make sure you set WD to where they are
setwd("C:/Users/lichen/Documents/R")

#Read in CSV files downloaded from Verizon
raw.data<-read.csv("UnbilledData.csv")
raw.voice <-read.csv("UnbilledVoice.csv")
raw.msg <-read.csv("UnbilledMessaging.csv")

#preview data
head(raw.data)
head(raw.voice)
head(raw.msg)

#Clean data for data usage
cell.data <- raw.data[,c(1,2,4)]
head(cell.data)
    cell.data <- raw.data[, c("Date", "Time", "Unbilled.Data.Usage.in.Megabytes.MB.")]
    head(cell.data)

#Clean data for voice usage
cell.voice <- raw.voice[, c("Date", "Time", "Destination", "Minutes")]
  head(cell.voice)
#clean data for messages
cell.msg <- raw.msg[, c("Date", "Time", "Direction", "Message.Type")]
cell.msg$value <- 1
#convert date and time columns to show date and time of class POSIXlt
?strptime
  cell.data$Date <- paste(cell.data$Date, cell.data$Time)
    cell.data$Date<-strptime(cell.data$Date, "%m/%d/%y %I:%M %p")
    cell.data <-cell.data[order(cell.data$Date),]

#convert date and time columns for cell.voice, cell.msg
  cell.voice$Date<- paste(cell.voice$Date, cell.voice$Time)
  cell.voice$Date <-strptime(cell.voice$Date, "%m/%d/%y %I:%M %p")
  
  cell.msg$Date<- paste(cell.msg$Date, cell.msg$Time)
  cell.msg$Date<-strptime(cell.msg$Date, "%m/%d/%y %I:%M %p")

#change col names to be shorter for cell.data
colnames(cell.data)[3] <- c("Data.in.MB")
head(cell.data)

# ------------------------VISUALIZATION----------------------------------------------------------------------
library("ggplot2")
#plot one data frame
p1<- ggplot()
p1<-p1 + geom_point(data=cell.data, aes(x=Date, y=Data.in.MB),size=3, color="blue")
p1

#What if we wanted to see them all plot on one graph?

p2 <- p1 + geom_point(data=cell.voice, aes(x=Date, y=Minutes), size =3, color="red")
p2

p3 <- p2+ geom_point(data=cell.msg, aes(x=Date, y=1), size=3, color = "green")
p3


#-------------------------------SUMMING / DETERMINE COSTS-----------------------------------------------------------------


#approximate the data I used while in Canada
start<- as.POSIXct("2015-02-20 16:15:00") #I looked up approximate dates/times when I crossed the border
end <- as.POSIXct("2015-02-23 18:30:00")

#sum data usage
#       returns all columns the rows have Date that is between start and end, 
  intl.data <-cell.data[cell.data$Date >= start & cell.data$Date <= end, ]
  total.data<-sum(intl.data$Data.in.MB)
  total.price <- total.data * 2.05
  total.price #9.55 (not bad...)

voice <- cell.voice[cell.voice$Date >= start & cell.voice$Date <= end, ]
voice.sum <- sum(voice$Minutes)
voice.cost <- voice.sum * 0.89
voice.cost # $8.01

# 0.25 for text received, 0.50 for text sent
msg <- cell.msg[cell.msg$Date >= start & cell.msg$Date <= end,]
msg.cts<-table(msg$Direction, msg$value)
msg.cost <- msg.cts["Received",] * 0.25 + msg.cts["Sent",]*0.50
msg.cost # 7.25

sum(msg.cost, voice.cost, total.price) #$24.81
#Teaser for Session in 2 weeks---------------------------------------------------------------------

# merge data frames into one data frame
#   First, want to make sure we add a column to tell us the type of data
    cell.data$type.data <- "data"
    cell.voice$type.data <- "voice"
    cell.msg$type.data <- "texts"
# Second, merge the data frames using fxn merge
#   if confused...
    ?merge
#     then begin using it
    merged.dat<- merge (cell.data, cell.voice, by = c("Date","type.data"), all=TRUE)
    dat  <- merge(merged.dat, cell.msg, by=c("Date", "type.data"), all= TRUE)
