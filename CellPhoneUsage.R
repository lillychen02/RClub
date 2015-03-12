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

#convert date and time columns to class Date
  cell.data$Date <- paste(cell.data$Date, cell.data$Time)
    cell.data$Date<-strptime(cell.data$Date, "%m/%d/%y %I:%M %p")
    cell.data <-cell.data[order(cell.data$Date),]

#change col names to be shorter
colnames(cell.data)[3] <- c("Data.in.MB")
head(cell.data)

#find out how much data I used while in Canada (somewhere feb20 around 4pm - to feb 23 around 630pm)

library("ggplot2")
#plot this
p1<- ggplot()
p1<-p1 + geom_point(data=cell.data, aes(x=Date, y=Data.in.MB),size=3, color="blue")
p1

#approximate the data I used while in Canada
start<- as.POSIXct("2015-02-20 16:15:00") #I looked up approximate dates/times when I crossed the border
end <- as.POSIXct("2015-02-23 18:30:00")

#sum data usage
intl.data <-celldata[cell.data$Date >= start & cell.data$Date <= end, ]
total.data<-sum(intl.data$Data.in.MB)
total.price <- total.data * 2.05
total.price #9.55 (not bad...)

