#This R session will explore cell phone usage data for ~1 month
#   (1) Determine the amount of data I used while in Canada
#   (2) Determine the extra charges I'll incur

#Concepts
#   (1) Reading in data from CSV files
#   (2) Subsetting Data and Reformatting/Cleaning Data
#   (3) Visualizing Data with ggplot2 
#   (4) Merge, Functions, Reduce, For Loops [[likely in the next session, more advanced and automates a lot of the processes here]]

#ensure working directory is correct
getwd()

#clear working space
rm(list=ls())

# if ever confused about functions, use the ?
?read.csv

#Read in CSV files downloaded from Verizon and turn into a data frame
raw.data<-read.csv("UnbilledData.csv")
raw.voice <-read.csv("UnbilledVoice.csv")
raw.msg <-read.csv("UnbilledMessaging.csv")

#preview data 
#note: use head to see the first few rows of your data
head(raw.data)
head(raw.voice)
head(raw.msg)

#How do I want data to be strutured ultimately? I want to plot data, messages and calls on one plot

#change date format to time

#want date and time as one column
  date.time<-paste(raw.data$Date, raw.data$Time)
  head(date.time)
  class(date.time)

  raw.data$date.time<- strptime(date.time, "%m/%d/%y %I:%M %p")
  raw.data$type <- "Data"
  head(raw.data)
#we'll need to do the same thing for all 3 data frames...let's write a for loop that will:
#   1. change the date format to include the date and time
#   2. subset the data to include only relevant columns
#   3. change name of the data frame

#first, we will create a list of the 3 data frames
cell.all <- list(raw.data, raw.voice, raw.msg)

#preview data
head(cell.all[[1]]) #c(1,3,4)
head(cell.all[[2]]) #c(1,3,5,6)
head(cell.all[[3]]) #c(1,5,6)

#create a list of columns wanted
index <- list(c(1,2,4), c(1,3,5,6), c(1,5,6))
cell.dat <- cell.all
#character list of names of each data frame
names <- names(cell.dat) <- c("Data","Calls", "Messages")

#use for loop to change format of the date
#note that all 3 data frames need to change date
for (i in 1:3) {
  cell.dat[[i]]$Date <- strptime(paste(cell.all[[i]]$Date, cell.all[[i]]$Time), "%m/%d/%y %I:%M %p")
  cell.dat[[i]]<- cell.dat[[i]][,index[[i]]]
  cell.dat[[i]]$type <- names[i]
}

#cell.dat <- list(data, voice, msg)
head(cell.dat$Data) #c(3)
head(cell.dat$Calls) #c(4)
head(cell.dat$Messages,100) #1

# want data structured long format (not wide)
cell.dat$Data$value <- cell.dat$Data[,3]
cell.dat$Calls$value <- cell.dat$Calls[,4]
cell.dat$Messages$value <- 1


# -----------------------------------------PAUSE---------------------------------------------#

#create new column called cost.if that 
# calculates the cost if global roaming charges were applied
#     for messages, change depending if Received or Sent
#     $0.25 for every picture message sent or received, 
#     $0.25 for text received, $0.50 for each text sent

cell.dat$Messages$cost.if <-cell.dat$Messages$value* 0.25
cell.dat$Messages$cost.if[cell.dat$Messages$Message.Type == "Text" & cell.dat$Messages$Direction=="Sent"] <-
  cell.dat$Messages$value[cell.dat$Messages$Message.Type =="Text" & cell.dat$Messages$Direction =="Sent"] *0.50

#2.05 per MB of data
cell.dat$Data$cost.if <- 2.05* cell.dat$Data[,3]

#0.89 per min
cell.dat$Calls$cost.if <- 0.89 * cell.dat$Calls[,4]

#------------------------MERGE-----------------------------------------------------------------------
#but...wait, we have a list of data frames...we want one data frame

#Merge the data for cell data and cell calls 
cell.dat2<- merge(cell.dat$Data , cell.dat$Calls, by=c("Date", "type", "cost.if","value"), all=TRUE)

#Then merge result with data for Messages
cell.dat2<- merge(cell.dat2, cell.dat$Messages, by=c("Date", "type", "cost.if", "value"), all=TRUE)

#EASIER WAY / SHORTCUT

merge.all <- function(x,y) {
  merge(x,y,all=TRUE, by=c("Date", "type", "cost.if","value"))
}

#Now we use Reduce to iterate 
#   Reduce applies a function to successively 
#     combine elements in a given vector and inital value
all.dat <- Reduce(merge.all, cell.dat) 
head(all.dat)
tail(all.dat)
#When did we enter/leave Canada?
start<- as.POSIXct("2015-02-20 16:15:00") #I looked up approximate dates/times when I crossed the border
end <- as.POSIXct("2015-02-23 18:30:00")

#BUT If I didn't know that...I could plot and guess when I entered / left Canada based on usage pattern
library("ggplot2")

#plot activity
a1 <- ggplot()
a1 <- a1 + geom_point(data= cell.dat2, aes(x=Date, y=value, color = type), size=3)
al <- a1 + labs(title="Current Mobile Phone Usage") 
a1 <- a1 + geom_vline(xintercept = as.numeric(start), linetype="dashed") + geom_vline(xintercept = as.numeric(end), linetype="dashed")
a1  #should I split out received and sent text messages?

#plot potential costs vs. time
a2 <- ggplot()
a2 <- a2 + geom_point(data= all.dat, aes(x=Date, y=cost.if, color = type), size=3)
a2 <- a2+ geom_vline(xintercept = as.numeric(start), linetype="dashed")
a2 <- a2 + geom_vline(xintercept = as.numeric(end), linetype="dashed")
a2 <- a2 + ylim(0,10) #limits to 0 > $10 charges
a2 <- a2 + labs(title="Current Mobile Phone Usage Costs") 
a2 

international.usage<-all.dat[all.dat$Date >= start & all.dat$Date <= end,]
#plot the costs during usage time
i <- ggplot() + geom_point(data=international.usage, aes(x=Date, y=cost.if, color = type), size =3)
i2 <- i + geom_point(data=international.usage, aes(x=Date, y=value), size =5, color = "black") 
i
i2

#find out how much data I used while in Canada (somewhere feb20 around 4pm - to feb 23 around 630pm)
#Calculate the total costs incurred
total.costs<-rowsum(international.usage[,3:4], international.usage$type)
colnames(total.costs) <- c("Min|MB|Texts","Cost.USD")
sum(total.costs$Cost.USD)

#----------------------------------------------------------------------------------------------------
#plot this
p1<- ggplot()
p1<-p1 + geom_point(data=cell.dat$Data, aes(x=Date, y=value), size=3, color="blue")
p1

#plot the minutes / calls made 
p2 <- ggplot()
p2 <- p1+ geom_point(data=cell.dat$Calls, aes(x=Date, y=value), size= 3, color="purple")
p2

#plot the texts made 
p3 <- ggplot()
p3 <- p2+ geom_point(data=cell.dat$Messages, aes(x=Date, y=value), size= 3, color="red")
p3
