
# SR Strand
# Coursera Exploratory Data Analysis Class - Project 1
# 2015/10/11

# Read in power consumption data for household
# Make plots for 2007-02-01 through 2007-02-02

# The first part is the same for all four plots

get_data <- function() 
{
  
# Read in the data
zipfile="exdata-data-household_power_consumption.zip"
datfile="household_power_consumption.txt"
df_power <- read.csv(unz(zipfile,datfile),sep=";",stringsAsFactors=FALSE,header=TRUE)

cat("Dimensions of power data:",dim(df_power),"\n")
for(i in 1:length(df_power)) {
  cat("Column ",i,"Name: ", colnames(df_power)[i],"\n")
  idx_na <- is.na(df_power[,i])
  cat("Number of NA values:", sum(idx_na),"\n\n")
  idx_q <- df_power[i,]=="?"
   any(idx_q)
}

# Convert first column to a date
date_power=as.Date(df_power$Date,format="%d/%m/%Y")

# Specify desired date range
date1="2007-02-01"
date2="2007-02-02"

# Find index of records with desired date range
idx_sel <-(date_power>=date1) & (date_power<=date2)
date_sel = date_power[idx_sel]

cat("There are ",length(date_sel)," records between the date ",date1," and ", date2,"\n")

# Make a subset of the data to the desired dates
df_sel = df_power[idx_sel,]

# Convert date and time columns to a POSIX date
date_sel_str <- as.character(date_sel)
datetime_sel_str <- paste(date_sel_str,df_sel$Time)
datetime_sel <- strptime(datetime_sel_str,format="%Y-%m-%d %H:%M:%S")


# Examine the data set to look for missing values and ? characters
cat("Dimensions of power data subset:",dim(df_sel),"\n")
for(i in 1:length(df_sel)) {
  cat("Column ",i,"Name: ", colnames(df_sel)[i],"\n")
  idx_na <- is.na(df_sel[,i])
  cat("Number of NA values:", sum(idx_na),"\n\n")
  idx_q <- df_sel[i,]=="?"
  cat("Number of ? values:", sum(idx_q),"\n\n")
}

# Numeric data seems to all be characters, so work on the 3rd through last columns to convert them to numeric
df_sel_num = df_sel[,-(1:2)]

# Convert columns to numeric
for(i in 1:length(df_sel_num)) {
  df_sel_num[,i]=as.numeric(df_sel_num[,i])
}

# Make a data frame with the posix date as first column and the numeric columns to the right
df_power_sub <- cbind(datetime=datetime_sel,df_sel_num)
save(df_power_sub,file="df_power_sub.RData")
return(df_power_sub)
}


read_new <- TRUE
if(read_new) {
  df_power_sub <- get_data()
} else {
  load("df_power_sub.RData")  
}

do_plot_2 <- function() {
  plot(Global_active_power ~ datetime, data=df_power_sub,ylab="Global Active Power (kilowatts)",type="l",xlab="")
}


png(filename="plot2.png")
do_plot_2()
dev.off()


