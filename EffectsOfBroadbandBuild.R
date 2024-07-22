# Link to .xml file:
# <https://drive.google.com/file/d/1glvqlRvuzHgvwbv9QBN2-Fc5GvuhGDJV/view?usp=drive_link>
# Link to .dat file:
# <https://drive.google.com/file/d/1mTCeVp7XNXZJaeXV1cY-mGGzXh2H-KT1/view?usp=drive_link>

# Sample data is 2016 - 2021 ACS Surveys acquired via IPUMS

# Set working directory (***This will need to be changed if ran on another machine***)
setwd("C:/Users/jack/Downloads")

# Load Data
library("ipumsr")
# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).

if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

ddi <- read_ipums_ddi("usa_00006.xml")
data <- read_ipums_micro(ddi)

# Remove NAs
data<-na.omit(data)

# Remove data with NIU
data<-subset(data, CINETHH!=0 & CILAPTOP!=0 & CISMRTPHN!= 0 & CITABLET!=0 & CIOTHCOMP!=0 & CIHISPEED!=00 & FTOTINC!=9999999 & EDUC!=00 & STATEICP!=99)

# Prune data so that all observations in the dataset are the head of household
data<-subset(data, RELATE==01)

# Special case if the observation is from Alaska or Hawaii, federal poverty line is different for these households
# For Alaskan observations:

data<-subset(data, (STATEICP==81 & ((FAMSIZE==1&FTOTINC<36420) | (FAMSIZE==2&FTOTINC<49280) | (FAMSIZE==3&FTOTINC<62140) | (FAMSIZE==4&FTOTINC<75000) | (FAMSIZE==5&FTOTINC<87860) | (FAMSIZE==6&FTOTINC<100720) | (FAMSIZE==7&FTOTINC<113580) |
                                      (FAMSIZE==8&FTOTINC<126440))) | STATEICP!=81)
# For Hawaiian observations:
data<-subset(data, (STATEICP==82 & ((FAMSIZE==1&FTOTINC<33540) | (FAMSIZE==2&FTOTINC<45360) | (FAMSIZE==3&FTOTINC<57180) | (FAMSIZE==4&FTOTINC<69000) | (FAMSIZE==5&FTOTINC<80820) | (FAMSIZE==6&FTOTINC<92640) | (FAMSIZE==7&FTOTINC<104460) |
                                      (FAMSIZE==8&FTOTINC<116280))) | STATEICP!=82)

# Prune data further so that all observations in the dataset are below 200% of the federal poverty line <https://aspe.hhs.gov/sites/default/files/documents/1c92a9207f3ed5915ca020d58fe77696/detailed-guidelines-2023.pdf>
data <- subset(data, (FAMSIZE==1&FTOTINC<29160) | (FAMSIZE==2&FTOTINC<39440) | (FAMSIZE==3&FTOTINC<49720) | (FAMSIZE==4&FTOTINC<60000) | (FAMSIZE==5&FTOTINC<70280) | (FAMSIZE==6&FTOTINC<80560) | (FAMSIZE==7&FTOTINC<90840) |
                 (FAMSIZE==8&FTOTINC<101120))

# Create new variable that denotes whether an observation has broadband internet access of any kind.
data$HASBBIA<-1
data$HASBBIA[which(data$CIHISPEED==20)]<-0

# Log transform the dependent variable
data$LOGINCTOT<-log(data$INCTOT)
#Clean up NANs and INFs produced
data[is.na(data) | data==Inf | data==-Inf] = NA
data<-na.omit(data)

# Save dataset here
write.csv(data, "C:/Users/jack/Downloads/HOHData.csv", row.names = FALSE)
