#Assignment

#The basic goal of this assignment is to explore the NOAA Storm Database and answer some basic questions about severe weather events. 
#You must use the database to answer the questions below and show the code for your entire analysis. Your analysis can consist of tables, 
#figures, or other summaries. You may use any R package you want to support your analysis.

#Questions

#Your data analysis must address the following questions:
  
#Across the United States, which types of events (as indicated in the EVTYPE variable) are most harmful with respect to population health?

#Across the United States, which types of events have the greatest economic consequences?

#Consider writing your report as if it were to be read by a government or municipal manager who might be responsible for preparing for severe 
#weather events and will need to prioritize resources for different types of events. However, there is no need to make any specific recommendations 
#in your report.

#MUST INCLUDE:
#title, synopsis, data processing section, results section, 1-3 figures.

#set working directory for project
setwd("~/Desktop/Programs/RepData_PeerAssessment2/")

#download file if not in current directory
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
if (file.exists('Storm.Data.csv.bz2') == FALSE) {
  download.file(url, dest='Storm.Data.csv.bz2', method = 'curl') }

#load data
stormData <- read.csv(bzfile('Storm.Data.csv.bz2'))

#subset columns relevent for analysis and convert to lower case
stormData2 <- subset(stormData, select = c("BGN_DATE","EVTYPE", "FATALITIES","INJURIES","PROPDMG","PROPDMGEXP","CROPDMG","CROPDMGEXP"))
names(stormData2) <- tolower(names(stormData2))

#subset for years 1996 and later (more thorough classification of weather events records per documentation)
stormData2$bgn_date <- as.Date(as.character(stormData2$bgn_date), "%m/%d/%Y")
head(stormData2)
stormDataRecent <- subset(stormData2, stormData2$bgn_date > "1995-12-31") 

#clean up evtype values (weather event descriptions)
length(levels(stormDataRecent$evtype)) #985 types of weather events
stormDataRecent$evtype <- as.factor(tolower(stormDataRecent$evtype))
containsummary <- grepl('summary', stormDataRecent$evtype)
stormDataFinal$evtype <- gsub("tstm","thunderstorm", stormDataFinal$evtype)
stormDataFinal <- stormDataFinal[!containsummary,] #remove summary records
table(stormDataFinal$evtype) #determine the most frequent events, relatively minimal occurances in "other" category so decision not to clean up values further.

#aggregate fatalities and injuries by event type and then sort in descending order, subset first 10                               
head(stormDataFinal)
fatalities <- aggregate(fatalities ~ evtype, stormDataFinal, sum)
fatalitiesOrder <- fatalities[order(fatalities$fatalities, decreasing = TRUE), ] 
injuries <- aggregate(injuries ~ evtype, stormDataFinal, sum)
injuriesOrder <- injuries[order(injuries$injuries, decreasing = TRUE), ] 
injuriesOrderPlot <- injuriesOrder[1:10,]
fatalitiesOrderPlot <- fatalitiesOrder[1:10,]

#renumber key/rows!!!!!!!!!!!!!!
#revalue events (heat and excessive heat??)

#top 10
fatalitiesOrder[1:10,]
injuriesOrder[1:10,]
library(ggplot2)

ggplot(data = injuriesOrderPlot, aes(x = evtype, y = injuries)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Total Human Injuries by Weather Event in the U.S.,\n 1996 - 2011") +
  coord_flip() +
  theme_bw(base_family = "Avenir") +
  labs(x = "Weather Event") +
  labs(y = "Total Number of Injuries")

ggplot(data = fatalitiesOrderPlot, aes(x = evtype, y = fatalities)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Total Human Fatalities by Weather Event in the U.S.,\n 1996 - 2011") +
  coord_flip() +
  theme_bw(base_family = "Avenir") +
  labs(x = "Weather Event") +
  labs(y = "Total Number of Fatalities")

#identify economic impacts
names(stormData)
