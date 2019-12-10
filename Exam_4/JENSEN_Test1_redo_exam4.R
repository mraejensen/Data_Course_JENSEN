library(ggplot2)
library(tidyverse)

DNA <- read.csv("./DNA_Conc_by_Extraction_Date.csv")
str(DNA)

####TASK 1####
#create separate histograms of the DNA concentrations for Katy and Ben. 
#Make sure to add nice labels to these (x-axis and main title).
ggplot(DNA, aes(x=DNA_Concentration_Katy))+
  geom_histogram()+
  labs(title = "Katy's DNA", x="Concentration")

ggplot(DNA, aes(x=DNA_Concentration_Ben))+
  geom_histogram()+
  labs(title = "Ben's DNA", x="Concentration")

####TASK 2####
#recreate ZAHN_Plot1.jpeg and ZAHN_Plot2.jpeg
DNA$Year_Collected <- as.factor(DNA$Year_Collected)
plot(x=DNA$Year_Collected, y=DNA$DNA_Concentration_Katy, main="Katy's Extractions", xlab="YEAR", ylab="DNA Concentration")
plot(x=DNA$Year_Collected, y=DNA$DNA_Concentration_Ben, main="Ben's Extractions", xlab="YEAR", ylab="DNA Concentration")

####TASK 3####
#Save the images using code, with your last name instead of ZAHN
jpeg("./JENSEN_Plot1.jpeg")
plot(x=DNA$Year_Collected, y=DNA$DNA_Concentration_Katy, main="Katy's Extractions", xlab="YEAR", ylab="DNA Concentration")
dev.off()

jpeg("./JENSEN_Plot2.jpeg")
plot(x=DNA$Year_Collected, y=DNA$DNA_Concentration_Ben, main="Ben's Extractions", xlab="YEAR", ylab="DNA Concentration")
dev.off()

####TASK 4####
#in which extraction YEAR, was Ben's performance the lowest RELATIVE TO Katy's performance?
DNA$Ben_diff <- DNA$DNA_Concentration_Ben - DNA$DNA_Concentration_Katy
ggplot(DNA, aes(x=Year_Collected, y=Ben_diff))+
  geom_boxplot()+
  theme_minimal()

Year_diff <- DNA %>%
  group_by(Year_Collected)%>%
  summarize(MeanDiff = mean(Ben_diff), MinDiff=min(Ben_diff))

which(Year_diff$MeanDiff == min(Year_diff$MeanDiff))
which(Year_diff$MeanDiff == min(Year_diff$MeanDiff))
Year_diff$Year_Collected[1]
#year: 2000

####TASK 5####
#Subset the data frame so it's just the "Downstairs" lab.
#make a scatterplot of the downstairs lab data such that "Date_Collected" is on the x-axis and "DNA_Concentration_Ben" is on the y-axis. 
#Save this scatterplot as "Ben_DNA_over_time.jpg" in your Exam_1 directory
#may need some class conversions
class(DNA$Lab)
levels(DNA$Lab)
#just checking that Upstairs and Downstairs are the only values, not lowercase or anything
Down <- DNA[DNA$Lab == "Downstairs",]
class(Down$Date_Collected)
levels(Down$Date_Collected)
#for format reasons
#coerce to POSIX.ct
Down$Date_Collected <- as.Date(Down$Date_Collected, format = "%Y-%m-%d")
class(Down$Date_Collected)
Down$Date_Collected <- as.POSIXct(Down$Date_Collected)
class(Down$Date_Collected)

jpeg("./Ben_DNA_over_time.jpeg")
plot(x=Down$Date_Collected, y=Down$DNA_Concentration_Ben, xlab="Date_Collected", ylab="DNA_Concentration_Ben")
dev.off()

####TASK 6####
#make a new data frame (just using Ben's values) that has one column containing the years that DNA extractions were made, 
#and another column that contains the AVERAGE of the values within that year.  
#Just to be clear, this data frame should have only 12 rows (one for each year)
#write some code that shows which extraction year has the highest average DNA concentration (and what that concentration is) 
#save the 12-row dataframe as a new csv file called "Ben_Average_Conc.csv"

Ben_AVG <- DNA %>%
  group_by(Year_Collected)%>%
  summarize(Mean = mean(DNA_Concentration_Ben))

#Greatest mean concentration over each year
Ben_AVG[max(Ben_AVG$Mean),]

write.csv(Ben_AVG, "./Ben_Average_Conc.csv")


