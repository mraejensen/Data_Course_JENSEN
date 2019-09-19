DNA_ext <- read.csv("DNA_Conc_by_Extraction_Date.csv")
library(ggplot2)

hist(DNA_ext$DNA_Concentration_Katy, main="Katy's DNA Concentration", xlab="Concentration")

hist(DNA_ext$DNA_Concentration_Ben, main="Ben's DNA Concentration", xlab="Concentration", breaks=30)  

DNA_ext$year <- as.factor(DNA_ext$Year_Collected)

plot(x=DNA_ext$year, y=DNA_ext$DNA_Concentration_Katy, main="Katy's Extractions", xlab="YEAR", ylab="DNA Concentration")

plot(x=DNA_ext$year, y=DNA_ext$DNA_Concentration_Ben, main="Ben's Extractions", xlab="YEAR", ylab="DNA Concentration")

jpeg("JENSEN_Plot1.jpeg")
plot(x=DNA_ext$year, y=DNA_ext$DNA_Concentration_Katy, main="Katy's Extractions", xlab="YEAR", ylab="DNA Concentration")
dev.off()

jpeg("JENSEN_Plot2.jpeg")
plot(x=DNA_ext$year, y=DNA_ext$DNA_Concentration_Ben, main="Ben's Extractions", xlab="YEAR", ylab="DNA Concentration")
dev.off()

DNA_ext$Ben_over <- DNA_ext$DNA_Concentration_Ben - DNA_ext$DNA_Concentration_Katy
plot(DNA_ext, x=DNA_ext$year, y=DNA_ext$Ben_over)


Downstairs_Lab <- DNA_ext[DNA_ext$Lab=="Downstairs",]

Downstairs_Lab$date <- as.Date(Downstairs_Lab$Date_Collected)

plot(Downstairs_Lab, x=Downstairs_Lab$Date_Collected, y=Downstairs_Lab$DNA_Concentration_Ben)

jpeg("Ben_DNA_Over_Time.jpeg")
plot(Downstairs_Lab, x=Downstairs_Lab$Date_Collected, y=Downstairs_Lab$DNA_Concentration_Ben)
dev.off()










