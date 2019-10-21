ITS_mapping <- read.delim("../../../Data_Course/Data/ITS_mapping.csv", 
                          header = TRUE, sep = "\t")
summary(ITS_mapping)
str(ITS_mapping)
class(ITS_mapping$Run)
levels(ITS_mapping$Ecosys_Type)
mean(ITS_mapping$Lat, na.rm = TRUE)


png("./silly_boxplot.png")
plot(x=ITS_mapping$Ecosystem, y=ITS_mapping$Lat, xlab="Ecosystem", ylab="Latitude", main="Latitude vs. Ecosystem")
dev.off()







