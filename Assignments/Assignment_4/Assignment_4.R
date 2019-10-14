ITS_mapping <- read.delim("../../../Data_Course/Data/ITS_mapping.csv", header = TRUE, sep = "")

levels(ITS_mapping$SampleID)
names(ITS_mapping)

which(ITS_mapping$SampleID == "9.9.2002")
levels(ITS_mapping$BarcodeSequence)
levels(ITS_mapping$LinkerPrimerSequence)
levels(ITS_mapping$Run)
levels(ITS_mapping$Ecosystem)
levels(ITS_mapping$Island)
levels(ITS_mapping$Lat)
which(ITS_mapping$Lat == "1.10.2010")
which(ITS_mapping$Lat == "9.9.2002")
which(ITS_mapping$SampleID == "1.10.2010")
levels(ITS_mapping$Lon)
levels(ITS_mapping$Collection_Date)
levels(ITS_mapping$F_Primer)
levels(ITS_mapping$R_Primer)
levels(ITS_mapping$Ecosys_Type)
levels(ITS_mapping$Host_Type)

halp <- ITS_mapping[939:1315 ,]
colnames(halp)[colnames(halp) == "SampleID"] <- "Collection_date"
colnames(halp)[colnames(halp) == "Run"] <- "island"
colnames(halp)[colnames(halp) == "Ecosystem"] <- "lat"
colnames(halp)[colnames(halp) == "Island"] <- "long"
colnames(halp)[colnames(halp) == "Lon"] <- "Fprimer"
colnames(halp)[colnames(halp) == "Collection_Date"] <- "Rprimer"
colnames(halp)[colnames(halp) == "F_Primer"] <- "Ecosystem_type"
colnames(halp)[colnames(halp) == "Ecosys_Type"] <- "Input_filename"
colnames(halp)[colnames(halp) == "Host_Type"] <- "Description"
colnames(halp)[colnames(halp) == "LinkerPrimerSequence"] <- "Ecosystem"
colnames(halp)[colnames(halp) == "BarcodeSequence"] <- "unknown1"
colnames(halp)[colnames(halp) == "Lat"] <- "unknown2"
colnames(halp)[colnames(halp) == "Host"] <- "unknown3"
colnames(halp)[colnames(halp) == "InputFileName"] <- "unknown4"
colnames(halp)[colnames(halp) == "Description"] <- "unknown5"
colnames(halp)[colnames(halp) == "R_Primer"] <- "na.column"
str(halp)

halp$Collection_date <- as.character(halp$Collection_date)








