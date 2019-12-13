library(stringr)

df <- readRDS("./bird_and_point.RDS")

str(df)

levels(df$SP_code)

which(df$SP_code == "#N/A")

dfNA <- df[df$SP_code == "#N/A",]

df2 <- df[-c(287594, 310599, 326870, 326871, 339483) ,]
#df2_trial <- df[!df$SP_code == "#N/A",]
#ok the second one works the same way, but is MUCH shorter to type in general; will use that from now on

unique(df2$Date)
class(df2$Date)

which(df2$Date == "")

df2BlankDate = df2[df2$Date == "",]
#not much other data in the blank dates

df3 <- df2[!df2$Date == "" ,]

#class(df3$habitat)

#which(is.na(df3$habitat))
#NoHab <- df3[is.na(df3$habitat),]
#mostly just missing habitat row
#which(is.na(df3$land_use))
#NoLand <- df3[is.na(df3$land_use),]
#unique(df3$Start)
#which(is.na(df3$BBA_ID))
#which(is.na(df3$GPS_N))
#which(is.na(df3$GPS_W))
#which(is.na(df3$Temp))

#NoTemp <- df3[is.na(df3$Temp),]
#turns out there is no data beyond the count data in part of this frame...
#NoStart <- df3[is.na(df3$Start) ,]

#which(NoTemp$Date %in% NoStart$Date)
#which(NoStart$Date %in% NoTemp$Date)
#so the first 45 entries of both of these are the same, I think...
#which(NoTemp$BBA_ID %in% NoStart$BBA_ID)
#yeah, seems like it...

#Different approach; going to try conglomerating things again, getting rid of the individual trial counts, THEN get rid of NA
df4 <- df3
df4 <- df4[c(1,2,16:40)]

df4_cc <- df4[complete.cases(df4),]
#just to check that complete cases worked...
which(is.na(df4_cc))
#awesome
#this might be the best bet, just seeing how it works on the data... I need complete data

str(df4_cc)
levels(df4_cc$Date)
which(df4_cc$Date == "")

df4_cc$Date <- as.Date(df4_cc$Date, format = "%m/%d/%y")
df4_cc$Start <- str_pad(df4_cc$Start, 4, pad=0)
df4_cc$POSIX <- as.POSIXct(paste(df4_cc$Date, df4_cc$Start), format="%Y-%m-%d %H%M")
str(df4_cc)
#ok going to rearrange
df4_cc <- df4_cc[c(1:11, 28, 12:27)]
df4_cc2 <- df4_cc[c(1:9, 12:28)]
#not sure if I'm going to keep this, since the time and date can count as different variables


write.csv(df4_cc, file = "./clean_counts.csv")
#AAAND just in case, i'll save it as an RDS
saveRDS(df4_cc, file = "./clean_counts.RDS")





