library(ggplot2)
library(stringr)
library(dplyr)
library(tidyverse)

df <- readRDS("./Term_Project/Bird_counts.RDS")
df_point <- readRDS("./Term_Project/point_data.RDS")
df_adj <- readRDS("./Term_Project/dist_adjusted_data.RDS")

names(df_point) <- c("BBA_ID" , "GPS_N", "GPS_W", "Observer", "obscode", "Date", "Start", "Temp","wind_speed", "precip", "cloud_cover", "hemlock_abundance", "hemlock_health", "spruce_abundance", "spruce_qual", "understory", "deer_impact_understory", "land_use", "livestock", "nest_cav", "nest_box", "road_type", "habitat") 

names(df) <- str_replace(names(df), pattern = "\\.", replacement = "_")
names(df) <- str_replace(names(df), pattern = "X", replacement = "Trial")
names(df) <- str_replace(names(df), pattern = "75.1", replacement = "out")
names(df) <- str_replace(names(df), pattern = "75", replacement = "in")

df <- full_join(df, df_point, by="BBA_ID")

#just to check that it lined up
which(is.na(df$BBA_ID)) 

str_replace(names(df), pattern="\\.", replacement = "_")

#going to try and convert trials to "in" and "out" only
df$In75m <- rowSums(df[, c(3,5,7,9,11,13,15)], na.rm = TRUE)
df$Out75m <- rowSums(df[, c(4,6,8,10,12,14)], na.rm=TRUE)
#how to account for fly-overs? maybe include in df$In75m
#reminder: total count is also a column: TotalCounts

#going to see what happens if I omit the columns with the in/out data
df2 <- df[, -c(3:15)]

which(names(df2)=="In75m")
df2 <- df2[, c(1,2,26,27, 3:25)]


#tidying df2?
df2 <- gather(df2, key="range_val", value = "count", 3:5)
which(names(df2)=="range_val")
df2 <- df2[, c(1,2, 25, 26, 3:24)]

#df for reference, still with in/out data intact; same general order as df2 otherwise
df <- df[, c(1:15, 39, 40, 16:38)]







str(df2)

levels(df2$SP_code)
which(df2$SP_code == "#N/A")
#WHY IS THERE AN N/A SPECIES CODE?!

dfNA <- df2[df$SP_code == "#N/A" ,]

saveRDS(df, file="./Term_Project/bird_and_point.RDS")
saveRDS(df2, file="./Term_Project/bird_and_point_short.RDS")


