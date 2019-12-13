library(dplyr)

df <- read.csv("./Term_Project/2ndPA_BirdAtlas_bird_counts.csv")
df_point <- read.csv("./Term_Project/2ndPA_BirdAtlas_pointcount_location_and_conditions.csv")
df_adj1 <- read.csv("./Term_Project/adjusted_1km_allvars.csv")
df_adj2 <- read.csv("./Term_Project/adjusted_2km_allvars.csv")
df_adj4 <- read.csv("./Term_Project/adjusted_4km_allvars.csv")
df_adj6 <- read.csv("./Term_Project/adjusted_6km_allvars.csv")
df_adj8 <- read.csv("./Term_Project/adjusted_8km_allvars.csv") 
df_adj10 <- read.csv("./Term_Project/adjusted_10km_allvars.csv")
df_adj12 <- read.csv("./Term_Project/adjusted_12km_allvars.csv")
df_adj16 <- read.csv("./Term_Project/adjusted_16km_allvars.csv")
df_adj200 <- read.csv("./Raw_Data/Data/adjusted_200m_allvars.csv")
df_adj500 <- read.csv("./Raw_Data/Data/adjusted_500m_allvars.csv")


str(df)
str(df_point)
str(df_adj1)

which(!(df_adj1$LID %in% df$BBA_ID))
which(!(df$BBA_ID %in% df_adj1$LID))

which(!(df$BBA_ID %in% df_point$BBA_ID))
which(!(df_point$BBA_ID %in% df$BBA_ID))

#df and df_point have the same values for the sample region ID, but there are differences from df_adj1
#there are values missing from df that are in df_adj1, and vice versa

which(!(df_adj1$LID %in% df_adj2$LID))

#some values are the same in both adusted distance files...
#maybe if I conglomerated all of the adj files, they would match up with the df and df_point?
#keeping the distance adjustment
df_adj1$dist <- 1
df_adj2$dist <- 2
df_adj4$dist <- 4
df_adj6$dist <- 6
df_adj8$dist <- 8
df_adj10$dist <- 10
df_adj12$dist <- 12
df_adj16$dist <- 16
df_adj200$dist <- 200
df_adj500$dist <- 500
#dist for distance, to distinguish between datasets

adj <- bind_rows(df_adj1, df_adj2, df_adj4, df_adj6, df_adj8, df_adj10, df_adj12, df_adj16)

adj2 <- bind_rows(df_adj200, df_adj500)

adj <- adj[, -1]
which(colnames(adj2)=="MQCOMP2")
adj2 <- adj2[,-22]

colnames(adj)
colnames(adj2)

adj_fin <- bind_rows(adj, adj2)

adj_fin$dist <- as.factor(adj_fin$dist)

which(!(adj_fin$LID %in% df$BBA_ID))
which(!(df$BBA_ID %in% adj_fin$LID))

#still no luck
which(adj_fin$LID %in% df$BBA_ID)
which(df$BBA_ID %in% adj_fin$LID)

#there are more in common than not though

udf <- unique(df$BBA_ID)
uadj <- unique(adj_fin$LID)

length(udf)
length(uadj)

#more unique values in udf than in uadj
#starting to clean up the adjusted values:
adj_fin <- na.omit(adj_fin)

which(is.na(df$BBA_ID))

#try to find total occurrences of each species in each area

str(df)
df$Non.songCue.75.1 <- as.integer(as.character(df$Non.songCue.75.1))
str(df)

df$TotalCounts <- rowSums(df[, -c(1, 2)], na.rm = TRUE)

write.csv(df, file = "./Term_Project/Bird_counts.csv")
saveRDS(df, file = "./Term_Project/Bird_counts.RDS")
saveRDS(adj_fin, file = "./Term_Project/dist_adjusted_data.RDS")
saveRDS(df_point, file = "./Term_Project/point_data.RDS")









