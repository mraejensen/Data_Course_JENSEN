library(stringr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(modelr)

#will be commenting out ggplots after I use them for running code, since they take 

count1 <- readRDS("./clean_counts.RDS")
str(count1)
levels(count1$SP_code)
count1$Start <- as.integer(count1$Start)

#month is not very helpful, so i'm changing it
count1$month <- month(count1$Date)
class(count1$month)
count1$month <- month.abb[count1$month]
class(count1$month)
which(names(count1)=="POSIX")
#reordering now...
count1 <- count1[c(1:12, 29, 13:28)]
count1$month <- as.factor(count1$month)
levels(count1$month)
#yeah that isn't even in migration season... will have to find different ways to do that

names(count1)

#I'm trying to find the max counts and identifying the species with it, but I can't figure it out
count1 %>%
  group_by(BBA_ID)%>%
  summarize(maxCount = max(TotalCounts))

#trial 2:
count1 %>%
  select(BBA_ID, TotalCounts, SP_code)%>%
  group_by(BBA_ID)%>%
  summarize(maxCounts=max(TotalCounts))
#ok that LITERALLY just did the exact same thing

#trial 3
MaxCount <- count1 %>%
  select(BBA_ID, TotalCounts, SP_code)%>%
  group_by(BBA_ID)%>%
  filter(TotalCounts == max(TotalCounts))
#ok got it!!
#there are more entries than area codes because there were several tied for maximum
#going to try doing the maxCount later, since I did something else that may affect it somewhat

levels(MaxCount$SP_code)
#it looks like there's not a lot of variation from the actual values of SP_code

#if I could find out which species are predatory and which are not, I could subset based on predatory or not
#also maybe by overall form (passerine, raptor, wading bird, duck, etc)
#maybe that could count as niche?
count1duck <- count1[count1$SP_code == c("ABDU", "AMCO", "AMWI", "BUFF", "BWTE", "CAGO", "CANV", "COLO", "COME", "GADW", "GWTE", "HOME", "LESC", "MALL", "MUSW", "NOPI", "PBGR", "RBME", "RNDU", "RUDU", "TRUS", "WODU"),]
count1wade <- count1[count1$SP_code == c("AMBI", "BCNH", "BEKI", "CAEG", "COMO", "GBHE", "GLIB", "GREG", "GRHE", "KIRA", "LEBI", "SACR", "SNEG", "SORA", "VIRA", "WISN", "YCNH", "YEAR"),]
count1pass <- count1[count1$SP_code == c("ACFL", "ALFL", "AMCR", "AMGO", "AMRE", "AMRO", "BANS", "BAOR", "BARS", "BAWW", "BBCU", "BCCH", "BGGN", "BHCO", "BHVI", "BLBW", "BLGR", "BLJA", "BLPW", "BOBO", "BRCR", "BRTH", "BRWA", "BTBW", "BTNW", "BWWA", "CACH", "CARW", "CAWA", "CCSP", "CEDW", "CERW", "CHSP", "CLSW", "COGR", "CORA", "COYE", "CSWA", "DEJU", "DICK", "EABL", "EAKB", "EAME", "EAPH", "EATO", "EAWP", "EUST", "FICR", "FISP", "GCFL", "GCKI", "GRCA", "GRSP", "GWWA", "HESP",  "HETH", "HOFI", "HOLA", "HOSP", "HOWA", "HOWR", "INBU", "KEWA", "LASP", "LAWA", "LEFL", "LOSH", "LOWA", "MAWA", "MAWR", "MOWA", "NAWA", "NOBO", "NOCA", "NOMO", "NOPA", "NOWA", "NRWS", "OROR", "OSFL", "OVEN", "PISI", "PIWA", "PRAW", "PROW", "PUFI", "PUMA", "RBGR", "RBNU", "RCKI", "RECR", "REVI", "RWBL", "SAVS", "SCTA", "SEWR", "SOSP", "SUTA", "SWSP", "SWTH", "SWWA", "TRES", "TUTI", "VEER", "VESP", "WAVI", "WBNU", "WEME", "WEVI", "WEWA", "WIFL", "WIWR", "WOTH", "WTSP", "WWCR", "YBCH", "YBCU", "YBFL", "YRWA", "YTVI", "YTWA", "YWAR"),]  
count1rapt <- count1[count1$SP_code == c("AMKE", "BADO", "BAEA", "BANO", "BLVU", "BWHA", "CLSW", "COHA", "EASO", "GHOW", "LEOW", "MERL", "NOGO", "NOHA", "NSWO", "OSPR", "PEFA", "RSHA", "RTHA", "SEOW", "SSHA", "TUVU"),]
count1shor <- count1[count1$SP_code == c("AMWO", "KILL", "SPSA", "UPSA"),]
count1tern <- count1[count1$SP_code == c("BLTE", "BOGU", "CATE", "COTE", "DCCO", "FOTE", "GBBG", "HEGU", "LAGU", "LBBG", "LETE", "RBGU", "RHWO"),]
count1game <- count1[count1$SP_code == c("CHUK", "RNPH", "RUGR", "WITU"),]
count1goat <- count1[count1$SP_code == c("CONI", "CWWI", "WPWI"),]
count1peck <- count1[count1$SP_code == c("DOWO", "HAWO", "NOFL", "PIWO", "RBWO", "YBSA"),]
count1dove <- count1[count1$SP_code == c("EUCD", "MODO", "ROPI"),]
count1other <- count1[count1$SP_code == c("CHSW", "RTHU"),]
count1unk <- count1[count1$SP_code == c("XXXX", "ZZZZ"),]
#this could be used to see if there is a difference in distribution of range with, say, collared doves and pigeons (invasive) compared to mourning dove (native)
#JUST TO NAME AN EXAMPLE

#Now: going to add a species type column to the dataframes, then combine them again
count1duck$species_type <- "Anatid"
count1wade$species_type <- "Wading_bird"
count1pass$species_type <- "Passerine"
count1rapt$species_type <- "Raptor"
count1shor$species_type <- "Shorebird"
count1tern$species_type <- "Larid"
count1game$species_type <- "Galliform"
count1goat$species_type <- "Goatsucker"
count1peck$species_type <- "Woodpecker"
count1dove$species_type <- "Dove"
count1other$species_type <- "Other"
count1unk$species_type <- "Unknown"
#mostly based on niche levels and/or highest organization that way, which might be sick and wrong, but it could potentially be useful

count2 <- rbind(count1duck, count1wade, count1pass, count1rapt, count1shor, count1tern, count1game, count1goat, count1peck, count1dove, count1other, count1unk)
count2 <- count2[order(count2$BBA_ID),]

MaxCount2 <- count2 %>%
  group_by(BBA_ID) %>%
  filter(TotalCounts == max(TotalCounts))
MaxCount2 <- MaxCount2[order(MaxCount2$BBA_ID),]

#wait count2 has way fewer entries than my previous dataset...
levels(count1$SP_code)
levels(count2$SP_code)
#same number of levels, so I didn't leave anything off...

sum(count1$SP_code == "AMKE")
sum(count2$SP_code == "AMKE")
#aw crap I lost data somewhere... 
#looks like it was when I split the dataset
nrow(count1dove)+ nrow(count1duck)+ nrow(count1game)+ nrow(count1goat)+ nrow(count1other)+ nrow(count1pass)+ nrow(count1peck)+ nrow(count1rapt)+ nrow(count1shor)+ nrow(count1tern)+ nrow(count1wade) +nrow(count1unk)
#so HOW DOES THAT WORK?!
#literally, how did I go from 495 thousand rows to just over 13 thousand?!
sum(count1$SP_code == "CHSW")
sum(count1$SP_code == c("CHSW", "RTHU"))
sum(count1$SP_code == "AMKE")
sum(count1$SP_code == c("AMKE", "BADO", "BAEA", "BANO", "BLVU", "BWHA", "CLSW", "COHA", "EASO", "GHOW", "LEOW", "MERL", "NOGO", "NOHA", "NSWO", "OSPR", "PEFA", "RSHA", "RTHA", "SEOW", "SSHA", "TUVU"))
#how is that LESS?!
#maybe it uses the BBA_ID that has all of the above?
counttest <- count1[count1$SP_code %in% c("CHSW", "RTHU"),]
#since there is a lot of overlap between columns, I am going to add a number column (for later use)
count1$number <- 1:nrow(count1)
#that gave a higher number of rows, so let's try that and see if it works for everything else
count1duck <- count1[count1$SP_code %in% c("ABDU", "AMCO", "AMWI", "BUFF", "BWTE", "CAGO", "CANV", "COLO", "COME", "GADW", "GWTE", "HOME", "LESC", "MALL", "MUSW", "NOPI", "PBGR", "RBME", "RNDU", "RUDU", "TRUS", "WODU"),]
count1wade <- count1[count1$SP_code %in% c("AMBI", "BCNH", "BEKI", "CAEG", "COMO", "GBHE", "GLIB", "GREG", "GRHE", "KIRA", "LEBI", "SACR", "SNEG", "SORA", "VIRA", "WISN", "YCNH", "YEAR"),]
count1pass <- count1[count1$SP_code %in% c("ACFL", "ALFL", "AMCR", "AMGO", "AMRE", "AMRO", "BANS", "BAOR", "BARS", "BAWW", "BBCU", "BCCH", "BGGN", "BHCO", "BHVI", "BLBW", "BLGR", "BLJA", "BLPW", "BOBO", "BRCR", "BRTH", "BRWA", "BTBW", "BTNW", "BWWA", "CACH", "CARW", "CAWA", "CCSP", "CEDW", "CERW", "CHSP", "CLSW", "COGR", "CORA", "COYE", "CSWA", "DEJU", "DICK", "EABL", "EAKI", "EAME", "EAPH", "EATO", "EAWP", "EUST", "FICR", "FISP", "GCFL", "GCKI", "GRCA", "GRSP", "GWWA", "HESP",  "HETH", "HOFI", "HOLA", "HOSP", "HOWA", "HOWR", "INBU", "KEWA", "LASP", "LAWA", "LEFL", "LOSH", "LOWA", "MAWA", "MAWR", "MOWA", "NAWA", "NOBO", "NOCA", "NOMO", "NOPA", "NOWA", "NRWS", "OROR", "OSFL", "OVEN", "PISI", "PIWA", "PRAW", "PROW", "PUFI", "PUMA", "RBGR", "RBNU", "RCKI", "RECR", "REVI", "RWBL", "SAVS", "SCTA", "SEWR", "SOSP", "SUTA", "SWSP", "SWTH", "SWWA", "TRES", "TUTI", "VEER", "VESP", "WAVI", "WBNU", "WEME", "WEVI", "WEWA", "WIFL", "WIWR", "WOTH", "WTSP", "WWCR", "YBCH", "YBCU", "YBFL", "YRWA", "YTVI", "YTWA", "YWAR"),]  
count1rapt <- count1[count1$SP_code %in% c("AMKE", "BADO", "BAEA", "BANO", "BLVU", "BWHA", "CLSW", "COHA", "EASO", "GHOW", "LEOW", "MERL", "NOGO", "NOHA", "NSWO", "OSPR", "PEFA", "RSHA", "RTHA", "SEOW", "SSHA", "TUVU"),]
count1shor <- count1[count1$SP_code %in% c("AMWO", "KILL", "SPSA", "UPSA"),]
count1tern <- count1[count1$SP_code %in% c("BLTE", "BOGU", "CATE", "COTE", "DCCO", "FOTE", "GBBG", "HEGU", "LAGU", "LBBG", "LETE", "RBGU", "RHWO"),]
count1game <- count1[count1$SP_code %in% c("CHUK", "RNPH", "RUGR", "WITU"),]
count1goat <- count1[count1$SP_code %in% c("CONI", "CWWI", "WPWI"),]
count1peck <- count1[count1$SP_code %in% c("DOWO", "HAWO", "NOFL", "PIWO", "RBWO", "YBSA"),]
count1dove <- count1[count1$SP_code %in% c("EUCD", "MODO", "ROPI"),]
count1other <- count1[count1$SP_code %in% c("CHSW", "RTHU"),]
count1unk <- count1[count1$SP_code %in% c("XXXX", "ZZZZ"),]
#those dataframe observation numbers look a lot better...
nrow(count1dove)+ nrow(count1duck)+ nrow(count1game)+ nrow(count1goat)+ nrow(count1other)+ nrow(count1pass)+ nrow(count1peck)+ nrow(count1rapt)+ nrow(count1shor)+ nrow(count1tern)+ nrow(count1wade) +nrow(count1unk)
#still missing 3000 or so rows
counttest2 <- rbind(count1dove, count1duck, count1game, count1goat, count1other, count1pass, count1peck, count1rapt, count1shor, count1tern, count1unk, count1wade)
counttest1 <- count1
#antijoin by row number

count_missing <- anti_join(counttest1, counttest2, by = "number")

nrow(counttest1) - nrow(counttest2)
nrow(count_missing)
#ok that number doesn't match up now
count_missing <- counttest1[!(counttest1$number %in% counttest2$number),]
#that gives the same number of rows as the antijoin... maybe there is an error in subtraction?

count_missing$SP_code <- as.character(count_missing$SP_code)
unique(count_missing$SP_code)
#LEYE: lesser yellowlegs - shorebird (DONE)
#RODO: same as ROPI (Rock pigeon/rock dove)
#TRFL: combined with willow flycatcher, WIFL
#HYCH: hybrid chickadee - passerine (this is likely a hybrid of carolina and black capped chickadees) (DONE)
#BDOW: same as BADO
#MUSC: can't find anything on it; maybe a typo of Mute swan?
#ESOW: same as EASO, Eastern Screech owl
#SNGO: snow goose, wasn't on the original - anatid (DONE)
#WCSP: white crowned sparrow, wasn't on original - passerine (DONE)
#CMWA: cape may warbler - passerine (DONE)
#ECDO: eurasian collared dove, same as EUCD
#SBDO:short billed dowitcher - shorebird (DONE)

count1$SP_code <- as.character(count1$SP_code)
count1$SP_code[count1$SP_code == "RODO"] <- "ROPI"
count1$SP_code[count1$SP_code == "TRFL"] <- "WIFL"
count1$SP_code[count1$SP_code == "BDOW"] <- "BADO"
count1$SP_code[count1$SP_code == "ESOW"] <- "EASO"
count1$SP_code[count1$SP_code == "ECDO"] <- "EUCD"

count1$SP_code <- as.factor(count1$SP_code)
levels(count1$SP_code)

#now to add the ones I missed in the right categories...
#just going to do the whole thing since it is being stubborn
count1duck <- count1[count1$SP_code %in% c("SNGO","ABDU", "AMCO", "AMWI", "BUFF", "BWTE", "CAGO", "CANV", "COLO", "COME", "GADW", "GWTE", "HOME", "LESC", "MALL", "MUSW", "NOPI", "PBGR", "RBME", "RNDU", "RUDU", "TRUS", "WODU"),]
count1wade <- count1[count1$SP_code %in% c("AMBI", "BCNH", "BEKI", "CAEG", "COMO", "GBHE", "GLIB", "GREG", "GRHE", "KIRA", "LEBI", "SACR", "SNEG", "SORA", "VIRA", "WISN", "YCNH", "YEAR"),]
count1pass <- count1[count1$SP_code %in% c("CMWA", "WCSP", "HYCH", "ACFL", "ALFL", "AMCR", "AMGO", "AMRE", "AMRO", "BANS", "BAOR", "BARS", "BAWW", "BBCU", "BCCH", "BGGN", "BHCO", "BHVI", "BLBW", "BLGR", "BLJA", "BLPW", "BOBO", "BRCR", "BRTH", "BRWA", "BTBW", "BTNW", "BWWA", "CACH", "CARW", "CAWA", "CCSP", "CEDW", "CERW", "CHSP", "CLSW", "COGR", "CORA", "COYE", "CSWA", "DEJU", "DICK", "EABL", "EAKI", "EAME", "EAPH", "EATO", "EAWP", "EUST", "FICR", "FISP", "GCFL", "GCKI", "GRCA", "GRSP", "GWWA", "HESP",  "HETH", "HOFI", "HOLA", "HOSP", "HOWA", "HOWR", "INBU", "KEWA", "LASP", "LAWA", "LEFL", "LOSH", "LOWA", "MAWA", "MAWR", "MOWA", "NAWA", "NOBO", "NOCA", "NOMO", "NOPA", "NOWA", "NRWS", "OROR", "OSFL", "OVEN", "PISI", "PIWA", "PRAW", "PROW", "PUFI", "PUMA", "RBGR", "RBNU", "RCKI", "RECR", "REVI", "RWBL", "SAVS", "SCTA", "SEWR", "SOSP", "SUTA", "SWSP", "SWTH", "SWWA", "TRES", "TUTI", "VEER", "VESP", "WAVI", "WBNU", "WEME", "WEVI", "WEWA", "WIFL", "WIWR", "WOTH", "WTSP", "WWCR", "YBCH", "YBCU", "YBFL", "YRWA", "YTVI", "YTWA", "YWAR"),]  
count1rapt <- count1[count1$SP_code %in% c("AMKE", "BADO", "BAEA", "BANO", "BLVU", "BWHA", "CLSW", "COHA", "EASO", "GHOW", "LEOW", "MERL", "NOGO", "NOHA", "NSWO", "OSPR", "PEFA", "RSHA", "RTHA", "SEOW", "SSHA", "TUVU"),]
count1shor <- count1[count1$SP_code %in% c("SBDO","LEYE", "AMWO", "KILL", "SPSA", "UPSA"),]
count1tern <- count1[count1$SP_code %in% c("BLTE", "BOGU", "CATE", "COTE", "DCCO", "FOTE", "GBBG", "HEGU", "LAGU", "LBBG", "LETE", "RBGU", "RHWO"),]
count1game <- count1[count1$SP_code %in% c("CHUK", "RNPH", "RUGR", "WITU"),]
count1goat <- count1[count1$SP_code %in% c("CONI", "CWWI", "WPWI"),]
count1peck <- count1[count1$SP_code %in% c("DOWO", "HAWO", "NOFL", "PIWO", "RBWO", "YBSA"),]
count1dove <- count1[count1$SP_code %in% c("EUCD", "MODO", "ROPI"),]
count1other <- count1[count1$SP_code %in% c("CHSW", "RTHU"),]
count1unk <- count1[count1$SP_code %in% c("XXXX", "MUSC", "ZZZZ"),]

counttest2 <- rbind(count1dove, count1duck, count1game, count1goat, count1other, count1pass, count1peck, count1rapt, count1shor, count1tern, count1unk, count1wade)
counttest1 <- count1

count_missing <- anti_join(counttest1, counttest2, by="number")
#OK that took care of everything... now for labels
count1duck$species_type <- "Waterfowl"
count1wade$species_type <- "Wading_bird"
count1pass$species_type <- "Passerine"
count1rapt$species_type <- "Raptor"
count1shor$species_type <- "Shorebird"
count1tern$species_type <- "Larid"
count1game$species_type <- "Galliform"
count1goat$species_type <- "Goatsucker"
count1peck$species_type <- "Woodpecker"
count1dove$species_type <- "Dove"
count1other$species_type <- "Other"
count1unk$species_type <- "Unknown"

count2 <- rbind(count1dove, count1duck, count1game, count1goat, count1other, count1pass, count1peck, count1rapt, count1shor, count1tern, count1unk, count1wade)
#except now I have more in this dataframe than I started with

sum(duplicated(count2final$number))
#88 duplicate entries... I know they are duplicates because I am the one who numbered them initially!!

count2 <- count2[!(duplicated(count2$number)),]
#ok that did it, yay
#going to change some things to factors because it will make things easier; they aren't supposed to be numerals to be averaged
unique(count2$wind_speed)
#uhhh... there shouldn't be any decimals, like 2.5 is NOT an option in the key
#do I round up or down? or omit?
weirdwind <- count2[count2$wind_speed == 2.5 ,]
#they all come from one observer...
sum(count2$Observer == "BECO")
#not all of the BECO observations have the weird numbering... in wind speed at least
unique(count2$precip)#that one's good
unique(count2$cloud_cover) #HECK
unique(count2$hemlock_abundance)
unique(count2$hemlock_health)
unique(count2$spruce_abundance)#WHY?!
unique(count2$spruce_qual)#I don't even know how to "round" that
unique(count2$understory)#... really?
unique(count2$deer_impact_understory)
unique(count2$land_use)
unique(count2$livestock)
unique(count2$nest_cav)
unique(count2$nest_box)
unique(count2$road_type) #again with the 2.5
unique(count2$habitat)

#one thing I wonder is whether all the weird levelings come from BECO
sum(count2$cloud_cover == 2.5) #36
sum(count2$spruce_abundance == 0.5 | count2$spruce_abundance == 1.5) #25
sum(count2$spruce_qual == 0.5 | count2$spruce_qual == 1.5) #33
sum(count2$understory == 2.5) #21
sum(count2$road_type == 2.5) #13

countBECO <- count2[count2$Observer == "BECO" ,]
sum(countBECO$cloud_cover == 2.5) #36
#that checks out...
sum(countBECO$spruce_abundance == 0.5 | countBECO$spruce_abundance == 1.5) #yup
sum(countBECO$spruce_qual == 0.5 | countBECO$spruce_qual == 1.5) #uh huh
sum(countBECO$understory == 2.5) #yet again
sum(countBECO$road_type == 2.5) #aaand again
#DANG IT BECO!
#either there ARE trees or there AREN'T, there is no in between!!! gosh
#I might have to delete those simply because they suck and it's impossible to know whether it was supposed to be a 0 or 1, etc
count3 <- count2[!count2$cloud_cover == 2.5 ,]
unique(count3$cloud_cover) #ok that worked
count3 <- count3[!count3$wind_speed == 2.5 ,]
count3 <- count3[!count3$spruce_abundance == 0.5 ,]
count3 <- count3[!count3$spruce_abundance == 1.5 ,]
unique(count3$spruce_abundance)
count3 <- count3[!count3$spruce_qual == 0.5 ,]
count3 <- count3[!count3$spruce_qual == 1.5 ,]
unique(count3$spruce_qual)
count3 <- count3[!count3$understory == 2.5 ,]
unique(count3$understory)
count3 <- count3[!count3$road_type == 2.5 ,]
unique(count3$road_type)
#ok NOW we can convert to factor

count3$wind_speed <-  as.factor(count3$wind_speed)
count3$precip <- as.factor(count3$precip)
count3$cloud_cover <- as.factor(count3$cloud_cover)
count3$hemlock_abundance <- as.factor(count3$hemlock_abundance)
count3$hemlock_health <- as.factor(count3$hemlock_health)
count3$spruce_abundance <- as.factor(count3$spruce_abundance)
count3$spruce_qual <- as.factor(count3$spruce_qual)
count3$understory <- as.factor(count3$understory)
count3$deer_impact_understory <- as.factor(count3$deer_impact_understory)
count3$land_use <- as.factor(count3$land_use)
count3$livestock <- as.factor(count3$livestock)
count3$nest_cav <- as.factor(count3$nest_cav)
count3$nest_box <- as.factor(count3$nest_box)
count3$road_type <- as.factor(count3$road_type)
count3$habitat <- as.factor(count3$habitat)

str(count3)


MaxCount2 <- count3 %>%
  group_by(BBA_ID)%>%
  filter(TotalCounts == max(TotalCounts))%>%
  arrange(BBA_ID)

#saving the data now...
saveRDS(count3, file = "./bird_point_and_type.RDS")
saveRDS(MaxCount2, file = "./bird_point_and_type_MAX.RDS")

#Will do more ACTUAL data exploration in the next script












