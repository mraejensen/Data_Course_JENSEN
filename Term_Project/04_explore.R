library(ggplot2)
library(tidyverse)
library(modelr)
library(lme4)

df <- readRDS("./bird_point_and_type.RDS")
maxdf <- readRDS("./bird_point_and_type_MAX.RDS")

str(df)

#how many entries of each kind of bird were there? (variety per BBA_ID)
ggplot(df, aes(x=species_type))+
  geom_bar()

#how many of the max counts belonged to each type of bird?
ggplot(maxdf, aes(x=species_type))+
  geom_bar()
#ok both fit what I thought... passerine birds take the cake

#what about non-passerine birds? Since songbirds make up a majority of bird species
#not max counts:
noSong <- df[!(df$species_type == "Passerine"),]
unique(noSong$species_type)
noSong$species_type <- as.character(noSong$species_type)


ggplot(noSong, aes(x=species_type))+
  geom_bar()

#maximum counts:
noSongMax <- maxdf[!(maxdf$species_type == "Passerine"),]

ggplot(noSongMax, aes(x=species_type))+
  geom_bar()

#doves have the highest non-passerine count overall, but also overtakes all other non-passerine species in the max counts.
#woodpeckers come second in the general count, but no species types are even close to doves for max counts per area



which(maxdf$TotalCounts == max(maxdf$TotalCounts))
maxdf[51472 ,]
#maximum count belonged to a MALLARD! that's interesting...

ggplot(df, aes(x=GPS_W, y=GPS_N, color = species_type))+
  geom_point()+
  facet_wrap(~habitat)
#that doesn't particularly help...

ggplot(noSong, aes(x=GPS_W, y=GPS_N, color = species_type))+
  geom_point()+
  facet_wrap(~habitat)
#still doesn't work very well... too many points

ggplot(df, aes(x=species_type, y=TotalCounts))+
  geom_boxplot()
#there is an instance in which Waterfowl has a huge count...
#that's the mallard, this was found out earlier in the script

ggplot(df, aes(x=habitat, y=TotalCounts))+
  geom_boxplot()+
  facet_wrap(~species_type)
#might have done this wrong, trying something else

ggplot(df, aes(x=species_type, y=TotalCounts))+
  geom_boxplot()+
  facet_wrap(~habitat, scales = "free")
#this is mostly just for scale relative to the habitat, so free scales should be ok
str(df)

#going to try modeling based on all the original variables
#mod1 <- aov(data = df, TotalCounts ~ BBA_ID + SP_code + GPS_N + GPS_W + Observer + Date + Start + month+Temp+wind_speed+precip+cloud_cover+hemlock_abundance+hemlock_health+spruce_abundance+spruce_qual+understory+deer_impact_understory+land_use+livestock+nest_cav+road_type+habitat)
#heheehe that was too much
#let's try one with just the determining factors
mod1 <- aov(data = df, TotalCounts~ month+Temp+wind_speed+precip+cloud_cover+hemlock_abundance+hemlock_health+spruce_abundance+spruce_qual+understory+deer_impact_understory+land_use+livestock+nest_cav+nest_box+road_type+habitat)

dfPred <- add_predictions(df, model = mod1)

ggplot(dfPred, aes(x=SP_code))+
  geom_point(aes(y=TotalCounts))+
  geom_point(aes(y=pred), color = "red")
#yeah, that doesn't really do much

ggplot(df, aes(x=Temp))+
  geom_histogram()
#not sure if this is an accurate measure of total counts, but the entries had a peak around 17 - 18 degrees C
#going to try to get total counts into this...

ggplot(df, aes(x=Temp, y=TotalCounts))+
  geom_histogram(stat = "identity")
#oh wow that is an almost normal distribution!
#i'm not sure if that was what I was going for, but that is impressive!

#going to try a model putting counts by temperature
mod2 <- lm(data = df, TotalCounts ~ Temp)

dfPred <- add_predictions(dfPred, model = mod2, var = "pred2")

ggplot(dfPred, aes(x=Temp))+
  geom_point(aes(y=TotalCounts))+
  geom_point(aes(y=pred2), color = "red")
#that doesn't work either... hmmm
#maybe because it is a linear model or something...
mod3 <- aov(data = df, TotalCounts ~Temp)
dfPred <- add_predictions(dfPred, model = mod3, var = "pred3")

ggplot(dfPred, aes(x=Temp))+
  geom_point(aes(y=TotalCounts))+
  geom_point(aes(y=pred3), color = "red")
#ok that gives the same result... maybe temperature and species type? or just species type?

mod4 <- aov(data = df, TotalCounts~Temp * species_type)
summary(mod4)
#that shows significant results, I will check the others
summary(mod1)
#ok that says all but the deer understory are significant, so that probably doesn't mean much...
dfPred <- add_predictions(dfPred, model = mod4, var = "pred4")
ggplot(dfPred, aes(x=Temp))+
  geom_point(aes(y=TotalCounts))+
  geom_point(aes(y=pred4), color = "red")
#that's a little better... not much though...
ggplot(dfPred, aes(x=species_type))+
  geom_point(aes(y=TotalCounts))+
  geom_point(aes(y=pred4), color = "red")
#that's even worse, Temperature showed better result.

#try to plot mod1 on the temperature graph?
ggplot(dfPred, aes(x=Temp))+
  geom_point(aes(y=TotalCounts))+
  geom_point(aes(y=pred1), color = "red")
#nope...

#trying a gaussian distribution, maybe?
mod5 <- glm(family = gaussian, data = df, formula = TotalCounts~Temp)

dfPred <- add_predictions(dfPred, mod5, var = "pred5")

ggplot(dfPred, aes(x=Temp))+
  geom_point(aes(y=TotalCounts))+
  geom_point(aes(y=pred5), color = "red")
#yeah that didn't work

mod6 <- aov(data = df, TotalCounts ~ Temp+species_type+habitat)
summary(mod6)
dfPred <- add_predictions(dfPred, mod6, var = "pred6")

ggplot(dfPred, aes(x=Temp))+
  geom_point(aes(y=TotalCounts))+
  geom_point(aes(y=pred6), color = "red")
#better...ish

mod7 <- aov(data=df, TotalCounts~Temp+month+species_type+habitat)
summary(mod7)
dfPred <- add_predictions(dfPred, mod7, var = "pred7")

ggplot(dfPred, aes(x=Temp))+
  geom_point(aes(y=TotalCounts))+
  geom_point(aes(y=pred7), color = "red")
#i'm starting to think there is just a lot of random chance here...

mod8 <- aov(data = df, TotalCounts~Temp+month+Start+species_type+habitat+nest_box+nest_cav)
summary(mod8)            
dfPred <- add_predictions(dfPred, model = mod8, var = "pred8")            

ggplot(dfPred, aes(x=Temp))+
  geom_point(aes(y=TotalCounts))+
  geom_point(aes(y=pred8), color = "red")

#do the gps coordinates have anything to do with the counts?

mod9 <- aov(data = df, TotalCounts~Temp+month+Start+species_type+habitat+GPS_N+GPS_W)
summary(mod9)            
#GPS_N has a little bit of significance,the GPS_W does not.
dfPred <- add_predictions(dfPred, model = mod9, var = "pred9")

ggplot(dfPred, aes(x=Temp))+
  geom_point(aes(y=TotalCounts))+
  geom_point(aes(y=pred9), color = "red")
#not much different from when we just had temperature and species type, honestly

#maybe a model that doesn't have passerine birds would be more accurate?
modNS1 <- aov(data = noSong, TotalCounts~Temp+month+Start+species_type+habitat)
summary(modNS1)

dfPred <- add_predictions(dfPred, model = modNS1, var = "predNS1")
#i guess not...

mod10 <- lmer(data = df, formula = TotalCounts~(1|species_type/SP_code)+Observer+GPS_W+GPS_N+Date+Start+month+precip+cloud_cover+Temp+wind_speed+hemlock_abundance+hemlock_health+spruce_abundance+spruce_qual+understory+deer_impact_understory+land_use+livestock+nest_cav+nest_box+road_type+habitat)
dfPred <- add_predictions(dfPred, model = mod10, var = "pred10")

ggplot(dfPred, aes(x=Temp))+
  geom_point(aes(y=TotalCounts))+
  geom_point(aes(y=pred10), color = "red")

#going to take a (short) break from trying to determine the counts based on variables since it doesn't seem to work

#how did the temperature vary from month to month and with environmental conditions like cloud cover and precipitation?
modT1 <- aov(data = df, Temp ~ month * wind_speed+cloud_cover+ precip)
dfPredT <- add_predictions(data = df, model = modT1)

ggplot(dfPredT, aes(x=month))+
  geom_point(aes(y=Temp))+
  geom_point(aes(y=pred), color = "red")
#that doesn't do a great job either... too many factor thingies?

#going to try distribution of birds by month
ggplot(df, aes(x=GPS_W, y=GPS_N))+
  geom_point()+
  facet_wrap(~month)
#ok that's interesting... it looks like there is more south in the late spring/early summer,
#more widespread in June, and progresses north overall (do I already have this graph?)

ggplot(df, aes(x=GPS_W, y=GPS_N, color = month))+
  geom_point()+
  facet_wrap(~species_type)
#might be easier to subset by month... or by type  


  
  
  
  
  

