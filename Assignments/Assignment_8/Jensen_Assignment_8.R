library(modelr)
library(broom)
library(dplyr)
library(fitdistrplus)
library(tidyr)
library(ggplot2)
mush <- read.csv("./mushroom_growth.csv")

ggplot(mush, aes(x=Light, y= Nitrogen))+
  geom_point()

mushmod1 <- lm(GrowthRate~Nitrogen, data=mush)
summary(mushmod1)
plot(mushmod1)
abline(mushmod1)

ggplot(mush, aes(x=Nitrogen, y=GrowthRate))+
  geom_point()

mushmod2 <- aov(GrowthRate~Light, data=mush)
plot(mushmod3)

mushmod3 <- aov(GrowthRate~Species, data = mush)
plot(mushmod4)

mushmod4 <- lm(GrowthRate~Temperature, data = mush)

mushmod5 <- lm(GrowthRate ~ Temperature*Humidity, data = mush)

mushmod6 <- aov(GrowthRate~Temperature + Light, data = mush)

mushmod7 <- lm(GrowthRate~Temperature*Humidity*Light, data = mush)

mushmod8 <- lm(GrowthRate~Temperature*Species*Light, data=mush)

mean(mushmod1$residuals^2)
mean(mushmod2$residuals^2)
mean(mushmod3$residuals^2)
mean(mushmod4$residuals^2)
mean(mushmod5$residuals^2)
mean(mushmod6$residuals^2)
mean(mushmod7$residuals^2)
mean(mushmod8$residuals^2)
#lowest mean square: mushmod7

pred = add_predictions(mush, mushmod7)

ggplot(pred, aes(x=Light, y=GrowthRate))+
  geom_point()+
  geom_smooth(method = "lm")+
  geom_point(aes(y=pred), color = "red")

{plot(mush$GrowthRate ~ mush$Light*mush$Temperature*mush$Humidity)
points(x=pred$Light,y=pred$pred, col="Red")
abline(mushmod7)}


