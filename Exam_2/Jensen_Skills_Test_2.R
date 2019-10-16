library(tidyverse)
library(ggplot2)
library(modelr)

#PART 1
salary <- read.csv("./salaries.csv")
#convert to tidy format
salary <- gather(salary, key="FacultyRank", value = "Salary", 5:7)

#create boxplot of salary by University tier, filed by faculty rank
#save to jpeg
jpeg("Jensen_exam2_plot1.jpeg")
ggplot(salary, aes(x=Tier, y=Salary, fill = FacultyRank))+
  geom_boxplot(color="white")+
  labs(title="Faculty Salaries - 1995", 
       x= "Tier", y= "Salary", fill="Rank")+
  theme(panel.background = element_rect(fill="black"), 
        panel.grid = element_line(color="#6e6e6e"), 
        plot.background = element_rect(fill= "dark grey"),
        legend.key=element_rect(fill="dark grey", color="dark grey"),
        legend.background = element_rect(fill="dark grey"))
dev.off()
  
#PART 2
atm <- read.csv("./atmosphere.csv")
#linear models with Diversity as dependent variable
mod1 <- aov(Diversity ~ Month, data= atm)
mod2 <- aov(Diversity ~ CO2_Concentration*Aerosol_Density, data=atm)
mod3 <- aov(Diversity~ (CO2_Concentration+Aerosol_Density)*Precip, data=atm)

#compare explanatory power with mean squares
mean(mod1$residuals^2)
mean(mod2$residuals^2)
mean(mod3$residuals^2)
#lowest mean square residual: mod3

allpred <- gather_predictions(atm,mod1, mod2, mod3)

#add predictions on all models
pred1 <- add_predictions(atm, mod1)
pred2 <- add_predictions(atm, mod2)
pred3 <- add_predictions(atm, mod3)

jpeg("./Jensen_exam2_plot2.jpeg")
ggplot(allpred, aes(x=Precip, y=Diversity))+
  geom_point(aes(y=pred, color=model), alpha=.3)+
  geom_point()+
  geom_smooth(method="lm", aes(y=pred, color=model), se=FALSE)+
  labs(title="Model prediction of diversity", x="Precip (mm)")
dev.off()

#show predicted values using the hypothetical data
hyp <- read.csv("./hyp_data.csv")
names(hyp)[names(hyp)=="Hypothetical_Sample"] <- "SampleID"
hyp$Month <- as.character(hyp$Month)
hyp$Month[hyp$Month== "3"] <- "March"
hyp$Month[hyp$Month == "9"] <- "September"

hypPred1 <- add_predictions(hyp, mod1)
hypPred2 <- add_predictions(hyp, mod2)
hypPred3 <- add_predictions(hyp, mod3)

hypPred1$pred
hypPred2$pred
hypPred3$pred


sink(file="./model_summaries.txt")
print("MOD 1")
mod1
print("MOD 2")
mod2
print("MOD 3")
mod3
sink()

#BONUS 1
#add hypothetical to points of actual data
hypPred1$model <- "mod1"
hypPred2$model <- "mod2"
hypPred3$model <- "mod3"

allHyp <- rbind(hypPred1, hypPred2, hypPred3)

jpeg("./Jensen_exam2_bonus_plot.jpeg")
ggplot(atm, aes(x=Precip, y=Diversity))+
  geom_point()+
  geom_point(data=allHyp, aes(y=pred, color=model))+
  geom_smooth(data=allHyp, aes(y= pred, color=model),method="lm", se=FALSE)+
  labs(title="Modeling on Hypothetical Data", x="Precip (mm)", caption="compared to actual data (black points)")+
  theme_minimal()
dev.off()

#BONUS 2
#split atmosphere data into training and testing sets
library(caret)
trainingrows <- createDataPartition(atm$Diversity, p=.5, list=FALSE)
train <- atm[trainingrows ,]
test <- atm[-trainingrows ,]

mod4 <- aov(Diversity~ (CO2_Concentration+Aerosol_Density)*Precip, data=train)
#same as mod3 (best model for the whole dataset), but fitted to the training data

test <- add_predictions(data=test, model=mod4)

#showing the fit of the model on test:
jpeg("./Jensen_exam2_bonus_crossValplot.jpeg")
ggplot(test, aes(x=Precip, y=Diversity))+
  geom_point()+
  geom_point(aes(y=pred), color="red")+
  ggtitle("Cross Validation of data")+
  labs(caption="red points show prediction values")+
  theme_minimal()
dev.off()








