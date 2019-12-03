library(modelr)
library(ggplot2)

admit <- read.csv("GradSchool_Admissions.csv")
str(admit)
mod1 <- aov(data = admit, formula = admit ~ gpa)

admit <- add_predictions(admit, mod1)

ggplot(admit, aes(y=admit))+
  geom_point(aes(x=gpa))+
  geom_point(aes(x=pred), color = "red")

mod2 <- aov(data = admit, formula= admit ~rank * gpa)

admit <- add_predictions(admit, mod1, var = "pred2")

ggplot(admit, aes(y=admit))+
  geom_point(aes(x=gpa))+
  geom_point(aes(x=pred2), color = "red")

mod3 <- aov(data = admit, formula = admit ~ rank*gpa*gre)
admit <- add_predictions(admit, mod3, var = "Pred1")

ggplot(admit, aes(y=admit))+
  geom_point(aes(x=gpa))+
  geom_point(aes(x=Pred1), color = "red")





