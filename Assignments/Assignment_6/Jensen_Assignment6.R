library(tidyverse)

data("mtcars")
str(mtcars)

auto <- mtcars[mtcars$am== 0,]
write.csv(auto, "automatic_mtcars.csv")

png("mpg_vs_hp_auto.png")
ggplot(auto, aes(x=hp, y=mpg))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(title = "Effect of horsepower on miles per gallon",
       x="Horsepower", y= "Miles per Gallon")
dev.off()

tiff(filename="mpg_vs_wt_auto.tiff")
ggplot(auto, aes(x=wt, y=mpg))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "Effect of weight on miles per gallon",
       x="Weight (x1000lb)", y="Miles per Gallon")
dev.off()

low_disp <- mtcars[mtcars$disp <= 200,]
write.csv(low_disp, "mtcars_max200_displ.csv")

maxHPgen <- max(mtcars$hp)
maxHPauto <- max(auto$hp)
maxHPdisp <- max(low_disp$hp)

HPmax <- c(maxHPgen, maxHPauto, maxHPdisp)
HPlabs <- c("mtcarsMAX", "autoMAX", "dispMAX")
MaxHP <- cbind(HPlabs, HPmax)
write.table(MaxHP, "hp_maximums.txt", row.names = FALSE)
