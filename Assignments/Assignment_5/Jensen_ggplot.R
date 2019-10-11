library(tidyverse)
data("iris")

png(filename= "iris_fig1.png")
ggplot(iris, aes(x= Sepal.Length, y= Petal.Length, color = Species))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title = "Sepal length vs petal length", subtitle = "for three iris species")+
  theme_minimal()
dev.off()

png(filename= "iris_fig2.png")
ggplot(iris, aes(x= Petal.Width, fill = Species))+
  geom_density(alpha=.5)+
  labs(title = "Distribution of Petal Widths", subtitle= "for three iris species", xlabs = "Petal Width")+
  theme_minimal()
dev.off()

iris$Width_Ratio <- (iris$Petal.Width / iris$Sepal.Width)

png(filename = "iris_fig3.png")
ggplot(iris, aes(x= Species, y= Width_Ratio, fill = Species))+
  geom_boxplot()+
  labs(title = "Sepal- to Petal-Width Ratio", 
       subtitle = "for three iris species", y = "Ratio of Sepal Width to Petal Width")+
  theme_minimal()
dev.off()

iris$Sepal.Deviance <- iris$Sepal.Length - mean(iris$Sepal.Length)
iris <- iris[order(iris$Sepal.Deviance),]
count(iris)
iris$entry <- c(1:150)

png("iris_fig4.png")
ggplot(iris, aes(x=entry, y=Sepal.Deviance, fill = Species))+
  geom_bar(stat="identity", width=.85)+
  labs(title = "Sepal length deviance from the mean of all observations", 
       x = "", y = "Deviance from the Mean", caption = "Note: Deviance = Sepal.Length - mean(Sepal.Length)")+
  scale_x_continuous(name="", breaks = round(seq(min(iris$entry), max(iris$entry), by=5), 1), expand = c(0,0))+
  coord_flip()+
  theme_minimal()+
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())
  
dev.off()









