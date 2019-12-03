#GGplot awful graph stuff
library(ggplot2)
data("iris")

ggplot(iris, aes(x=Sepal.Width, y= Sepal.Length, color = Species))+
  geom_point(size = 12)+
  ggtitle("irises")+
  theme(plot.title = element_text(color = "#daa570", size = 125))+
  labs(col = "setosaversicolorvirginica")+
  scale_color_manual(labels = c("species", "species", "species"), values = c("#9ce3f0", "#ff73ff", "#00f7ff"))+
  theme(panel.background = element_rect(fill = "#80ff9b"))+
  theme(plot.background = element_rect(fill= "#daa520"))
  
    

?scale_fill_discrete

 


