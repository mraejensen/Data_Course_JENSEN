mush <- read.csv("./mushroom_growth.csv")
mushmod1 <- lm(Light~Nitrogen, data=mush)
summary(mushmod1)
plot(mushmod1)
abline(mushmod1)

mushmod2 <- lm(Light~Nitrogen, data=mush)
abline(mushmod2)
