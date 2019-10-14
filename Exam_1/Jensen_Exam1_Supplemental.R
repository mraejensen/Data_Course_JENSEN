#PART 4

difference = DNA_ext$DNA_Concentration_Ben - DNA_ext$DNA_Concentration_Katy
mindiff = which(difference == min(difference))
which(difference == min(difference))
DNA_ext[mindiff, "Year_Collected"]


#PART 5 

class(DNA_ext$Date_Collected)
Downstairs_Lab$Date_Collected <- as.POSIXct(Downstairs_Lab$Date_Collected)

plot(x=Downstairs_Lab$Date_Collected, y=Downstairs_Lab$DNA_Concentration_Ben, xlab="Date Collected", ylab = "DNA Concentration Ben")

#PART 6
#method 1 (please finish)
ben2000 <- mean(DNA_ext[DNA_ext$DNA_Concentration_Ben, DNA_ext$Year_Collected == "2000"])

#for-loop way (please finish)

x = 1
v=c()
for(i in levels(DNA_ext$Year_Collected)){
  v[x] <- mean(DNA_ext[DNA_ext$Year_Collected == i, "DNA_Concentration_Ben"])
  x=1
}

#third method
library(dplyr)
df=DNA_ext %>%
  group_by(Year_Collected) %>%
  summarize(Mean = mean(DNA_Concentration_Ben))
df

# %>% is a pipe (like | in terminal); think of it as "and then"