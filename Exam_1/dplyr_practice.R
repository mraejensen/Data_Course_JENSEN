library(dplyr)

#mutate command

DNA_ext
dat <- mutate(DNA_ext,DIFF=DNA_Concentration_Ben - DNA_Concentration_Katy)
#OR
DNA_ext %>%
  mutate(DIFF=DNA_Concentration_Ben - DNA_Concentration_Katy)

dat <- dat%>%
  mutate(QWERTY = paste(Year_Collected,Extract.Code))

dat <- dat %>%
  mutate(YEAR_NUMERIC = as.numeric(as.character(Year_Collected)))

#Mutate adds new variables based on existing variables

#select command
# narrows down variable values by column name; simplified syntax from dat[, column]
#filter is like subsetting, a little
select(dat,"Lab") %>%
  filter(Lab == "Upstairs")

dat2 <- select(dat,-1)

dat3 <- select(dat, starts_with("D"))

dat4 <- select(dat, ends_with("a"))

dat5 <- dat %>%
  filter(Lab == "Upstairs") %>%
  select(starts_with("D"))

#summarise command
#can also write it as summarize for us americans
#gives single value for a column; mean, median, mode, min, max, etc. BUT ONLY ONE AT A TIME
?summarize

summarize(dat, MeanDNA = mean(DNA_Concentration_Ben),
          MeanKaty = mean(DNA_Concentration_Katy))
#functionally equivalent to mean(dat$DNA_Concentration_Ben)
#summarize gives dataframe, mean() just gives a value
#be sure to add a label
df=dat%>%
  group_by(Year_Collected)%>%
  summarize(MeanBen = mean(DNA_Concentration_Ben))
#this is the easy way to do problem 6 on test

dat %>%
  group_by(Lab) %>%
  summarize(MeanBen = mean(DNA_Concentration_Ben),
            MeanKaty = mean(DNA_Concentration_Katy),
            SDDEVBen = sd(DNA_Concentration_Ben),
            SDDEVKaty = sd(DNA_Concentration_Katy),
            N= n())

#arrange command
#changes the order of entries by values in one column
arrange(dat, Extract.Code)
#for descending order, do arrange(dataframe, desc(columnname))
arrange(dat, desc(DNA_Concentration_Ben))
#can sort by multiple columns
#Order matters; sort by first, then for tiebreakers, then for those tiebreakers, etc
arrange(dat, Year_Collected, Date_Collected)











