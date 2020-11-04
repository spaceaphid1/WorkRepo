library(tidyverse)
library(readxl)


errorDat <- read_excel("~/Repos/WorkRepo/Seed_Error_Analysis/ErrorDat.xlsx")

str(errorDat)

errorDat$SeedID <- as.factor(errorDat$SeedID)
errorDat$MeasurementNum <- as.factor(errorDat$MeasurementNum)
str(errorDat)


#PreliminaryVis

#By protocol
ggplot(errorDat, aes(Protocol, Weight)) +
  geom_boxplot()

#By MeasurementNum
ggplot(errorDat, aes(MeasurementNum, Weight)) +
  geom_boxplot()

#By Seed
ggplot(errorDat, aes(SeedID, Weight)) +
  geom_boxplot()

ggplot(errorDat, aes(SeedID, Weight)) +
  geom_bar(stat = "identity", aes(fill = Protocol), position = "dodge")


