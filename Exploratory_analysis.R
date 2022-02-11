# loading libraries
library(tidyverse)
library(lubridate)

#loading datasets
hist_esp = read.csv("HistoricalEsportData.csv")
gen_esp = read.csv("GeneralEsportData.csv")

summary(hist_esp)
sapply(hist_esp, class)

summary(gen_esp)
sapply(gen_esp, class)

# Change format for data in hist_esp
hist_esp$Date = ymd(hist_esp$Date)

# Add Genre from gen_esp to hist_esp

Genre = gen_esp %>% select(Game, Genre)
hist_esp = merge(x = hist_esp, y = Genre, by = "Game", all.x = TRUE)
hist_esp = hist_esp[,c(1,6,2,3,4,5)]

sum(hist_esp$Earnings)

# plotting some shit 
hist_esp %>% ggplot(aes(x = Date, y = Earnings)) +
  geom_bar()
