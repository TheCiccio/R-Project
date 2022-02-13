# loading libraries
library(tidyverse)
library(lubridate)
library(corrplot)
library(ggcorrplot)
library(rcartocolor)
library(zoo)

#loading datasets
hist_esp = read.csv("HistoricalEsportData.csv")
gen_esp = read.csv("GeneralEsportData.csv")
twitch_global = read.csv("Twitch_global_data.csv")
twitch_game = read.csv("Twitch_game_data.csv")
esports = read.csv("esports.csv")

summary(hist_esp)
sapply(hist_esp, class)

summary(gen_esp)
sapply(gen_esp, class)

sapply(twitch_game, class)

twitch_game$Hours_Streamed = as.numeric(str_extract(twitch_game$Hours_Streamed, "[0-9]+"))

# Change format for data in hist_esp
hist_esp$Date = ymd(hist_esp$Date)
class(hist_esp$Date)

hist_esp$Year <- as.numeric(format(hist_esp$Date,'%Y'))
hist_esp$Month <- as.numeric(format(hist_esp$Date,'%m'))
hist_esp = select(hist_esp, -Date)

# Add Genre from gen_esp to hist_esp
hist_esp = merge(x = hist_esp, y = gen_esp[,c(1,3)], by = "Game", all.x = TRUE)
hist_esp = hist_esp[,c(1,7,5,6,2,3,4)]

twitch_game = merge(x = twitch_game, y = gen_esp[,c(1,3)], by = "Game", all.x = TRUE)
twitch_game = twitch_game %>% drop_na()

new_data = merge(x = hist_esp, y = twitch_game, by = c("Game","Genre", "Year", "Month"), all.x = TRUE)
new_data = new_data %>% drop_na()


# plotting Earnings by tournaments by year per genre
hist_esp %>% ggplot(aes(Year, Earnings/1000000, fill = Genre)) +
  geom_col() +
  scale_fill_manual(values = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                               "#44AA99", "#999933", "#661100", "#6699CC", "#882255"))


# plotting Earnings by tournaments by year per genre from 2016

new_data %>% ggplot(aes(Year, Earnings/1000000, fill = Genre)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                               "#44AA99", "#999933", "#661100", "#6699CC", "#882255"))


# plotting average viewers on twitch by year per genre
twitch_game %>% ggplot(aes(Year, Avg_viewers/1000, fill = Genre)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                               "#44AA99", "#999933", "#661100", "#6699CC", "#882255"))


new_data %>% group_by(Year, Genre) %>%
  count(Earnings) %>%
  ggplot(aes(x= reorder(Year, Earnings), y = Earnings/1000000, fill = Genre)) +
  geom_col(position = "dodge")


# geometry line with increase of earning per genre by month

grouped_df <- hist_esp %>% group_by(Genre, Year, Month) %>% 
  summarise(monthly_earnings = sum(Earnings)) %>%
  arrange(Genre, Year, Month)

grouped_df$Year_Month <- as.yearmon(paste(grouped_df$Year, grouped_df$Month), "%Y %m")

cleaned_df <- grouped_df %>% 
  ungroup() %>%
  select(Genre, Earnings = monthly_earnings, Year_Month)

cleaned_df %>%
  ggplot(aes(Year_Month, log10(Earnings), color = Genre)) +
  geom_line() +
  facet_wrap(~Genre) + 
  geom_smooth()


# same with twitch
grouped_df <- twitch_game %>% group_by(Genre, Year, Month) %>% 
  summarise(monthly_avg_viewers = sum(Avg_viewers)) %>%
  arrange(Genre, Year, Month)

grouped_df$Year_Month <- as.yearmon(paste(grouped_df$Year, grouped_df$Month), "%Y %m")

cleaned_df <- grouped_df %>% 
  ungroup() %>%
  select(Genre, Avg_viewers = monthly_avg_viewers, Year_Month)

cleaned_df %>%
  ggplot(aes(Year_Month, log10(Avg_viewers), color = Genre)) +
  geom_line() +
  facet_wrap(~Genre) + 
  geom_smooth()

# corr_matrix for games
df <- dplyr::select_if(new_data, is.numeric)
r <- cor(df, use="complete.obs")
ggcorrplot(r)

new_data %>% 
  subset(Genre == 'Multiplayer Online Battle Arena') %>%
  group_by(Game) %>%
  summarise(total_earnings = sum(Earnings)) %>%
  arrange(desc(total_earnings)) %>%
  ggplot(aes(Game, total_earnings/100000, fill = Game)) +
  geom_col()


# esperimentoìi

grouped_df2 <- new_data %>% 
  filter(Game == "Dota 2") %>% 
  group_by(Game, Year, Month) %>% 
  summarise(monthly_earnings = sum(Earnings))
  
grouped_df2$Year_Month <- as.yearmon(paste(grouped_df2$Year, grouped_df2$Month), "%Y %m")
  
cleaned_df <- grouped_df2 %>% 
  ungroup() %>%
  select(Game, Earnings = monthly_earnings, Year_Month)

cleaned_df %>%
  ggplot(aes(Year_Month, log10(Earnings), color = 'red')) +
  geom_line() +
  geom_smooth()

# plotting for genre

grouped_df <- new_data %>% 
  filter(Genre == "Strategy") %>% 
  group_by(Genre, Year, Month) %>% 
  summarise(monthly_earnings = sum(Earnings))

grouped_df$Year_Month <- as.yearmon(paste(grouped_df$Year, grouped_df$Month), "%Y %m")

cleaned_df <- grouped_df %>% 
  ungroup() %>%
  select(Genre, Earnings = monthly_earnings, Year_Month)

cleaned_df %>%
  ggplot(aes(Year_Month, log10(Earnings), color = 'red')) +
  geom_line() +
  geom_smooth()




