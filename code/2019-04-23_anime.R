library(tidyverse)

## load data
anime = read_csv("https://github.com/rfordatascience/tidytuesday/blob/master/data/2019/2019-04-23/tidy_anime.csv") 

## supply lost end dates for titles with >3000 mins (data from MAL)
anime$end_date[anime$name == "Naruto"] = as.Date("2007-02-08")
anime$end_date[anime$name == "Manga Nippon Mukashibanashi (1976)"] = as.Date("1994-09-03")
anime$end_date[anime$name == "Kiteretsu Daihyakka"] = as.Date("1996-06-09")
anime$end_date[anime$name == "Keroro Gunsou"] = as.Date("2011-04-04")
anime$end_date[anime$name == "Perman (1983)"] = as.Date("1985-07-02")
anime$end_date[anime$name == "Pokemon Diamond & Pearl"] = as.Date("2010-09-09")
anime$end_date[anime$name == "Lupin III: Part II"] = as.Date("1980-10-06")
anime$end_date[anime$name == "Kinnikuman"] = as.Date("1986-10-01")

## filtering & cleaning
franschise_clean_all = anime %>% 
  distinct(animeID, .keep_all=T) %>%
  filter(scored_by > 100) %>%

#### cleaning names  
  mutate(franchise = str_replace(name, "(.*?):.*", "\\1"),
         franchise = str_replace(franchise, ".*?Gundam.*", "Gundam"),
         franchise = str_replace(franchise, ".*?Pokemon.*", "Pokemon"),
         franchise = str_replace(franchise, ".*?Sailor Moon.*", "Sailor Moon"),
         franchise = str_replace(franchise, ".*?Dragon Ball.*", "Dragon Ball"),
         franchise = str_replace(franchise, "Fate/.*", "Fate/"),
         franchise = str_replace(franchise, ".*?Gintama.*", "Gintama"),
         franchise = str_replace(franchise, ".*Da Capo.*", "Da Capo"),
         franchise = str_replace(franchise, ".*Digimon.*", "Digimon"),
         franchise = str_replace(franchise, "Kochira Katsushikaku.*", "Kochikame"),
         franchise = str_replace(franchise, "Manga Nippon.*", "Mukashibanashi"),
         franchise = str_replace(franchise, "Neon Genesis ", ""),
         franchise = str_replace(franchise, "Duel Masters.*", "Duel Masters"),
         franchise = tolower(franchise),
         franchise = str_replace(franchise, " specials?$", ""),
         franchise = str_replace(franchise, " \\(.*?\\)$", ""),
         franchise = str_replace(franchise, " .*? season", ""),
         franchise = str_replace(franchise, " \\dnd ?.*?", ""),
         franchise = str_replace(franchise, " OVA", ""),
         franchise = str_replace(franchise, "(the)? movie", ""),
         franchise = str_replace(franchise, "(the)? animation", ""),
         franchise = str_replace(franchise, "picture drama", ""),
         franchise = str_replace(franchise, " \\d{1,2}$", ""),
         franchise = str_replace(franchise, " \\'*$", ""),
         franchise = str_replace(franchise, "\\+.*", ""),
         franchise = str_replace(franchise, "(.*?)[\\?\\!]{1,2}.*", "\\1"),
         franchise = toupper(franchise)) %>%
#### cleaning duration
  mutate(hrs = replace_na(as.numeric(str_replace(duration, "(^\\d) hr.*", "\\1"))*60, 0),
         mins = replace_na(as.numeric(str_replace(duration, "(.*?)?(\\d*?) min.*", "\\2")), 0),
         sec = replace_na(as.numeric(str_replace(duration, "(^\\d*?) sec.*", "\\1"))/60, 0),
         time_length = (hrs + mins + sec)*episodes) %>%
#### select relevant columns & count franchise length
  select(name, franchise, type, start_date, end_date, time_length) %>%
  filter(time_length > 0) %>%
  group_by(franchise) %>%
  mutate(time_franchise = sum(time_length)) %>%
  ungroup()

## tv series (franchise duration >6 days)
tv_shows = franschise_clean_all %>%
  filter(!is.na(end_date), time_franchise > 8640) %>%
  gather(key="position", value="time", start_date:end_date)
  
## movies
movies = franschise_clean_all %>%
  filter(is.na(end_date), time_franchise > 8640) %>%
  mutate(type="movies/specials") %>%
  rename(time = start_date)

## labels for plot
names = tv_shows %>%
  distinct(franchise, .keep_all = T) %>%
  mutate(time_franchise = paste("~", round(time_franchise/1440), " days", sep=""),
         time = as.Date("1963-01-01"))

## palette
pltt = c(line="#F0E3A3", background="#3B5578", axistext="#E0DEBC", points="#ADAC91")

## plot
tv_shows %>%
  ggplot(aes(x=time, y=reorder(franchise, -time_franchise), 
             group=interaction(franchise, name))) +
  geom_line(size=7, color = pltt[1]) +
#### points for movies
  geom_point(data=movies, aes(x=time, y=franchise,shape=type), 
             size = 5, color=pltt[4]) +
  geom_text(data=names, aes(x=time, y=franchise, group=franchise, label = time_franchise), 
            hjust = 0.4, size=4, vjust=-0.3, color=pltt[4]) +
#### scales
  scale_shape_manual(values="\u066D") +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y") +
#### labs
  labs(title = "Time Travelers",
       subtitle = "Lifespans of anime series/franchise that will take >6 full days of your life to watch.\nYou will need only ~18 years to watch the entire MAL database (only complete titles)\nif you make sure you do it for 8 hours each day.",
       x = "",
       y = "",
       caption = "#TidyTuesday 2019-04-23 @artjomshl") +
  theme(
#### plot meta
    plot.title = element_text(color=pltt[1], size = 30, face = "bold"),
    plot.subtitle = element_text(color=pltt[3], size = 16),
    plot.caption = element_text(color=pltt[4], size = 12),
#### legend
    legend.position = c(0.7,1),
    legend.title = element_blank(),
    legend.background = element_rect(fill=pltt[2]),
    legend.margin=margin(c(0.5,0.5,0.5,0.5)),
    legend.key.size= unit(0.1,"line"),
    legend.key = element_rect(fill=pltt[2], color=pltt[2]),
    legend.text = element_text(color=pltt[3], size = 12),
#### axis
    axis.text = element_text(color=pltt[3],size = 14),
    axis.ticks = element_blank(),
#### background & grids
    panel.background = element_rect(fill=pltt[2]),
    plot.background = element_rect(fill=pltt[2]),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color=pltt[1], size = 0.3),
    panel.grid.minor = element_blank()
    
  ) +
  guides(shape = guide_legend(override.aes = list(size = 10)))



ggsave("timetravelers.png", width=12, height=7.5)
