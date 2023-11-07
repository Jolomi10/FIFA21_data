## loading Packages
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library("plotly")
library("maps")

##Data Loading, Cleaning and Data Preparation
players <- read_excel("Downloads/football/Career Mode player datasets - FIFA 15-22.xlsx")
View(players)

players <- players %>%
  select(-sofifa_id,release_clause_eur,defending_marking_awareness, defending_standing_tackle, defending_sliding_tackle)

players <- players %>%
  select(-club_loaned_from)

## Players by their nationalities 
France <- subset(players,players$nationality_name=="France")
India <- subset(players,players$nationality_name=="India")
Argentina <- subset(players,players$nationality_name=="Argentina")
Portugal <- subset(players,players$nationality_name=="Portugal")
Brazil <- subset(players,players$nationality_=="Brazil")
Spain <- subset(players,players$nationality_name=="Spain")
Germany <- subset(players,players$nationality_name=="Germany")

## Some club-wise selections 
Barcelona <- subset(players,players$club_name=="FC Barcelona")
BMunich <- subset(players,players$club_name=="FC Bayern M?nchen")
Juventus <- subset(players,players$club_name=="Juventus")
Chelsea <- subset(players,players$club_name=="Chelsea")
ManUnited <- subset(players,players$club_name=="Manchester United")
Arsenal <- subset(players,players$club_name=="Arsenal")
RMadrid <- subset(players,players$club_name=="Real Madrid CF")

## SOME LEAGUE-WISE SELECTIONS
La_Liga <- subset(players,players$league_name=="Spain Primera Division")
Serie_A <- subset(players,players$league_name=="Italian Serie A")
Bundesliga <- subset(players,players$league_name=="German 1. Bundesliga")
Ligue_1 <- subset(players,players$league_name=="French Ligue 1")
EPL <- subset(players,players$league_name=="English Premier League")

## Distribution and the Average Age of The Players in each League
summ <- players %>% 
  group_by(league_name) %>% 
  summarise(Age = mean(age))



options(repr.plot.width = 12, repr.plot.height = 8)

ggplot()+
  geom_histogram(players, mapping = aes(age, fill = league_name))+
  geom_vline(summ, mapping = aes(xintercept = Age), color = "red", size = 1.5)+
  geom_text(summ, mapping = aes(x = Age+3, y = 65, label = round(Age,digits = 2)))+
  facet_wrap(league_name~.)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(y = "Frequency", title = "Distribution & The Average Age of The Players in each League", 
       caption = "@EA Sports - FIFA 21")

## World map and Players available in Fifa 21 Game
options(repr.plot.width = 12, repr.plot.height = 8)

world_map <- map_data("world")

numofplayers <- world_map %>% 
  mutate(region = as.character(region)) %>% 
  left_join((players %>% mutate(nationality_name= as.character(nationality_name),
                            nationality_name = if_else(nationality_name %in% "England", 
                                                  "UK", nationality_name)) %>%
               count(nationality_name, name = "Number of Player") %>%
               rename(region = nationality_name) %>%
               mutate(region = as.character(region))), by = "region")


ggplot(numofplayers, aes(long, lat, group = group))+
  geom_polygon(aes(fill = factor(`Number of Player`) ), color = "grey", show.legend = F)+
  scale_fill_viridis_d(option = "D")+
  theme_void()+
  labs(fill = "Number of Player",
       title = "PLAYERS ENLISTED FROM COUNTRIES")

## Players from Barcelona(As Was in Fifa22)
options(repr.plot.width = 12, repr.plot.height = 8)

Barcelona %>% 
  select(short_name, overall, potential) %>% 
  arrange(-overall) %>% 
  head(15) %>% 
  gather(variable, Exp, -short_name) %>% 
  ggplot(aes(short_name, Exp, fill = variable))+
  geom_col(position = "dodge")+
  geom_text(aes(label = Exp),position = position_dodge(width = 0.9), vjust = -0.5)+
  scale_fill_manual(values = c("#004D98", "#A50044"))+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(fill = NULL, x = NULL, title = "BARCELONA")+
  theme(axis.text.x = element_text(face="bold",angle = 90, vjust = 0.5, hjust=1))

## PLAYERS FROM FRANCE
options(repr.plot.width = 12, repr.plot.height = 8)

France %>% 
  select(short_name, overall, potential) %>% 
  arrange(-overall) %>% 
  head(15) %>% 
  gather(variable, Exp, -short_name) %>% 
  ggplot(aes(short_name, Exp, fill = variable))+
  geom_col(position = "dodge")+
  geom_text(aes(label = Exp),position = position_dodge(width = 0.9), vjust = -0.5)+
  scale_fill_manual(values = c("#009c3b", "#ffdf00"))+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(fill = NULL, x = NULL, title = "Brazil")+
  theme(axis.text.x = element_text(face="bold",angle = 90, vjust = 0.5, hjust=1))

## La liga Native VS foreign players
L_NAT <- La_Liga %>% mutate(nationality_name=as.character(nationality_name),
                            nationality_name = if_else(nationality_name %in% "Spain","Native","Foreigner"))

ggplot(L_NAT)+geom_bar(aes(x=nationality_name,fill= nationality_name),show.legend = F)+
  facet_wrap(club_name~.)+labs(title = "La Liga Native and Foreigner player")

## EPL Native VS foreign players
EPL_NAT <- EPL %>% mutate(nationality_name=as.character(nationality_name),
                          nationality_name = if_else(nationality_name %in% "England","Native","Foreigner"))

ggplot(EPL_NAT)+geom_bar(aes(x=nationality_name,fill= nationality_name),show.legend = F)+
  facet_wrap(club_name~.)+labs(title = "EPL Native and Foreigner player")

## Bundesliga Native VS foreign players
Bund_NAT <- Bundesliga %>% mutate(nationality_name=as.character(nationality_name),
                                  nationality_name = if_else(nationality_name %in% "Germany","Native","Foreigner"))

ggplot(Bund_NAT)+geom_bar(aes(x=nationality_name,fill= nationality_name),show.legend = F)+
  facet_wrap(club_name~.,nrow = 3)+labs(title = "Bundesliga Native and Foreigner player")

## Distribution of player positions in the entire Fifa data
options(repr.plot.width = 15,repr.plot.height = 8)


players %>% drop_na(player_positions)%>%
  ggplot()+geom_bar(aes(x=player_positions,fill=player_positions),show.legend = F)+
  labs(title = "Player position distribution in the World")

## TOP 20 RANKINGS
## Forward
subset(players,player_positions=="CF") %>% arrange(desc(overall))%>%head(20)%>%
  ggplot(aes(x=overall,y=reorder(short_name,overall)))+geom_col(aes(fill=short_name),show.legend = F)+
  labs(x="Overall",y="Name",title = "Top 20 Forwards in the World")

## Wingers
### Left wingers
subset(players,player_positions=="LW") %>% arrange(desc(overall))%>%head(20)%>%
  ggplot(aes(x=overall,y=reorder(short_name,overall)))+geom_col(aes(fill=short_name),show.legend = F)+
  labs(x="Overall",y="Name",title = "Top 20 Left Wingers in the World")

### Right Wingers
subset(players,player_positions=="RW") %>% arrange(desc(overall))%>%head(20)%>%
  ggplot(aes(x=overall,y=reorder(short_name,overall)))+geom_col(aes(fill=short_name),show.legend = F)+
  labs(x="Overall",y="Name",title = "Top 20 Right Wingers in the World")

## Midfilders
### Center Midfilders
subset(players,player_positions=="CM") %>% arrange(desc(overall))%>%head(20)%>%
  ggplot(aes(x=overall,y=reorder(short_name,overall)))+geom_col(aes(fill=short_name),show.legend = F)+
  labs(x="Overall",y="Name",title = "Top 20 Center Midfilders in the World")

### Right Midfilders
subset(players,player_positions=="RM") %>% arrange(desc(overall))%>%head(20)%>%
  ggplot(aes(x=overall,y=reorder(short_name,overall)))+geom_col(aes(fill=short_name),show.legend = F)+
  labs(x="Overall",y="Name",title = "Top 20 Right Midfilders in the World")

### Left Midfilders
subset(players,player_positions=="LM") %>% arrange(desc(overall))%>%head(20)%>%
  ggplot(aes(x=overall,y=reorder(short_name,overall)))+geom_col(aes(fill=short_name),show.legend = F)+
  labs(x="Overall",y="Name",title = "Top 20 Left Midfilders in the World")

### Center Defensive Midfilders
subset(players,player_positions=="CDM") %>% arrange(desc(overall))%>%head(20)%>%
  ggplot(aes(x=overall,y=reorder(short_name,overall)))+geom_col(aes(fill=short_name),show.legend = F)+
  labs(x="Overall",y="Name",title = "Top 20 Center Defensive Midfilders in the World")

## Strikers
subset(players,player_positions=="ST") %>% arrange(desc(overall))%>%head(20)%>%
  ggplot(aes(x=overall,y=reorder(short_name,overall)))+geom_col(aes(fill=short_name),show.legend = F)+
  labs(x="Overall",y="Name",title = "Top 20 Strikers in the World")

## Goal Keepers
subset(players,player_positions=="GK") %>% arrange(desc(overall))%>%head(20)%>%
  ggplot(aes(x=overall,y=reorder(short_name,overall)))+geom_col(aes(fill=short_name),show.legend = F)+
  labs(x="Overall",y="Name",title = "Top 20 Goal Keepers in the World")

## Most Powerful Clubs
players %>%
  group_by(club_name,player_positions) %>%
  summarise(mean=mean(overall)) %>%
  ungroup() %>% 
  filter(club_name %in% players$club_name) %>%
  ggplot(aes(reorder(club_name,mean),mean,fill= player_positions))+
  geom_col(position = "fill")+
  geom_text(aes(label = round(mean,digits = 2)),position = position_fill(0.5),size=3.5)+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "top",axis.text.y = element_text(face = "bold"),axis.text.x = element_blank())+
  labs(x="",y="",title = "Top 20 powerful clubs with their position class")

## Promising New Talents
players %>% filter(age<20, potential>72) %>%
  arrange(-potential) %>%
  group_by(age)%>%
  do(head(.,10)) %>%
  ggplot(aes(reorder(paste0(paste(short_name,player_positions, sep = ", "),"(",club_name,
                            ")"),potential),potential,fill=as.factor(age)))+
  geom_col(show.legend = F)+
  coord_flip()+
  facet_wrap(age~.,scales = "free")+
  labs(x="",y="Potential",title = "Kids With Highest Potential")

## Most Expensive Team Possible (Based on data available)
d2 <- data.frame(   x=c(0, 0, 16.5, 100, 100,83.5), 
                    xend=c(16.5,16.5, 16.5, 83.5,83.5,83.5),
                    y=rep(c(13.68, 61.32, 13.68),2), 
                    yend=rep(c(13.68,61.32,61.32),2))

pp <- data.frame(   x=c(0,16.5,16.5,25,25,50,50,75,75,87.5),
                    y=c(37.5,13.68,61.32,0,75,18.75,56.25,0,75,37.5),
                    name=c("J. Oblak"," V. van Dijk","A. Laporte",
                           "T. Alexander-Arnold","A. Robertson",
                           "T. Kroos","F. de Jong",
                           "M. Salah","Neymar Jr",
                           "K. Mbapp?"))
p<- ggplot(players)+
  xlim(0,100)+ylim(0,75)+
  geom_vline(xintercept = c(0,50,100), color="white") + 
  geom_segment(data = d2,aes(x=x, xend=xend, y=y,yend=yend), color="white") +
  geom_point(aes(x=50,y=75/2), size=2, color="white") +
  geom_point(data=pp,aes(x=x,y=y), size=7, color="yellow")+
  geom_text(data=pp,aes(x=x,y=y,label = name),size=5)+
  theme(panel.background = element_rect(fill = "darkgreen"),
        panel.grid = element_line(colour = "darkgreen"))+
  labs(title = "Most Expensive team possible in Fifa 21",subtitle = "With most recent player valuation")+
  xlab("")+ylab("")

p

