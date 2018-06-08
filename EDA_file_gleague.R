##EDA File


#Cleaning
source('web_scraper_bbref.R')
#Earliest the function can work is 2002
gleague_data<-bbref_web_scraper("gleague",2002,2018)
#nba_data<-NBA_web_scraper_bbref(2003,2005)

str(gleague_data)
#Cool everything should be the right dtype

#Doing some EDA to weed out not qualifying players---if I want to make this a package I might have to create another func out of this
first_quarter_minutes<-quantile(gleague_data$G_MP,.25,na.rm = TRUE)

#Filtering out players who don't have enough minutes or games
gleague_data<-gleague_data %>%
  filter(G_MP>first_quarter_minutes) %>%
  filter(G_G>2)

#Take out lotto picks
draft_data<-draft_data_bbref(2000,2017)
draft_data$PK<-as.numeric(draft_data$PK)
lotto_picks<-draft_data[draft_data$PK<=14,]

gleague_data<-subset(gleague_data,!(gleague_data$player_page %in% lotto_picks$player_page))
#might wwant to look at lottos anyway--either with all the data or seperately
gleague_data_filtered_lotto<-subset(gleague_data_filtered,(gleague_data_filtered$player_page %in% lotto_picks$player_page))



  

#Import NBA data
nba_data<-bbref_web_scraper("NBA",2002,2018)


#Write to CSV so that I don't have to re-pull everytime I want to look at the data.

write.csv(gleague_data,'gleague_2002_2018_raw.csv')
write.csv(nba_data,'NBA_2002_2018_raw.csv')

gleague_data_<-read.csv('gleague_2002_2018_raw.csv')
nba_data<-read.csv('NBA_2002_2018_raw.csv')

#this little complicated pipe operator seperates out players by season. Then it looks to see if a player was on multiple teams during the season. 
#It then groups by player and eliminates their non-total statistics for players who had multiple observations per season.
gleague_grouped<-gleague_data %>% 
  group_by(year)%>%
  mutate(dup=ifelse(duplicated(player_page),"dup","")) %>%
  group_by(player_page,add=TRUE)%>%
  filter(dup!='dup') %>%
  ungroup()

nba_grouped<-nba_data%>%
  group_by(year)%>%
  mutate(dup=ifelse(duplicated(player_page),"dup",""))%>%
  group_by(player_page,add=TRUE)%>%
  filter(dup!='dup')%>%
  ungroup()

#Cool so who played in the NBA?
played_in_NBA<-gleague_grouped[gleague_grouped$player_page %in% nba_grouped$player_page,]
#1395 of 3346 non-Lotto picks played at least 1 game in the NBA, that's actually a lot higher than I thought, I should be able to have more predictive power.

#Merge NBA and gleague data
nba_grouped$year<-as.numeric(nba_grouped$year)
gleague_grouped$year<-as.numeric(gleague_grouped$year)

merged_data<-full_join(nba_grouped,gleague_grouped,by=c("Player",'year'))
merged_data$year<-as.numeric(merged_data$year)

write.csv(merged_data,'merged_2002_2018.csv')

merged_data<-read.csv('merged_2002_2017.csv')



