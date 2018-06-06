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
gleague_data_filtered<-gleague_data %>%
  filter(G_MP>first_quarter_minutes) %>%
  filter(G_G>2)

#Take out lotto picks
draft_data<-draft_data_bbref(2000,2017)
draft_data$PK<-as.numeric(draft_data$PK)
lotto_picks<-draft_data[draft_data$PK<=14,]

gleague_data_filtered_non_lotto<-subset(gleague_data_filtered,!(gleague_data_filtered$player_page %in% lotto_picks$player_page))
#might wwant to look at lottos anyway--either with all the data or seperately
gleague_data_filtered_lotto<-subset(gleague_data_filtered,(gleague_data_filtered$player_page %in% lotto_picks$player_page))

#this little complicated pipe operator seperates out players by season. Then it looks to see if a player was on multiple teams during the season. 
#It then groups by player and eliminates their non-total statistics for players who had multiple observations per season.
gleague_grouped<-gleague_data_filtered_non_lotto %>% 
  group_by(year)%>%
  mutate(dup=ifelse(duplicated(player_page),"dup","")) %>%
  group_by(player_page,add=TRUE)%>%
  filter(dup!='dup') %>%
  ungroup()

  

#Import NBA data
nba_data<-bbref_web_scraper("NBA",2002,2018)
nba_grouped<-nba_data%>%
  group_by(year)%>%
  mutate(dup=ifelse(duplicated(player_page),"dup",""))%>%
  group_by(player_page,add=TRUE)%>%
  filter(dup!='dup')%>%
  ungroup()

#Write to CSV so that I don't have to re-pull everytime I want to look at the data.

write.csv(gleague_data,'gleague_2002_2018_raw.csv')
write.csv(nba_data,'NBA_2002_2018_raw.csv')

#Cool so who played in the NBA?
played_in_NBA<-gleague_grouped[gleague_grouped$player_page %in% nba_grouped$player_page,]
#1395 of 3346 non-Lotto picks played at least 1 game in the NBA, that's actually a lot higher than I thought, I should be able to have more predictive power.

#Merge NBA and gleague data
merged_data<-full_join(nba_grouped,gleague_grouped,by=c("player_page",'year','Player'))
merged_data$year<-as.numeric(merged_data$year)
#Merge looks like it worked as expected. Now I have to replace some NA columns with 0s
##Note-- this throws the distribution off, is it better to have blanks there instead? 
# numeric_cols<-c(seq(6,73,1),seq(80,142,1))
# merged_data[,numeric_cols]<-apply(merged_data[,numeric_cols],2,function(x) as.numeric(x))
# merged_data[,numeric_cols]<-apply(merged_data[,numeric_cols],2,function(x) replace(x,is.na(x),0))

write.csv(merged_data,'merged_2002_2018.csv')
#Assign labels to players-- tentatively 1) never played 2) benchwarmer 3) Defensive roleplayer 4) offensive role player 5) Defensive star 6) Offensive star 7)All-NBA 8) Rebound machine 9) point god 

merged_data<-read.csv('merged_2002_2017.csv')



