##Function to Manually Asign Categories to NBA players V.1
manual_NBA_categories<-function(merge_data){
  library(dplyr)
  breakdown<-summary(merge_data)
  AVG_NBA_MP<-mean(merge_data$NBA_MP,na.rm = TRUE)
  all_NBA_OWS<-quantile(merge_data$NBA_OWS,.935,na.rm = TRUE)
  all_NBA_DWS<-quantile(merge_data$NBA_DWS,.925,na.rm=TRUE)
  DWS_Dstar<-quantile(merge_data$NBA_DWS,.86,na.rm = TRUE)
  STL_Dstar<-quantile(merge_data$NBA_STL_perc,.81,na.rm = TRUE)
  BLK_Dstar<-quantile(merge_data$NBA_BLK_perc,.81,na.rm = TRUE)
  OWS_Ostar<-quantile(merge_data$NBA_OWS,.88,na.rm=TRUE)
  PTS_Ostar<-quantile(merge_data$NBA_PTS,.88,na.rm = TRUE)
  USG_Ostar<-quantile(merge_data$NBA_USG,.84,na.rm=TRUE)
  
  #Elite Defender
  
  #Elite Scorer
  
  
  #Above Average Defender
  
  #Above Average Offensive Player
  
  #Offensive Role Player
  
  #Defensive Role Player
  
  
  DWS_Rim<-quantile(merge_data$NBA_DWS,.82,na.rm = TRUE)
  Blk_Rim<-quantile(merge_data$NBA_BLK,.85,na.rm=TRUE)
  TRB_Rim<-quantile(merge_data$NBA_TRB_perc,.8,na.rm = TRUE)
  OWS_Post<-quantile(merge_data$NBA_OWS,.76,na.rm = TRUE)
  PTS_Post<-quantile(merge_data$NBA_PTS,.76,na.rm = TRUE)
  eFG_Post<-quantile(merge_data$NBA_eFG,.80,na.rm=TRUE)
  threes_Shoot<-quantile(merge_data$NBA_3P,.85,na.rm = TRUE)
  TS_Shoot<-quantile(merge_data$NBA_TS,.8,na.rm = TRUE)
  STL_Perimeter<-quantile(merge_data$NBA_STL_perc,.75,na.rm = TRUE)
  threes_Perimeter<-quantile(merge_data$NBA_3PA,.60,na.rm = TRUE)
  AST_Dist<-quantile(merge_data$NBA_AST,.68,na.rm = TRUE)
  TOV_Dist<-quantile(merge_data$NBA_TOV,.65,na.rm = TRUE)
  FG_Dist<-quantile(merge_data$NBA_FG_perc,.50,na.rm = TRUE)
  MP_BW<-quantile(merge_data$NBA_MP,.1,na.rm = TRUE)
  TRB_Strech<-quantile(merge_data$NBA_TRB,.72,na.rm = TRUE)
  BLK_Strech<-quantile(merge_data$NBA_BLK,.58,na.rm=TRUE)
  FGA_Scorer<-quantile(merge_data$NBA_FGA,.70,na.rm = TRUE)
  PER_Scorer<-quantile(merge_data$NBA_PER,.65,na.rm = TRUE)
  PTS_Scorer<-quantile(merge_data$NBA_PTS,.85,na.rm = TRUE)
  TRB_Reb<-quantile(merge_data$NBA_TRB,.88,na.rm = TRUE)
  ORB_Reb<-quantile(merge_data$NBA_ORB_perc,.5,na.rm = TRUE)
  eFG_Reb<-quantile(merge_data$NBA_eFG,.5,na.rm=TRUE)
  USG_VScorer<-quantile(merge_data$NBA_USG,.7,na.rm = TRUE)
  PTS_VScorer<-quantile(merge_data$NBA_PTS,.78,na.rm = TRUE)
  DWS_Post<-quantile(merge_data$NBA_DWS,.70,na.rm = TRUE)
  DRB_Post<-quantile(merge_data$NBA_DRB,.78,na.rm = TRUE)
  BLK_Post<-quantile(merge_data$NBA_BLK,.65,na.rm = TRUE)
  #Actually came up with these independently, but nice to see that others found similar roles https://www.wired.com/2012/04/analytics-basketball/
  
  
  
  
  #Start of the Massive if/else chain
  merge_data<-merge_data%>%
    mutate(nba_category=
             #First determine whether they actually played in the NBA/Had any contribution
             ifelse(is.na(NBA_G),"Never NBA",ifelse(NBA_G<10 | NBA_MP<MP_BW,"BenchWarmer",
             #I think that catergorizing the middle players will be the hardest, so let's work our way down-- I do realize that the order in which I do this matters but my thought is that by going from more specific to less, we can capture niche players better                           
             ifelse((NBA_MP>AVG_NBA_MP & NBA_OWS>all_NBA_OWS & NBA_DWS>all_NBA_DWS),"All-NBA", #Good
             ifelse(NBA_MP>AVG_NBA_MP & NBA_OWS>OWS_Ostar & NBA_PTS>PTS_Ostar & NBA_USG>USG_Ostar, "Offensive Star", #maybe too big
             ifelse(NBA_MP>AVG_NBA_MP & NBA_DWS>DWS_Dstar & NBA_STL_perc>STL_Dstar & NBA_BLK_perc>BLK_Dstar,"Defensive Star", #maybe too big
             ifelse(NBA_OWS>OWS_Post& NBA_PTS>PTS_Post & NBA_eFG>eFG_Post & NBA_TRB_perc>TRB_Rim,"Elite Post Player", #Good
             ifelse(NBA_DWS>DWS_Rim & NBA_BLK>Blk_Rim & NBA_TRB_perc> TRB_Rim, "Elite Rim Protector", #Good
             ifelse(NBA_OWS>OWS_Post & NBA_3P>threes_Shoot & NBA_TS>TS_Shoot,"Elite Shooter", #Good
             ifelse(NBA_DWS>DWS_Rim & NBA_STL_perc>STL_Perimeter & NBA_3PA > threes_Perimeter,"Elite Perimeter Defender", #slightly too small
             ifelse(NBA_AST>AST_Dist & NBA_TOV < TOV_Dist & NBA_FG_perc > FG_Dist,"Elite BallHandler",#getting better
             ifelse(NBA_3PA>threes_Perimeter & NBA_TRB>TRB_Strech & NBA_OWS>OWS_Post & NBA_BLK>BLK_Strech,"Elite Strech Big", #Good
             ifelse(NBA_3PA>threes_Perimeter & NBA_DWS>DWS_Rim,"3+D", #Honestly first stab was good
             ifelse(NBA_FGA>FGA_Scorer  & NBA_PTS>PTS_Scorer & NBA_PER>PER_Scorer & NBA_MP>AVG_NBA_MP,"Elite Scorer",
             ifelse(NBA_TRB>TRB_Reb & NBA_ORB_perc> ORB_Reb & NBA_eFG>eFG_Reb,"Elite Rebounder",
             ifelse(NBA_PTS>PTS_VScorer & NBA_FGA>FGA_Scorer & NBA_USG>USG_VScorer,"Volume Scorer", #slightly too big?
             ifelse(NBA_DWS>DWS_Post & NBA_BLK>BLK_Post & NBA_DRB>DRB_Post,"Post Defender",
             ifelse(NBA_OWS>NBA_DWS,"Offensive Role Player","Defensive Role Player"
             )))))))))))))))))
    )

#I would love to be able to use 2 years projections as well--- but might be limiting my data too much
merged_data_categories<-merge_data%>%
    group_by(player_page)%>%
    mutate(nextyr_cat=ifelse(is.na(lead(nba_category,n=1)),"Never NBA",lead(nba_category,n=1)))%>%
    ungroup()

  

}

#So first attempt is with 18 categories, I did a pretty good job of sorting them, but we might not have enough data to sort into this many categories