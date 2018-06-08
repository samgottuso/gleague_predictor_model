#Reducted_NBA_Categories

reduced_NBA_categories<-function(merge_data){
  library(dplyr)

  merge_data<-merge_data%>%
    mutate(nba_category=
             #First determine whether they actually played in the NBA/Had any contribution
             ifelse(is.na(NBA_G),"Never NBA",ifelse(NBA_G<10 | NBA_MP<quantile(merge_data$NBA_MP,.25,na.rm=TRUE),"BenchWarmer",
             ifelse(NBA_OWS>NBA_DWS,"Offensive Player", "Defensive Player"
    ))))
  
  #I would love to be able to use 2 years projections as well--- but might be limiting my data too much
  merged_data_categories<-merge_data%>%
    group_by(Player)%>%
    mutate(nextyr_cat=ifelse(is.na(lead(nba_category,n=1)),"Never NBA",lead(nba_category,n=1)))%>%
    ungroup()
  
  
  
}