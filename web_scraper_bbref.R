#Building a Web Scraper for Basketball Reference

#Setup
library(rvest)
library(dplyr)

#Season Statistics- #G League
bbref_web_scraper<-function(league,year1,year2){
seasons_list<-as.list(seq(year1,year2,1))
data_list<-c()
if(league=="gleague"){
for (year in seasons_list){
  url_pergame=paste0("https://www.basketball-reference.com/gleague/years/gleague_",as.character(year),"_per_game.html")
  url_per36=paste0("https://www.basketball-reference.com/gleague/years/gleague_",as.character(year),"_per_minute.html")
  url_advanced=paste0("https://www.basketball-reference.com/gleague/years/gleague_",as.character(year),"_advanced.html")
  season_page_pergame<-read_html(url_pergame)
  season_page_per36 <- read_html(url_per36)
  season_page_advanced<-read_html(url_advanced)
  season_table_pergame<-html_nodes(season_page_pergame,'table')
  season_table_data_per36<-html_nodes(season_page_per36,'table')
  season_table_data_advanced<-html_nodes(season_page_advanced,'table')
  season_r_data_pergame<-as.data.frame(html_table(season_table_pergame))
  season_r_data_per36<-as.data.frame(html_table(season_table_data_per36))
  season_r_data_advanced<-as.data.frame(html_table(season_table_data_advanced))
  #Combining the Two tables just on the metrics we need
  season_r_data_per36<-subset(season_r_data_per36,select=7:27)
  season_r_data_advanced<-subset(season_r_data_advanced,select=6:22)
  season_r_data<-cbind.data.frame(season_r_data_pergame,season_r_data_per36)
  season_r_data<-cbind.data.frame(season_r_data,season_r_data_advanced)
  #Taking out repeated headers
  season_r_data<-season_r_data[season_r_data$Player!="Player",]
  season_r_data$year<-year
  season_r_data$g_player_page<-(season_page_pergame %>% html_nodes("table th:nth-child(1) a") %>% html_attr('href'))
  season_r_data$player_page<-gsub("/gleague","",season_r_data$g_player_page)
  season_r_data$player_page<-gsub("d.html",".html",season_r_data$player_page)
  #So the difference between a gleague player's URL and their NBA URL is the addition of /gleague/ in front of players and then a d in front of .html
  data_list[[year]]<-season_r_data
}

seasons_player_data<-do.call("rbind",data_list)


#Rename_columns
gleague_colnames<-c('Player','Tm','Age','G_G','G_GS','G_MP','G_FG','G_FGA','G_FG_perc','G_3P','G_3PA','G_3P_perc','G_2P','G_2PA','G_2P_perc','G_eFG','G_FT','G_FTA','G_FT_perc','G_ORB','G_DRB','G_TRB','G_AST','G_STL','G_BLK','G_TOV','G_PF','G_PTS','G_FG_36','G_FGA_36','G_FG_perc_36','G_3P_36','G_3PA_36','G_3P_perc_36','G_2P_36','G_2PA_36','G_2P_perc_36','G_FT_36','G_FTA_36','G_FT_perc_36','G_ORB_36','G_DRB_36','G_TRB_36','G_AST_36','G_STL_36','G_BLK_36','G_TOV_36','G_PF_36','G_PTS_36','G_PER','G_TS','G_3PAr','G_FTr','G_ORB_perc','G_DRB_perc','G_TRB_perc','G_AST_perc','G_STL_perc','G_BLK_perc','G_TOV_perc','G_USG','G_WS.dum','G_OWS','G_DWS','G_WS','G_WS_48','year','g_player_page','player_page')
colnames(seasons_player_data)<-gleague_colnames
#Changing necessary columns to numeric
numeric_cols<-c(seq(3,67,1))
seasons_player_data[,numeric_cols]<-apply(seasons_player_data[,numeric_cols],2,function(x) as.numeric(x))
return(seasons_player_data)

}else if(league=="NBA"){  

#NBA Web Scraper-- finding anyone who played a game in the NBA 
  
  for (year in seasons_list){
    url_pergame=paste0("https://www.basketball-reference.com/leagues/NBA_",as.character(year),"_per_game.html")
    url_per36=paste0("https://www.basketball-reference.com/leagues/NBA_",as.character(year),"_per_minute.html")
    url_advanced=paste0("https://www.basketball-reference.com/leagues/NBA_",as.character(year),"_advanced.html")
    season_page_pergame<-read_html(url_pergame)
    season_page_per36 <- read_html(url_per36)
    season_page_advanced<-read_html(url_advanced)
    season_table_pergame<-html_nodes(season_page_pergame,'table')
    season_table_data_per36<-html_nodes(season_page_per36,'table')
    season_table_data_advanced<-html_nodes(season_page_advanced,'table')
    season_r_data_pergame<-as.data.frame(html_table(season_table_pergame))
    season_r_data_per36<-as.data.frame(html_table(season_table_data_per36))
    season_r_data_advanced<-as.data.frame(html_table(season_table_data_advanced))
    #Combining the Two tables just on the metrics we need
    season_r_data_per36<-subset(season_r_data_per36,select=9:29)
    season_r_data_advanced<-subset(season_r_data_advanced,select=8:29)
    season_r_data<-cbind.data.frame(season_r_data_pergame,season_r_data_per36)
    season_r_data<-cbind.data.frame(season_r_data,season_r_data_advanced)
    #Taking out repeated headers
    season_r_data<-season_r_data[season_r_data$Player!="Player",]
    season_r_data$year<-year
    season_r_data$player_page<-(season_page_pergame %>% html_nodes("table td:nth-child(2) a") %>% html_attr('href'))
    data_list[[year]]<-season_r_data
  }
  
  seasons_player_data<-do.call("rbind",data_list)
  
  nba_colnames<-c('Ind','Player','Pos','Age','TM','NBA_G','NBA_GS','NBA_MP','NBA_FG','NBA_FGA','NBA_FG_perc','NBA_3P','NBA_3PA','NBA_3P_perc','NBA_2P','NBA_2PA','NBA_2P_perc','NBA_eFG','NBA_FT','NBA_FTA','NBA_FT_perc','NBA_ORB','NBA_DRB','NBA_TRB','NBA_AST','NBA_STL','NBA_BLK','NBA_TOV','NBA_PF','NBA_PTS','NBA_FG_36','NBA_FGA_36','NBA_FG_perc_36','NBA_3P_36','NBA_3PA_36','NBA_3P_perc_36','NBA_2P_36','NBA_2PA_36','NBA_2P_perc_36','NBA_FT_36','NBA_FTA_36','NBA_FT_perc_36','NBA_ORB_36','NBA_DRB_36','NBA_TRB_36','NBA_AST_36','NBA_STL_36','NBA_BLK_36','NBA_TOV_36','NBA_PF_36','NBA_PTS_36','NBA_PER','NBA_TS','NBA_3PAr','NBA_FTr','NBA_ORB_perc','NBA_DRB_perc','NBA_TRB_perc','NBA_AST_perc','NBA_STL_perc','NBA_BLK_perc','NBA_TOV_perc','NBA_USG','NBA_WS.dum','NBA_OWS','NBA_DWS','NBA_WS','NBA_WS_48','blank','NBA_OBPM','NBA_DBPM','NBA_BPM','NBA_VORP','year','player_page')
  colnames(seasons_player_data)<-nba_colnames
  
  #Changing necessary columns to numeric
  numeric_cols<-c(4,seq(6,74,1))
  seasons_player_data[,numeric_cols]<-apply(seasons_player_data[,numeric_cols],2,function(x) as.numeric(x))
  return(seasons_player_data)

}
}

draft_data_bbref<-function(year1,year2){
  seasons_list<-as.list(seq(year1,year2,1))
  data_list<-c()
  for (year in seasons_list){
    url=paste0('https://www.basketball-reference.com/draft/NBA_',as.character(year),".html")
    draft_page<-read_html(url)
    draft_table<-html_nodes(draft_page,'table')
    draft_data<-as.data.frame(html_table(draft_table))
    draft_data<-subset(draft_data,select=2:5)
    draft_colnames<-c('PK','Tm','Player','College')
    colnames(draft_data)<-draft_colnames
    draft_data<-draft_data[draft_data$Player!="Player",]
    draft_data<-draft_data[draft_data$Player!="Round 2",]
    
    
    #Adding Columns
    draft_data$year<-year
    draft_data$player_page<-""
    #HAVE to do this in a less convinent fashion because not all draft picks have URLs to use.
    url_list<-c()
    for (pick in seq(1,nrow(draft_data))){
    player_row<-paste0("table tr:nth-child(",as.character(pick),") td:nth-child(4) a")
    player_url<-html_attr(html_nodes(draft_page,player_row),'href')
      if (identical(player_url,character(0))){
      player_url<-"Never Played in the NBA"
    }else{
      player_url<-player_url
    }
    draft_data$player_page[pick]<-player_url
    }
    data_list[[year]]<-draft_data
  }
  all_draft_data<-do.call("rbind",data_list)
  return(all_draft_data)
}


