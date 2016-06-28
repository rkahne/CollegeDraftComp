library(jsonlite)
library(magrittr)
library(ggplot2)
library(gridExtra)
library(xtable)



Draft <- fromJSON('http://stats.nba.com/stats/drafthistory/?leagueid=00')

CollegeIds<-function(school){
  return(Draft[[3]][[3]][[1]][,1][which(Draft[[3]][[3]][[1]][,11]==school)])
}

NamesSince09<-function(school){
  return(Draft[[3]][[3]][[1]][,2][which(Draft[[3]][[3]][[1]][,11]==school & Draft[[3]][[3]][[1]][,3] %>% as.numeric()>2009)])
}

schools<-data.frame(name=Draft[[3]][[3]][[1]][,11] %>% unique())
schools$players<-sapply(schools$name,function(i){
  return(Draft[[3]][[3]][[1]][,1][which(Draft[[3]][[3]][[1]][,11]==i)] %>% length())
})
schools$playersSince09<-sapply(schools$name,function(i){
  return(Draft[[3]][[3]][[1]][,1][which(Draft[[3]][[3]][[1]][,11]==i & Draft[[3]][[3]][[1]][,3] %>% as.numeric()>2009)] %>% length())
})

GPFrame<-function(id){
  data <- fromJSON(paste('http://stats.nba.com/stats/playercareerstats/?permode=Totals&PlayerID=',id,sep=''))
  name<-fromJSON(paste('http://stats.nba.com/stats/commonplayerinfo/?PlayerID=',id,sep=''))
  if(data[[3]][[3]][[1]] %>% length() == 0){
    (data.frame(
      id=name[[3]][[3]][[1]][1],
      name=name[[3]][[3]][[1]][4],
      year=paste(substr(data[[3]][[3]][[7]][2],1,4) %>% as.numeric()+1,'-',substr(data[[3]][[3]][[7]][2],6,7) %>% as.numeric()+1,sep = ''),
      team=Draft[[3]][[3]][[1]][,8][which(Draft[[3]][[3]][[1]][,1]==id)],
      GP=0,
      MIN=0))
  }else{
    (data.frame(
      id=name[[3]][[3]][[1]][1],
      name=name[[3]][[3]][[1]][4],
      year=data[[3]][[3]][[1]][,2],
      team=data[[3]][[3]][[1]][,5],
      GP=data[[3]][[3]][[1]][,7] %>% as.numeric(),
      MIN=data[[3]][[3]][[1]][,9] %>% as.numeric()
      )
    )
  }
}

TeamFrame<-function(ids){
  ret<-data.frame(id=NULL,name=NULL,year=NULL,team=NULL,GP=NULL,MINUTES=NULL)
  for(i in 1:length(ids)){
    ret<-rbind(ret, GPFrame(ids[i]))
  }
  ret$bigid<-paste(ret$id,ret$year,sep='')
  ret$idteam<-paste(ret$id,ret$team,sep='')
  ret$keep<-vector(length=length(ret$id))
  for(i in 1:length(ret$id)){
    if(which(ret$bigid[i]==ret$bigid) %>% length() > 1){
      if(ret$team[i]=='TOT'){
        ret$keep[i]<-T
      }else{
        ret$keep[i]<-F
      }
    }else{
      ret$keep[i]<-T
    }  
  }
  ret$GP<-ret$GP %>% as.numeric()
  ret$MIN<-ret$MIN %>% as.numeric()
  return(subset(ret, keep==T, select=c('id','name','year','team','GP','MIN')))
}

Drafted09<-function(seasons){
  return(sapply(seasons$id,function(i){
    d <- subset(seasons, id==i)
    yrs<-substr(d$year,1,4) %>% as.numeric()
    if(is.na(min(yrs))){
      return(NA)
    }else if(min(yrs)<2010){
      return(F)
    }else{
      return(T)
    }
  }))
}

GPHist<-function(frame,team,c){
  ggplot(frame, aes(GP))+
    geom_histogram(binwidth=3, fill=c)+
    ylim(0,10)+xlim(0,82)+
    xlab("Games Played")+ggtitle(paste("Games Per Season - ",team,"\nPlayers Drafted Since 2009",sep=''))
}

MINHist<-function(frame,team,c){
  ggplot(frame, aes(MIN))+
    geom_histogram(binwidth=300, fill=c)+
    ylim(0,10)+xlim(0,4000)+
    xlab("Minutes Played")+ggtitle(paste("Minutes Per Season - ",team,"\nPlayers Drafted Since 2009",sep=''))
}

GPTable<-function(frame){
  t<-xtable(frame[2:6][order(-frame$GP),])
  print.xtable(t, type='html', include.rowname=F) %>% return()
}

MINTable<-function(frame){
  t<-xtable(frame[2:6][order(-frame$MIN),])
  print.xtable(t, type='html', include.rowname=F) %>% return()
}

Combo<-function(team){
  period<-c('2008-09', '2009-10', '2010-11','2011-12','2012-13','2013-14','2014-15','2015-16')
  ret<-CollegeIds(team) %>% TeamFrame()
  ret$Drafted09<-Drafted09(ret)
  return(subset(ret, year %in% period & Drafted09==T))
}

Kentucky<-Combo('Kentucky')
Louisville<-Combo('Louisville')
Duke<-Combo('Duke')
NC<-Combo('North Carolina')
Kansas<-Combo('Kansas')
UCLA<-Combo('California-Los Angeles')
Arizona<-Combo('Arizona')
Florida<-Combo('Florida')
Syracuse<-Combo('Syracuse')
Michigan<-Combo('Michigan')

grid.arrange(
  GPHist(Kentucky,'Kentucky','#005DAA'), 
  GPHist(Duke,'Duke','#001A57'),
  GPHist(NC,'North Carolina','#7BAFD4'),
  GPHist(Kansas,'Kansas','#0051BA'),
  GPHist(UCLA,'UCLA','#FFB300'),
  GPHist(Arizona,'Arizona','#0C234B'),
  GPHist(Michigan,'Michigan','#00274C'),
  GPHist(Syracuse,'Syracuse','#D44500'),
  GPHist(Florida,'Florida','#F14B20'),
  GPHist(Louisville,'Louisville','#AD0000'),
  ncol=2)

grid.arrange(
  MINHist(Kentucky,'Kentucky','#005DAA'), 
  MINHist(Duke,'Duke','#001A57'),
  MINHist(NC,'North Carolina','#7BAFD4'),
  MINHist(Kansas,'Kansas','#0051BA'),
  MINHist(UCLA,'UCLA','#FFB300'),
  MINHist(Arizona,'Arizona','#0C234B'),
  MINHist(Florida,'Florida','#F14B20'),
  MINHist(Louisville,'Louisville','#AD0000'),
  ncol=2
  )

 
