
# Web scraping and employing colley ranking method on desired year of data
# Author: Yanni Dalkos

rm(list=ls())

# college games data is available in a different file for every year at the website:
# http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf****gms.txt
#   where **** is the year of the season the games were played

largedf <- data.frame()   # initialize the df that will hold every d1 game from every year 


for(i in 1960:2010){
  
  z <- paste("http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf",i,"gms.txt",sep = "")  # pulls from internet
  df <- read.fwf(z, widths = c(11,28,3,28,3))
  yearnow <- i          # record the current year to fill up year vector
  names(df) <- c("date","hteam","hscore","ateam","ascore")
  
  # ******************* Find each unique team that played in the given year *********************************************
  df$hteam <- as.character(df$hteam)
  df$hteam <- gsub(" ","",df$hteam)
  df$ateam <- as.character(df$ateam)
  df$ateam <- gsub(" ","",df$ateam)
  
  teamsh <- gsub(" ","",unique(df$hteam))
  teamsa <- gsub(" ","",unique(df$ateam))
  allteams <- c(teamsh,teamsa)
  # ***********   ***********    ***********    ***********    ***********    ***********    ***********    ***********   
  teams <- unique(allteams); rm(teamsa,teamsh,allteams)    # get a final list of teams and delete unecessary variables      
  
  
  # first delete any d2 teams and any games d1 teams played a d2 team ****************************************************
  d2teams <- character()
  for (i in 1:length(teams)){
    teamnow <- teams[i]
    gamesplayed <- 0
    for (j in 1:length(df$hteam)){
      if ((teamnow==df$hteam[j])|(teamnow==df$ateam[j])){
        gamesplayed <- gamesplayed + 1 
      }
    }
    if (gamesplayed <= 5){
      d2teams <- rbind(d2teams,teamnow)  # store all d2 teams in order to remove them from team list later
    }
  }
  
  # now delete d2 teams
  for (i in 1:length(d2teams)){
    teams <- teams[teams != d2teams[i]]
    df <- df[!(df$hteam==d2teams[i]),] 
    df <- df[!(df$ateam==d2teams[i]),] 
  }
  
  
  # ********************************** finish initializing final data frame ***********************************************
  id <- as.numeric(1:length(teams))     # Initialize vectors that will go into final data frame
  wins <- rep(0,length(teams))
  losses <- rep(0,length(teams))
  year <- rep(yearnow,length(teams))      
  opponents <- rep("",length(teams))
  
  newdf <- data.frame(id,teams,year,wins,losses,opponents)  # Put together vectors into a data frame
  newdf$opponents <- as.list(9999)                          # remeber to make opponents a list since it will 
  #  contain different length schedules for each team
  # ***********************************************************************************************************************
  
  df <- df[!(df$hscore==df$ascore),]   # get rid of tie games
  
  names(id) <- teams                   # give id vector names of corresponding team for easy reference in the upcoming loop
  
  # Now start to fill in newdf with numwins, numlosses, Opponents *********************************************************
  for (i in 1:length(df$hteam)){
    hometeamnow <- df$hteam[i]
    awayteamnow <- df$ateam[i]
    newdf$opponents[[id[hometeamnow]]] <- cbind(newdf$opponents[[id[hometeamnow]]],id[awayteamnow])
    newdf$opponents[[id[awayteamnow]]] <- cbind(newdf$opponents[[id[awayteamnow]]],id[hometeamnow])
    if (df$hscore[i] > df$ascore[i]){
      newdf$wins[id[hometeamnow]] <- newdf$wins[id[hometeamnow]] + 1
      newdf$losses[id[awayteamnow]] <- newdf$losses[id[awayteamnow]] +1
      
    } else {
      newdf$wins[id[awayteamnow]] <- newdf$wins[id[awayteamnow]] + 1
      newdf$losses[id[hometeamnow]] <- newdf$losses[id[hometeamnow]] +1
    }
  }
  
  # This is a correction to the oppoenents vector from initializing it for each team
  for (i in 1:length(newdf$opponents)){      
    newdf$opponents[[i]] = newdf$opponents[[i]][c(2:length(newdf$opponents[[i]]))]
  }
  # *********************************************** newdf is complete ****************************************************
  
  
  largedf <- rbind(largedf,newdf)  # concatenate newdf into the final dataframe storing each years games 
}

# **********   **********   **********   Web scrape is complete at this point    **********   ***********   ************


save(largedf,file="CollegeFootballData.Rdata")   # save the df for future use


