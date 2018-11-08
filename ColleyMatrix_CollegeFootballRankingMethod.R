
# Web scraping and employing colley ranking method on desired year of data
# Author: Yanni Dalkos

# ******************************************************   PART 2   ****************************************************
# Now generate the rankings for the desired year
load("CollegeFootballData.rdata")  # First load dataframe that was created in part 1

# Colley Function
yeartoinspect <- 1975

colley <- function(desiredyear){
  # In order for colleyfunction to work, dfyear must be a datafram with 4 columns titled, teams, wins, losses, oppenents
  # ****************************************************************************************************************
  
  dfyear <- largedf[(largedf$year == desiredyear),]
  
  # Initialize the colley matrix length 120x120
  colley <- matrix(0,nrow = length(dfyear$teams), ncol = length(dfyear$teams))
  
  # Fill the Colley matrix nesting for loops and if statements in for loops
  for (i in 1:length(dfyear$teams)){
    # The first loop will go through each row, ie each team
    # and the next for loop will take us through each column of each row
    for (j in 1:length(dfyear$teams)){
      
      # if we reach a diagonal fill it with 2+number of games team i plays
      if(i==j){
        colley[i,j] <- 2+(length(dfyear$opponents[[i]]))
      }
      
      # else find out if team i has played the team corresponding to column j
      # and fill index[i,j] in the colley matrix with -1
      else{
        for (k in 1:length(dfyear$opponents[[i]])){
          if(dfyear$opponents[[i]][k] == j){
            colley[i,j] <- colley[i,j]-1
          }
        }
      }
    }
  }
  
  # create a vector where the ith element is
  #   1 + (team[i] num of wins - team[i] num of losses)/2
  b = NULL
  for (i in 1:length(dfyear$teams)){
    b[i] <- 1+(dfyear$wins[i]-dfyear$losses[i])/2
  }
  
  rank <- c(1:length(dfyear$teams))  # create a vector 1:120 to show rank of teams
  score <- solve(colley,b)       # calculate each teams rank score with the colley and b 
  teamscore <- data.frame(dfyear$teams,score)   # combine team name and colley score into a dfyear
  solution <- data.frame(teamscore[order(teamscore[,2],decreasing = TRUE),],rank)  # sort teamscore by score and add rank vector
  
  return(solution)
}


colley(yeartoinspect)






