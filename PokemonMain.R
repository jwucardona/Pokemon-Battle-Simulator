##########################################################################
############                Final Assignment                ##############
############                Analysis Toolbox                ##############
############             by Joella Wu-Cardona               ##############
############                27 October 2023                 ##############
##########################################################################

##Assignment requirements
  # Create a custom function with at least 2 arguments
  # • one of the arguments should have FALSE as a default
  # • called at least twice, with different arguments
  # • does something that would have required more code otherwise.
  # Write a table with summary data to disk.
  # • Table can be ASCII (csv or tsv), fancier formats also allowed
  # • Header showing which summary statistics are in the columns
  # • First column designating the grouping for which you provide summary data (e.g. year, country, pokemon type)
  # Write a series of figures created with a for-loop to disk (either as a panel graph or separate images)
  # Do a statistical analysis and use paste() to embed the important numbers in a sentence

##Project goal
  # This project aims to display R skills and statistical analyses through mock Pokemon battles
  # I will use the information provided by the Pokemon dataset to determine how selected Pokemon 
  # attack/defend against other Pokemon. Some types are more effective than others. 
  # At the end, I will summarize the significant game stats for the Pokemon in an output file


##SETUP
setwd("C:/Users/jwuca/Documents")
allPokemon <- read.csv(file = "pokemon.csv", header=TRUE, stringsAsFactors = FALSE)
#replace so that it matches "against_type"
allPokemon$type1 <- replace(allPokemon$type1, allPokemon$type1 == "fighting", "fight")
allPokemon$type2 <- replace(allPokemon$type2, allPokemon$type2 == "fighting", "fight")
#new rows for game stats
allPokemon$offensive <- 0
allPokemon$defensive <- 0
allPokemon$fainted <- FALSE
allPokemon$damageDealt <- 0
allPokemon$damageRecieved <- 0

##FUNCTIONS
###Do a statistical analysis and use paste() to embed the important numbers in a sentence
###Create a custom function with at least 2 arguments
#calculate how much damage should be dealt against opponent
dmgDealt <- function(attacker, defender) {
  efficiency <- 0
  if (defender$type2 != "") { #if there are 2 types
    efficiency <- mean(attacker[,paste0("against_", defender$type1)], attacker[,paste0("against_", defender$type2)])
  }
  else { #if there is one type
    efficiency <- attacker[,paste0("against_", defender$type1)]
  }
  
  #calculate damage (efficiency * attack amount - defense amount)
  dmg <- efficiency * attacker$attack - defender$defense
  if (dmg < 0) { #make sure damage isnt negative
    dmg <- 0
  }
  
  print(paste0(attacker$name, " attacks ", defender$name, " with ", efficiency, " effectiveness for ", dmg, " damage!"))

  return(dmg)
}

###Write a table with summary data to disk.
###Create a custom function with at least 2 arguments
### • one of the arguments should have FALSE as a default
summarizeGameStats <- function(team, fileName, winningTeam = FALSE) {
  PkmnNames <- c()
  RemainingHealth <- c()
  OffensiveMoves <- c()
  DefensiveMoves <- c()
  Fainted <- c()
  DamageDealt <- c()
  DamageRecieved <- c()
  for (i in 1:numPerTeam) {
    PkmnNames <- c(PkmnNames, team[[i]]$name)
    RemainingHealth <- c(RemainingHealth, team[[i]]$hp)
    OffensiveMoves <- c(OffensiveMoves, team[[i]]$offensive)
    DefensiveMoves <- c(DefensiveMoves, team[[i]]$defensive)
    Fainted <- c(Fainted, team[[i]]$fainted)
    DamageDealt <- c(DamageDealt, team[[i]]$damageDealt)
    DamageRecieved <- c(DamageRecieved, team[[i]]$damageRecieved)
  }
  if (winningTeam) {    #add column if winning team
    df <- data.frame(PkmnNames, RemainingHealth, OffensiveMoves, DefensiveMoves, Fainted, DamageDealt, DamageRecieved, Winner = c(rep("Winner!", numPerTeam)))
  }
  else {              #do not add if losing team
    df <- data.frame(PkmnNames, RemainingHealth, OffensiveMoves, DefensiveMoves, Fainted, DamageDealt, DamageRecieved, Winner = c(rep("", numPerTeam)))
    colnames(df)[8] <-""    #set blank col name
  }
  write.table(df, file = paste0(fileName, ".csv"), append = FALSE, sep = ",", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
  return(df)
}

###Write a series of figures created with a for-loop to disk (either as a panel graph or separate images)
#this is not relevant to my game, this is for the purposes of the assignment requirements.
printPlots <- function() {
  png(filename="randomPlots.png", height=2400, width=2400)
  par(mfrow=c(3,4))
  #these are the columns of numeric data for plots in the pokemon dataset. not all are used in game
  plotOptions <- c('attack', 'base_egg_steps', 'base_happiness', 'base_total', 
                   'defense', 'height_m', 'hp', 'percentage_male', 
                   'sp_attack', 'sp_defense', 'speed', 'weight_kg')
  for (i in 1:12) {
    hist(allPokemon[,plotOptions[i]], main=plotOptions[i], xlab=plotOptions[i])
  }
  dev.off()
}

#which team starts determined by average speed of team
startingTeam <- function() {
  speed1 <- 0
  speed2 <- 0
  for (i in 1:numPerTeam) {
    speed1 <- speed1 + team1[[i]]$speed
    speed2 <- speed2 + team2[[i]]$speed
  }
  speed1 <- speed1 / numPerTeam
  speed2 <- speed2 / numPerTeam
  
  #return opposite because it swaps at start of while loop
  if (speed1 > speed2) {
    return("team2")
  } else {
    return("team1")
  }
}


#Determine if randomized runthrough or user played
interactive <- readline(prompt="Do you want to run this game automated (enter 0) or interactive (enter 1)?: ")
while (!interactive %in% 0:1) {
  print("Invalid input.")
  interactive <- readline(prompt="Do you want to run this game automated (enter 0) or interactive (enter 1)?: ")
}
interactive <- as.numeric((interactive))

#Choose number of pokemon per team, otherwise go to default
if (interactive) {
  numPerTeam <- readline(prompt="How many Pokemon do you want per team?: ")
  while (!numPerTeam %in% 0:9) {
    print("Invalid input.")
    numPerTeam <- readline(prompt="How many Pokemon do you want per team?: ")
  }
  numPerTeam <- as.numeric((numPerTeam))
} else {
  numPerTeam <- 6  #default - easy to change for testing purposes
}

#interactive team selection
if (interactive) {
  #read user input for Pokemon teams
  team1 <- list()
  team2 <- list()
  savedNames <- c()
  for (i in 1:(numPerTeam*2)) {
    pokemon <- NA
    #loop until valid input
    while (!pokemon %in% allPokemon$name|| pokemon %in% savedNames) {   #make sure it is a valid pokemon and hasnt already been used
      if (i <= numPerTeam) {
        pokemon <- readline(prompt=paste0("You are creating Team 1. Enter the English name of Pokemon ", i, "/", numPerTeam, ": "))
      }
      else {
        pokemon <- readline(prompt=paste0("You are creating Team 2. Enter the English name of Pokemon ", (i-numPerTeam), "/", numPerTeam, ": "))
      }
      if (!pokemon %in% allPokemon$name) {
        print("Invalid Pokemon name! Try again.")
      } else if (pokemon %in% savedNames) {
        print("Please do not select duplicate Pokemon! Try again.")
      }
    }
    #identify full Pokemon row information and add to respective team
    savedNames <- c(savedNames, pokemon)
    if (i <= numPerTeam) {
      team1[pokemon] <- list(allPokemon[which(allPokemon$name==pokemon),])  
    }
    else {
      team2[pokemon] <- list(allPokemon[which(allPokemon$name==pokemon),])
    }
  }
} else {  #automated selection
  team1 <- list()
  team2 <- list()
  for (i in 1:(numPerTeam*2)) {
    #identify full Pokemon row information and add to team
    pokemon <- allPokemon[sample(1:nrow(allPokemon), 1),]
    if (i <= numPerTeam) {
      team1[pokemon$name] <- list(pokemon)
    }
    else {
      team2[pokemon$name] <- list(pokemon)
    }
  }
}

#set list of pokemon names for future reference
team1Names <- c()
team2Names <- c()
for (i in 1:numPerTeam) { 
  team1Names <- c(team1Names, team1[[i]]$name)
  team2Names <- c(team2Names, team2[[i]]$name)
}


#gameplay
player <- startingTeam()
opponent <- 0
count <- 0
team1Pokemon <- numPerTeam
team2Pokemon <- numPerTeam
faintedPokemon <- c()
while (team1Pokemon > 0 && team2Pokemon > 0 && count < 1000) { #run until winner or stalemate
  count <- count + 1    #to make sure the game doesn't go forever if neutral attacks
  #switch turns
  if (player == "team1") {
    player <- "team2"
    opponent <- "team1"
  } else if (player == "team2") {
    player <- "team1"
    opponent <- "team2"
  }
  
  if (interactive) {
    cat("\n")
    #choose available pokemon to fight
    print(paste0("It is ", player, "'s turn. Pokemon available:"))
    for (i in 1:length(get(player))) {
      if (get(player)[[i]]$hp > 0) {
        print(paste0(get(player)[[i]]$name, " HP: ", get(player)[[i]]$hp))
      }
    }
    chosenAttacker <- readline(prompt="Choose Pokemon from your team to be the attacker: ")
    if (!chosenAttacker %in% get(paste0(player, "Names")) || chosenAttacker %in% faintedPokemon) { #make sure selected pokemon is on teams and not fainted
      print("Invalid Pokemon! Try again.")
      chosenAttacker <- readline(prompt="Choose Pokemon from your team to be the attacker: ")
    }
    chosenAttacker <- get(player)[[chosenAttacker]]
    
    #choose opposing pokemon to fight
    print(paste0(opponent, "'s Pokemon available:"))
    for (i in 1:length(get(opponent))) { 
      if (get(opponent)[[i]]$hp > 0) {
        print(paste0(get(opponent)[[i]]$name, " HP: ", get(opponent)[[i]]$hp))
      }
    }
    chosenOpponent <- readline(prompt="Choose Pokemon from opposing team to be the target: ")
    if (!chosenOpponent %in% get(paste0(opponent, "Names")) || chosenOpponent %in% faintedPokemon) { #make sure selected pokemon is on teams and not fainted
      print("Invalid Pokemon! Try again.")
      chosenOpponent <- readline(prompt="Choose Pokemon from opposing team to be the target: ")
    }
    chosenOpponent <- get(opponent)[[chosenOpponent]]
    
  } else { #AUTOMATED
    cat("\n")
    #choose pokemon
    chosenAttacker <- get(player)[[sample(1:length(get(player)), 1)]]
    while(!chosenAttacker$name %in% get(paste0(player, "Names")) || chosenAttacker$name %in% faintedPokemon) { #make sure selected pokemon is on teams and not fainted
      chosenAttacker <- get(player)[[sample(1:length(get(player)), 1)]]
    }
    
    chosenOpponent <- get(opponent)[[sample(1:length(get(opponent)), 1)]]
    while(!chosenOpponent$name %in% get(paste0(opponent, "Names")) || chosenOpponent$name %in% faintedPokemon) { #make sure selected pokemon is on teams and not fainted
      chosenOpponent <- get(opponent)[[sample(1:length(get(opponent)), 1)]]
    }
  }
  
  #attack and deal damage!
  dmg <- dmgDealt(chosenAttacker, chosenOpponent)
  #update stats
  if (opponent == "team1") {
    team1[[chosenOpponent$name]]$hp <- (team1[[chosenOpponent$name]]$hp - dmg)
    team2[[chosenAttacker$name]][['offensive']] <- team2[[chosenAttacker$name]][['offensive']] + 1
    team1[[chosenOpponent$name]][['defensive']] <- team1[[chosenOpponent$name]][['defensive']] + 1
    team2[[chosenAttacker$name]][['damageDealt']] <- team2[[chosenAttacker$name]][['damageDealt']] + dmg
    team1[[chosenOpponent$name]][['damageRecieved']] <- team1[[chosenOpponent$name]][['damageRecieved']] + dmg
  } else if (opponent == "team2") {
    team2[[chosenOpponent$name]]$hp <- (team2[[chosenOpponent$name]]$hp - dmg)
    team1[[chosenAttacker$name]][['offensive']] <- team1[[chosenAttacker$name]][['offensive']] + 1
    team2[[chosenOpponent$name]][['defensive']] <- team2[[chosenOpponent$name]][['defensive']] + 1
    team1[[chosenAttacker$name]][['damageDealt']] <- team1[[chosenAttacker$name]][['damageDealt']] + dmg
    team2[[chosenOpponent$name]][['damageRecieved']] <- team2[[chosenOpponent$name]][['damageRecieved']] + dmg
  }
  
  #check if pokemon has fainted
  if (get(opponent)[[chosenOpponent$name]]$hp <= 0) {
    print(paste0(get(opponent)[[chosenOpponent$name]]$name, " has fainted!"))
    if (opponent == "team1") {
      faintedPokemon <- c(faintedPokemon, chosenOpponent$name)
      team1[[chosenOpponent$name]]$fainted <- TRUE
      team1Pokemon <- team1Pokemon - 1
    } else if (opponent == "team2") {
      faintedPokemon <- c(faintedPokemon, chosenOpponent$name)
      team2[[chosenOpponent$name]]$fainted <- TRUE
      team2Pokemon <- team2Pokemon - 1
    }
  } else { #report damage
    print(paste0(get(opponent)[[chosenOpponent$name]]$name, " has ", get(opponent)[[chosenOpponent$name]]$hp, " hp left."))
  }
}

#end code
if (team1Pokemon == 0) {
  print("Team 2 has won!")
  #return stats to output file
  team1Stats <- summarizeGameStats(team1, "team1Stats")
  team2Stats <- summarizeGameStats(team2, "team2Stats", winningTeam = TRUE)
} else if (team2Pokemon == 0) {
  print("Team 1 has won!")
  #return stats to output file
  team1Stats <- summarizeGameStats(team1, "team1Stats", winningTeam = TRUE)
  team2Stats <- summarizeGameStats(team2, "team2Stats")
} else {
  print("Tied game.")
  #return stats to output file
  team1Stats <- summarizeGameStats(team1, "team1Stats")
  team2Stats <- summarizeGameStats(team2, "team2Stats")
}

#compile summary of all stats
allStats <- Map(c, team1Stats, team2Stats)

#extra plot stats output
png(filename="finalPlots.png", height=1000, width=1000)
par(mfrow=c(2,1))

plot(allStats$DamageDealt, allStats$DamageRecieved, xlab = "Damage Dealt", ylab = "Damage Recieved")
text(allStats$DamageDealt, allStats$DamageRecieved, labels=allStats$PkmnNames)  #add name labels

plot(allStats$OffensiveMoves, allStats$DefensiveMoves, xlab = "Offensive Moves", ylab = "Defensive Moves")
text(allStats$OffensiveMoves, allStats$DefensiveMoves, labels=allStats$PkmnNames)   #add name labels

dev.off()
