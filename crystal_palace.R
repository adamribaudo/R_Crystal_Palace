#Global variables
playing <-  T
rows <-  10
cols <-  10
level <-  1
playerChar = "X"
crystalChar = "*"
enemyChar = "^"
player <- vector("integer")
crystal <- vector("integer")
enemies <- list()

resetPlayer <- function()
{
  return(c(as.integer(runif(1) * rows + 1), as.integer(runif(1) * cols + 1)))
}

resetCrystal <- function()
{
  crystal <-  c(as.integer(runif(1) * rows + 1), as.integer(runif(1) * cols + 1))
  #ensure player is not on top of crystal
  while(identical(player, crystal))crystal <-c(as.integer(runif(1) * rows + 1), as.integer(runif(1) * cols + 1))
  return(crystal)
}

resetEnemies <- function()
{
  #generate enemies based on level
  enemies <- list()
  for (i in 1:level) {
    enemies[[i]] <- c(as.integer(runif(1) * rows + 1), as.integer(runif(1) * cols + 1))
    while(identical(enemies[[i]], player) | identical(enemies[[i]], crystal))enemies[[i]] <- c(as.integer(runif(1) * rows + 1), as.integer(runif(1) * cols + 1))
  }
  return(enemies)
}

player <- resetPlayer()
crystal <- resetCrystal()
enemies <- resetEnemies()

while(playing)
{
  #set board
  board = matrix("", ncol=cols, nrow=rows)
  board[crystal[1], crystal[2]] = crystalChar
  board[player[1], player[2]] = playerChar
  for(i in 1:length(enemies))
  {
    board[enemies[[i]][1], enemies[[i]][2]] = enemyChar
  }
  
  #play game
  cat("\014") 
  print(paste("LEVEL: ", level) )
  print(board)
  print("Your player, X, must capture the crystal, *")
  print("Direction? U, D, L, R to move. T to teleport. Q to quit")
  input <- readline()
  if(input == "Q"){playing <- F;break}
  else if(input == "U"){if(player[1] > 1)player[1] <- as.integer(player[1] - 1)}
  else if(input == "D"){if(player[1] < rows)player[1] <- as.integer(player[1] + 1)}
  else if(input == "L"){if(player[2] > 1)player[2] <- as.integer(player[2] - 1)}
  else if(input == "R"){if(player[2] < cols)player[2] <- as.integer(player[2] + 1)}
  else if(input=="T"){player <- c(as.integer(runif(1) * rows + 1), as.integer(runif(1) * cols + 1))}
  
  #Move enemies
  for(i in 1:length(enemies))
  {
    if(enemies[[i]][1] < player[1])enemies[[i]][1] <- as.integer(enemies[[i]][1] + 1)
    else if(enemies[[i]][1] > player[1])enemies[[i]][1] <- as.integer(enemies[[i]][1] - 1)
    if(enemies[[i]][2] < player[2])enemies[[i]][2] <- as.integer(enemies[[i]][2] + 1)
    else if(enemies[[i]][2] > player[2])enemies[[i]][2] <- as.integer(enemies[[i]][2] - 1)
    if(identical(enemies[[i]], player)){
      playing <- F
      cat("\014")
      print(board)
      print("YOU LOSE!")
    }
  }
  
  #Check level complete
  if(identical(player, crystal)){
    level = level + 1
    player <- resetPlayer()
    crystal <- resetCrystal()
    enemies <- resetEnemies()
  }
}