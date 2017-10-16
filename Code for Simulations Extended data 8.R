## Simulations for Extended Data Figure 8  in Bolnick and Stutz

# Set number of generations
generations <- 16



# Extended Data Figure 8
{
{
# three simulations with directional selection  
par(mfrow = c(3,3), mar = c(2.5,2.5,1.5,0.3), oma = c(1.5,2,0,0))
cex.lab.val <- 1.5

# Simulations with SYMMETRIC migration, directional selection, and with or without NFDS (solid vs dashed lines)

# Initialize population
Allele_A_freq <- matrix( nrow = generations, ncol = 2)
colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
start.freq <- c(1,0.0) # starting allele frequency
Allele_A_freq[1,] <- start.freq # starting allele frequency

# Set NFDS parameter
gamma <- -0.6
# Set migration rate
m <- 0.01
# Set fitness values based on empirical data in main text Figure 1
w_obs <- matrix(c(0.883,0.712,0.65,0.266), nrow = 2, byrow = T)

w <- t(t(w_obs)/apply(w_obs, 2, max))
#w <- w_obs/max(w_obs)
colnames(w) <- c("Habitat_A (stream)", "Habitat_B (lake)")
rownames(w) <- c("A (stream) natives", "B (lake) natives")

# Function to introduce NFDS into selection
adjust_w_freq <- function(Allele_A_freq_temp, w, gamma){
	freqs <- rbind(freqA = Allele_A_freq_temp, freqB = 1-Allele_A_freq_temp)
	w_adj <- w + gamma*(freqs-0.5)
	w_adj[w_adj < 0] <- 0
	w_adj <- t(t(w_adj)/apply(t(w_adj), 1, max))
	return(w_adj)
}


# Run simulations with NFDS
for(i in 2:generations){
	# Migration
	Allele_A_freq_temp <- Allele_A_freq[i-1,]*(1-m) + rev(Allele_A_freq[i-1,])*m
	# Selection
	w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma)
	Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
}
plot(Allele_A_freq[,1] ~ c(1:generations), type = "l", lwd = 4, col = "green", ylab = "" , xlab = "", ylim  = c(0,1), main = "a) directional selection", cex.lab = cex.lab.val)
lines(c(1:generations), Allele_A_freq[,2] , lwd = 4, col = "blue")

# Rerun simulations without NFDS, add to same plot
Allele_A_freq <- matrix( nrow = generations, ncol = 2)
colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
Allele_A_freq[1,] <- start.freq # starting allele frequency
for(i in 2:generations){
	# Migration
	Allele_A_freq_temp <- Allele_A_freq[i-1,]*(1-m) + rev(Allele_A_freq[i-1,])*m
	# Selection
	w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma = 0)
	Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
}
lines(c(1:generations), Allele_A_freq[,1] , lwd = 4, lty = 2, col = "green")
lines(c(1:generations), Allele_A_freq[,2] , lwd = 4, lty = 2, col = "blue")









###### Repeat above with slight modification
# Simulations with ASYMMETRIC migration, directional selection, and with or without NFDS (solid vs dashed lines)
# Reset allele frequencies
Allele_A_freq <- matrix( nrow = generations, ncol = 2)
colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
Allele_A_freq[1,] <- start.freq # starting allele frequency

# Initiate asymmetric selection
asymmetric_m <- 0
migration <- c(   m, m*asymmetric_m )


for(i in 2:generations){
  # Migration
  Allele_A_freq_temp <- Allele_A_freq[i-1,]*(1-migration) + rev(Allele_A_freq[i-1,])*migration
  # Selection
  w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma)
  Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
}
plot(Allele_A_freq[,1] ~ c(1:generations), type = "l", lwd = 4, col = "green", ylab = "" , xlab = "", ylim  = c(0,1), main = "b) directional selection & asymmetric migration", cex.lab = cex.lab.val)
lines(c(1:generations), Allele_A_freq[,2] , lwd = 4, col = "blue")

Allele_A_freq <- matrix( nrow = generations, ncol = 2)
colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
Allele_A_freq[1,] <- start.freq # starting allele frequency
for(i in 2:generations){
  # Migration
  Allele_A_freq_temp <- Allele_A_freq[i-1,]*(1-migration) + rev(Allele_A_freq[i-1,])*migration
  # Selection
  w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma = 0)
  Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
}
lines(c(1:generations), Allele_A_freq[,1] , lwd = 4, lty = 2, col = "green")
lines(c(1:generations), Allele_A_freq[,2] , lwd = 4, lty = 2, col = "blue")






###### Repeat above with slight modification
# Simulations with symmetric migration, directional selection, and biased migration
# with or without NFDS (solid vs dashed lines)
# assume allele A preferentially moves into habitat A, and B into habitat B
# To make this easy, let's assume that if you are in the wrong habitat you move with prob m. If you are in the correct habitat you always stay
Allele_A_freq <- matrix( nrow = generations, ncol = 2)
colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
start.freq <- c(0.8, 0.2)
Allele_A_freq[1,] <- start.freq # starting allele frequency

#m <-0.3
m <- 0.65
migration <- c( m, m )


# Introduce function to run biased migration
biased_migrate <- function(Allele_A_freq_now, mig){
  p1 <- Allele_A_freq_now[1]
  p2 <- Allele_A_freq_now[2]
  m12 <- mig[1]
  m21 <- mig[2]
  p1new <- (p1 + p2*m21)/(p1 + (1-p1)*(1-m12) + p2*m21) 
  p2new <- (p2*(1-m21) + 0*m12)/((1-p2) + p2*(1-m21)+ (1-p1)*m12)
  return(c(p1new, p2new))
}


# Simulate with NFDS
for(i in 2:generations){
  # Migration
  Allele_A_freq_temp <- biased_migrate(Allele_A_freq[i-1,],migration)
  # Selection
  w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma)
  Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
}
plot(Allele_A_freq[,1] ~ c(1:generations), type = "l", lwd = 4, col = "green", ylab = "" , xlab = "", ylim  = c(0,1), main = "c) directional selection & biased migration", cex.lab = cex.lab.val)
lines(c(1:generations), Allele_A_freq[,2] , lwd = 4, col = "blue")

# Add lines without NFDS
Allele_A_freq <- matrix( nrow = generations, ncol = 2)
colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
Allele_A_freq[1,] <- start.freq # starting allele frequency
gamma <- 0
for(i in 2:generations){
  # Migration
  Allele_A_freq_temp <- biased_migrate(Allele_A_freq[i-1,],migration)
  # Selection
  w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma)
  Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
}
lines(c(1:generations), Allele_A_freq[,1] , lwd = 4, lty = 2, col = "green")
lines(c(1:generations), Allele_A_freq[,2] , lwd = 4, lty = 2, col = "blue")


#### End three figures
}








###################   Next row of figures
###### Same as above but with equal initial allele frequency
{
  # three simulations with asymmetric selection  
  #par(mfrow = c(1,3), mar = c(3,3,2,0.1), oma = c(0,0,0,0))
  cex.lab.val <- 1.5
  
  # Simulations with symmetric migration, directional selection, and with or without NFDS (solid vs dashed lines)
  Allele_A_freq <- matrix( nrow = generations, ncol = 2)
  colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
  start.freq <- c(0.55, 0.45) # starting allele frequency
  Allele_A_freq[1,] <- start.freq # starting allele frequency
  
  gamma <- -0.6
  m <- 0.01
  
  w_obs <- matrix(c(0.883,0.712,0.65,0.266), nrow = 2, byrow = T)
  
  w <- t(t(w_obs)/apply(w_obs, 2, max))
  #w <- w_obs/max(w_obs)
  colnames(w) <- c("Habitat_A (stream)", "Habitat_B (lake)")
  rownames(w) <- c("A (stream) natives", "B (lake) natives")
  
  
  for(i in 2:generations){
    # Migration
    Allele_A_freq_temp <- Allele_A_freq[i-1,]*(1-m) + rev(Allele_A_freq[i-1,])*m
    # Selection
    w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma)
    Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
  }
  plot(Allele_A_freq[,1] ~ c(1:generations), type = "l", lwd = 4, col = "green", ylab = "" , xlab = "", ylim  = c(0,1), main = "d) directional selection", cex.lab = cex.lab.val)
  lines(c(1:generations), Allele_A_freq[,2] , lwd = 4, col = "blue")
  
  Allele_A_freq <- matrix( nrow = generations, ncol = 2)
  colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
  Allele_A_freq[1,] <- start.freq # starting allele frequency
  for(i in 2:generations){
    # Migration
    Allele_A_freq_temp <- Allele_A_freq[i-1,]*(1-m) + rev(Allele_A_freq[i-1,])*m
    # Selection
    w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma = 0)
    Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
  }
  lines(c(1:generations), Allele_A_freq[,1] , lwd = 4, lty = 2, col = "green")
  lines(c(1:generations), Allele_A_freq[,2] , lwd = 4, lty = 2, col = "blue")
  

  
  # Simulations with ASYMMETRIC migration, directional selection, and with or without NFDS (solid vs dashed lines)
  Allele_A_freq <- matrix( nrow = generations, ncol = 2)
  colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
  Allele_A_freq[1,] <- start.freq # starting allele frequency
  
  #m <-0.3
  asymmetric_m <- 0
  migration <- c(   m, m*asymmetric_m )
  
  
  for(i in 2:generations){
    # Migration
    Allele_A_freq_temp <- Allele_A_freq[i-1,]*(1-migration) + rev(Allele_A_freq[i-1,])*migration
    # Selection
    w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma)
    Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
  }
  plot(Allele_A_freq[,1] ~ c(1:generations), type = "l", lwd = 4, col = "green", ylab = "" , xlab = "", ylim  = c(0,1), main = "e) directional selection & asymmetric migration", cex.lab = cex.lab.val)
  lines(c(1:generations), Allele_A_freq[,2] , lwd = 4, col = "blue")
  
  Allele_A_freq <- matrix( nrow = generations, ncol = 2)
  colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
  Allele_A_freq[1,] <- start.freq # starting allele frequency
  for(i in 2:generations){
    # Migration
    Allele_A_freq_temp <- Allele_A_freq[i-1,]*(1-migration) + rev(Allele_A_freq[i-1,])*migration
    # Selection
    w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma = 0)
    Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
  }
  lines(c(1:generations), Allele_A_freq[,1] , lwd = 4, lty = 2, col = "green")
  lines(c(1:generations), Allele_A_freq[,2] , lwd = 4, lty = 2, col = "blue")
  
  
  
  
  
  
  
  # Simulations with symmetric migration, directional selection, and biased migration
  # with or without NFDS (solid vs dashed lines)
  # assume allele A preferentially moves into habitat A, and B into habitat B
  # To make this easy, let's assume that if you are in the wrong habitat you move with prob m. If you are in the correct habitat you always stay
  Allele_A_freq <- matrix( nrow = generations, ncol = 2)
  colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
  Allele_A_freq[1,] <- start.freq # starting allele frequency
  
  #m <-0.3
  m <- 0.65
  migration <- c( m, m )
    
  
  for(i in 2:generations){
    # Migration
    Allele_A_freq_temp <- biased_migrate(Allele_A_freq[i-1,],migration)
    # Selection
    w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma)
    Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
  }
  plot(Allele_A_freq[,1] ~ c(1:generations), type = "l", lwd = 4, col = "green", ylab = "" , xlab = "", ylim  = c(0,1), main = "f) directional selection & biased migration", cex.lab = cex.lab.val)
  lines(c(1:generations), Allele_A_freq[,2] , lwd = 4, col = "blue")
  
  Allele_A_freq <- matrix( nrow = generations, ncol = 2)
  colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
  Allele_A_freq[1,] <- start.freq # starting allele frequency
  gamma <- 0
  for(i in 2:generations){
    # Migration
    Allele_A_freq_temp <- biased_migrate(Allele_A_freq[i-1,],migration)
    # Selection
    w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma)
    Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
  }
  lines(c(1:generations), Allele_A_freq[,1] , lwd = 4, lty = 2, col = "green")
  lines(c(1:generations), Allele_A_freq[,2] , lwd = 4, lty = 2, col = "blue")
  
  
  #### End row of three figures
}


 



####################     Third row of figures
###### Same as above but with divergent selection

{
 # generations <- 40
  # three simulations with asymmetric selection  
 # par(mfrow = c(1,3), mar = c(3,3,2,0.1), oma = c(0,0,0,0))
  cex.lab.val <- 1.5
  
  # Simulations with symmetric migration, asymmetric selection, and with or without NFDS (solid vs dashed lines)
  Allele_A_freq <- matrix( nrow = generations, ncol = 2)
  colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
  start.freq <- c(0.5, 0.5) # starting allele frequency
  Allele_A_freq[1,] <- start.freq # starting allele frequency
  
  gamma <- -0.3
  m <- 0.1
  s <- 0.2
  w_obs <- matrix(c(1, 1-s, 1-s, 1), nrow = 2, byrow = T)
  
  w <- t(t(w_obs)/apply(w_obs, 2, max))
  #w <- w_obs/max(w_obs)
  colnames(w) <- c("Habitat_A (stream)", "Habitat_B (lake)")
  rownames(w) <- c("A (stream) natives", "B (lake) natives")
  

  for(i in 2:generations){
    # Migration
    Allele_A_freq_temp <- Allele_A_freq[i-1,]*(1-m) + rev(Allele_A_freq[i-1,])*m
    # Selection
    w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma)
    Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
  }
  plot(Allele_A_freq[,1] ~ c(1:generations), type = "l", lwd = 4, col = "green", ylab = "" , xlab = "", ylim  = c(0,1), main = "g) divergent selection", cex.lab = cex.lab.val)
  lines(c(1:generations), Allele_A_freq[,2] , lwd = 4, col = "blue")
  
  Allele_A_freq <- matrix( nrow = generations, ncol = 2)
  colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
  Allele_A_freq[1,] <- start.freq # starting allele frequency
  for(i in 2:generations){
    # Migration
    Allele_A_freq_temp <- Allele_A_freq[i-1,]*(1-m) + rev(Allele_A_freq[i-1,])*m
    # Selection
    w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma = 0)
    Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
  }
  lines(c(1:generations), Allele_A_freq[,1] , lwd = 4, lty = 2, col = "green")
  lines(c(1:generations), Allele_A_freq[,2] , lwd = 4, lty = 2, col = "blue")
  

  
  
  
  # Simulations with ASYMMETRIC migration, asymmetric selection, and with or without NFDS (solid vs dashed lines)
  Allele_A_freq <- matrix( nrow = generations, ncol = 2)
  colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
  Allele_A_freq[1,] <- start.freq # starting allele frequency
  
  #m <-0.3
  asymmetric_m <- 0
  migration <- c( m,  m*asymmetric_m)

  
  for(i in 2:generations){
    # Migration
    Allele_A_freq_temp <- Allele_A_freq[i-1,]*(1-migration) + rev(Allele_A_freq[i-1,])*migration
    # Selection
    w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma)
    Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
  }
  plot(Allele_A_freq[,1] ~ c(1:generations), type = "l", lwd = 4, col = "green", ylab = "" , xlab = "", ylim  = c(0,1), main = "h) divergent selection & asymmetric migration", cex.lab = cex.lab.val)
  lines(c(1:generations), Allele_A_freq[,2] , lwd = 4, col = "blue")
  
  Allele_A_freq <- matrix( nrow = generations, ncol = 2)
  colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
  Allele_A_freq[1,] <- start.freq # starting allele frequency
  for(i in 2:generations){
    # Migration
    Allele_A_freq_temp <- Allele_A_freq[i-1,]*(1-migration) + rev(Allele_A_freq[i-1,])*migration
    # Selection
    w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma = 0)
    Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
  }
  lines(c(1:generations), Allele_A_freq[,1] , lwd = 4, lty = 2, col = "green")
  lines(c(1:generations), Allele_A_freq[,2] , lwd = 4, lty = 2, col = "blue")
  
  
  
  
  
  
  
  # Simulations with symmetric migration, asymmetric selection, and biased migration
  # with or without NFDS (solid vs dashed lines)
  # assume allele A preferentially moves into habitat A, and B into habitat B
  # To make this easy, let's assume that if you are in the wrong habitat you move with prob m. If you are in the correct habitat you always stay
  Allele_A_freq <- matrix( nrow = generations, ncol = 2)
  colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
  #start.freq <- c(0.6, 0.4)
  Allele_A_freq[1,] <- start.freq # starting allele frequency
  
  #m <-0.3
  asymmetric_m <- 0
  migration <- c( m, m )
  

  for(i in 2:generations){
    # Migration
    Allele_A_freq_temp <- biased_migrate(Allele_A_freq[i-1,],migration)
    # Selection
    w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma)
    Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
  }
  plot(Allele_A_freq[,1] ~ c(1:generations), type = "l", lwd = 4, col = "green", ylab = "" , xlab = "", ylim  = c(0,1), main = "i) divergent selection & biased migration", cex.lab = cex.lab.val)
  lines(c(1:generations), Allele_A_freq[,2] , lwd = 4, col = "blue")
  
  Allele_A_freq <- matrix( nrow = generations, ncol = 2)
  colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
  Allele_A_freq[1,] <- start.freq # starting allele frequency
  gamma <- 0
  for(i in 2:generations){
    # Migration
    Allele_A_freq_temp <- biased_migrate(Allele_A_freq[i-1,],migration)
    # Selection
    w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma)
    Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
  }
  lines(c(1:generations), Allele_A_freq[,1] , lwd = 4, lty = 2, col = "green")
  lines(c(1:generations), Allele_A_freq[,2] , lwd = 4, lty = 2, col = "blue")
  
  
  #### End three figures
}

mtext("frequency of allele A", side = 2, outer = T, cex = 1.5)
mtext("generation", side = 1, outer = T, cex = 1.5)


}


##########################################################
##########################################################

#   End Extended Data Figure 8



















  