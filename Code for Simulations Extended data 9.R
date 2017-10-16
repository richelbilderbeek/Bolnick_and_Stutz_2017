## Simulations for Extended Data Figure 9 in Bolnick and Stutz


##########################################################
##########################################################
#      Extended Data Figure 9


{
  par(mfrow = c(1,2), mar = c(3,4.5,0.25, 0.25), oma = c(2,0,0,0))
 
####
#  Panel A, with divergent selection
 
  # iterate through levels of gamma and record equilibrium divergence after 100 generations
gammas <- seq(0, -1, by = -0.01)  # range of gamma values for NFDS being considered
generations <- 100
delta_ps <- c()  # record ending allele frequencies

# iterate through gamma values, obtain ending allele frequency difference
for(gamma_i in gammas){  
  Allele_A_freq <- matrix( nrow = generations, ncol = 2)
  colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
  start.freq <- c(1,0.0) # starting allele frequency
  Allele_A_freq[1,] <- start.freq # starting allele frequency
  
  gamma <- gamma_i
  m <- 0.01
  
  w <- matrix(c(1,0.8,0.9,1), nrow = 2, byrow = F)

  colnames(w) <- c("Habitat_A (stream)", "Habitat_B (lake)")
  rownames(w) <- c("A (stream) natives", "B (lake) natives")
  
  for(i in 2:generations){
    # Migration
    Allele_A_freq_temp <- Allele_A_freq[i-1,]*(1-m) + rev(Allele_A_freq[i-1,])*m
    # Selection
    w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma_i)
    Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
  }
  delta_ps <- c(delta_ps, Allele_A_freq[generations,1] - Allele_A_freq[generations,2])
}

plot(delta_ps ~ gammas, type = "l", lwd = 3, xlab = "", ylab = "allele frequency difference between habitats", cex.lab = 1.5)
text(-0.95, 0.85, "a", cex = 1.5)




###
# Panel B, directional selection case

# iterate through levels of gamma and record equilibrium divergence 
gammas <- seq(0, -1, by = -0.01)
generations <- 100
delta_ps <- c()

for(gamma_i in gammas){  
  # Simulations with symmetric migration, asymmetric selection, and with or without NFDS (solid vs dashed lines)
  Allele_A_freq <- matrix( nrow = generations, ncol = 2)
  colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
  start.freq <- c(1,0.0) # starting allele frequency
  Allele_A_freq[1,] <- start.freq # starting allele frequency
  
  gamma <- gamma_i
  m <- 0.01
  
  w <- matrix(c(1,0.8,1,0.9), nrow = 2, byrow = F)
  
  colnames(w) <- c("Habitat_A (stream)", "Habitat_B (lake)")
  rownames(w) <- c("A (stream) natives", "B (lake) natives")
  
  for(i in 2:generations){
    # Migration
    Allele_A_freq_temp <- Allele_A_freq[i-1,]*(1-m) + rev(Allele_A_freq[i-1,])*m
    # Selection
    w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma_i)
    Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
  }
  delta_ps <- c(delta_ps, Allele_A_freq[generations,1] - Allele_A_freq[generations,2])
}
plot(delta_ps ~ gammas, type = "l", lwd = 3, xlab = "", ylab = "", cex.lab = 1.5)

mtext("strength of frequency-dependent selection", side = 1, outer = 2, cex = 1.5)
text(-0.95, 0.16, "b", cex = 1.5)

}






  