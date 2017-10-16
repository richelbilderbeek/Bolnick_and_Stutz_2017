#### Code for simulations for Extended Data 1 of Bolnick and Stutz

library(fields)

# Define function to adjust divergent selection based on allele frequencies. This is just phenomenological, not driven by explicit ecological dynamics
adjust_w_freq <- function(Allele_A_freq_temp, w, gamma){
	freqs <- rbind(freqA = Allele_A_freq_temp, freqB = 1-Allele_A_freq_temp)
	w_adj <- w + gamma*(freqs-0.5)
	w_adj[w_adj < 0] <- 0
	w_adj <- t(t(w_adj)/apply(t(w_adj), 1, max))
	return(w_adj)
}


###########
# Panel A; single simulation example comparing with versus without NFDS
generations <- 50

# Define initial genotype frequencies
par(mfrow = c(1,1))
Allele_A_freq <- matrix( nrow = generations, ncol = 2)
colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
Allele_A_freq[1,] <- c(0.5, 0.5) # starting allele frequency

# parameter values
gamma <- -0.5
m <- 0.01
s <- 0.2
w_A <- c(1, 1-s)
w_B <- c(1-s, 1)
w <- rbind(w_A, w_B)
colnames(w) <- c("Habitat_A", "Habitat_B")

# Simulate with NFDS
for(i in 2:generations){
	# Migration
	Allele_A_freq_temp <- Allele_A_freq[i-1,]*(1-m) + rev(Allele_A_freq[i-1,])*m
	# Selection
	w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma)
	Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
}
par(mar = c(5,5,2,1))
plot(Allele_A_freq[,1] ~ c(1:generations), type = "l", lwd = 4, col = "green", ylab = "frequency of allele A" , xlab = "generation", cex.lab = 1.4, ylim  = c(0,1), main = "m = 0.01, s = 0.2")
lines(c(1:generations), Allele_A_freq[,2] , lwd = 4, col = "blue")

# Add simulation without NFDS
Allele_A_freq <- matrix( nrow = generations, ncol = 2)
colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
Allele_A_freq[1,] <- c(0.5, 0.5) # starting allele frequency
gamma <- 0
for(i in 2:generations){
	# Migration
	Allele_A_freq_temp <- Allele_A_freq[i-1,]*(1-m) + rev(Allele_A_freq[i-1,])*m
	# Selection
	w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma)
	Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
}
lines(c(1:generations), Allele_A_freq[,1] , lwd = 4, lty = 2, col = "green")
lines(c(1:generations), Allele_A_freq[,2] , lwd = 4, lty = 2, col = "blue")
text(1.2,0.99, "a", cex = 2.5)
######################
#   End Panel A





#####################
#   Panels B-D
#   Find equilibrium allele frequency differences for many combinations of m, gamma, s
# define parameter space to search
mvals <- seq(0,0.5, by = 0.025)
gammavals <- seq(-1,0, by = 0.025)
svals <- seq(0,0.5, by = 0.01) 

# matrix to record results
records <- matrix(nrow = length(mvals)*length(gammavals)*length(svals), ncol = 4)

#iterate through parameter space
counter <- 1
for(m in mvals){
	for(gamma in gammavals){
		for(s in svals){
			# Define fitness matrix for this simulation iteration
			w_A <- c(1, 1-s)
			w_B <- c(1-s, 1)
			w <- rbind(w_A, w_B)
			colnames(w) <- c("Habitat_A", "Habitat_B")
			# Initiate population
			Allele_A_freq <- matrix( nrow = generations, ncol = 2)
			colnames(Allele_A_freq) <- c("Habitat_A", "Habitat_B")
			Allele_A_freq[1,] <- c(0.9, 0.1) # starting allele frequency
			# Iterate through generations
			for(i in 2:generations){
				# Migration
				Allele_A_freq_temp <- Allele_A_freq[i-1,]*(1-m) + rev(Allele_A_freq[i-1,])*m
				# Selection
				w_freq <- adjust_w_freq(Allele_A_freq_temp, w, gamma)
				Allele_A_freq[i,] <- Allele_A_freq_temp* w_freq[1,] / (Allele_A_freq_temp* w_freq[1,]+(1-Allele_A_freq_temp)*w_freq[2,])
				}
			divergence <- Allele_A_freq[generations,1]-Allele_A_freq[generations,2]
			records[counter,] <- c(m, gamma, s, divergence)	
			counter <- counter + 1
		}
	}
	print(m)
}
colnames(records) <- c("m", "gamma", "s", "divergence")
records <- as.data.frame(records)


# Plot
library(fields)
theta.x <- 40   # or try 225
phi.x <- 10   # or try 30
par(mfrow = c(1,3), mar = c(2,1,1.5,1.5), oma = c(0,3,0,0))
records_subset<- records[records$s == 0.05,]
xvals <- unique(records_subset$m)
yvals <- unique(records_subset$gamma)
zvals <- matrix(records_subset$divergence,  nrow = length(xvals), ncol = length(yvals), byrow = T)
drape.color( zvals, zlim =c(0,1))-> zcol
persp(xvals, yvals, zvals, ticktype = "detailed", theta = theta.x , phi = phi.x , main = "b)  s = 0.05", zlim = c(0,1), col = zcol$color.index, xlab = "migration rate, m", ylab = "frequency-dependence", zlab = "", cex.lab = 1.5, cex.main = 1.5)


records_subset<- records[records$s == 0.25,]
xvals <- unique(records_subset$m)
yvals <- unique(records_subset$gamma)
zvals <- matrix(records_subset$divergence,  nrow = length(xvals), ncol = length(yvals), byrow = T)
drape.color( zvals, zlim =c(0,1))-> zcol
persp(xvals, yvals, zvals, ticktype = "detailed",  theta = theta.x , phi = phi.x,main = "c)  s = 0.25",zlim = c(0,1), col = zcol$color.index, xlab = "migration rate, m", ylab = "frequency-dependence", zlab = "", cex.lab = 1.5, cex.main = 1.5)


records_subset<- records[records$s == 0.5,]
xvals <- unique(records_subset$m)
yvals <- unique(records_subset$gamma)
zvals <- matrix(records_subset$divergence,  nrow = length(xvals), ncol = length(yvals), byrow = T)
drape.color( zvals, zlim =c(0,1))-> zcol
persp(xvals, yvals, zvals, ticktype = "detailed",  theta = theta.x , phi = phi.x ,main = "d)  s = 0.5",zlim = c(0,1), col = zcol$color.index, xlab = "migration rate, m", ylab = "frequency-dependence", zlab = "", cex.lab = 1.5, cex.main = 1.5)

mtext("allele frequency difference", side = 2, outer = T, line = 1)

######
#  End Figure ED1









