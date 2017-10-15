# Questions:
#  - Why use the *absolute* deviation from cage mean body mass, as in figure 2?
#  - How would it look like if the signed deviation is used?
rm(list = ls())
setwd("~/GitHubs/Bolnick_and_Stutz_2017")
traits_filename <- "Bolnick_traits.txt"

if (!file.exists(traits_filename)) {
  stop("File '", traits_filename,
    "' not found. Set the correct working directory")
}

pre_dat <- read.csv(traits_filename, sep = " ")

# Start of original code
cage_mass_sd <- tapply(pre_dat$pre_mass, pre_dat$enclosure, sd, na.rm = T)
cage_mass_mean <- tapply(pre_dat$pre_mass, pre_dat$enclosure, mean, na.rm = T)
pre_dat$cage_mass_stdev <- NA
pre_dat$cage_mass_mean <- NA
#Fixed
for(i in 1:nrow(pre_dat)){
#Original
#for(i in 1:length(pre_dat$majority)){
pre_dat$cage_mass_stdev[i] <- cage_mass_sd[names(cage_mass_sd) == pre_dat$enclosure[i]]
pre_dat$cage_mass_mean[i] <- cage_mass_mean[names(cage_mass_mean) == pre_dat$enclosure[i]]
}

pre_dat$cage_mass_mean_deviation_sd <- abs(pre_dat$pre_mass -
pre_dat$cage_mass_mean)/pre_dat$cage_mass_stdev

par(mar = c(5,5,1,1))
plot(jitter(survived, 0.2) ~ cage_mass_mean_deviation_sd, pre_dat, pch = 15+as.numeric(transplant), col = 5-as.numeric(origin), ylab = "survival", xlab = "absolute deviation from cage mean body mass",cex.lab = 1.5)

tempmod <- glm(survived ~ cage_mass_mean_deviation_sd, pre_dat[pre_dat$origin == "Lake" & pre_dat$transplant == "Lake",], family = "binomial"); summary(tempmod)
X <- pre_dat$cage_mass_mean_deviation_sd[pre_dat$origin == "Lake" & pre_dat$transplant == "Lake"]
xvals <- seq(min(X), max(X), by = 0.01)
yvals <- predict(tempmod, newdata = data.frame(cage_mass_mean_deviation_sd = xvals), se.fit = T, "response")
lines(xvals, yvals$fit, col = "blue", lwd = 3)

tempmod <- glm(survived ~ cage_mass_mean_deviation_sd, pre_dat[pre_dat$origin == "Lake" & pre_dat$transplant == "Stream",], family = "binomial"); summary(tempmod)
X <- pre_dat$cage_mass_mean_deviation_sd[pre_dat$origin == "Lake" & pre_dat$transplant == "Stream"]
xvals <- seq(min(X), max(X), by = 0.01)
yvals <- predict(tempmod, newdata = data.frame(cage_mass_mean_deviation_sd = xvals), se.fit = T, "response")
lines(xvals, yvals$fit, col = "blue", lwd = 3, lty = 2)


tempmod <- glm(survived ~ cage_mass_mean_deviation_sd, pre_dat[pre_dat$origin == "Stream" & pre_dat$transplant == "Lake",], family = "binomial"); summary(tempmod)
X <- pre_dat$cage_mass_mean_deviation_sd[pre_dat$origin == "Stream" & pre_dat$transplant == "Lake"]
xvals <- seq(min(X), max(X), by = 0.01)
yvals <- predict(tempmod, newdata = data.frame(cage_mass_mean_deviation_sd = xvals), se.fit = T, "response")
lines(xvals, yvals$fit, col = "dark green", lwd = 3, lty = 2)

tempmod <- glm(survived ~ cage_mass_mean_deviation_sd, pre_dat[pre_dat$origin == "Stream" & pre_dat$transplant == "Stream",], family = "binomial"); summary(tempmod)
X <- pre_dat$cage_mass_mean_deviation_sd[pre_dat$origin == "Stream" & pre_dat$transplant == "Stream"]
xvals <- seq(min(X), max(X), by = 0.01)
yvals <- predict(tempmod, newdata = data.frame(cage_mass_mean_deviation_sd = xvals), se.fit = T, "response")
lines(xvals, yvals$fit, col = "dark green", lwd = 3)
text(1,0.18, "P = 0.502", col = "blue", cex = 1.2)
text(1,0.65, "P = 0.048", col = "blue", cex = 1.2)
text(1,0.84, "P = 0.101", col = "dark green", cex = 1.2)
text(1,0.9, "P = 0.073", col = "darkgreen", cex = 1.2)



# Start of code using the non-absolute value
cage_mass_sd <- tapply(pre_dat$pre_mass, pre_dat$enclosure, sd, na.rm = T)
cage_mass_mean <- tapply(pre_dat$pre_mass, pre_dat$enclosure, mean, na.rm = T)
pre_dat$cage_mass_stdev <- NA
pre_dat$cage_mass_mean <- NA
#Fixed
for(i in 1:nrow(pre_dat)){
#Original
#for(i in 1:length(pre_dat$majority)){
pre_dat$cage_mass_stdev[i] <- cage_mass_sd[names(cage_mass_sd) == pre_dat$enclosure[i]]
pre_dat$cage_mass_mean[i] <- cage_mass_mean[names(cage_mass_mean) == pre_dat$enclosure[i]]
}

# Original with abs
# pre_dat$cage_mass_mean_deviation_sd <- abs(pre_dat$pre_mass - pre_dat$cage_mass_mean)/pre_dat$cage_mass_stdev
# New without abs
pre_dat$cage_mass_mean_deviation_sd <- pre_dat$pre_mass - pre_dat$cage_mass_mean /pre_dat$cage_mass_stdev

par(mar = c(5,5,1,1))
plot(jitter(survived, 0.2) ~ cage_mass_mean_deviation_sd, pre_dat, pch = 15+as.numeric(transplant), col = 5-as.numeric(origin), ylab = "survival", xlab = "deviation from cage mean body mass",cex.lab = 1.5)

tempmod <- glm(survived ~ cage_mass_mean_deviation_sd, pre_dat[pre_dat$origin == "Lake" & pre_dat$transplant == "Lake",], family = "binomial"); summary(tempmod)
X <- pre_dat$cage_mass_mean_deviation_sd[pre_dat$origin == "Lake" & pre_dat$transplant == "Lake"]
xvals <- seq(min(X), max(X), by = 0.01)
yvals <- predict(tempmod, newdata = data.frame(cage_mass_mean_deviation_sd = xvals), se.fit = T, "response")
lines(xvals, yvals$fit, col = "blue", lwd = 3)

tempmod <- glm(survived ~ cage_mass_mean_deviation_sd, pre_dat[pre_dat$origin == "Lake" & pre_dat$transplant == "Stream",], family = "binomial"); summary(tempmod)
X <- pre_dat$cage_mass_mean_deviation_sd[pre_dat$origin == "Lake" & pre_dat$transplant == "Stream"]
xvals <- seq(min(X), max(X), by = 0.01)
yvals <- predict(tempmod, newdata = data.frame(cage_mass_mean_deviation_sd = xvals), se.fit = T, "response")
lines(xvals, yvals$fit, col = "blue", lwd = 3, lty = 2)


tempmod <- glm(survived ~ cage_mass_mean_deviation_sd, pre_dat[pre_dat$origin == "Stream" & pre_dat$transplant == "Lake",], family = "binomial"); summary(tempmod)
X <- pre_dat$cage_mass_mean_deviation_sd[pre_dat$origin == "Stream" & pre_dat$transplant == "Lake"]
xvals <- seq(min(X), max(X), by = 0.01)
yvals <- predict(tempmod, newdata = data.frame(cage_mass_mean_deviation_sd = xvals), se.fit = T, "response")
lines(xvals, yvals$fit, col = "dark green", lwd = 3, lty = 2)

tempmod <- glm(survived ~ cage_mass_mean_deviation_sd, pre_dat[pre_dat$origin == "Stream" & pre_dat$transplant == "Stream",], family = "binomial"); summary(tempmod)
X <- pre_dat$cage_mass_mean_deviation_sd[pre_dat$origin == "Stream" & pre_dat$transplant == "Stream"]
xvals <- seq(min(X), max(X), by = 0.01)
yvals <- predict(tempmod, newdata = data.frame(cage_mass_mean_deviation_sd = xvals), se.fit = T, "response")
lines(xvals, yvals$fit, col = "dark green", lwd = 3)
text(1,0.18, "P = 0.502", col = "blue", cex = 1.2)
text(1,0.65, "P = 0.048", col = "blue", cex = 1.2)
text(1,0.84, "P = 0.101", col = "dark green", cex = 1.2)
text(1,0.9, "P = 0.073", col = "darkgreen", cex = 1.2)
