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

#-------------------------------------------------------------------------------
# Original code, added comments and did a trivial fix
#-------------------------------------------------------------------------------
# Calculate the mean and standard deviation of pre_mass per enclosure
# Original
# cage_mass_sd <- tapply(pre_dat$pre_mass, pre_dat$enclosure, sd, na.rm = T)
# cage_mass_mean <- tapply(pre_dat$pre_mass, pre_dat$enclosure, mean, na.rm = T)
# Modified: there are no NAs to remove here
cage_mass_sd <- tapply(pre_dat$pre_mass, pre_dat$enclosure, sd)
cage_mass_mean <- tapply(pre_dat$pre_mass, pre_dat$enclosure, mean)

pre_dat$cage_mass_stdev <- NA
pre_dat$cage_mass_mean <- NA
#Fixed
for(i in 1:nrow(pre_dat)) {
  #Original
  #for(i in 1:length(pre_dat$majority)){
  pre_dat$cage_mass_stdev[i] <- cage_mass_sd[names(cage_mass_sd) == pre_dat$enclosure[i]]
  pre_dat$cage_mass_mean[i] <- cage_mass_mean[names(cage_mass_mean) == pre_dat$enclosure[i]]
}

# Take the absolution normalized deviation
pre_dat$cage_mass_mean_deviation_sd <- abs(pre_dat$pre_mass - pre_dat$cage_mass_mean)/pre_dat$cage_mass_stdev

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



#-------------------------------------------------------------------------------
# Start of code using the non-absolute value
#-------------------------------------------------------------------------------
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
pre_dat$cage_mass_mean_deviation_sd <- (pre_dat$pre_mass - pre_dat$cage_mass_mean) / pre_dat$cage_mass_stdev

testthat::expect_equivalent(mean(pre_dat$cage_mass_mean_deviation_sd, na.rm = TRUE), 0.0)

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

legend(
  x = 0.8, y = 0.6,
  c("LL","LS","SL","SS"),
  col = c("blue", "blue", "green", "green"),
  lwd = 3,
  lty = c("solid", "dashed", "dashed", "solid")
)

# Don't keep these, as these are false
#text(1,0.18, "P = 0.502", col = "blue", cex = 1.2)
#text(1,0.65, "P = 0.048", col = "blue", cex = 1.2)
#text(1,0.84, "P = 0.101", col = "dark green", cex = 1.2)
#text(1,0.9, "P = 0.073", col = "darkgreen", cex = 1.2)

#-------------------------------------------------------------------------------
# Re-do same analysis with confidence intervals
#-------------------------------------------------------------------------------
survival_per_mass <- dplyr::select(pre_dat, c(origin, transplant, survived, cage_mass_mean_deviation_sd))

# Remove Controls
survival_per_mass <- survival_per_mass[ survival_per_mass$transplant != "Control", ]
calc_history <- function(origin, transplant) {
  testit::assert(origin == "Stream" || origin == "Lake")
  testit::assert(transplant == "Stream" || transplant == "Lake")
  from <- ifelse(origin == "Stream", "s", "l")
  to <- ifelse(transplant == "Stream", "s", "l")
  return(paste0(from, to))
}
#dplyr::tally(traits$origin)
survival_per_mass$history <- as.factor(mapply(calc_history, survival_per_mass$origin, survival_per_mass$transplant))


binomial_smooth <- function(...) {
  ggplot2::geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}
ggplot2::ggplot(
  survival_per_mass,
  ggplot2::aes(x = cage_mass_mean_deviation_sd, y = survived, color = history, linetype = history)
) +
    ggplot2::scale_color_manual(values = c("blue", "blue", "green", "green")) +
  ggplot2::geom_point() +
  binomial_smooth(alpha = 0.25) +
  ggplot2::scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid"))

ggplot2::ggsave("fig2_non_absolute_ggplot.png")
ggplot2::ggsave("fig2_non_absolute_ggplot.svg")


# Did the individuals with an extreme mass have a higher survival?
# If yes, a parabola fit and a LOESS (locally weighted scatterplot smoothing) fit should show this
ggplot2::ggplot(
  data = pre_dat, ggplot2::aes(x = pre_mass, y = survived)
) +
  ggplot2::geom_jitter(width = 0.0, height = 0.02, na.rm = TRUE) +
  ggplot2::geom_smooth(method = "lm", color = "blue", formula = y ~ x + I(x^2), na.rm = TRUE, alpha = 0.1) +
  ggplot2::scale_y_continuous(limits = c(0.0, 1.0)) +
  ggplot2::labs(title = "Survival per mass")

ggplot2::ggsave("pre_mass_survival_1.png")
ggplot2::ggsave("pre_mass_survival_1.svg")
ggplot2::ggplot(
  data = pre_dat, ggplot2::aes(x = pre_mass, y = survived)
) +
  ggplot2::geom_jitter(width = 0.0, height = 0.02, na.rm = TRUE) +
  # ggplot2::geom_smooth(method = "lm", color = "blue", na.rm = TRUE, alpha = 0.1) +
  ggplot2::geom_smooth(method = "lm", color = "blue", formula = y ~ x + I(x^2), na.rm = TRUE, alpha = 0.1) +
  ggplot2::geom_smooth(method = "loess", color = "red", na.rm = TRUE, alpha = 0.1) +
  ggplot2::scale_y_continuous(limits = c(0.0, 1.0)) +
  ggplot2::labs(title = "Survival per mass")

ggplot2::ggsave("pre_mass_survival_2.png")
ggplot2::ggsave("pre_mass_survival_2.svg")

# What is the distribution of masses
ggplot2::ggplot(
  data = pre_dat, ggplot2::aes(x = pre_mass)
) + ggplot2::geom_histogram(binwidth = 0.1, na.rm = TRUE) + ggplot2::labs(title = "Pre-mass distribution")

ggplot2::ggsave("pre_mass_distribution.png")


# Did the individuals with an extreme mass have a higher survival?
sum_post_mass <- sum(pre_dat$post_mass, na.rm = TRUE)
sum_pre_mass <- sum(pre_dat$pre_mass, na.rm = TRUE)

df <- data.frame(
  normalized_mass = c(pre_dat$pre_mass / sum_pre_mass, pre_dat$post_mass / sum_post_mass),
  category = c(rep("pre", length(pre_dat$pre_mass)), rep("post", length(pre_dat$pre_mass))),
  stringsAsFactors = TRUE
)

ggplot2::ggplot(
    df,
    ggplot2::aes(normalized_mass, fill = category)
) + ggplot2::geom_histogram(alpha = 0.25, ggplot2::aes(y = ..density..),
    position = 'identity',
    binwidth = 0.0005
) + ggplot2::geom_density(alpha = 0.25)
