# Questions:
#  - Why use the *absolute* deviation from cage mean body mass, as in figure 2?
#  - How would it look like if the signed deviation is used?
rm(list = ls())
par(mfrow = c(1,1))

# Big font for presentation
ggplot2::theme_set(ggplot2::theme_gray(base_size = 22))

#-------------------------------------------------------------------------------
# Functions
#-------------------------------------------------------------------------------
# Add history as one value
calc_history <- function(origin, transplant) {
  testthat::expect_true(origin == "Stream" || origin == "Lake")
  testthat::expect_true(transplant == "Stream" || transplant == "Lake")
  from <- ifelse(origin == "Stream", "s", "l")
  to <- ifelse(transplant == "Stream", "s", "l")
  return(paste0(from, to))
}

testthat::expect_equal(calc_history("Lake", "Lake"), "ll")
testthat::expect_equal(calc_history("Lake", "Stream"), "ls")
testthat::expect_equal(calc_history("Stream", "Lake"), "sl")
testthat::expect_equal(calc_history("Stream", "Stream"), "ss")

# Find out the item present least frequent
least_frequent <- function(items) {
  # Debugging
  if (length(items) != 3) { return(NA) }
  testthat::expect_true(length(items) == 3)
  # First is single
  if (items[1] != items[2] && items[1] != items[3]) {
    testthat::expect_true(items[2] == items[3])
    return(items[1])
  }
  # Second is single
  if (items[2] != items[1] && items[2] != items[3]) {
    testthat::expect_true(items[1] == items[3])
    return(items[2])
  }
  testthat::expect_true(items[1] == items[2])
  return(items[3])
}
testthat::expect_equal(least_frequent(c("a", "b", "b")), "a")
testthat::expect_equal(least_frequent(c("b", "a", "b")), "a")
testthat::expect_equal(least_frequent(c("b", "b", "a")), "a")

# Ploty survival per mass with a binomial fit
binomial_smooth <- function(...) {
  ggplot2::geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

#-------------------------------------------------------------------------------
# Start of program
#-------------------------------------------------------------------------------
testthat::expect_true(stringr::str_detect(getwd(), "Bolnick_and_Stutz_2017"))
traits_filename <- "Bolnick_traits.txt"

if (!file.exists(traits_filename)) {
  stop("File '", traits_filename,
    "' not found. Set the correct working directory, ",
    "e.g. '~/GitHubs/Bolnick_and_Stutz_2017'"
  )
}



pre_dat <- read.csv(traits_filename, sep = " ")
pre_dat$origin <- as.factor(pre_dat$origin) # RJCB
pre_dat$transplant <- as.factor(pre_dat$transplant) # RJCB

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

# ADDED: save to file for easier analysis
readr::write_csv(pre_dat, "data_with_abs_added_cols.csv") # Added by RJCB

png("fig2_reproduced.png") # Added by RJCB

par(mar = c(5,5,1,1))
plot(jitter(survived, 0.2) ~ cage_mass_mean_deviation_sd,
  pre_dat, 
  pch = 15+as.numeric(transplant), 
  col = 5-as.numeric(origin), 
  ylab = "survival", 
  xlab = "absolute deviation from cage mean body mass",
  cex.lab = 1.5
)

llfit <- glm(survived ~ cage_mass_mean_deviation_sd, pre_dat[pre_dat$origin == "Lake" & pre_dat$transplant == "Lake",], family = "binomial"); summary(llfit)
X <- pre_dat$cage_mass_mean_deviation_sd[pre_dat$origin == "Lake" & pre_dat$transplant == "Lake"]
xvals <- seq(min(X), max(X), by = 0.01)
yvals <- predict(llfit, newdata = data.frame(cage_mass_mean_deviation_sd = xvals), se.fit = T, "response")
lines(xvals, yvals$fit, col = "blue", lwd = 3)

lsfit <- glm(survived ~ cage_mass_mean_deviation_sd, pre_dat[pre_dat$origin == "Lake" & pre_dat$transplant == "Stream",], family = "binomial"); summary(lsfit)
X <- pre_dat$cage_mass_mean_deviation_sd[pre_dat$origin == "Lake" & pre_dat$transplant == "Stream"]
xvals <- seq(min(X), max(X), by = 0.01)
yvals <- predict(lsfit, newdata = data.frame(cage_mass_mean_deviation_sd = xvals), se.fit = T, "response")
lines(xvals, yvals$fit, col = "blue", lwd = 3, lty = 2)


slfit <- glm(survived ~ cage_mass_mean_deviation_sd, pre_dat[pre_dat$origin == "Stream" & pre_dat$transplant == "Lake",], family = "binomial"); summary(slfit)
X <- pre_dat$cage_mass_mean_deviation_sd[pre_dat$origin == "Stream" & pre_dat$transplant == "Lake"]
xvals <- seq(min(X), max(X), by = 0.01)
yvals <- predict(slfit, newdata = data.frame(cage_mass_mean_deviation_sd = xvals), se.fit = T, "response")
lines(xvals, yvals$fit, col = "dark green", lwd = 3, lty = 2)

ssfit <- glm(survived ~ cage_mass_mean_deviation_sd, pre_dat[pre_dat$origin == "Stream" & pre_dat$transplant == "Stream",], family = "binomial"); summary(ssfit)
X <- pre_dat$cage_mass_mean_deviation_sd[pre_dat$origin == "Stream" & pre_dat$transplant == "Stream"]
xvals <- seq(min(X), max(X), by = 0.01)
yvals <- predict(ssfit, newdata = data.frame(cage_mass_mean_deviation_sd = xvals), se.fit = T, "response")
lines(xvals, yvals$fit, col = "dark green", lwd = 3)

text(1,0.18, paste0("P = ", formatC(as.double(coef(summary(llfit))[,4][2]), digits = 3)), col = "blue", cex = 1.2)
text(1,0.65, paste0("P = ", formatC(as.double(coef(summary(lsfit))[,4][2]), digits = 3)), col = "blue", cex = 1.2)
text(1,0.84, paste0("P = ", formatC(as.double(coef(summary(slfit))[,4][2]), digits = 3)), col = "dark green", cex = 1.2)
text(1,0.9 , paste0("P = ", formatC(as.double(coef(summary(ssfit))[,4][2]), digits = 2)), col = "darkgreen", cex = 1.2)

dev.off()  # Added by RJCB

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
# pre_dat$cage_mass_mean_deviation_sd <- abs(pre_dat$pre_mass - pre_dat$cage_mass_mean) / pre_dat$cage_mass_stdev
# New without abs
pre_dat$cage_mass_mean_deviation_sd <-    (pre_dat$pre_mass - pre_dat$cage_mass_mean) / pre_dat$cage_mass_stdev

testthat::expect_equivalent(mean(pre_dat$cage_mass_mean_deviation_sd, na.rm = TRUE), 0.0)

# ADDED: save to file for easier analysis
readr::write_csv(pre_dat, "data_with_added_cols.csv") # Added by RJCB

png("fig2_reproduced_no_abs.png") # Added by RJCB

par(mar = c(5,5,1,1))
plot(jitter(survived, 0.2) ~ cage_mass_mean_deviation_sd, pre_dat, pch = 15+as.numeric(transplant), col = 5-as.numeric(origin), ylab = "survival", xlab = "deviation from cage mean body mass",cex.lab = 1.5)

llfit <- glm(survived ~ cage_mass_mean_deviation_sd, pre_dat[pre_dat$origin == "Lake" & pre_dat$transplant == "Lake",], family = "binomial")
X <- pre_dat$cage_mass_mean_deviation_sd[pre_dat$origin == "Lake" & pre_dat$transplant == "Lake"]
xvals <- seq(min(X), max(X), by = 0.01)
yvals <- predict(llfit, newdata = data.frame(cage_mass_mean_deviation_sd = xvals), se.fit = T, "response")
lines(xvals, yvals$fit, col = "blue", lwd = 3)

lsfit <- glm(survived ~ cage_mass_mean_deviation_sd, pre_dat[pre_dat$origin == "Lake" & pre_dat$transplant == "Stream",], family = "binomial")
X <- pre_dat$cage_mass_mean_deviation_sd[pre_dat$origin == "Lake" & pre_dat$transplant == "Stream"]
xvals <- seq(min(X), max(X), by = 0.01)
yvals <- predict(lsfit, newdata = data.frame(cage_mass_mean_deviation_sd = xvals), se.fit = T, "response")
lines(xvals, yvals$fit, col = "blue", lwd = 3, lty = 2)


slfit <- glm(survived ~ cage_mass_mean_deviation_sd, pre_dat[pre_dat$origin == "Stream" & pre_dat$transplant == "Lake",], family = "binomial")
X <- pre_dat$cage_mass_mean_deviation_sd[pre_dat$origin == "Stream" & pre_dat$transplant == "Lake"]
xvals <- seq(min(X), max(X), by = 0.01)
yvals <- predict(slfit, newdata = data.frame(cage_mass_mean_deviation_sd = xvals), se.fit = T, "response")
lines(xvals, yvals$fit, col = "dark green", lwd = 3, lty = 2)

ssfit <- glm(survived ~ cage_mass_mean_deviation_sd, pre_dat[pre_dat$origin == "Stream" & pre_dat$transplant == "Stream",], family = "binomial")
X <- pre_dat$cage_mass_mean_deviation_sd[pre_dat$origin == "Stream" & pre_dat$transplant == "Stream"]
xvals <- seq(min(X), max(X), by = 0.01)
yvals <- predict(ssfit, newdata = data.frame(cage_mass_mean_deviation_sd = xvals), se.fit = T, "response")
lines(xvals, yvals$fit, col = "dark green", lwd = 3)

text(1,0.15, paste0("P = ", formatC(as.double(coef(summary(llfit))[,4][2]), digits = 3)), col = "blue", cex = 1.2)
text(1,0.70, paste0("P = ", formatC(as.double(coef(summary(lsfit))[,4][2]), digits = 3)), col = "blue", cex = 1.2)
text(0.75,0.84, paste0("P = ", formatC(as.double(coef(summary(slfit))[,4][2]), digits = 3)), col = "dark green", cex = 1.2)
text(1,0.9 , paste0("P = ", formatC(as.double(coef(summary(ssfit))[,4][2]), digits = 2)), col = "darkgreen", cex = 1.2)

legend(
  x = 0.8, y = 0.6,
  c("LL","LS","SL","SS"),
  col = c("blue", "blue", "green", "green"),
  lwd = 3,
  lty = c("solid", "dashed", "dashed", "solid")
)

dev.off()  # Added by RJCB

# Don't keep these, as these are false
#text(1,0.18, "P = 0.502", col = "blue", cex = 1.2)
#text(1,0.65, "P = 0.048", col = "blue", cex = 1.2)
#text(1,0.84, "P = 0.101", col = "dark green", cex = 1.2)
#text(1,0.9, "P = 0.073", col = "darkgreen", cex = 1.2)

#-------------------------------------------------------------------------------
# Re-do same analysis with confidence intervals
#-------------------------------------------------------------------------------
names(pre_dat)
head(pre_dat)
survival_per_mass <- dplyr::select(pre_dat, c(enclosure, origin, transplant, survived, cage_mass_mean_deviation_sd))
# Remove Controls
survival_per_mass <- survival_per_mass[ survival_per_mass$transplant != "Control", ]

# Not all cages have three fishes?
table(survival_per_mass$enclosure)
table(table(survival_per_mass$enclosure))

# Create a category to determine the minority
`%>%` <- dplyr::`%>%`
names(survival_per_mass)
minorities <- survival_per_mass %>% dplyr::group_by(enclosure) %>%
       dplyr::summarise(minority = least_frequent(origin))
names(minorities)
survival_per_mass <-merge(x = survival_per_mass, y = minorities, by = "enclosure", all = TRUE)

survival_per_mass$history <- as.factor(mapply(calc_history, survival_per_mass$origin, survival_per_mass$transplant))

# Figure 2, non-absolute value
ggplot2::ggplot(
  survival_per_mass,
  ggplot2::aes(x = cage_mass_mean_deviation_sd, y = survived, color = history, linetype = history)
) +
  ggplot2::scale_color_manual(values = c("blue", "blue", "green", "green")) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm", formula = y ~ x + I(x^2), na.rm = TRUE, alpha = 0.1) +
  # ggplot2::geom_smooth(alpha = 0.25) +
  # binomial_smooth(alpha = 0.25, fullrange = FALSE) +
  ggplot2::scale_linetype_manual(values = c("solid", "dashed", "dashed", "solid")) +
  ggplot2::labs(title = "Survival for different body masses")
  #  ggplot2::theme(legend.text = ggplot2::element_text(size = 14))

ggplot2::ggsave("fig2_non_absolute_ggplot.png", width = 7, height = 7)
ggplot2::ggsave("fig2_non_absolute_ggplot.svg", width = 7, height = 7)


# General pattern
ggplot2::ggplot(
  survival_per_mass,
  ggplot2::aes(x = cage_mass_mean_deviation_sd, y = survived)
) +
  ggplot2::geom_point() +
  #ggplot2::geom_smooth(method = "lm", na.rm = TRUE, alpha = 0.1, color = "black") +
  ggplot2::geom_smooth(method = "lm", formula = y ~ x + I(x^2), na.rm = TRUE, alpha = 0.1, color = "blue") +
  ggplot2::geom_smooth(alpha = 0.25, color = "red") +
  ggplot2::labs(title = "Survival of normalized body masses")

ggplot2::ggsave("fig2_non_absolute_ggplot_all.svg", width = 7, height = 7)
ggplot2::ggsave("fig2_non_absolute_ggplot_all.png", width = 7, height = 7)

# General pattern per minority fish
ggplot2::ggplot(
  na.omit(survival_per_mass[ survival_per_mass$origin == survival_per_mass$minority, ]),
  ggplot2::aes(x = cage_mass_mean_deviation_sd, y = survived)
) +
  ggplot2::geom_jitter(height = 0.02) +
  #ggplot2::geom_smooth(method = "lm", na.rm = TRUE, alpha = 0.1) +
  ggplot2::geom_smooth(method = "lm", formula = y ~ x + I(x^2), na.rm = TRUE, alpha = 0.1) +
  #ggplot2::geom_smooth(alpha = 0.25) +
  # binomial_smooth(alpha = 0.25, fullrange = FALSE) +
  ggplot2::labs(title = "Fitnesses of fish that are the minority")

ggplot2::ggsave("minority_fish_fitnesses.svg")
ggplot2::ggsave("minority_fish_fitnesses.png")

ggplot2::ggplot(
  na.omit(survival_per_mass[ survival_per_mass$origin == survival_per_mass$minority, ]),
  ggplot2::aes(x = cage_mass_mean_deviation_sd, y = survived, color = minority)
) +
  ggplot2::geom_jitter(height = 0.02) +
  #ggplot2::geom_smooth(method = "lm", na.rm = TRUE, alpha = 0.1) +
  ggplot2::geom_smooth(method = "lm", formula = y ~ x + I(x^2), na.rm = TRUE, alpha = 0.1) +
  #ggplot2::geom_smooth(alpha = 0.25) +
  # binomial_smooth(alpha = 0.25, fullrange = FALSE) +
  ggplot2::labs(title = "Fitnesses of fish that are the minority, seperated by origin")

ggplot2::ggplot(
  na.omit(survival_per_mass[ survival_per_mass$origin != survival_per_mass$minority, ]),
  ggplot2::aes(x = cage_mass_mean_deviation_sd, y = survived, color = minority)
) +
  ggplot2::geom_jitter(height = 0.02) +
  #ggplot2::geom_smooth(method = "lm", na.rm = TRUE, alpha = 0.1) +
  ggplot2::geom_smooth(method = "lm", formula = y ~ x + I(x^2), na.rm = TRUE, alpha = 0.1) +
  #ggplot2::geom_smooth(alpha = 0.25) +
  # binomial_smooth(alpha = 0.25, fullrange = FALSE) +
  ggplot2::labs(title = "Fitnesses of fish that are the majority")



#-------------------------------------------------------------------------------
# Misc
#-------------------------------------------------------------------------------

ggplot2::ggplot(
    survival_per_mass,
    ggplot2::aes(cage_mass_mean_deviation_sd)
) + ggplot2::geom_histogram(alpha = 0.25, ggplot2::aes(y = ..density..),
    position = 'identity',
    binwidth = 0.1
) + ggplot2::geom_density() + ggplot2::labs(title = "Distribution of deviations")




# Did the individuals with an extreme mass have a higher survival?
# If yes, a parabola fit and a LOESS (locally weighted scatterplot smoothing) fit should show this
ggplot2::ggplot(
  data = pre_dat, ggplot2::aes(x = pre_mass, y = survived)
) +
  ggplot2::geom_jitter(width = 0.0, height = 0.02, na.rm = TRUE) +
  ggplot2::geom_smooth(method = "lm", color = "blue", formula = y ~ x + I(x^2), na.rm = TRUE, alpha = 0.1) +
  ggplot2::scale_y_continuous(limits = c(0.0, 1.0)) +
  ggplot2::labs(title = "Survival per mass")

ggplot2::ggsave("pre_mass_survival_1.png", width = 7, height = 7)
ggplot2::ggsave("pre_mass_survival_1.svg", width = 7, height = 7)
ggplot2::ggplot(
  data = pre_dat, ggplot2::aes(x = pre_mass, y = survived)
) +
  ggplot2::geom_jitter(width = 0.0, height = 0.02, na.rm = TRUE) +
  # ggplot2::geom_smooth(method = "lm", color = "blue", na.rm = TRUE, alpha = 0.1) +
  ggplot2::geom_smooth(method = "lm", color = "blue", formula = y ~ x + I(x^2), na.rm = TRUE, alpha = 0.1) +
  ggplot2::geom_smooth(method = "loess", color = "red", na.rm = TRUE, alpha = 0.1) +
  ggplot2::scale_y_continuous(limits = c(0.0, 1.0)) +
  ggplot2::labs(title = "Survival per mass")

ggplot2::ggsave("pre_mass_survival_2.png", width = 7, height = 7)
ggplot2::ggsave("pre_mass_survival_2.svg", width = 7, height = 7)

# What is the distribution of masses
# ggplot2::ggplot(
#   data = pre_dat, ggplot2::aes(x = pre_mass)
# ) + ggplot2::geom_histogram(binwidth = 0.1, na.rm = TRUE) + ggplot2::labs(title = "Pre-mass distribution")
#
# ggplot2::ggsave("pre_mass_distribution.png")

df <- data.frame(
  mass = c(pre_dat$pre_mass, pre_dat$post_mass),
  category = c(rep("pre", length(pre_dat$pre_mass)), rep("post", length(pre_dat$pre_mass))),
  stringsAsFactors = TRUE
)
ggplot2::ggplot(
    na.omit(df),
    ggplot2::aes(mass, fill = category)
) + ggplot2::geom_histogram(alpha = 0.25, ggplot2::aes(y = ..density..),
    position = 'identity',
    binwidth = 0.05
) + ggplot2::geom_density(alpha = 0.25)


# Did the individuals with an extreme mass have a higher survival?
sum_post_mass <- sum(pre_dat$post_mass, na.rm = TRUE)
sum_pre_mass <- sum(pre_dat$pre_mass, na.rm = TRUE)
# Ten percent decrease in body mass:
#  100 * (sum_post_mass - sum_pre_mass) / sum_pre_mass

df <- data.frame(
  normalized_mass = c(pre_dat$pre_mass / sum_pre_mass, pre_dat$post_mass / sum_post_mass),
  category = c(rep("pre", length(pre_dat$pre_mass)), rep("post", length(pre_dat$pre_mass))),
  stringsAsFactors = TRUE
)

ggplot2::ggplot(
    na.omit(df),
    ggplot2::aes(normalized_mass, fill = category)
) + ggplot2::geom_histogram(alpha = 0.25, ggplot2::aes(y = ..density..),
    position = 'identity',
    binwidth = 0.0005
) + ggplot2::geom_density(alpha = 0.25)

