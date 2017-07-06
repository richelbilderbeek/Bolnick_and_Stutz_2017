# Questions:
#  - Why use the *absolute* deviation from cage mean body mass, as in figure 2?
#  - How would it look like if the signed deviation is used?
rm(list = ls())

traits_filename <- "Bolnick_traits.txt"

if (!file.exists(traits_filename)) {
  stop("File '", traits_filename,
    "' not found. Set the correct working directory")
}

traits <- read.csv(traits_filename, sep = " ")
ncol(traits)
nrow(traits)
names(traits)
head(traits)

calc_history <- function(origin, transplant) {
  from <- ifelse(origin == "Stream", "s", "l")
  to <- ifelse(transplant == "Stream", "s", "l")
  return(paste0(from, to))
}
traits$history <- rep(NA, nrow(traits))
#dplyr::tally(traits$origin)
traits$history <- as.factor(mapply(calc_history, traits$origin, traits$transplant))

# Calculate mean body size per enclosure
library(dplyr)
enclosure <- traits %>% group_by(enclosure) %>%
       summarise(mean_pre=mean(pre_mass), mean_post=mean(post_mass))

names(traits)
fig2 <- merge(
  x = subset(traits, select = c("enclosure", "origin", "transplant", "history", "pre_mass", "post_mass", "survived")),
  y = enclosure, by = "enclosure", all = TRUE)
fig2$delta_pre <- fig2$pre_mass - fig2$mean_pre
fig2$delta_post <- fig2$post_mass - fig2$mean_post
fig2$survived <- as.double(fig2$survived)

names(fig2)
head(fig2)

fig2
sum(is.na(fig2$delta_pre))

## Use the deviation of body weight from average at the *beginning* of the experiment

# Use real value
ggplot2::ggplot(
  data = na.omit(subset(fig2, select = c("delta_pre", "survived"))),
  ggplot2::aes(x = delta_pre, y = survived)
) + ggplot2::geom_point() + ggplot2::geom_smooth(method = "glm") +
    ggplot2::scale_x_continuous("Deviation from cage mean body mass (negative if smaller than average)") +
    ggplot2::ggtitle("The effect of having a lower or higher pre-mass on survival")

ggplot2::ggplot(
  data = na.omit(subset(fig2, select = c("delta_pre", "survived", "history"))),
  ggplot2::aes(x = delta_pre, y = survived, color = history)
) + ggplot2::geom_point() + ggplot2::geom_smooth(method = "glm") +
    ggplot2::scale_x_continuous("Deviation from cage mean body mass (negative if smaller than average)") +
    ggplot2::ggtitle("The effect of having a lower or higher pre-mass on survival")

# Use absolute value
ggplot2::ggplot(
  data = na.omit(subset(fig2, select = c("delta_pre", "survived"))),
  ggplot2::aes(x = abs(delta_pre), y = survived)
) + ggplot2::geom_point() + ggplot2::geom_smooth(method = "glm") +
    ggplot2::scale_x_continuous("Absolute deviation from cage mean body mass") +
    ggplot2::ggtitle("The effect of having a lower or higher pre-mass on survival")

ggplot2::ggplot(
  data = na.omit(subset(fig2, select = c("delta_pre", "survived", "history"))),
  ggplot2::aes(x = abs(delta_pre), y = survived, color = history)
) + ggplot2::geom_point() +
    #ggplot2::geom_smooth() +
    #ggplot2::geom_smooth(method = "lm") +
    ggplot2::geom_smooth(method = "glm") +
    ggplot2::scale_x_continuous("Absolute deviation from cage mean body mass") +
    ggplot2::ggtitle("The effect of having a lower or higher pre-mass on survival")


fig2_no_outliers <- filter(fig2, abs(delta_pre) < 1.2)
ggplot2::ggplot(
  data = na.omit(subset(fig2_no_outliers, select = c("delta_pre", "survived", "history"))),
  ggplot2::aes(x = abs(delta_pre), y = survived, color = history)
) + ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "glm") +
    ggplot2::scale_x_continuous("Absolute deviation from cage mean body mass") +
    ggplot2::ggtitle("The effect of having a lower or higher pre-mass on survival,\nwith outliers removed")

## Use the deviation of body weight from average at the *end* of the experiment

# Use real value
ggplot2::ggplot(
  data = na.omit(subset(fig2, select = c("delta_post", "survived"))),
  ggplot2::aes(x = delta_post, y = survived)
) + ggplot2::geom_point() + ggplot2::geom_smooth(method = "glm") +
    ggplot2::scale_x_continuous("Deviation from cage mean body mass (negative if smaller than average)") +
    ggplot2::ggtitle("The effect of having a lower or higher post-mass on survival")

ggplot2::ggplot(
  data = na.omit(subset(fig2, select = c("delta_post", "survived", "history"))),
  ggplot2::aes(x = delta_post, y = survived, color = history)
) + ggplot2::geom_point() + ggplot2::geom_smooth(method = "glm") +
    ggplot2::scale_x_continuous("Deviation from cage mean body mass (negative if smaller than average)") +
    ggplot2::ggtitle("The effect of having a lower or higher post-mass on survival")

# Use absolute value
ggplot2::ggplot(
  data = na.omit(subset(fig2, select = c("delta_post", "survived"))),
  ggplot2::aes(x = abs(delta_post), y = survived)
) + ggplot2::geom_point() + ggplot2::geom_smooth(method = "glm") +
    ggplot2::scale_x_continuous("Absolute deviation from cage mean body mass") +
    ggplot2::ggtitle("The effect of having a lower or higher post-mass on survival")

ggplot2::ggplot(
  data = na.omit(subset(fig2, select = c("delta_post", "survived", "history"))),
  ggplot2::aes(x = abs(delta_post), y = survived, color = history)
) + ggplot2::geom_point() +
    #ggplot2::geom_smooth() +
    #ggplot2::geom_smooth(method = "lm") +
    ggplot2::geom_smooth(method = "glm") +
    ggplot2::scale_x_continuous("Absolute deviation from cage mean body mass") +
    ggplot2::ggtitle("The effect of having a lower or higher post-mass on survival")
