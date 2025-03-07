library(dplyr)

mtcars %>%
  group_by(am) %>%
  summarize(average_mpg = mean(mpg))
