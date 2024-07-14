library(tidyverse)
library(Cairo)

DATASETS <- "../../datasets/trump-data/"

column_rename <- function(df) {
  colnames(df) <- c("time", "trend")
  df
}

data_cleaning <- function(df, time_zone) {
  df %>%
    column_rename() %>%
    mutate(trend = as.numeric(gsub("<1", as.character(runif(1)), trend))) %>%
    mutate(date = substring(time, 1, 10), local_time = substring(time, 12, 19)) %>%
    mutate(time = as.POSIXct(paste0(date, " ", local_time), format="%Y-%m-%d %H:%M:%OS", tz=time_zone)) %>%
    select(time, trend)
}

read_all <- function(countries) {
  map_df(countries, 
         ~ read.table(paste0(DATASETS, .x, ".csv"), header=TRUE, sep=",", skip=2) %>%
           data_cleaning(time_zone = "America/New_York") %>%
           mutate(country = .x)
  ) %>%
    mutate(country = as.factor(country))
}

df <- read_all(c("USA", "UK", "Canada", "Australia", "New Zealand"))


CairoPNG("trump.png")
ggplot(df, aes(time, trend, color=country)) +
  geom_point(alpha = 0.6, size = 0.5) +
  geom_line() +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Interest in Donald Trump after the Assassination Attempt",
    subtitle = "Proxy: Web Searches, Time Zone: New York, Year: 2024",
    caption = "Source: Google Trends",
    x = "Time",
    y = "Trend"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10),
    legend.position = "bottom",
    legend.title = element_blank()
  )
dev.off()


