library(tidyverse)

DATASETS <- "../../datasets/trump-data/"

column_rename <- function(df){
	colnames(df) <- c("time", "trend")
	df
}

data_cleaning <- function(df, time_zone) {
	df %>% 
		column_rename %>%
		mutate( trend = as.numeric(gsub("<1", as.character(runif(1)), trend)) ) %>%
		mutate( date = substring(time, 1, 10), local_time = substring(time, 12, 19) ) %>%
		mutate( time = as.POSIXct(paste0(date, " ", local_time), format="%Y-%m-%d %H:%M:%OS", tz=time_zone) ) %>%
		select(time, trend)
}


read_all <- function(countries) {
	map_df(countries, 
		~ read.table(paste0(DATASETS, .x,".csv"), header=TRUE, sep=",", skip=2) %>% data_cleaning(time_zone = "America/New_York") %>%
			mutate(country = .x)
	) %>% 
	mutate(country = as.factor(country))
}

df <- read_all(c("USA", "UK", "Canada", "Australia", "New Zealand"))

ggplot(df, aes(time, trend, color=country)) +
	geom_point() + 
	geom_line() +
	labs(subtitle="Time Zone: New York") +
	theme_minimal()



