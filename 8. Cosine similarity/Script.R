required_packages <- c("magrittr", "tm", "mlr", "e1071", "dplyr", "kernlab", "lubridate", "dtplyr",
                       "readr", "ggplot2", "tidytext", "stringr", "tidyr", "scales", "broom",
                       "SnowballC", "wordcloud", "reshape2", "RTextTools", "hunspell")

raw.reviews <- read.csv2("Hotel_Reviews.csv", header = TRUE, sep =  ",")

