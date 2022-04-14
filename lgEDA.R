library(tidyverse)

data <- read.csv("lgNepal.csv")
View(data)

#EDAs

filt_data <- data %>% 
  filter(name_of_province == "1",
         district_name_english == "BHOJPUR",
         palika_type == "Gaunpalika")
View(filt_data)


#total population for given data

total_pop <-sum(filt_data$total_population)
View(total_pop)
total_pop
