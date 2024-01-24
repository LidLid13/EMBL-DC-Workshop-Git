library(tidyverse)

surveys <- read_csv("data_raw/portal_data_joined.csv")
str(surveys)

# select column
select(surveys, plot_id, species_id, weight)

# exclude
select(surveys, -record_id, -species_id)

# filter only row with 1995 and M
filter(surveys, year == 1995, sex=="M")

# filter and select
surveys2 <- filter(surveys, weight < 5 )
surveys_sml <- select (surveys2, species_id, sex, weight)
surveys_sml

surveys_sml2 <- select(filter(surveys, weight<5), species_id, sex, weight)
surveys_sml2

# when having multiple operation use pipes to nest functions, it uses the results of previous for the next
surveys %>%
  filter(weight<5) %>% 
  select (species_id, sex, weight)

surveys %>%
  filter(year<1995) %>% 
  select (year, sex, weight)

# change 

surveys %>%
  mutate(weight_kg = weight/1000, weight_lb = weight_kg*2.2) %>%
  view()

# remove the NA
surveys %>%
filter(!is.na(weight)) %>%
view() 

# calculate avg weight for the 3 sex categories and plot them in a new small table
# split-apply-combine
surveys %>%
  group_by(sex) %>% 
  summarise(mean_weight= mean(weight, na.rm = T))

# let's remove na
surveys %>%
  filter(!is.na(sex)) %>%
  group_by(sex) %>% 
  summarise(mean_weight= mean(weight, na.rm = T)) %>% 
view()

# select input by 2 variables, calculate mean
surveys %>%
  group_by(sex, species_id) %>% 
  summarise(mean_weight= mean(weight, na.rm = T)) %>% 
  view()
  
# let's remove na in sex and weight
surveys %>%
  filter(!is.na(sex), !is.na(weight)) %>%
  group_by(sex) %>% 
  summarise(mean_weight= mean(weight, na.rm = T))

# calculate also min weight  
surveys %>%
  filter(!is.na(sex), !is.na(weight)) %>%
  group_by(sex, species_id) %>% 
  summarise(mean_weight= mean(weight, na.rm = T), min_weight = min(weight)) %>% 
view ()

# change order of row
arrange(desc (min_weight))

surveys %>%
  filter(!is.na(sex), !is.na(weight)) %>%
  group_by(sex, species_id) %>% 
  summarise(mean_weight= mean(weight, na.rm = T), min_weight = min(weight)) %>% 
  arrange(desc (min_weight)) %>% 
view ()

# look at the number of observations, in a new column n
surveys %>%
  count(sex, species) %>% 
  arrange(species, desc(n)) %>%
view()

# how many animals were caught in each plot_type
surveys %>%
  count(plot_type) %>% 
  view()

# use group by and summarize to find mean, min max hinfoot length of each species, number of obs? 
surveys %>%
  filter(!is.na (hindfoot_length)) %>%
  group_by(species_id) %>% 
  summarise(mean_hfl= mean(hindfoot_length), 
            min_hfl = min(hindfoot_length), 
            max_hfl=max(hindfoot_length), 
            n = n()) %>%
  view()

#this one is crap!
surveys %>%
  group_by(species_id) %>% 
  summarise(mean_hfl= mean(hindfoot_length, na.rm = T), min_hfl = min(hindfoot_length, na.rm = T), max_hfl=max(hindfoot_length, na.rm = T)) %>%
  view()

#what was the heaviest animal measured in each year? return year genus spcies weight
surveys %>%
  filter(!is.na (weight)) %>%
  group_by(year) %>% 
  filter (weight== max(weight)) %>% 
  select (year, genus, species_id, weight) %>% 
  arrange(year) %>% 
  view ()
# add unique to remove years in which 2 animal have the fsame weight

surveys %>%
  filter(!is.na (weight)) %>%
  group_by(year) %>% 
  filter (weight== max(weight)) %>% 
  select (year, genus, species_id, weight) %>% 
  arrange(year) %>% 
  unique() %>% 
  view ()

# how to rearrange table format. from long to wide. with pivot_wider you tell which column is gonna be the head colum 

surveys_gw <- surveys %>% 
  filter(!is.na (weight)) %>%
  group_by(plot_id, genus) %>% 
  summarize(mean_weight=mean(weight))
view(surveys_gw)

str(surveys_gw)

surveys_gw %>% 
  pivot_wider(names_from = genus, values_from = mean_weight)

#to fill empty values, add values_fill
surveys_wide <- surveys_gw %>% 
  pivot_wider(names_from = genus, values_from = mean_weight, values_fill = 0)
 
# from wide to long format
surveys_long <- surveys_wide %>% 
  pivot_longer(names_to = "genus", values_to = "mean_weight", cols = -plot_id) 

str (surveys_long)

# 3.10 
# 1
surveys_long <- surveys %>% 
  pivot_longer(names_to = "measurement", values_to = "values", cols = c(hindfoot_length, weight))
view(surveys_long)

# 2 NON FUNZIONA
surveys_long %>% 
  group_by(year, measurement, plot_type) %>% 
  summarise(mean_value = mean(values, na.rm=T)) %>% 
  pivot_wider(names_from = measurement, values_from = mean_value)

#3
surveys_complete <- surveys %>% 
  filter(!is.na(weight),
         !is.na (hindfoot_length),
         !is.na(sex))

write_csv(surveys_complete, file= "surveys_complete.csv")
         

