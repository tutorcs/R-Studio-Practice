# Load packages
library(dplyr) # to use all dplyr functions such as group_by, summarise, aggregate, list, filter, select etc.
library(readxl) # to use read_excel function and read_csv function for importing the dataset
library(ggplot2) # to use ggplot commands for data visualisation
library(magrittr) # to use pipe in operator

# import the dataset
Movies <- read_excel(file.choose())

# Q1. Movies revenues by genres
revenue<- Movies %>% group_by(Main_Genre) %>% summarise(Revenue_by_Genre = sum(worldwide_gross, na.rm = TRUE))

# Q2. Movie Revenues(worlwide gross) by year
Movies_Year<- Movies %>% group_by(Movies$year) %>% summarise(Revenue_by_Year = sum(worldwide_gross))

# Q.2 Movie worldwide gross over years (through visualisation)
Movies %>% ggplot(aes(x= year, y= Movies$worldwide_gross))+
  geom_col(position = "stack", alpha = 1)+
  coord_flip()+
  theme_grey()+
  labs(x= "Years", y = "Sum of Worldwide gross", title = "Worldwide gross over years")
# Dont use dodge (in Q.No 2) just stack it to get the total yearly figures It should be 2012 and not 2009.
# See by both the ways you arrive on 2012 as the best year for movies
# To remove the scientific notations (in the visualisation) use the given command. this will be applicable to this complete R-Session.
options(scipen = 100)

# Additonal features related to Q.2 (Not in the question)
# We can filter and visualise only for a particular year
Movies_2012<- filter(Movies, year == 2012) # To filter and save the result in a new dataframe only for a particular year i.e 2012 
Movies_2012 %>% ggplot(aes(x= Movies_2012$studio, y= Movies_2012$worldwide_gross))+
  geom_col(position = "dodge", alpha = 1)+
  theme_grey()+
  labs(x= "Studio", y = "Sum of Worldwide gross", title = "Worldwide gross in 2012 accoss studio")
# Can you see which studio contributes to the highest business in 2012?

# Q.3 Which studio makes the highest grossing action movies?
# Using data visualisation
# 1st way
Movies_Action<- filter(Movies, Main_Genre == "Action") # We are filtering and saving it as a new dataframe only for action movies
Movies_Action %>% ggplot(aes(x= Movies_Action$studio, y= Movies_Action$worldwide_gross))+
  geom_col(position = "stack", alpha = 1)+
  theme_grey()+
  labs(x= "Studio", y = "Sum of Worldwide gross", title = "Worldwide gross in Action  movies accoss studio")

# 2nd Way by using facet wrap and creating facets for all genres
Movies %>% ggplot(aes(x= Movies$studio, y= Movies$worldwide_gross))+
  geom_col(position = "stack", alpha = 1)+
  theme_grey()+
  coord_flip()+
  facet_wrap(~Movies$Main_Genre)+
  labs(x= "Studio", y = "Sum of Worldwide gross", title = "Worldwide gross in Action  movies accoss studio")

# 3rd Way by using fill to color by studios
Movies %>% ggplot(aes(x= Movies$Main_Genre, y= Movies$worldwide_gross, fill = Movies$studio))+
  geom_col(position = "stack", alpha = .5)+
  theme_grey()+
  labs(x= "Studio", y = "Sum of Worldwide gross", title = "Worldwide gross in Action  movies accoss studio")

# 4th Way Using Dplyr (Group by and Summarise)
Highest_Grossing_Action_Movies<- Movies %>% group_by(Main_Genre, studio) %>% summarise(Revenue_by_Genre_studio = sum(worldwide_gross, na.rm = TRUE))
# 5th Way by using Dplyr (aggregate and list)
Highest_Grossing_Action_Movies2<- aggregate(Movies$worldwide_gross, list(Movies$Main_Genre, Movies$studio), FUN= sum)

