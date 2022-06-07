library(tidyverse)
library(ggthemes)
library(ggplot2)
library(gapminder)
library(pacman)
library(datasets)

#ANOVA, create a data set set to work with

view(iris)
names(iris)

df <- iris %>%
  filter(Sepal.Length & Species %in% c("setosa", "virginica", "versicolor")) %>%
  select(Sepal.Length, Species)

df %>%
  group_by(Species) %>%
  summarise(Mean_Sepal_Length = mean(Sepal.Length))

df %>%
  aov(Sepal.Length ~ Species, data =.) %>%
  summary()
















df1 <- gapminder %>%
  filter(year == 2007 & continent %in% c("Americas", "Europe", "Asia")) %>%
  select(continent, lifeExp)

df1 %>%
  group_by(continent) %>%
  summarise(Mean_life = mean(lifeExp))


#clear data
rm(list = ls())

# Clear packages
detach("package:datasets", unload = TRUE)

# Clear plots
dev.off()  # But only if there IS a plot

# Clear console
cat("\014")  # ctrl+L

# Clear mind :) 