library(ggplot2)

data("diamonds")

summary(diamonds)

names(diamonds)

dim(diamonds)

str(diamonds)

by(diamonds$price, diamonds$color, summary)

?diamonds

qplot(diamonds$price)

summary(diamonds$price)

dim(subset(diamonds, diamonds$price < 500))
dim(subset(diamonds, diamonds$price < 250))
dim(subset(diamonds, diamonds$price >= 15000))

qplot(x = price, data = diamonds, binwidth=20) +
  scale_x_continuous(limits=c(500, 1000), breaks=seq(500, 1000, 100))

qplot(x = price / carat, data = diamonds, binwidth=0.01) +
  facet_wrap(~cut, scales = 'free_y') +
  scale_x_continuous(trans = 'log10')

by(diamonds$price, diamonds$cut, summary)

ggplot(data = diamonds, aes(x = cut, y = price)) +
  geom_boxplot()

summary(subset(diamonds$price, diamonds$color == 'D'))

summary(subset(diamonds$price, diamonds$color == 'J'))

IQR(subset(diamonds$price, diamonds$color == 'D'))

IQR(subset(diamonds$price, diamonds$color == 'J'))

ggplot(data = diamonds, aes(x = color, y = price / carat)) +
  geom_boxplot()


ggplot(data = diamonds, aes(carat)) +
  geom_freqpoly(binwidth=0.1)

ggplot(data = diamonds, aes(carat)) +
  geom_freqpoly(binwidth=0.01)

ggplot(data = diamonds, aes(carat)) +
  geom_freqpoly(binwidth=0.01) +
  coord_cartesian(xlim = c(0.25, 0.5))

ggplot(data = diamonds, aes(carat)) +
  geom_freqpoly(binwidth=0.01) +
  coord_cartesian(xlim = c(1, 1.1))

#########################################


install.packages('dplyr')
install.packages('tidyr')
library(plyr)
library(dplyr)
library(tidyr)

aids_raw = read.csv('Indicator_Annual number of AIDS deaths.csv')

names(aids_raw)

aids = aids_raw  %>%
  select(-c(X2010, X2011)) %>%
  na.omit() %>% 
  gather(key = 'year', value= 'death', -one_of('Annual.number.of.AIDS.deaths'), na.rm = TRUE) %>% 
  rename(c('Annual.number.of.AIDS.deaths'='country'))

names(aids)

head(aids)

aids %>%
  group_by(year) %>%
  summarise(death_sum = sum(death))

aids %>%
  filter(country %in% c('France', 'Germany', 'Spain')) %>% 
  qplot(data = ., x = year, y = death, color = country)

aids %>%
  filter(country %in% c('South Africa', 'Nigeria')) %>% 
  qplot(data = ., x = year, y = death, color = country)

aids %>%
  qplot(data = ., x = year, y = death)

aids %>%
  ggplot(., aes(x = year, y = death)) + geom_bar(stat = 'identity')