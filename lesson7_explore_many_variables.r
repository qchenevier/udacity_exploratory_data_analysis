library(dplyr)

pf <- read.delim('pseudo_facebook.tsv')


pf.fc_by_age_gender = pf %>% 
  filter(!is.na(gender)) %>% 
  group_by(age, gender) %>% 
  summarise(
    mean_friend_count = mean(friend_count),
    median_friend_count = median(friend_count),
    n = n()
  ) %>% 
  ungroup()

pf.fc_by_age_gender %>% names

library(ggplot2)
ggplot(data = pf.fc_by_age_gender, aes(x = age, y = mean_friend_count, color = gender)) +
  geom_line()

install.packages("reshape2")
library(reshape2)

pf.fc_by_age_gender.wide = dcast(
  pf.fc_by_age_gender,
  age ~ gender,
  value.var = 'median_friend_count',
)

pf.fc_by_age_gender.wide %>%
  mutate(ratio = female / male) %>% 
  ggplot(aes(x = age, y = ratio)) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = 2)

pf.fc_by_age_gender.wide %>%
  ggplot(aes(x = age, y = female / male)) +
  geom_line() +
  geom_hline(yintercept = 1, alpha = 0.2, linetype = 2)

pf$year_joined = ceiling(2014 - pf$tenure / 365)

pf$year_joined.bucket = cut(pf$year_joined, breaks = c(2004, 2009, 2011, 2012, 2014))

ggplot(subset(pf, !is.na(gender)), aes(x = age, y = friend_count)) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = median)

ggplot(subset(pf, !is.na(gender)), aes(x = age, y = friend_count)) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = median) +
  geom_line(stat = 'summary', fun.y = mean, linetype = 2)

pf2 = pf %>%
  filter(tenure > 0) %>%
  mutate(friending_rate = friend_count / tenure)

qplot(pf2$friending_rate, binwidth=0.01) + xlim(0, 2)

max(pf2$friending_rate)
median(pf2$friending_rate)
summary(pf2$friending_rate)

with(subset(pf, tenure > 0), summary(friend_count / tenure))

pf %>% names

pf %>%
  filter(tenure > 0) %>% 
  ggplot(aes(x = tenure, y = friendships_initiated / tenure)) +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', fun.y = mean)

pf %>%
  filter(tenure > 0) %>% 
  ggplot(aes(x = tenure, y = friendships_initiated / tenure)) +
  geom_smooth(aes(color = year_joined.bucket))

yo = read.csv('yogurt.csv') %>% mutate(id = factor(id))

yo %>% str

yo %>% 
  ggplot(aes(x = time)) +
  geom_histogram(binwidth = 1)


ggplot(yo, aes(x = time)) +
  geom_histogram(binwidth = 3)

ggplot(yo, aes(x = price)) +
  geom_histogram(binwidth = 1)

yo %>% names

yo$all.purchases = yo$strawberry +
  yo$blueberry +
  yo$pina.colada +
  yo$plain +
  yo$mixed.berry

yo = yo %>%
  mutate(all.purchases = strawberry + blueberry + pina.colada + plain + mixed.berry)

ggplot(yo, aes(x = time, y = price)) +
  geom_point(alpha = 0.05, aes(size = all.purchases))


set.seed(4230)
set.seed(4231)


sample.ids = sample(yo$id, 16)

ggplot(subset(yo, id %in% sample.ids) , aes(x = time, y = price)) +
  facet_wrap(~ id) +
  geom_point(aes(size = all.purchases), pch = 1) +
  geom_line()


install.packages("GGally")
library(GGally)


