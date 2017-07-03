library(ggplot2)

data("diamonds")

names(diamonds)

ggplot(diamonds, aes(x = price)) +
  facet_wrap(~ color) +
  geom_histogram(aes(fill = cut)) +
  scale_fill_brewer(type = 'qual')

ggplot(diamonds, aes(x= table, y = price)) +
  geom_point(aes(color = cut)) +
  scale_color_brewer(type = 'qual')

ggplot(diamonds, aes(x = table)) +
  xlim(50, 70) +
  facet_wrap(~ cut) +
  geom_histogram()

transform(diamonds, volume = x * y * z)

diamonds = transform(diamonds, volume = x * y * z)

ggplot(data = diamonds, aes(x = volume, y = price)) +
  scale_y_continuous(trans='log10') +
  xlim(0, quantile(diamonds$volume, 0.99)) +
  geom_point(aes(color = clarity), alpha = 0.1)



pf = read.csv('pseudo_facebook.tsv', sep = '\t')

names(pf)

pf = transform(pf, prop_initiated = friendships_initiated / friend_count)

pf = transform(pf, year_joined = floor(2014 - tenure / 365))
pf = transform(pf, year_joined.bucket = cut(year_joined, c(2004, 2009, 2011, 2012, 2014)))

ggplot(pf, aes(x = tenure, y = prop_initiated)) +
  geom_line(aes(color = year_joined.bucket), stat='summary', fun.y = median)

ggplot(pf, aes(x = tenure, y = prop_initiated)) +
  geom_smooth(aes(color = year_joined.bucket))

pf %>%
  filter(!is.na(prop_initiated)) %>% 
  group_by(year_joined.bucket) %>% 
  summarise(mean = mean(prop_initiated))


diamonds %>% str

ggplot(diamonds, aes(x = cut, y = price / carat)) +
  facet_wrap(~ clarity) +
  geom_jitter(aes(color = color), alpha = 0.1)

