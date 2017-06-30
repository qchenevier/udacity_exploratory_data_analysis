library(ggplot2)
library(dplyr)

data("diamonds")

names(diamonds)

ggplot(data=diamonds, aes(x=price, y=x)) +
  ylim(0, 100) +
  scale_y_continuous(trans="log10") +
  geom_point(alpha=1/20)


ggplot(data=diamonds, aes(x=price, y=x)) +
  scale_x_continuous(trans='log10') +
  geom_point(alpha=1/20)

cor.test(diamonds$price, diamonds$x)
cor.test(diamonds$price, diamonds$y)
cor.test(diamonds$price, diamonds$z)

cor.test(log10(diamonds$price), diamonds$x)

ggplot(data=diamonds, aes(x=price, y=depth)) +
  geom_point(alpha=1/20)

ggplot(data=diamonds, aes(x=price, y=depth)) +
  geom_point(alpha=1/100)

cor.test(diamonds$price, diamonds$depth)

ggplot(data=diamonds, aes(x=price, y=carat)) +
  xlim(0, quantile(diamonds$price, 0.99)) +
  ylim(0, quantile(diamonds$carat, 0.99)) +
  geom_point(alpha=1/100)

diamonds = diamonds %>% 
  mutate(volume = x * y * z)


ggplot(data=diamonds[diamonds$volume > 0,], aes(x=price, y=volume)) +
  ylim(0, 800) +
  geom_point(alpha=1/10)

diamonds_filtered = diamonds %>% 
  filter(volume > 0) %>% 
  filter(volume < 800)

cor.test(diamonds_filtered$volume, diamonds_filtered$price)

ggplot(data=diamonds_filtered, aes(x=price, y=volume)) +
  geom_smooth(method='lm') +
  geom_point(alpha=1/100)

ggplot(data=subset(diamonds, volume > 0 & volume < 800), aes(x=price, y=volume)) +
  geom_smooth(method='lm') +
  geom_point(alpha=1/100)

diamondsByClarity = diamonds %>% 
  group_by(clarity) %>% 
  summarise(
    mean_price = mean(price),
    median_price = median(price),
    min_price = min(price),
    max_price = max(price),
    n = n())


diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity, mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color, mean_price = mean(price))

library(gridExtra)

grid.arrange(
  ggplot(data=diamonds_mp_by_color, aes(x=color, y=mean_price)) + geom_bar(stat='identity'),
  ggplot(data=diamonds_mp_by_clarity, aes(x=clarity, y=mean_price)) + geom_bar(stat='identity')
)

