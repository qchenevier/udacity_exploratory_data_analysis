pf = read.csv('pseudo_facebook.tsv', sep='\t')
install.packages('ggplot2')
library(ggplot2)

names(pf)

qplot(x=dob_day, data=pf) +
  scale_x_continuous(breaks=1:31)

qplot(x=dob_day, data=pf) +
  scale_x_continuous(breaks=1:31) +
  facet_wrap(~dob_month, ncol=3)

qplot(x=friend_count, data=pf,
      binwidth=25)

qplot(x=friend_count, data=pf,
      binwidth=25) +
  scale_x_continuous(limits=c(0, 1000), breaks=seq(0,1000,25))

qplot(x=friend_count, data=pf,
      binwidth=25) +
  scale_x_continuous(limits=c(0, 1000), breaks=seq(0,1000,25)) +
  facet_wrap(~gender)

qplot(x=friend_count, data=subset(pf, !is.na(gender)),
      binwidth=25) +
  scale_x_continuous(limits=c(0, 1000), breaks=seq(0,1000,25)) +
  facet_wrap(~gender)

table(pf$gender)
by(pf$friend_count, pf$gender, summary)

qplot(x=tenure/365, data=subset(pf, !is.na(gender)),
      binwidth=0.1, color=I('grey'), fill=I('#00AA00')) +
  scale_x_continuous(limits=c(0, 8), breaks=seq(0, 20, 1)) +
  facet_wrap(~gender)
