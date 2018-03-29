library(dplyr)
library(ggplot2)
activity<-read.csv("activity.csv", header=TRUE, sep=",")
by_date<-group_by(activity, date)
summ_steps<-summarize(by_date,
  sum=sum(steps, na.rm=TRUE)
  )
ggplot(summ_steps, aes(x = sum)) +
  geom_histogram(
    binwidth=3000, 
    color="grey30", 
    fill="white") +
  labs(x="Total steps per day") +
  geom_vline(xintercept=mean(sum), color="red")