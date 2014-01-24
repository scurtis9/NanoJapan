library(psych)
library(ggplot2)
library(reshape2)
library(plyr)

sumstats <- describeBy(idi.df[,4:5],group=idi.df$program)

#Delete Missing Cases
idi12.df <- subset(idi.df[-c(12,13,20),])
#Mean scores by group
idi.sum.df <- ddply(idi12.df,"program", summarize, pretest=mean(pretest), posttest=mean(posttest))
idi.sum.df <- melt(idi.sum.df, id="program")

idi <- ggplot(idi.sum.df,aes(factor(variable), value, colour=program)) + geom_line(aes(group=program)) + geom_point()
idi

idi1.df <- data.frame(idi.df[,3:5])


idi.1.df <- melt(idi1.df, id="program")

write.csv2(idi.1.df,file="idi1.csv")
######################################################
# IDI score distribution
######################################################

i2 <- ggplot(idi.df,aes(factor(program), pretest))
i2 + geom_boxplot(aes(fill=program)) + geom_jitter()

i3 <- ggplot(idi.df,aes(factor(program), posttest))
i3 + geom_boxplot(aes(fill=program)) + geom_jitter()

i4 <- ggplot(idi.1.df, aes(factor(program), y=value, group=program)) + geom_boxplot(aes(fill=program)) + facet_wrap(~variable) + geom_jitter()
i4

sum.idi <- describeBy(idi12.df[,4:5],group=idi12.df$program,mat=T,digits=2)
idi.t <- t(sum.idi)
