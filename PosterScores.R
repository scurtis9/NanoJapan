des.mat <- describeBy(poster.scores.df,group=poster.scores.df$program,mat=T,digits=2)

library(plyr)

#Reverse Code the Items
keys <- c(-1,-1,-1,-1,-1,-1)
ps.r.df <- reverse.code(keys,poster.scores.df[,2:7], mini=rep(1,6), maxi=rep(5,6))
vars <- names(poster.scores.df)
ps.recode.df <- data.frame(cbind(poster.scores.df[,1],ps.r.df))
names(ps.recode.df) <- vars

ps.recode.df$program <- factor(ps.recode.df$program,levels=c(2,1),labels=c("RQI","NanoJapan"))

#Mean for each dimenson by group
ps.df <- ddply(ps.recode.df, "program", summarize, perf.dim.1=mean(perf.dim.1),perf.dim.2=mean(perf.dim.2),perf.dim.3=mean(perf.dim.3),perf.dim.4=mean(perf.dim.4,na.rm=T),perf.dim.5=mean(perf.dim.5),perf.dim.6=mean(perf.dim.6))

#Restructure data for Graphing
molten <- melt(ps.df,id="program")

ps7.gg <- ggplot(molten,aes(variable,value,group=program)) + geom_bar(aes(fill=program),stat="identity", position="dodge")



ps7.gg + labs(title="Mean Scores on Performance Dimensions by Program",x="Performance Dimensions")

write.csv2(molten,file="molten.csv")


stargazer(ps.df, summary=F, digits=2)
