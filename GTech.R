attach(gtech.df)
library(ggplot2)

gtech.df$program <- factor(gtech.df$program, labels=c('RQI','NanoJapan'))
gtech.df$time<- factor(gtech.df$time, labels=c('Pretest','Posttest'))



#Reverse Code the Items
keys <- c(rep(-1,116))
gt.r.df <- reverse.code(keys,gtech.df[,c(3:60,83:140)], mini=rep(1,116), maxi=rep(5,116))
vars <- names(gtech.df[,c(1,3:60,83:140)])
gt.recode.df <- data.frame(cbind(gtech.df[,1],gt.r.df))
names(gt.recode.df) <- vars

#Mean for each item
gt.df <- ddply(gt.recode.df, .(program), colwise(mean,na.rm=T, is.numeric))



gt.t <- t(gt.df)

GT.FINAL1 <- data.frame(gt.t)

names(GT.FINAL1) <- c("RQI", "NanoJapan")

g <- GT.FINAL1[-1,]

gt.final.df <- g

ggg <- data.frame(cbind(
gt.final.df[c(5,6,8,18:23),2],
gt.final.df[c(28,29,31,41:46),2],
gt.final.df[c(63,64,67,76:81),2],
gt.final.df[c(86,87,89,99:104),2],
gt.final.df[c(5,6,8,18:23),1],
gt.final.df[c(28,29,31,41:46),1],
gt.final.df[c(63,64,67,76:81),1],
gt.final.df[c(86,87,89,99:104),1]))

ggg.under <- data.frame(cbind(gt.final.df[c(51:52),2],
                              gt.final.df[c(57:58),2],
                              gt.final.df[c(109:110),2],
                              gt.final.df[c(115:116),2],
                              gt.final.df[c(51:52),1],
                              gt.final.df[c(57:58),1],
                              gt.final.df[c(109:110),1],
                              gt.final.df[c(115:116),1]))


#Skills and Abilities
gt.sa.df <- subset(gtech.df[,c(1,63:65,68:69,144:146,149:150)])



#Mean for each item
gt.sa.means.df <- ddply(gt.sa.df, .(program), colwise(mean,na.rm=T, is.numeric))

gt.sa.t <- data.frame(t(gt.sa.means.df))






gt.m.df <- melt(gt.df,id=c("program","time"))
write.csv2(gt.m.df,file="gtmelt.csv")

gt.sub.df <- subset(gt.df[,c("program","time","preimportanceability5","preimportanceability6","preimportanceability8","preimportanceability18","preimportanceability19","preimportanceability20","preimportanceability21","preimportanceability22","preimportanceability23","prepreparednessability5","prepreparednessability6","prepreparednessability8","prepreparednessability18","prepreparednessability19","prepreparednessability20","prepreparednessability21","prepreparednessability22","prepreparednessability23")])



gt.sub1.df <- subset(gt.sub.df,program=="NanoJapan")
gt.sub2.df <- subset(gt.sub.df,program=="RQI")

gt.sub12.df <- subset(gt.sub1.df[,c("program","time","preimportanceability5","prepreparednessability5","preimportanceability6","prepreparednessability6","preimportanceability8","prepreparednessability8","preimportanceability18","prepreparednessability18","preimportanceability19","prepreparednessability19","preimportanceability20","prepreparednessability20","preimportanceability21","prepreparednessability21","preimportanceability22","prepreparednessability22","preimportanceability23","prepreparednessability23")])


gt.sub22.df <- subset(gt.sub2.df[,c("program","time","preimportanceability5","prepreparednessability5","preimportanceability6","prepreparednessability6","preimportanceability8","prepreparednessability8","preimportanceability18","prepreparednessability18","preimportanceability19","prepreparednessability19","preimportanceability20","prepreparednessability20","preimportanceability21","prepreparednessability21","preimportanceability22","prepreparednessability22","preimportanceability23","prepreparednessability23")])

GT.FINAL <- data.frame(rbind(gt.sub12.df,gt.sub22.df))


gt.2.df <- data.frame(gt.m.df[c(17:24,29:32,69:92),])

write.csv2(gt.2.df,file="gt2df.csv")

gt.gg <- ggplot(gt.2.df,aes(factor(time), value, colour=program)) + geom_line(aes(group=program)) + geom_point() +facet_wrap(~ variable)
gt.gg
#############################################################################################
#
#############################################################################################


p.all.gg <- ggplot(gt.2.df, aes(time,value, fill=program)) + geom_bar(position="dodge", stat="identity") + facet_wrap(~variable)
