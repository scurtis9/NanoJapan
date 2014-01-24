library(psych)
library(plyr)
library(nlme)
library(ggplot2)

# Means for all questions by year and time ------------------------------------------------------
des.all.df <- ddply(alldata.df, .(program, time), colwise(mean, na.rm = T, is.numeric))

# Create a Summary Data Frame for Each Variable -------------------------------------------------

df.b <- ddply(alldata.df, .(program, time), summarize,
              mean = mean(gt_ability_prep_b, na.rm = T),
              sd = sd(gt_ability_prep_b, na.rm = T),
              length = sum(is.na(gt_ability_prep_b) == FALSE),
              se = sd / sqrt(length),
              semax = mean + 1.96*se,
              semin = mean - 1.96*se)

opar <- theme_update(panel.grid.major = theme_blank(),
                     panel.grid.minor = theme_blank(),
                     panel.background = theme_rect(colour = "black"))
gb <- ggplot(df.b, aes(x=time, y=mean, colour=program, group=program))
gb + geom_line(aes(linetype=program), size=.6) + 
  geom_point(aes(shape=program), size=3) + 
  geom_errorbar(aes(ymax=semax, ymin=semin), width=.1) +
  coord_cartesian(ylim=c(1,5))
theme_set(opar)

df.f <- ddply(alldata.df, .(program, time), summarize,
              mean = mean(gt_ability_prep_f, na.rm = T),
              sd = sd(gt_ability_prep_f, na.rm = T),
              length = sum(is.na(gt_ability_prep_f) == FALSE),
              se = sd / sqrt(length),
              semax = mean + 1.96*se,
              semin = mean - 1.96*se)

opar <- theme_update(panel.grid.major = theme_blank(),
                     panel.grid.minor = theme_blank(),
                     panel.background = theme_rect(colour = "black"))
gf <- ggplot(df.f, aes(x=time, y=mean, colour=program, group=program))
gf + geom_line(aes(linetype=program), size=.6) + 
  geom_point(aes(shape=program), size=3) + 
  geom_errorbar(aes(ymax=semax, ymin=semin), width=.1) +
  coord_cartesian(ylim=c(1,5))
theme_set(opar)

df.h <- ddply(alldata.df, .(program, time), summarize,
              mean = mean(gt_ability_prep_h, na.rm = T),
              sd = sd(gt_ability_prep_h, na.rm = T),
              length = sum(is.na(gt_ability_prep_h) == FALSE),
              se = sd / sqrt(length),
              semax = mean + 1.96*se,
              semin = mean - 1.96*se)

opar <- theme_update(panel.grid.major = theme_blank(),
                     panel.grid.minor = theme_blank(),
                     panel.background = theme_rect(colour = "black"))
gh <- ggplot(df.h, aes(x=time, y=mean, colour=program, group=program))
gh + geom_line(aes(linetype=program), size=.6) + 
  geom_point(aes(shape=program), size=3) + 
  geom_errorbar(aes(ymax=semax, ymin=semin), width=.1) +
  coord_cartesian(ylim=c(1,5))
theme_set(opar)

df.k <- ddply(alldata.df, .(program, time), summarize,
              mean = mean(gt_ability_prep_k, na.rm = T),
              sd = sd(gt_ability_prep_k, na.rm = T),
              length = sum(is.na(gt_ability_prep_k) == FALSE),
              se = sd / sqrt(length),
              semax = mean + 1.96*se,
              semin = mean - 1.96*se)

opar <- theme_update(panel.grid.major = theme_blank(),
                     panel.grid.minor = theme_blank(),
                     panel.background = theme_rect(colour = "black"))
gk <- ggplot(df.k, aes(x=time, y=mean, colour=program, group=program))
gk + geom_line(aes(linetype=program), size=.6) + 
  geom_point(aes(shape=program), size=3) + 
  geom_errorbar(aes(ymax=semax, ymin=semin), width=.1) +
  coord_cartesian(ylim=c(1,5))
theme_set(opar)

df.l <- ddply(alldata.df, .(program, time), summarize,
              mean = mean(gt_ability_prep_l, na.rm = T),
              sd = sd(gt_ability_prep_l, na.rm = T),
              length = sum(is.na(gt_ability_prep_l) == FALSE),
              se = sd / sqrt(length),
              semax = mean + 1.96*se,
              semin = mean - 1.96*se)

opar <- theme_update(panel.grid.major = theme_blank(),
                     panel.grid.minor = theme_blank(),
                     panel.background = theme_rect(colour = "black"))
gl <- ggplot(df.l, aes(x=time, y=mean, colour=program, group=program))
gl + geom_line(aes(linetype=program), size=.6) + 
  geom_point(aes(shape=program), size=3) + 
  geom_errorbar(aes(ymax=semax, ymin=semin), width=.1) +
  coord_cartesian(ylim=c(1,5))
theme_set(opar)

df.s <- ddply(alldata.df, .(program, time), summarize,
              mean = mean(gt_ability_prep_s, na.rm = T),
              sd = sd(gt_ability_prep_s, na.rm = T),
              length = sum(is.na(gt_ability_prep_s) == FALSE),
              se = sd / sqrt(length),
              semax = mean + 1.96*se,
              semin = mean - 1.96*se)

opar <- theme_update(panel.grid.major = theme_blank(),
                     panel.grid.minor = theme_blank(),
                     panel.background = theme_rect(colour = "black"))
gs <- ggplot(df.s, aes(x=time, y=mean, colour=program, group=program))
gs + geom_line(aes(linetype=program), size=.6) + 
  geom_point(aes(shape=program), size=3) + 
  geom_errorbar(aes(ymax=semax, ymin=semin), width=.1) +
  coord_cartesian(ylim=c(1,5))
theme_set(opar)

df.t <- ddply(alldata.df, .(program, time), summarize,
              mean = mean(gt_ability_prep_t, na.rm = T),
              sd = sd(gt_ability_prep_t, na.rm = T),
              length = sum(is.na(gt_ability_prep_t) == FALSE),
              se = sd / sqrt(length),
              semax = mean + 1.96*se,
              semin = mean - 1.96*se)

opar <- theme_update(panel.grid.major = theme_blank(),
                     panel.grid.minor = theme_blank(),
                     panel.background = theme_rect(colour = "black"))
gt <- ggplot(df.t, aes(x=time, y=mean, colour=program, group=program))
gt + geom_line(aes(linetype=program), size=.6) + 
  geom_point(aes(shape=program), size=3) + 
  geom_errorbar(aes(ymax=semax, ymin=semin), width=.1) +
  coord_cartesian(ylim=c(1,5))
theme_set(opar)

df.u <- ddply(alldata.df, .(program, time), summarize,
              mean = mean(gt_ability_prep_u, na.rm = T),
              sd = sd(gt_ability_prep_u, na.rm = T),
              length = sum(is.na(gt_ability_prep_u) == FALSE),
              se = sd / sqrt(length),
              semax = mean + 1.96*se,
              semin = mean - 1.96*se)

opar <- theme_update(panel.grid.major = theme_blank(),
                     panel.grid.minor = theme_blank(),
                     panel.background = theme_rect(colour = "black"))
gu <- ggplot(df.u, aes(x=time, y=mean, colour=program, group=program))
gu + geom_line(aes(linetype=program), size=.6) + 
  geom_point(aes(shape=program), size=3) + 
  geom_errorbar(aes(ymax=semax, ymin=semin), width=.1) +
  coord_cartesian(ylim=c(1,5))
theme_set(opar)

df.v <- ddply(alldata.df, .(program, time), summarize,
              mean = mean(gt_ability_prep_v, na.rm = T),
              sd = sd(gt_ability_prep_v, na.rm = T),
              length = sum(is.na(gt_ability_prep_v) == FALSE),
              se = sd / sqrt(length),
              semax = mean + 1.96*se,
              semin = mean - 1.96*se)

opar <- theme_update(panel.grid.major = theme_blank(),
                     panel.grid.minor = theme_blank(),
                     panel.background = theme_rect(colour = "black"))
gv <- ggplot(df.v, aes(x=time, y=mean, colour=program, group=program))
gv + geom_line(aes(linetype=program), size=.6) + 
  geom_point(aes(shape=program), size=3) + 
  geom_errorbar(aes(ymax=semax, ymin=semin), width=.1) +
  coord_cartesian(ylim=c(1,5))
theme_set(opar)

df.3.e <- ddply(alldata.df, .(program, time), summarize,
              mean = mean(gt_3_e, na.rm = T),
              sd = sd(gt_3_e, na.rm = T),
              length = sum(is.na(gt_3_e) == FALSE),
              se = sd / sqrt(length),
              semax = mean + 1.96*se,
              semin = mean - 1.96*se)

opar <- theme_update(panel.grid.major = theme_blank(),
                     panel.grid.minor = theme_blank(),
                     panel.background = theme_rect(colour = "black"))
g3.e <- ggplot(df.3.e, aes(x=time, y=mean, colour=program, group=program))
g3.e + geom_line(aes(linetype=program), size=.6) + 
  geom_point(aes(shape=program), size=3) + 
  geom_errorbar(aes(ymax=semax, ymin=semin), width=.1) +
  coord_cartesian(ylim=c(1,5))
theme_set(opar)

df.3.h <- ddply(alldata.df, .(program, time), summarize,
                mean = mean(gt_3_h, na.rm = T),
                sd = sd(gt_3_h, na.rm = T),
                length = sum(is.na(gt_3_h) == FALSE),
                se = sd / sqrt(length),
                semax = mean + 1.96*se,
                semin = mean - 1.96*se)

opar <- theme_update(panel.grid.major = theme_blank(),
                     panel.grid.minor = theme_blank(),
                     panel.background = theme_rect(colour = "black"))
g3.h <- ggplot(df.3.h, aes(x=time, y=mean, colour=program, group=program))
g3.h + geom_line(aes(linetype=program), size=.6) + 
  geom_point(aes(shape=program), size=3) + 
  geom_errorbar(aes(ymax=semax, ymin=semin), width=.1) +
  coord_cartesian(ylim=c(1,5))
theme_set(opar)

# Difference Scores -----------------------------------------------------------------------------
diff.scores.2013 <- des.all.df[4, 3:78] - des.all.df[3, 3:78]
diff.scores.2013 <- round(t(diff.scores.2013), 2)

diff.scores.2012 <- des.all.df[2, 3:78] - des.all.df[1, 3:78]
diff.scores.2012 <- round(t(diff.scores.2012), 2)

diff.scores <- data.frame(cbind(diff.scores.2012, diff.scores.2013))
names(diff.scores)[1] <- "2012"
names(diff.scores)[2] <- "2013"

# Merge Data for Tables -------------------------------------------------------------------------
des.table <- data.frame(t(des.all.df))

try.1 <- cbind(des.table[3:78,], diff.scores)
try.1 <- try.1[c(1,2,5,3,4,6)]

# Descriptives for 2013 -------------------------------------------------------------------------
alldata.2013.df <- alldata.df[c(1:12, 25:36),]
des.2013 <- describeBy(alldata.2013.df[, 6:81], group = alldata.2013.df$time, mat = T, digits = 2)

# Descriptives for 2012 -------------------------------------------------------------------------
alldata.2012.df <- alldata.df[c(13:24, 37:48),]
describeBy(alldata.2012.df[, 5:81], group = alldata.2012.df$time)

# Subset Differences scores ---------------------------------------------------------------------
gt.import.diff.2012 <- data.frame(diff.scores.2012[c(1:23), ])
gt.import.diff.2012[with(gt.import.diff.2012, order(gt.import.diff.2012)), ]

# Subset Data -----------------------------------------------------------------------------------
nanojapan.13.df <- subset(alldata.df,subset= program == "nanojapan" & year == 2013, select= c(4:82))
rqi.13.df <- subset(alldata.df,subset= program == "rqi" & year == 2013, select= c(4:82))

# ANOVA Importance ------------------------------------------------------------------------------
summary(gls(gt_ability_import_a ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_b ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_c ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_d ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_e ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_f ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_g ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_h ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_i ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_j ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_k ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_l ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_m ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_n ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_o ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_p ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_q ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_r ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_s ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_t ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_u ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_v ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_import_w ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

# ANOVA Preparedness ---------------------------------------------------------------------------
summary(gls(gt_ability_prep_a ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_b ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_c ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_d ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_e ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_f ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_g ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_h ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_i ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_j ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_k ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_l ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_m ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_n ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_o ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_p ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_q ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_r ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_s ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_t ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_u ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_v ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_ability_prep_w ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

# GLS 3 ---------------------------------------------------------------------------------------
summary(gls(gt_3_a ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_3_b ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_3_c ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_3_d ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_3_e ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_3_f ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_3_g ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_3_h ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_3_i ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_3_j ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_3_k ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_3_l ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_3_m ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_3_n ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

summary(gls(gt_3_o ~ program*time, data = alldata.df, cor = corCompSymm(,form = ~1 |last_name), na.action = na.omit))

# Self-Efficacy Scale -------------------------------------------------------------------------
selfeff.df <- subset.data.frame(alldata.df, time == 1, c(gt_3_a:gt_3_o))

selfeff.al <- alpha(selfeff.df)
selfeff.al$alpha.drop

vss(selfeff.df, n=5, rotate="varimax", fm="pc")
pc1 <- principal(selfeff.df, nfactors = 1, rotate = "varimax")
pc1
pc1$values
summary(pc1)


selfeff.df <- subset.data.frame(alldata.df, select=c(program, time, gt_3_a:gt_3_o))


selfeff.df$total <- rowMeans(selfeff.df[,3:17], na.rm = T)
