library(ggplot2)
library(plyr)

foo <- read.delim("~/CurrentLx/OldNorse/whether/whetherif.ymebWithIDs.cod.ooo",header=F,sep=":")
colnames(foo) <- c("WQ","Disj","Year","textid")

whether.data <- subset(foo,WQ != "z" & Disj != "z" & Year != "z" & Year != "0")

library(gdata)

whether.data <- droplevels(whether.data)

whether.data$Year <- as.numeric(as.character(whether.data$Year))

whether.data$WQ <- as.numeric(as.character(whether.data$WQ))

whether.fit <- glm(WQ~Disj*Year, family = binomial, data=whether.data)
summary(whether.fit)
anova(whether.fit, test = "Chisq")

#Doing a mixed effects model now

library(lme4)

#Zing the numeric predictors so the mixed effects model doesnt barf so much:
whether.data$zDate <- scale(whether.data$Year, center=TRUE, scale=TRUE)

whether.fit <- glmer(WQ~(1|textid)+Disj*zDate, family = binomial, data=whether.data)
whether.fit2 <- glmer(WQ~(1|textid)+Disj+zDate, family = binomial, data=whether.data)
whether.fit3 <- glmer(WQ~(1|textid)+zDate, family = binomial, data=whether.data)

summary(whether.fit)
anova(whether.fit2, whether.fit, test = "Chisq")
anova(whether.fit3, whether.fit2, test = "Chisq")

#Trying the same thing with a dataset from half the time period

whether.data.halftime <- subset(whether.data, Year <= 1500)

whether.data.halftime <- droplevels(whether.data.halftime)

whether.data.halftime$zDate <- scale(whether.data.halftime$Year, center=TRUE, scale=TRUE)

whether.half.fit <- glmer(WQ~(1|textid)+Disj*zDate, family = binomial, data=whether.data.halftime)
whether.half.fit2 <- glmer(WQ~(1|textid)+Disj+zDate, family = binomial, data=whether.data.halftime)

summary(whether.half.fit)
anova(whether.half.fit2, whether.half.fit, test = "Chisq")






whether.table <- table(whether.data$Year,whether.data$WQ)

whether.data$Time <- floor(whether.data$Year/50)*50

whether.table <- table(whether.data$Time,whether.data$WQ)

plot.data <- ddply(whether.data, .(Time, Disj),summarize, whet = mean(WQ, na.rm = T), n = sum(!is.na(WQ)))

#pdf("whetherif_Eng_model.pdf")

library(RColorBrewer)
#I had to get rid of: "+ scale_size_area(to= c(0,12)) +" because it stopped working with the new version of ggplot2. This function call had to do with the dot size for the n-dots.

p <- ggplot(whether.data, aes(Year,WQ,color=Disj)) + scale_y_continuous(name = "Probability of Whether", breaks=seq(0,1,by=0.1), labels=c("If",seq(0.1,0.9,by = 0.1),"Whether") ) + scale_x_continuous(name = "\nTime") + stat_sum(aes(size=..n.., alpha=.5)) + scale_size_area(max_size=12) + stat_smooth(method="glm", family ="binomial",fullrange=F) + scale_alpha_continuous(guide="none", limits = c(0,.7)) + scale_color_brewer(palette = "Set1")

ggsave(p, file = "whetherifEngmodel.pdf", width = 8, height = 5)

#dev.off()

#pdf("whetherif_Eng.pdf")

#ggplot(aes(x = Time, y = whet, color = Disj, group = Disj), data = plot.data) + geom_line() + geom_point(aes(size = n)) + scale_y_continuous(name = "Proportion of Whether") + scale_x_continuous(name = "\nCentury") + scale_color_brewer(pal = "Set1")

whether.data$Time2 <- floor(whether.data$Year/100)*100

plot.data <- ddply(whether.data, .(Time2, Disj),summarize, whet = mean(WQ, na.rm = T), n = sum(!is.na(WQ)))

p <- ggplot(aes(x = Time2, y = whet, color = Disj, group = Disj), data = plot.data) + geom_line() + geom_point(aes(size = n)) + scale_y_continuous(name = "\nProportion of Whether", limits = c(0,1)) + scale_x_continuous(name = "\nCentury") + scale_color_brewer(palette = "Set1")

ggsave(p, file = "whetherifEng.pdf", width = 8, height = 5)

#dev.off()

#pdf("whetherifEngSimple.pdf")

plot.data <- ddply(whether.data, .(Time2),summarize, whet = mean(WQ, na.rm = T), n = sum(!is.na(WQ)))

p <- ggplot(aes(x = Time2, y = whet), data = plot.data) + geom_line() + geom_point(aes(size = n)) + scale_y_continuous(name = "\nProportion of Whether", limits = c(0,1)) + scale_x_continuous(name = "\nCentury") + scale_color_brewer(palette = "Set1")

ggsave(p, file = "whetherifEngSimple.pdf", width = 8, height = 5)

##Create a binary numeric variable for Disj

whether.data$DisjNum <- ifelse(whether.data$Disj == "disj", 1, 0)
whether.data$DisjNum <- as.numeric(as.character(whether.data$DisjNum))

whether.data$WQchar <- ifelse(whether.data$WQ == 1, c("whether"), c("if"))

### Plotting with Disj on y axis instead of WQ

plot.data <- ddply(whether.data, .(Time2, WQchar),summarize, whet = mean(DisjNum, na.rm = T), n = sum(!is.na(DisjNum)))

p <- ggplot(aes(x = Time2, y = whet, color = WQchar, group = WQchar), data = plot.data) + stat_smooth() + geom_point(aes(size = n)) + scale_y_continuous(name = "\nProportion with Disjunction", limits = c(0,1)) + scale_x_continuous(name = "\nCentury") + scale_color_brewer(palette = "Set1")

##Same thing but without binning; Note: updated this code on 9 Nov 2016 with geom_smooth() -- you should do this for the other plots too

p <- ggplot(whether.data, aes(Year,DisjNum,color=WQchar)) + scale_y_continuous(name = "Proportion of Disjunction", breaks=seq(0,1,by=0.1), labels=c("Simple",seq(0.1,0.9,by = 0.1),"Disjunction") ) + scale_x_continuous(name = "\nYear") + stat_sum(aes(size=..n.., alpha=.5)) + scale_size_area(max_size=12) + geom_smooth(alpha = 0.2, method="loess",fullrange=T) + scale_alpha_continuous(guide="none", limits = c(0,.7)) + scale_color_brewer(palette = "Set1") + theme_bw() + theme(panel.border = element_blank())

ggsave(p, file = "~/tyneside/articles/stableVarTalks/whetherifEngDisjByYear.pdf", width = 8, height = 5)

#different x, but also without binning 

p <- ggplot(whether.data, aes(Year,WQ,color=Disj)) + scale_y_continuous(name = "Proportion of Whether", breaks=seq(0,1,by=0.1), labels=c("If",seq(0.1,0.9,by = 0.1),"Whether") ) + scale_x_continuous(name = "\nYear") + stat_sum(aes(size=..n.., alpha=.5)) + scale_size_area(max_size=12) + geom_smooth(alpha = 0.2, method="loess",fullrange=T) + scale_alpha_continuous(guide="none", limits = c(0,.7)) + scale_color_brewer(palette = "Dark2") + theme_bw() + theme(panel.border = element_blank())

ggsave(p, file = "~/tyneside/articles/stableVarTalks/whetherifEngWQByYearUnbinned.pdf", width = 8, height = 5)


# Same as last two but with cubic splines. Note: updated this code on 9 Nov 2016 with geom_smooth() -- you should do this for the other plots too

library(splines)
library(MASS)

p <- ggplot(whether.data, aes(Year,DisjNum,color=WQchar)) + scale_y_continuous(name = "Proportion of Disjunction", breaks=seq(0,1,by=0.1), labels=c("Simple",seq(0.1,0.9,by = 0.1),"Disjunction") ) + scale_x_continuous(name = "\nYear") + stat_sum(aes(size=..n.., alpha=.5)) + scale_size_area(max_size=12) + geom_smooth(alpha = 0.2, method="lm",formula = y ~ ns(x,3),fullrange=T) + scale_alpha_continuous(guide="none", limits = c(0,.7)) + scale_color_brewer(palette = "Set1") + theme_bw() + theme(panel.border = element_blank())

ggsave(p, file = "~/tyneside/articles/stableVarTalks/whetherifEngDisjByYear.pdf", width = 8, height = 5)

#different x, but also without binning 

p <- ggplot(whether.data, aes(Year,WQ,color=Disj)) + scale_y_continuous(name = "Proportion of Whether", breaks=seq(0,1,by=0.1), labels=c("If",seq(0.1,0.9,by = 0.1),"Whether") ) + scale_x_continuous(name = "\nYear") + stat_sum(aes(size=..n.., alpha=.5)) + scale_size_area(max_size=12) + geom_smooth(alpha = 0.2, method="lm",formula = y ~ ns(x,3),fullrange=T) + scale_alpha_continuous(guide="none", limits = c(0,.7)) + scale_color_brewer(palette = "Dark2") + theme_bw() + theme(panel.border = element_blank())

ggsave(p, file = "~/tyneside/articles/stableVarTalks/whetherifEngWQByYearUnbinned.pdf", width = 8, height = 5)