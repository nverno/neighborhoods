# Neighborhood and size model using only target size and maximum height
#  within plot to estimate growth
#

df <- read.csv("long-df.csv")
df <- df[order(df$pplot,df$time),]

# add a maximum plot ht column to df
df$pplot <- as.factor(df$pplot)
maxhts <- ddply(df, .(pplot,time), summarise, max(ht, na.rm = TRUE))
times <- ddply(df, .(pplot,time), function(x) nrow(x))
df$maxplotht <- rep(maxhts[,3], times = times[,3])
df$relht <- df$ht/df$maxplotht

# add a maximum plot ba column to df
maxbas <- ddply(df, .(pplot,time), summarise, max(ba, na.rm = TRUE))
df$maxplotba <- rep(maxbas[,3], times = times[,3])
df$relba <- df$ba/df$maxplotba

# visualize
ggplot(df, aes(maxplotht)) + geom_histogram()
ggplot(df, aes(bagrowth)) + geom_histogram()
ggplot(df, aes(maxplotht, bagrowth)) + geom_boxplot()
ggplot(df, aes(priorba, bagrowth)) + geom_point()

# linear model, maxplotht as factor
lm.fit1 <- lm(bagrowth ~ priorba * maxplotht, data = df)
anova(lm.fit1)s
AIC(lm.fit1)
plot(lm.fit1)

plot(lm.fit1$model$maxplotht, fitted(lm.fit1), main = "fitted vs. max ht in plot")
plot(lm.fit1$model$priorba, fitted(lm.fit1), main = "fitted vs. priorba")
plot(fitted(lm.fit1), residuals(lm.fit1))

# linear model, maxplotht as number
lm.fit2 <- lm(bagrowth ~ priorba * as.numeric(maxplotht), data = df)
anova(lm.fit2)
AIC(lm.fit2)
plot(lm.fit2)

plot(lm.fit2$model$priorba, fitted(lm.fit2), main = "fitted vs. priorba")
plot(fitted(lm.fit2), residuals(lm.fit2))

# linear model, maxplotht as number
lm.fit3 <- lm(bagrowth ~ priorba + as.numeric(maxplotht), data = df)
anova(lm.fit3)
AIC(lm.fit3)
plot(lm.fit3)

plot(lm.fit3$model$priorba, fitted(lm.fit3), main = "fitted vs. priorba")
plot(fitted(lm.fit3), residuals(lm.fit3))

# same model as fit3
lm.fit4 <- lm(bagrowth ~ priorba + as.numeric(maxplotht) + priorba*as.numeric(maxplotht),
              data=df)
summary(lm.fit4)

# priorba * priorht * maxplotht
lm.fit5 <- lm(bagrowth ~ priorba*priorht*as.numeric(maxplotht), data = df)

# priorba * priorht
lm.fit6 <- lm(bagrowth ~ priorba * priorht, data=df)

# priorht * maxplotht
lm.fit7 <- lm(bagrowth ~ priorht * as.numeric(maxplotht), data=df)

# priorba * maxplotht * elev
lm.fit8 <- lm(bagrowth ~ priorba * as.numeric(maxplotht) * elev, data = df)

# base linear model: bagrowth ~ priorba
lm.fit9 <- lm(bagrowth ~ priorba, data = df)
plot(lm.fit9$model$priorba, fitted(lm.fit9))

anova(lm.fit6, lm.fit5)
AICtab(lm.fit1, lm.fit2, lm.fit3)
anova(lm.fit3, lm.fit2)


# htgrowth ~ priorht + relht
lm.fit10 <- lm(htgrowth ~ priorht + relht, data = df)
lm.fit11 <- lm(htgrowth ~ priorht * relht, data = df)
lm.fit12 <- lm(htgrowth ~ priorht + relht + priorht*relht, data =df)
summary(lm.fit10)
summary(lm.fit11)
anova(lm.fit10, lm.fit11)
par(mfrow = c(2,1))
plot(df$priorht, df$htgrowth)
plot(df$relht, df$htgrowth)
ggplot(df, aes(maxplotht)) + geom_histogram()

# bagrowth ~ priorba + relba
lm.fit15 <- lm(bagrowth ~ priorba, data = df)
lm.fit13 <- lm(bagrowth ~ priorba + relba, data = df)
lm.fit14 <- lm(bagrowth ~ priorba * relba, data = df)
summary(lm.fit13)
summary(lm.fit14)
summary(lm.fit15)

par(mfrow = c(2,1))
plot(df$priorba, df$bagrowth)
plot(df$relba, df$bagrowth)

# bagrowth ~ priorht + relht
lm.fit16 <- lm(bagrowth ~ priorht, data = df)
lm.fit17 <- lm(bagrowth ~ priorht + relht, data = df)
lm.fit18 <- lm(bagrowth ~ priorht * relht, data = df)

summary(lm.fit16)
summary(lm.fit17)
summary(lm.fit18)

par(mfrow = c(2,1))
plot(df$priorba, df$bagrowth)
plot(df$relba, df$bagrowth)

plot(lm.fit18)

