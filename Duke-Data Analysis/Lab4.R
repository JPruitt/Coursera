load(url("http://s3.amazonaws.com/assets.datacamp.com/course/dasi/nc.Rdata"))

names(nc)

summary(nc)

summary(nc$gained)
hist(nc$gained)
boxplot(nc$gained)

gained_clean = na.omit(nc$gained)
n = length(gained_clean)

boot_means = rep(NA, 100)

for (i in 1:100){
  samp=sample(gained_clean, n, replace = T)
  boot_means[i]<-mean(samp)
}

bootMean<-mean(boot_means)
boot90CI<-c(bootMean-(abs(qnorm(.05))*sd(boot_means)), bootMean+(abs(qnorm(.05))*sd(boot_means)))
boot90CI

hist(boot_means)

load(url("http://s3.amazonaws.com/assets.datacamp.com/course/dasi/inference.Rdata"))

inference(nc$gained, type = "ci", method = "simulation", conflevel = 0.9, est = "mean", boot_method = "perc")

inference(nc$gained, type = "ci", method = "simulation", conflevel = 0.95, est = "mean", boot_method = "perc")

inference(nc$gained, type = "ci", method = "simulation", conflevel = 0.95, est = "mean", boot_method = "se")

inference(nc$gained, type = "ci", method = "simulation", conflevel = 0.95, est = "median", boot_method = "se")

inference(nc$fage, type = "ci", method = "simulation", conflevel = 0.95, est = "mean", boot_method = "se")

boxplot(nc$weight ~ nc$habit)

by(nc$weight, nc$habit, mean)

by(nc$weight, nc$habit, length)

inference(y = nc$weight, x = nc$habit, est = "mean", type = "ht", null = 0, alternative = "twosided", method = "theoretical")

inference(y = nc$weight, x = nc$habit, est = "mean", type = "ht", null = 0, alternative ="twosided", method = "theoretical", order = c("smoker","nonsmoker"))

inference(y = nc$weight, x = nc$habit, est = "mean", type = "ci", null = 0, alternative ="twosided", method = "theoretical", order = c("smoker","nonsmoker"))

by(nc$mage, nc$mature, max)
by(nc$mage, nc$mature, min)

load(url("http://s3.amazonaws.com/assets.datacamp.com/course/dasi/gss.Rdata"))

summary(gss$wordsum)
summary(gss$class)

hist(gss$wordsum)
plot(gss$class)

by(gss$wordsum, gss$class, mean)
boxplot(gss$wordsum ~ gss$class)

inference(y = gss$wordsum, x = gss$class, est = "mean", method = "theoretical", type = "ht", alternative = "greater")

inference(y = gss$wordsum, x = gss$class, est = "mean", method = "theoretical", type = "ht", alternative = "greater")
