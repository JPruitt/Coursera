load(url("http://s3.amazonaws.com/assets.datacamp.com/course/dasi/mlb11.RData"))

plot(mlb11$runs ~ mlb11$at_bats)
plot(mlb11$at_bats ~ mlb11$runs)

correlation <- cor(mlb11$at_bats, mlb11$runs)

x1 = 5400
y1 = 600

x2 = 5700
y2 = 800

plot_ss(x = mlb11$at_bats, y = mlb11$runs, x1, y1, x2, y2, showSqares = TRUE)

plot_ss(x = mlb11$at_bats, y = mlb11$runs, leastSquares = TRUE)

m1 <- lm(runs ~ at_bats, data = mlb11)
m1
summary(m1)

m2 <- lm(runs ~ homeruns, data = mlb11)
m2
summary(m2)

plot(mlb11$runs ~ mlb11$at_bats)
abline(m1)

plot(m1$residuals ~ mlb11$at_bats) 
abline(h = 0, lty = 3)

qqnorm(m1$residuals) 
qqline(m1$residuals)
hist(m1$residuals)

names(mlb11)

m3 <- lm(runs ~ at_bats + hits + wins + bat_avg, data = mlb11)
summary(m3)

m4 <- lm(runs ~ hits, data = mlb11)
summary(m4)

m5 <- lm(runs ~ wins, data = mlb11)
summary(m5)

m6 <- lm(runs ~ bat_avg, data = mlb11)
summary(m6)

n1 <- lm(runs ~ new_obs, data = mlb11)
summary(n1)
n2 <- lm(runs ~ new_slug, data = mlb11)
summary(n2)
n3 <- lm(runs ~ new_onbase, data = mlb11)
summary(n3)
