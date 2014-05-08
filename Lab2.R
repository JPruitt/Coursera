load(url("http://s3.amazonaws.com/assets.datacamp.com/course/dasi/kobe.RData"))

head(kobe)
tail(kobe)

# Print the variable names of the data frame.
names(kobe)
# Print the first 9 values of the 'basket' variable
kobe$basket[1:9]

# Assign Kobe's streak lengths:
kobe_streak = calc_streak(kobe$basket)
# Draw a barplot of the result:
barplot(table(kobe_streak))

# Try some simulations!
outcomes = c("heads", "tails")
sample(outcomes, size=1, replace=TRUE)
sample(outcomes, size=1, replace=TRUE)
sample(outcomes, size=1, replace=TRUE)
sample(outcomes, size=1, replace=TRUE)

# Run the simulation:
outcomes = c("heads", "tails")
sim_fair_coin = sample(outcomes, size=100, replace=TRUE)
# Print the object:
sim_fair_coin
# Compute the counts of heads and tails:
table(sim_fair_coin)

# Run the simulation:
outcomes = c("heads", "tails")
sim_unfair_coin = sample(outcomes, size=100, replace=TRUE, prob=c(.2, .8))
# Print the object:
sim_unfair_coin
# Compute the counts of heads and tails:
table(sim_unfair_coin)

# Run the simulation and assign the result to 'sim_basket'.
outcomes = c("H", "M")
sim_basket = sample(outcomes, size = 133, replace = TRUE, prob=c(0.45, 0.55))
sim_basket
table(sim_basket)

kobe_streak<-calc_streak(kobe$basket)
sim_streak<-calc_streak(sim_basket)
summary(kobe_streak)
summary(sim_streak)
kobe_table<-table(kobe_streak)
sim_table<-table(sim_streak)
kobe_table
sim_table
barplot(kobe_table)
barplot(sim_table)

