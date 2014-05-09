load(url("http://s3.amazonaws.com/assets.datacamp.com/course/dasi/atheism.RData"))
names(atheism)
summary(atheism)

us12 = subset(atheism, atheism$nationality == "United States" & atheism$year == "2012")

# Calculate the proportion of atheist responses:
proportion = sum(us12$response=="atheist")/length(us12$response)
# Print the proportion:
proportion

inference(us12$response, est = "proportion", type = "ci", method = "theoretical", success = "atheist")

india = subset(atheism, atheism$nationality == "India" & atheism$year == "2012")
inference(india$response, est = "proportion", type = "ci", method = "theoretical", success = "atheist")

china = subset(atheism, atheism$nationality == "China" & atheism$year == "2012")
inference(china$response, est = "proportion", type = "ci", method = "theoretical", success = "atheist")

# The first step is to make a vector p that is a sequence from 0 to 1 with
# each number separated by 0.01:
n = 1000
p = seq(0, 1, 0.01)

# We then create a vector of the margin of error (me) associated with each
# of these values of p using the familiar approximate formula (ME = 2 X SE):
me = 2 * sqrt(p * (1 - p)/n)

# Finally, plot the two vectors against each other to reveal their
# relationship:
plot(me ~ p)

spain = subset(atheism, atheism$nationality == "Spain")
proportion = sum(spain$response=="atheist")/length(spain$response)
proportion
inference(y=spain$response, x=spain$year, est = "proportion", type = "ci", method = "theoretical", success = "atheist")

inference(y=spain$response, x=as.factor(spain$year), est = "proportion", type = "ht", null = 0, alternative ="twosided", method = "theoretical", success = "atheist")

us = subset(atheism, atheism$nationality == "United States")
proportion = sum(us$response=="atheist")/length(us$response)
proportion
inference(y=us$response, as.factor(x=us$year), est = "proportion", type = "ci", method = "theoretical", success = "atheist")

inference(y=us$response, x=as.factor(us$year), est = "proportion", type = "ht", null = 0, alternative ="twosided", method = "theoretical", success = "atheist")


