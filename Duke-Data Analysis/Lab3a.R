load(url("http://s3.amazonaws.com/assets.datacamp.com/course/dasi/ames.RData"))

head(ames)
names(ames)
dim(ames)

area<-ames$Gr.Liv.Area
price<-ames$SalePrice

summary(area)
hist(area)

samp0<-sample(area, 50)
hist(samp0)

samp1<-sample(area, 50)
hist(samp1)

mean(samp1)

# The vector 'sample_means50' is initialized with NA values
sample_means50 = rep(NA, 5000)
# The for loop runs 5000 times, with 'i' taking values 1 up to 5000
for (i in 1:5000) {
  # Take a random sample of size 50
  samp = sample(area, 50)
  # Store the mean of the sample in the 'sample_means50' vector on the ith
  # place
  sample_means50[i] = mean(samp)
  # Print the counter 'i'
  print(i)
}

# Print the first few random medians
head(sample_means50)


sample_means_small<-rep(NA, 100)
for (i in 1:100){
  samp=sample(area, 50)
  sample_means_small[i]<-mean(samp)
}
sample_means_small

# Initialize the sample distributions:
sample_means10 = rep(NA, 5000)
sample_means100 = rep(NA, 5000)

# Run the for loop:
for (i in 1:5000) {
  samp = sample(area, 10)
  sample_means10[i] = mean(samp)
  samp = sample(area, 100)
  sample_means100[i] = mean(samp)
}

# Take a look at the results:
head(sample_means10)
head(sample_means50)  # was already loaded
head(sample_means100)

# Divide the plot in 3 rows:
par(mfrow = c(3, 1))

# Define the limits for the x-axis:
xlimits = range(sample_means10)

# Draw the histograms:
hist(sample_means10, breaks=20, xlim=xlimits)
hist(sample_means50, breaks=20, xlim=xlimits)
hist(sample_means100, breaks=20, xlim=xlimits)

# Take a sample of size 50 from 'price':
sample_50<-sample(price, 50)

# Print the mean:
mean(sample_50)

sample_means50<-rep(NA, 5000)
sample_means150<-rep(NA, 5000)
for (i in 1:5000){
  samp=sample(price, 50)
  sample_means50[i]<-mean(samp)
  samp=sample(price, 150)
  sample_means150[i]<-mean(samp)
}
par(mfrow=c(2,1))
hist(sample_means50)
hist(sample_means150)
