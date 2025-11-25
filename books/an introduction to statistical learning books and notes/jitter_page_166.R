
# without jitter
plot(as.numeric(Bikeshare$hr), Bikeshare$bikers, pch = 16, col = rgb(0,0,1,0.3))

# Now with jitter
# jitter adds some noises to x. 
# add noise randomly from a uniform distribution (-0.3,+0.3)
plot(jitter(as.numeric(Bikeshare$hr), amount = 0.3),
     Bikeshare$bikers,
     pch = 16,
     col = rgb(0,0,1,0.3))
