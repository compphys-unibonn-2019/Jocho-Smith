## Random Walk



random_step <- function(l){
  ## makes one random step in a 2D grid
  x = l[1]
  y = l[2]
  # generate random number 
  a = round(runif(1))
  choice = (-1)^a
  a = round(runif(1))
  pm = (-1)^a
  if (choice==1){
    x = x + pm
  } else {
    y = y + pm
  }
  return(c(x,y))
}

random_walk <- function(N){
  ## makes N random step in a 2D grid
  l = c(0,0)
  
  for (i in 1:N){
    l = random_step(l)
  }
  return(l)
}
dist = 0
n = 1000
for (i in 1:n){
  dist = dist + sum(random_walk(n))
  avrg = dist/n # this is than the average distance from the origen after a random walk with n steps
}
print(avrg)
## this code runs about 12 seconds on my machine 
## in order to increase the speed one coule use less for loops and more vector operations. For example one could create a list of all steps in one direction and dann just sum up the entire list. 
