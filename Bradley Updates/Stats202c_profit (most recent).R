set.seed(1)

library(readxl)
library(tidyverse)

## Read
data <- read_xlsx("data.xlsx", col_names = FALSE)
colnames(data) <- c("X1", "X2", "Value")

### Set Up 2: Profit Function
data$Value <- -(data$Value * 100)

x <- lhs::maximinLHS(20,2)
sampling <- data.frame(X1 = round(x[,1],2),
                       X2 = round(x[,2],2))
sampling[which(sampling[,1]==1),1] = 0.99
sampling[which(sampling[,2]==1),2] = 0.99
sampling <- sampling %>% 
  left_join(data, by = c("X1" = "X1", "X2" = "X2"))

f_min <- min(sampling$Value)

## Estimate parameters
fitted_model <- DiceKriging::km(formula = ~1,
                                design = cbind(sampling$X1,sampling$X2), 
                                response = sampling$Value,
                                covtype = "exp",
                                control = list(trace=FALSE))

# negative expected improvement function (we try to minimize)
Neg_EI = function(x) {
  y <- predict(fitted_model, matrix(x,nrow = 1), "UK")$mean
  s <- predict(fitted_model, matrix(x,nrow = 1), "UK")$sd
  
  z = (f_min-y)/s
  -(f_min-y)*pnorm(z)-s*dnorm(z)
}

### Plots
plot(sampling$X1,sampling$X2,col="blue",pch=20)
abline(v=(1:10)/10)
abline(h=(1:10)/10)

plot_data <- data[sort(c(10*(1:1000)-9,seq(100,10000,by=100))),]
plot_data_new <- plot_data[c(rep(0,11),rep(110,11),rep(220,11),rep(330,11),rep(440,11),rep(550,11),
                               rep(660,11),rep(770,11),rep(880,11),rep(990,11),rep(1089,11))+ rep(1:11,11),] %>%
  arrange(X2)
contour(unique(plot_data_new$X1),unique(plot_data_new$X2),
        matrix(plot_data_new$Value, 11, 11),20,xlim=c(0,1),ylim=c(0,1),
        xlab = "x1", ylab = "x2")
points(sampling$X1,sampling$X2,col="blue",pch=20)

### contour plot of Neg_EI (slow)
grid_points = 1e2
grid = seq(0, 1, length.out=grid_points)
z = matrix(numeric(grid_points^2), nrow = grid_points)
for (i in 1:grid_points) {
  for (j in 1:grid_points) {
    z[i,j] = Neg_EI(c(grid[i],grid[j]))
  }
}
contour(x=grid,y=grid,z,xlab = "x", ylab = "y")
points(sampling$X1,sampling$X2,col="blue",pch=20)
points(0.50,0.94,col="red",pch="1")
points(0.88,0.87,col="red",pch="2")
points(0.94,0.57,col="red",pch="3")
points(0.81,0.49,col="red",pch="4")
points(0.66,0.84,col="red",pch="5")

### Algorithm

f_min_tracker <- c(f_min)

h = Neg_EI # comment to test other h

dim = 2

# number of iterations of simulated annealing
M = 4

# vector of T_1, T_2, ..., T_M closer to 0
big_T = (1/2)^(1:M)

# number of iterations for each Metropolis-Hastings (sampling) algorithm
N = rep(1e2,M)

# starting point
x0 = rep(0,dim)
# best value and point
best = c(h(x0),x0)

current = x0
for (k in 1:M) {
  target = function(x) {
    exp(-h(x) / big_T[k])
  }
  pi_current = target(current)
  ## investigate first instance of MH
  if (k == 1) {
    MH_samples1 = current
  }
  for (j in 1:N[k]) {
    # proposed point and target function value
    proposed = rnorm(dim,current,0.5)%%1
    pi_proposed = target(proposed)
    
    u = runif(1)
    # accept/reject proposed point
    if (pi_current < Inf & (pi_current == 0 | u <= pi_proposed / pi_current)) {
      current = proposed
      pi_current = pi_proposed
    }
    # update the best point and value if value is better
    h_current = h(current)
    if (h_current < best[1]) {
      best = c(h_current, current)
    }
    ## investigate first instance of MH
    if (k == 1) {
      MH_samples1 = rbind(MH_samples1,current)
    }
  }
}
best

contour(x=grid,y=grid,z,xlab = "x1", ylab = "x2",xlim=c(0.3,1),ylim=c(0.7,1))
points(sampling$X1,sampling$X2,col="blue",pch=20)
points(best[2],best[3],col="red",pch=20)

#optimize(h,interval=c(-10,10)) # if h is 1-dim

### Loop

MH_samples <- current # MH_samples for sample point 2 and SA iteration k = 2

for (i in 1:19) {
  top <- data.frame(X1 = round(best[2],2),
                    X2 = round(best[3],2))
  if (top$X1 == 1) {top$X1 = 0.99}
  if (top$X2 == 1) {top$X2 = 0.99}
  #top$Value <- DiceKriging::branin(top)
  top <- top %>%
    left_join(data, by = c("X1" = "X1", "X2" = "X2"))
  sampling <- rbind(top,sampling)
  f_min <- min(sampling$Value)
  f_min_tracker <- c(f_min_tracker, f_min)
  
  ## Estimate parameters
  fitted_model <- DiceKriging::km(formula = ~1,
                                  design = cbind(sampling$X1,sampling$X2), 
                                  response = sampling$Value,
                                  covtype = "exp",
                                  control = list(trace=FALSE))
  
  dim = 2
  
  # number of iterations
  M = 4
  
  # vector of T_1, T_2, ..., T_M closer to 0
  big_T = (1/2)^(1:M)
  
  # number of iterations for each Metropolis-Hastings (sampling) alogrithm
  N = rep(1e2,M)
  
  # starting point
  x0 = current
  # best value and point
  best = c(h(x0),x0)
  
  for (k in 1:M) {
    target = function(x) {
      exp(-h(x) / big_T[k])
    }
    pi_current = target(current)
    ## investigate lat instance of MH
    if (i == 19 & k == 4) {
      MH_samples2 = current
    }
    for (j in 1:N[k]) {
      # proposed point and target function value
      proposed = rnorm(dim,current,0.5)%%1
      pi_proposed = target(proposed)
      
      u = runif(1)
      # accept/reject proposed point
      if (pi_current < Inf & (pi_current == 0 | u <= pi_proposed / pi_current)) {
        current = proposed
        pi_current = pi_proposed
      }
      # update the best point and value if value is better
      h_current = h(current)
      if (h_current < best[1]) {
        best = c(h_current, current)
      }
      ## investigate last instance of MH
      if (i == 19 & k == 4) {
        MH_samples2 = rbind(MH_samples2,current)
      }
    }
  }
}

top <- data.frame(X1 = round(best[2],2),
                  X2 = round(best[3],2))
#top$Value <- DiceKriging::branin(top)
top <- top %>%
  left_join(data, by = c("X1" = "X1", "X2" = "X2"))
sampling <- rbind(top, sampling)
f_min <- min(sampling$Value)
f_min_tracker <- c(f_min_tracker,f_min)

### Plots

plot1 <- plot(0:20, -f_min_tracker, type = "l",
              xlab = "Iterations after Initial Sample", ylab = "Maximum Profit", main = "Maximum Profit at Iteration")

contour(unique(plot_data_new$X1),unique(plot_data_new$X2),
        matrix(plot_data_new$Value, 11, 11),20,xlim=c(0,1),ylim=c(0,1),
        xlab = "x1", ylab = "x2")
points(sampling[21:40,]$X1,sampling[21:40,]$X2,col="blue",pch=20)
points(sampling[1:20,]$X1,sampling[1:20,]$X2,col="red",pch=20)

plot(1:101,MH_samples1[,1],type="l",ylab="x1",xlab="",
     main="M-H Samples for Point 1, SA Iteration 1",
     ylim=c(0,1))
plot(1:101,MH_samples1[,2],type="l",ylab="x2",xlab="",
     main="M-H Samples for Point 1, SA Iteration 1",
     ylim=c(0,1))
plot(1:101,MH_samples2[,1],type="l",ylab="x1",xlab="",
     main="M-H Samples for Point 20, SA Iteration 4",
     ylim=c(0,1))
plot(1:101,MH_samples2[,2],type="l",ylab="x2",xlab="",
     main="M-H Samples for Point 20, SA Iteration 4",
     ylim=c(0,1))
plot(MH_samples1,col = rgb(red = 0, green = 0, blue = 1, alpha = 0.25),pch=20,
     main="M-H Samples for Point 1, SA Iteration 1",
     xlim=c(0,1),ylim=c(0,1),xlab="x1",ylab="x2")
plot(MH_samples2,col = rgb(red = 0, green = 0, blue = 1, alpha = 0.25),pch=20,
     main="M-H Samples for Point 20, SA Iteration 4",
     xlim=c(0,1),ylim=c(0,1),xlab="x1",ylab="x2")
