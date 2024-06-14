set.seed(2)

library(readxl)
library(tidyverse)

data <- read_xlsx("data.xlsx", col_names = FALSE) 
colnames(data) <- c("X1", "X2", "Value")

### Set Up 1: Brannin Function
data$Value <- apply(data[,1:2], 1, DiceKriging::branin)

x <- lhs::maximinLHS(20,2)
sampling <- data.frame(X1 = x[,1], X2 = x[,2], Value = 0)
colnames(sampling) <- c("X1", "X2", "Value")

sampling$Value <- apply(sampling[,1:2], 1, DiceKriging::branin)

# negative expected improvement function (we try to minimize)
Neg_EI = function(x) {
  y <- predict(fitted_model, matrix(x,nrow = 1), "UK")$mean
  s <- predict(fitted_model, matrix(x,nrow = 1), "UK")$sd
  
  z = (f_min-y)/s
  -(f_min-y)*pnorm(z)-s*dnorm(z)
}

plot(sampling$X1,sampling$X2,col="blue",pch=20)
abline(v=(1:20)/20)
abline(h=(1:20)/20)

contour(unique(data$X2),unique(data$X1),
        t(matrix(data$Value,100,100)),25,
        xlim=c(0,1),ylim=c(0,1))
points(sampling$X1,sampling$X2,col="blue",pch=20)

# Hyperparameters
dim = 2

# number of iterations of simulated annealing
M = 4

# vector of T_1, T_2, ..., T_M closer to 0
big_T = (1/2)^(1:M)

# number of iterations for each Metropolis-Hastings (sampling) algorithm
N = rep(1e2,M)

h = Neg_EI # comment to test other h

###################
#### Algorithm #### 
###################
f_min <- min(sampling$Value)
f_min_tracker <- c(f_min)

current = rep(0,dim)

for (i in 1:40) {
  
  if (i != 1) {
    top <- data.frame(X1 = c(best[2]), 
                      X2 = c(best[3]), 
                      Value = 0)
    top$Value <- DiceKriging::branin(c(top$X1,top$X2))
    sampling <- rbind(top,sampling)
    f_min <- min(sampling$Value)
    f_min_tracker <- c(f_min_tracker, f_min)
  }
  
  ## First estimate parameter
  fitted_model <- DiceKriging::km(formula = ~1,
                                  design = cbind(sampling$X1,sampling$X2), 
                                  response = sampling$Value,
                                  covtype = "exp",
                                  control = list(trace=FALSE))
  
  # starting point
  x0 = current
  # best value and point
  best = c(h(x0),x0)
  
  for (k in 1:M) {
    target = function(x) {
      exp(-h(x) / big_T[k])
    }
    pi_current = target(current)
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
    }
  }
  print(i)
}

top <- data.frame(X1 = c(best[2]), 
                  X2 = c(best[3]), 
                  Value = 0)
top$Value <- DiceKriging::branin(c(top$X1,top$X2))
sampling <- rbind(top,sampling)
f_min <- min(sampling$Value)
f_min_tracker <- c(f_min_tracker, f_min)

### Plots
plot1 <- plot(0:40, f_min_tracker, type = "l",
              xlab = "Iterations after Initial Sample", ylab = "Minimum Value", main = "Minimum Value at Iteration")

n.grid <- 100
x.grid <- y.grid <- seq(0,1,length=n.grid)
design.grid <- expand.grid(x.grid, y.grid)
response.grid <- apply(design.grid, 1, DiceKriging::branin)
z.grid <- matrix(response.grid, n.grid, n.grid)
contour(x.grid,y.grid,z.grid,40,
        xlab = "x1", ylab = "x2")
points(sampling[41:60,]$X1,sampling[41:60,]$X2,col="blue",pch=20)
points(sampling[1:40,]$X1,sampling[1:40,]$X2,col="red",pch=20)

