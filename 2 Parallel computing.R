# 2. Using parallel computing-------
library(doParallel)

# The same parts as the original script
simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 

MTweedieTests <-  
  function(N,M,sig){ 
    sum(replicate(M,simTweedieTest(N)) < sig)/M 
  } 

df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 

# Rewrite line 29-35 as follow: 
# Instantiate the cores
maxcores <- 4
Cores <- min(parallel::detectCores(), maxcores) 
cl <- makeCluster(Cores)

# Register the cluster
registerDoParallel(cl)

df <-
  foreach(
    i = 1:nrow(df),
    .combine = 'rbind', 
    .packages = c('magrittr', 'dplyr','tweedie')
  ) %dopar%
  tibble(
    N=df$N[i],
    M=df$M[i],
    share_reject= 
      MTweedieTests( 
        N=df$N[i], 
        M=df$M[i], 
        sig=.05
      ) 
  )


# Close off the clusters
stopCluster(cl)
