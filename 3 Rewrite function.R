# 3.  rewrite the function MTweedieTests
library(purrr)
library(furrr)
library(doParallel)

simTweedieTest <-  
  function(N){ 
    t.test( 
      rtweedie(N, mu=10000, phi=100, power=1.9), 
      mu=10000 
    )$p.value 
  } 

# rewrite the function 
MParTweedieTests <-
  function(N, M, sig) {
    Cores <- 8 #minimum of the chosen cores and the available cores.
    cl <- makeCluster(Cores) # Instantiate the cores
    registerDoParallel(cl) #register cluster
    m_per_core <- round(M / Cores) #split M into multiple cores
    m_total <- m_per_core * Cores #total tests
    
    #for each core, we use replicate() to apply simTweedieTests function m_per_core times with 
    # a sample size of N, the null hypothesis is rejected where p -value is lower than alpha = 0.05
    
    rejects_per_core <-
      foreach(
        i = 1:Cores,
        .combine = 'rbind',
        .packages = c('magrittr', 'dplyr', 'tweedie'),
        .export = c('simTweedieTest')
      ) %dopar%
      sum(replicate(m_per_core, simTweedieTest(N)) < sig)
    
    stopCluster(cl) #close off cluster
    
    #then we calculate the sum of rejection test over total number of test to see the rejection percentage
    sum(rejects_per_core) / m_total
}
# then you can apply this function for all rows in DF.


# original script
df <-  
  expand.grid( 
    N = c(10,100,1000,5000, 10000), 
    M = 1000, 
    share_reject = NA) 

for(i in 1:nrow(df)){ 
  df$share_reject[i] <-  
    MParTweedieTests( 
      N=df$N[i], 
      M=df$M[i], 
      sig=.05) 
} 
