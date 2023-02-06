library(tidyverse)
library(tweedie) 
library(tictoc)
library(doParallel)
library(purrr)
library(furrr)

# 1. Original Script--------------
# Create a function for printing out results in a data frame
printTicTocLog <-
  function() {
    tic.log() %>%
      unlist %>%
      tibble(logvals = .) %>%
      separate(logvals, 
               sep = ":",
               into = c("Function type", "log")) %>%
      mutate(log = str_trim(log)) %>% 
      separate(log,
               sep = " ",
               into = c("Seconds"),
               extra = "drop")
  }
tic.clearlog()

# original script 
tic.clearlog()
tic("1 Original script")

source("scripts/1 Original script.R")
toc(log = TRUE)

printTicTocLog() %>%
  knitr::kable()

# 2. Parallel computing------------------
tic(paste0("2 Parallel loop, ", Cores, " cores"))
source("scripts/2 Parallel computing.R")
toc(log = TRUE)

printTicTocLog() %>%
  knitr::kable()

# 3. Rewrite function-------------
tic("3 Furrr")
source("scripts/3 Rewrite function.R")
toc(log = TRUE)

printTicTocLog() %>%
  knitr::kable()

# In my computer, the results are as below:
  # |Function type            |Seconds |
  # |1 Original script        |51.146  |
  # |2 Parallel loop, 4 cores |28.803  |
  # |3 Furrr                  |29.753  |

# Comments: Parallel loop is the fastest method. It is because parallel loop allows 
# all of my computer cores to work at the same time to perform the iterations 
# instead of only working on one of them like the for loop does. As for the furrr 
# method, the whole process is divided into several individual parts, which can be 
# really helpful for dealing with complicated problems. However, in this case, it
# is not necessary.