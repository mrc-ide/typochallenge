#!/usr/bin/env Rscript
source("R/functions.R")

## n_objective was chosen with the following reasoning:
## if there is 5% of errors, 
## 50000 * 5% = 2500 --> a good sample size to work with
## assuming a lower error rate of 1%, we would still get
## 50000 * 1% = 500 --> still ok sample size to work with

plot_iframe <- function(n_target = c(5000, 10000, 20000, 50000), 
                        start_date = as.Date("2018/09/3"),  
                        end_date = as.Date("2018/10/14"), 
                        col = "#660066",
                        col2 = "#cc00cc") {
  d <- read_contributions()

  if (is.null(d)) {
    n_total <- 0L
    n_contributions <- 0L
  } else {
    n_total <- d$total_sum
    n_contributions <- d$n_contributions
  }

  tmp <- n_target - n_total > 0
  if (any(tmp)) {
    my_target <- n_target[min(which(tmp))]
  } else {
    my_target <- max(n_target)
  }
  
  prop <- n_total / my_target
  if (prop > 1) { # could happen if we exceed the largest target
    prop <- 1
  }
  
  png(filename = "iframe.png",
      width = 350, height = 400)
  on.exit(dev.off())
  
  par(mar = c(5, 0, 6, 0))
  
  ## plot polygons
  plot.new()
  polygon(x = c(0.1, 0.1, 0.9, 0.9), 
          y = c(0, 1, 1, 0),
          xlim = c(0, 1), 
          ylim = c(0, 1), 
          col = col)
  polygon(x = c(0.1, 0.1, 0.9, 0.9), 
          y = c(0, prop, prop, 0),
          col = col2)
  
  ## add text at bottom
  txt <- sprintf("%d data entries", n_total)
  mtext(txt, side = 1, cex = 2, col = col2)
  txt2 <- sprintf("of our %d target,", my_target)
  mtext(txt2, side = 1, line = 1.5, cex = 1.5)
  txt3 <- sprintf("thanks to %d contributors", n_contributions)
  mtext(txt3, side = 1, line = 3, cex = 1.5)
  
  ## add text at top
  txt <- "Challenge live from"
  txt2 <- sprintf("%s to %s", 
                  format(start_date, "%d/%m/%y"), 
                  format(end_date, "%d/%m/%y"))
  mtext(txt, side = 3, line = 3.5, cex = 2, col = col)
  mtext(txt2, side = 3, line = 1.5, cex = 2, col = col)
  n_days_remain <- as.numeric(end_date - as.Date(Sys.time()))
  txt3 <- sprintf("(%d days remaining)", n_days_remain)
  mtext(txt3, side = 3, cex = 1.5)
}

if (!interactive()) {
  plot_iframe()
}
