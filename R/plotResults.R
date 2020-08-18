#' Plot results
#'
#' @details
#' Visualize the accuracy rate (proportion correct) for a specified batch
#'
#' @param path path to store the plot
#' @param x a vector of counts of successes; could be obtained from getResults()
#' @param n a vector of counts of trials
#' @param taskname the name of the task for labeling, e.g., Word Intrusion, Optimal Label.
#' 
#' @export

plotResults <- function(path, x, n, taskname, ...){
  pdf(path, width = 3, height = 7)
  par(mgp = c(1.5, 0, 0), mar = c(2, 3, .7, .7))
  plot(NULL,
       main = NA,
       ylim=c(0, 1.02), 
       xlim = c(0.8, 1.2), 
       ylab = "Proportion Correct",
       xlab = NA,
       cex.lab = 1.2,
       axes = F, ...)
  axis(side = 2, at = seq(0, 1, by = 0.2), col.ticks = NA, cex.axis = 1.2)
  axis(side = 1, at = 1,
       labels = taskname, 
       las = 1,
       col = NA,
       col.ticks = NA,
       cex.axis = 1.2)
  # legend(0.55, 1.03,
  #        c("Model"),
  #        col = c(blue),
  #        lty = 1,
  #        lwd = 3,
  #        cex = 1.2,
  #        bty = 'n')
  
  # ----------------------------------------------------------------
  # abline(h = 0.25, col = "gray", lty = 1, lwd = 2)
  abline(h = 0.5, col = "gray", lty = 1, lwd = 2)
  
  # -------------------------------- bars ---------------------------------
  first <- x[1]/n[1]
  second <- x[2]/n[2]
  pool <- (first + second)/2
  
  points(x = 1, y = first, pch = 20, col = "blue", cex = 1.5)
  segments(x0 = 1, y0 = first-1.96*sqrt(first*(1-first)/n[1]), x1 = 1, y1 = first+1.96*sqrt(first*(1-first)/n[1]), col = "blue", lty = 2, lwd = 2)
  points(x = 1.02, y = second, pch = 20, col = "blue", lty = 2, cex = 1.5)
  segments(x0 = 1.02, y0 = second-1.96*sqrt(second*(1-second)/n[2]), x1 = 1.02, y1 = second+1.96*sqrt(second*(1-second)/n[2]), col = "blue", lty = 2, lwd = 2)
  points(x = 1.05, y = pool, pch = 20, col = "blue", cex = 2)
  segments(x0 = 1.05, y0 = pool-1.96*sqrt(pool*(1-pool)/(n[1]+n[2])), x1 = 1.05, y1 = pool+1.96*sqrt(pool*(1-pool)/(n[1]+n[2])), col = "blue", lwd = 3)

  dev.off()
}