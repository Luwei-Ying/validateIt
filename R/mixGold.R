#' Mix the gold-standard tasks with the tasks need to be validated
#'
#' @param tasks All tasks need to be validated
#' @param golds Gold standard tasks with the same structure

mixGold <- function(tasks, golds){
  if (nrow(tasks)/nrow(golds) != round(nrow(tasks)/nrow(golds))){
    stop("The number of tasks needs to be a multiple of the number of golds.")
  }
  tasks <- as.data.frame(tasks, stringsAsFactors = F)
  placeholder <- seq(0, (nrow(tasks)+nrow(golds) - 1), by = (nrow(tasks)/nrow(golds)+1))
  indices <- sample(1:(nrow(tasks)/nrow(golds)+1), nrow(golds), replace = T)
  if(indices[1] == 1){
    indices[1] <- 2
  }
  for(i in 1:nrow(golds)){
    tasks <- rbind(tasks[0:(placeholder[i]+indices[i]-1),],
                   golds[i,],
                   tasks[-(0:(placeholder[i]+indices[i]-1)),])
  }
  tasks <- cbind(tasks, 1:nrow(tasks))
  row.names(tasks) <- 1:nrow(tasks)
  colnames(tasks)[ncol(tasks)] <- "id"
  return(tasks)
}
