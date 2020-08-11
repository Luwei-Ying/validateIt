#' Evaluate results
#'
#' @details
#' Evaludate worker performance by gold-standard HITs;
#' Return the accuracy rate (proportion correct) for a specified batch
#'
#' @param results results of human choice; outputs from getResults()
#' @param key the local task record; outputs form recordTasks()
#' @param type Task structures to be specified. Must be one of "WI" (word intrusion),
#' "T8WSI" (top 8 word set intrusion), "R4WSI" (random 4 word set intrusion),
#' "LI" (Label Intrusion), and "OL" (Optimal Label)
#' @export

evalResults <- function(results, key, type = NULL){
  if(sum(!(key[[1]]$id %in% results$local_task_id)) != 0){
    key[[2]] <- key[[2]][key[[1]]$id %in% results$local_task_id,]
    key[[1]] <- key[[1]][key[[1]]$id %in% results$local_task_id,]
  }

  message(paste0(sum(results[,5] != 0), ' / ', nrow(key[[2]]), ' results will be evaluated'))
  results <- as.matrix(results)
  indicator <- NULL
  if(type == "R4WSI0" | type == "T8WSI" | type == "LI" | type == "OL"){
    for(i in 1:nrow(results)){
      if (results[i,6] != 0){
        correct <- as.vector(key[[2]][i,-1])[as.numeric(results[i,6])] == as.character(key[[1]][i, (ncol(key[[1]])-1)])
      } else {
        correct <- NA
      }
      indicator <- c(indicator, correct)
    }
  } else if (type == "R4WSI"){
    for(i in 1:nrow(results)){
      if (results[i,6] != 0){
        correct <- as.vector(key[[2]][i,])[as.numeric(results[i,6])] == as.character(key[[1]][i, (ncol(key[[1]])-1)])
      } else {
        correct <- NA
      }
      indicator <- c(indicator, correct)
    }
  } else if (type == "WI"){
    for(i in 1:nrow(results)){
      if (results[i,6] != 0){
        correct <- as.vector(key[[2]][i,])[as.numeric(results[i,6])] == as.character(key[[1]][i, (ncol(key[[1]])-1)])
      } else {
        correct <- NA
      }
      indicator <- c(indicator, correct)
    }
  } else {
    stop("Please specify a valid task type.")
  }
  # overall gold-standard hit correct rate
  num <- sum(indicator[key[[1]][,1] == "gold"], na.rm = T)
  denum <- sum(key[[1]][,1] == "gold" & results[,6] != 0)
  goldcorrect <- c(num/denum, paste0(num, ' / ', denum))
  message(paste0(num, ' / ', denum, ' gold-standard HITs are answered correct'))

  # gold-standard hit correct rate by workers
  goldcorrectbyworker <- table(results[key[[1]][,1] == "gold",5], indicator[key[[1]][,1] == "gold"])

  # non-gold-standard HITs correct rate
  num <- sum(indicator[key[[1]][,1] != "gold"], na.rm = T)
  denum <- sum(key[[1]][,1] != "gold" & results[,6] != 0)
  nongoldcorrect <- c(num/denum, paste0(num, ' / ', denum))
  message(paste0(num, ' / ', denum, ' non-gold-standard HITs are answered correct'))

  output <- list(goldcorrect, goldcorrectbyworker, nongoldcorrect)
  names(output) <- c("Gold-standard HIT Correct Rate",
                     "Gold-standard HIT Correct Rate by Workers",
                     "Non-gold-standard HIT Correct Rate")
  return(output)
}



