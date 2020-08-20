#' Check Agreement Rate between Identical Trails
#'
#' @details
#' Evaluate workers' performance by agreement rate between identical trails
#' (Notice that this means the two input, results1 and results2, must be identical.);
#' Return 1) the exact agreement rate when both workers agree on the exact same choice, and
#' 2) the binary agreement rate when both workers get the task either right or wrong simultaneously
#'
#' @param results1 first batch of results; outputs from getResults()
#' @param results2 first batch of results; outputs from getResults()
#' @param key the local task record; outputs from recordTasks()
#' @param type Task structures to be specified. Must be one of "WI" (word intrusion),
#' "T8WSI" (top 8 word set intrusion), "R4WSI" (random 4 word set intrusion),
#' "LI" (Label Intrusion), and "OL" (Optimal Label)
#' 
#' @export 

checkAgree <- function(results1, results2, key, type = NULL){
  if(sum(!(key[[1]]$id %in% results1$local_task_id)) != 0){
    key[[2]] <- key[[2]][key[[1]]$id %in% results1$local_task_id,]
    key[[1]] <- key[[1]][key[[1]]$id %in% results1$local_task_id,]
  }
  
  # message(paste0(sum(results1[,5] != 0), ' / ', nrow(key[[2]]), ' results will be evaluated'))
  
  # Remove the gold-standard HITs
  results1 <- results1[key[[1]][,1] != "gold",]
  results2 <- results2[key[[1]][,1] != "gold",]
  key[[2]] <- key[[2]][key[[1]][,1] != "gold",]
  key[[1]] <- key[[1]][key[[1]][,1] != "gold",]
  
  # Agreement rate on picking up the EXACT same choice
  AgreeExact <- sum(results1$result[results1$result != 0] == results2$result[results2$result != 0])/length(results1$result[results1$result != 0])
  
  results1 <- as.matrix(results1)
  results2 <- as.matrix(results2)
  
  # Calculate the "correct" vector
  indicator1 <- NULL
  indicator2 <- NULL
  if(type == "R4WSI0" | type == "T8WSI" | type == "LI" | type == "OL"){
    for(i in 1:nrow(results1)){
      if (results1[i,6] != 0){
        correct <- as.vector(key[[2]][i,-1])[as.numeric(results1[i,6])] == as.character(key[[1]][i, (ncol(key[[1]])-1)])
      } else {
        correct <- NA
      }
      indicator1 <- c(indicator1, correct)
    }
    for(i in 1:nrow(results2)){
      if (results2[i,6] != 0){
        correct <- as.vector(key[[2]][i,-1])[as.numeric(results2[i,6])] == as.character(key[[1]][i, (ncol(key[[1]])-1)])
      } else {
        correct <- NA
      }
      indicator2 <- c(indicator2, correct)
    }
  } else if (type == "R4WSI"){
    for(i in 1:nrow(results1)){
      if (results1[i,6] != 0){
        correct <- as.vector(key[[2]][i,])[as.numeric(results1[i,6])] == as.character(key[[1]][i, (ncol(key[[1]])-1)])
      } else {
        correct <- NA
      }
      indicator1 <- c(indicator1, correct)
    }
    for(i in 1:nrow(results2)){
      if (results2[i,6] != 0){
        correct <- as.vector(key[[2]][i,])[as.numeric(results2[i,6])] == as.character(key[[1]][i, (ncol(key[[1]])-1)])
      } else {
        correct <- NA
      }
      indicator2 <- c(indicator2, correct)
    }
  } else if (type == "WI"){
    for(i in 1:nrow(results1)){
      if (results1[i,6] != 0){
        correct <- as.vector(key[[2]][i,])[as.numeric(results1[i,6])] == as.character(key[[1]][i, (ncol(key[[1]])-1)])
      } else {
        correct <- NA
      }
      indicator1 <- c(indicator1, correct)
    }
    for(i in 1:nrow(results2)){
      if (results2[i,6] != 0){
        correct <- as.vector(key[[2]][i,])[as.numeric(results2[i,6])] == as.character(key[[1]][i, (ncol(key[[1]])-1)])
      } else {
        correct <- NA
      }
      indicator2 <- c(indicator2, correct)
    }
  } else {
    stop("Please specify a valid task type.")
  }
  
  # Agreement rate either both correcly or both wrongly
  AgreeBinary <- sum(indicator1[!is.na(indicator1)] == indicator2[!is.na(indicator1)])/length(indicator1[!is.na(indicator1)])

  output <- list(AgreeExact, AgreeBinary)
  names(output) <- c("Both workers agree on the exact same choice",
                     "Both workers answer correctly or wrongly")
  return(output)
}



