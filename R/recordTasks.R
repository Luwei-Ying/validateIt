#' Reform tasks to facilitate sending to Mturk
#'
#' @details
#' Randomize the order of options and record the tasks in a specified local directory
#'
#' @param type one of WI, T8WSI, R4WSI
#' @param tasks outputs from validateTopic(), validateLabel(), or mixGold() if users mix in gold-standard HITs
#' @param path path to record the tasks (with meta-information)
#' @export

recordTasks <- function(type, tasks, path){
  if(type == "WI"){
    optionidx <- 2:6
    optRandom <- tasks[,optionidx]
    optRandom <- as.data.frame(t(apply(optRandom, 1, function(x) x[sample(length(x))])),
                               stringsAsFactors = F)
    colnames(optRandom) <- paste0("word", 1:length(optionidx))
  } else if (type == "R4WSI0"|type == "T8WSI"|type == "LI"|type == "OL"){
    docindix <- 2
    optionidx <- 3:6
    optRandom <- tasks[,optionidx]
    optRandom <- as.data.frame(t(apply(optRandom, 1, function(x) x[sample(length(x))])),
                               stringsAsFactors = F)
    optRandom <- cbind.data.frame(tasks[,docindix], optRandom,
                                  stringsAsFactors = F)
    colnames(optRandom) <- c("passage", paste0("word", 1:length(optionidx)))
  } else if (type == "R4WSI"){
    optionidx <- 2:5
    optRandom <- tasks[,optionidx]
    optRandom <- as.data.frame(t(apply(optRandom, 1, function(x) x[sample(length(x))])),
                               stringsAsFactors = F)
    colnames(optRandom) <- paste0("word", 1:length(optionidx))
  } else {
    stop("Please specify a validate task type.")
  }
  record <- list(tasks, optRandom)
  save(record, file = path)
  message(paste("Record saved to", path))
  return(record)
}
