#' Get results from Mturk
#'
#' @details
#' this function works for complete or incomplete batches
#'
#' @param batch_id any number or string to annotate the batch
#' @param hit_ids hit ids returned from the MTurk API, i.e., output of sendTasks()
#' @param retry if TRUE, retry retriving results from Mturk API five times; default to TRUE
#' @param retry_in_seconds default to 60 seconds
#' @param AWS_id AWS_ACCESS_KEY_ID
#' @param AWS_secret AWS_SECRET_ACCESS_KEY
#' @param sandbox sanbox setting
#'
#' @import pyMTurkR
#' @export

getResults <- function(batch_id = "unspecified",
                       hit_ids,
                       retry = TRUE,
                       retry_in_seconds = 60,
                       AWS_id = Sys.getenv("AWS_ACCESS_KEY_ID"),
                       AWS_secret = Sys.getenv("AWS_SECRET_ACCESS_KEY"),
                       sandbox = getOption("pyMTurkR.sandbox", TRUE)){
  # check that amazon credentials & sandbox settings apply
  if(nchar(Sys.getenv("AWS_ACCESS_KEY_ID")) == 0){
    Sys.setenv(AWS_ACCESS_KEY_ID = AWS_id)
    Sys.setenv(AWS_SECRET_ACCESS_KEY = AWS_secret)
  }
  options(pyMTurkR.sandbox = sandbox)

  # convert all hit ids to character
  task_ids <- as.character(hit_ids[[2]][,1])
  mturk_ids <- as.character(hit_ids[[2]][,2])

  # retrieve results from mturk
  raw_results <- data.frame(stringsAsFactors = FALSE)
  message('Start getting HITs...')
  for(i in 1:length(mturk_ids)){
    turk_data <- suppressMessages(GetAssignment(hit = mturk_ids[i],
                                                get.answers = T))
    if(nrow(turk_data$Answers) == 0){
      this_hit_result <- as.data.frame(cbind(task_ids[i], mturk_ids[i], 0, 0, 0, 0),
                                       stringsAsFactors = FALSE)
    } else {
      this_hit_result <- as.data.frame(cbind(task_ids[i], mturk_ids[i],
                               turk_data$Answers$AssignmentId,
                               turk_data$Answers$WorkerId,
                               as.numeric(turk_data$Answers$FreeText),
                               as.character(turk_data$Assignments$SubmitTime)),
                               stringsAsFactors = FALSE)
    }
    raw_results <- rbind(raw_results, this_hit_result)
  }

  results <- cbind(batch_id, raw_results, stringsAsFactors = FALSE)
  colnames(results) <- c("batch_id", "local_task_id", "mturk_hit_id", "assignment_id", "worker_id", "result", "completed_at")

  n_results <- sum(results$result != 0)
  if(n_results == length(mturk_ids)){
    message(paste0('All ', n_results, ' HITs retrieved'))
  } else {
    message(paste0(n_results, ' / ', length(mturk_ids), ' results retrieved'))
    if(retry == T){
      Sys.sleep(retry_in_seconds)
      return(getResults(batch_id,
                        hit_ids,
                        retry,
                        retry_in_seconds,
                        AWS_id,
                        AWS_secret,
                        sandbox))
    }
  }
  return(results)
}
