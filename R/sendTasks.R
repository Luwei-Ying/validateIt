#' Send prepared task to Mturk and record the API-returned HIT ids.
#'
#' @details
#' Pairs the local ids with Mturk ids and save them to specified paths
#'
#' @param hit_type find from the Mturk requester's dashboard
#' @param hit_layout find from the Mturk requester's dashboard
#' @param type one of WI, T8WSI, R4WSI
#' @param tasksrecord output of recordTasks()
#' @param tasksids ids of tasks to send in numeric form. If left unspecified, the whole batch will be posted
#' @param HITidspath path to record the returned HITids
#' @param n_assignments number of of assignments per task. For the validation tasks, people almost always want 1
#' @param expire_in_seconds default 8 hours
#' @param batch_annotation add if needed
#'
#' @import pyMTurkR
#' @export

sendTasks <- function(hit_type = NULL,
                      hit_layout = NULL,
                      type = NULL,
                      tasksrecord = NULL,
                      tasksids = NULL,
                      HITidspath = NULL,
                      n_assignments = '1',
                      expire_in_seconds = as.character(60 * 60 * 8),
                      batch_annotation = NULL){

  if(is.null(tasksids)){
    tasksids <- tasksrecord[[1]][,"id"]
  }
  tasksids <- sort(tasksids)
  tosend <- tasksrecord[[2]][tasksrecord[[1]][,"id"] %in% tasksids,]

  if(type == "R4WSI0" | type == "T8WSI" | type == "LI" | type == "OL"){
    hit_param_names <- c('passage', 'word1', 'word2', 'word3', 'word4')
  } else if (type == "WI"){
    hit_param_names <- c('word1', 'word2', 'word3', 'word4', 'word5')
  } else if (type == "R4WSI"){
    hit_param_names <- c('word1', 'word2', 'word3', 'word4')
  } else {
    stop("Invalid task types")
  }

  current_HIT_ids <- rep(NA, nrow(tosend))
  map_ids <- as.data.frame(matrix(NA, nrow = nrow(tosend), ncol = 2))
  colnames(map_ids) <- c("tasksids", "Mturkids")
  message('Sending task to MTurk')
  for(i in 1:nrow(tosend)){
    hit_params <- list()
    for(j in 1:length(hit_param_names)){
      hit_params[[j]] <- list(Name = hit_param_names[j],
                              Value = tosend[i, j])
    }
    current_HIT_ids[i] <- suppressMessages(CreateHIT(hit.type = hit_type,
                                                     hitlayoutid = hit_layout,
                                                     hitlayoutparameters = hit_params,
                                                     assignments = n_assignments,
                                                     expiration = expire_in_seconds,
                                                     annotation = batch_annotation,
                                                     verbose = FALSE))$HITId

    map_ids[i,] <- cbind(tasksids[i], current_HIT_ids[i])
  }
  HITids <- list(current_HIT_ids, map_ids)
  save(HITids, file = HITidspath)
  message(paste("HITids saved to", HITidspath))
  return(HITids)
}
