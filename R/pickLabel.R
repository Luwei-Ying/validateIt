#' Pick the optimal label from candidate labels
#'
#' @details
#' Users need to specify four plausible labels for each topic
#'
#' @param n The number of desired tasks
#' @param text.predict A data frame or matrix containing both the text and the indicator(s)
#' of the model predicted topic(s).
#' @param text.name variable name in `text.predict` that indicates the text
#' @param top1.name variable name in `text.predict` that indicates the top1 model predicted topic
#' @param labels.index The topic index in correspondence with the labels, e.g., c(10, 12, 15).
#' @param candidate.labels A list of vectors containing the user-defined labels assigned to the topics,
#' Must be in the same length and order with `labels.index`.
#' 
#' @export

pickLabel <- function(n, text.predict = NULL, text.name = "text",
                      top1.name = "top1",
                      labels.index = NULL, candidate.labels = NULL){
  if(!(is.data.frame(text.predict) | is.matrix(text.predict))){
    stop("\"text.predict\" needs to be a data.frame or matrix")
  }
  if(!(text.name %in% colnames(text.predict))){
    stop("Please specify the correct variable name for text.")
  }
  if(!(top1.name %in% colnames(text.predict))){
    stop("Please specify the correct variable name for the model predicted most likely topic.")
  }
  text <- text.predict[,text.name]
  top1 <- text.predict[,top1.name]
  if(!all(labels.index %in% unique(top1))){
    stop("Some topic (labels.index) does not have any corresponding representative text.
           Consider removing that topic.")
  }
  if(length(candidate.labels) != length(labels.index)){
    stop("\"candidate.labels\" and \"labels.index\" have to be of the same length and in the exact same order.")
  }
  topic <- rep(labels.index, length.out = n)
  # define the output
  out <- matrix(NA, ncol = 6, nrow = n)
  colnames(out) <- c('topic', 'doc', 'opt1', 'opt2', 'opt3', 'optcrt')
  for(i in 1:n){
    # sample document by topic
    k <- topic[i]
    doc <- gsub('\n', '<br>', sample(text[top1 == k], 1))
    # put together a task
    out[i,] <- c(k, doc, unlist(candidate.labels[labels.index == k]))
  }
  return(out)
}