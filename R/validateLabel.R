#' Create validation tasks for labels assigned to the topics in the topic model of choice.
#'
#' @details
#' Users need to pick a topic model that they deem to be good and label the topics 
#' they later would like to use as measures.
#'
#' @param type Task structures to be specified. Must be one of "LI" (Label Intrusion) 
#' and "OL" (Optimal Label).
#' @param n The number of desired tasks
#' @param text.predict A data frame or matrix containing both the text and the indicator(s)
#' of the model predicted topic(s).
#' @param text.name variable name in `text.predict` that indicates the text
#' @param top1.name variable name in `text.predict` that indicates the top1 model predicted topic
#' @param top2.name variable name in `text.predict` that indicates the top2 model predicted topic
#' @param top3.name variable name in `text.predict` that indicates the top3 model predicted topic
#' @param labels The user-defined labels assigned to the topics
#' @param labels.index The topic index in correspondence with the labels, e.g., c(10, 12, 15).
#' Must be in the same length and order with `label`.
#' @param labels.add Labels from other broad catagories. Default to NULL. Users could 
#' specify them to evaluate how well different broad categories are distinguished from
#' one another.
#' 
#' @export

validateLabel <- function(type, n, text.predict = NULL, text.name = "text",
                          top1.name = "top1", top2.name = "top2", top3.name = "top3",
                          labels = NULL, labels.index = NULL, labels.add = NULL){
  if(!(is.data.frame(text.predict) | is.matrix(text.predict))){
    stop("\"text.predict\" needs to be a data.frame or matrix")
  }
  if(type == "OL"){
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
    if(length(labels) != length(labels.index)){
      stop("\"labels\" and \"labels.index\" have to be of the same length and in the exact same order.")
    }
    topic <- rep(labels.index, length.out = n)
    # define the output
    out <- matrix(NA, ncol = 6, nrow = n)
    colnames(out) <- c('topic', 'doc', 'opt1', 'opt2', 'opt3', 'optcrt')
    for(i in 1:n){
      # sample document by topic
      k <- topic[i]
      doc <- gsub('\n', '<br>', sample(text[top1 == k], 1))
      # prepare labels
      best.label <- labels[labels.index == k]
      if (is.null(labels.add)){
        intr.labels <- sample(labels[labels.index != k], 3)
      } else {
        intr.labels <- sample(c(labels[labels.index != k], labels.add), 3)
      }
      # put together a task
      out[i,] <- c(k, doc, intr.labels[1], intr.labels[2], intr.labels[3], best.label)
    }
  } else if (type == "LI"){
    if(!(text.name %in% colnames(text.predict))){
      stop("Please specify the correct variable name for text.")
    }
    if(!(top1.name %in% colnames(text.predict) & 
         top2.name %in% colnames(text.predict) &
         top3.name %in% colnames(text.predict))){
      stop("Please specify the correct variable names for the model predicted top3 topics.")
    }
    if(!all(labels.index %in% unique(c(text.predict[,c(top1.name)],
                                       text.predict[,c(top2.name)],
                                       text.predict[,c(top3.name)])))){
      warning("Some topic (labels.index) does not have any corresponding representative text.
              Consider removing that topic.")
    }
    if(!all(unique(c(text.predict[,c(top1.name)],
                     text.predict[,c(top2.name)],
                     text.predict[,c(top3.name)])) %in% labels.index)){
      stop("The top3 topics associated with some text are not all relevant.
           Consider refining the text pool.")
    }
    if(length(labels) != length(labels.index)){
      stop("\"labels\" and \"labels.index\" have to be of the same length and in the exact same order.")
    }
    # define the output
    out <- matrix(NA, ncol = 6, nrow = n)
    colnames(out) <- c('topic', 'doc', 'opt1', 'opt2', 'opt3', 'optcrt')
    for(i in 1:n){
      # randomly sample a row from the pool
      doc.idx <- sample(1:nrow(text.predict), 1)
      # prepare doc and labels
      doc <- gsub('\n', '<br>', text.predict[doc.idx, text.name])
      label1 <- labels[labels.index == text.predict[doc.idx, top1.name]]
      label2 <- labels[labels.index == text.predict[doc.idx, top2.name]]
      label3 <- labels[labels.index == text.predict[doc.idx, top3.name]]
      pred3 <- unlist(text.predict[doc.idx, c(top1.name, top2.name, top3.name)])
      if (is.null(labels.add)){
        intr.label <- sample(labels[!(labels.index %in% pred3)], 1)
      } else {
        intr.label <- sample(c(labels[!(labels.index %in% pred3)], labels.add), 1)
      }
      # put together the question
      out[i,] <- c(toString(pred3), doc, label1, label2, label3, intr.label)
    }
  } else {
    stop("Please specify a valid task structure.")
  }
  return(out)
}