#' Create validation tasks for topic model selection
#'
#' @details
#' Users need to fit their own topic models.
#'
#' @param type Task structures to be specified. Must be one of "WI" (word intrusion),
#' "T8WSI" (top 8 word set intrusion), and "R4WSI" (random 4 word set intrusion).
#' @param n The number of desired tasks
#' @param text The pool of documents to be shown to the Mturk workers
#' @param vocab A character vector specifying the words in the corpus. Usually, it
#' can be found in topic model output.
#' @param beta A matrix of word probabilities for each topic. Each row represents a
#' topic and each column represents a word. Note this should not be in the logged form.
#' @param theta A matrix of topic proportions. Each row represents a document and each
#' clums represents a topic. Must be specified if task = "T8WSI" or "R4WSI".
#' @param thres the threshold to draw words from, default to top 50 words.

validateTopic <- function(type, n, text = NULL, vocab, beta, theta = NULL, thres = 20){
  if (type == "WI"){
    if (is.vector(vocab)){
      vocab <- matrix(vocab, nrow = nrow(beta), ncol = length(vocab), byrow = T)
    }
    if (ncol(vocab) != ncol(beta)){
      stop("beta matrix does not correspond with the vocabulary.")
    }
    orderbeta <- t(apply(beta, 1, order, decreasing = TRUE))
    topic <- rep(1:nrow(beta), length.out = n)
    out <- matrix(NA, ncol = 6, nrow = n)
    colnames(out) <- c('topic', 'opt1', 'opt2', 'opt3', 'opt4', 'optcrt')
    for(i in 1:n){
      k <- topic[i]
      non.intr <- as.character(sample(vocab[k, orderbeta[k, 1:thres]], 4,
                                      prob = beta[k, orderbeta[k, 1:thres]]))
      intr.k <- sample((1:nrow(beta))[-k], 1)
      intr <- as.character(sample(vocab[intr.k, orderbeta[intr.k, 1:thres]], 1,
                                  prob = beta[intr.k, orderbeta[intr.k, 1:thres]]))
      out[i,] <- c(k, non.intr, intr)
    }
  } else if (type == "T8WSI"){
    if (length(text) != nrow(theta)){
      stop("theta matrix does not correspond with the documents.")
    }
    out <- matrix(NA, ncol = 6, nrow = n)
    colnames(out) <- c('topic', 'doc', 'opt1', 'opt2', 'opt3', 'optcrt')
    topwords <- lapply(1:nrow(beta),
                       function(x) toString(vocab[x, order(beta[x,], decreasing = T)][1:8]))
    for(i in 1:n){
      doc.idx <- sample(1:length(text), 1)
      # doc <- paste('<p align="left">', gsub('\n', '<br>', text[doc.idx]), '</p>')
      doc <- gsub('\n', '<br>', text[doc.idx])
      pred3 <- order(theta[doc.idx,], decreasing = T)[1:3]
      intr <- sample(order(theta[doc.idx,], decreasing = T)[-(1:3)], 1)
      out[i,] <- c(toString(pred3), doc,
                   topwords[[pred3[1]]], topwords[[pred3[2]]],
                   topwords[[pred3[3]]], topwords[[intr]])
    }
  } else if (type == "R4WSI0"){
    if (ncol(vocab) != ncol(beta)){
      stop("beta matrix does not correspond with the vocabulary.")
    }
    if (length(text) != nrow(theta)){
      stop("theta matrix does not correspond with the documents.")
    }
    pred1 <- t(apply(theta, 1, order, decreasing = T))[,1] # predict top 1 topic for each doc
    topic <- rep(sort(unique(pred1)), length.out = n)
    orderbeta <- t(apply(beta, 1, order, decreasing = TRUE))
    out <- matrix(NA, ncol = 6, nrow = n)
    colnames(out) <- c('topic', 'doc', 'opt1', 'opt2', 'opt3', 'optcrt')
    for(i in 1:n){
      k <- topic[i]
      # doc <- paste('<p align="left">', gsub('\n', '<br>', sample(text[pred1 == k], 1)), '</p>')
      doc <- gsub('\n', '<br>', sample(text[pred1 == k], 1))
      non.intr <- as.character(sample(vocab[k, orderbeta[k, 1:thres]], 12,
                                      prob = beta[k, orderbeta[k, 1:thres]]))
      intr.k <- sample((1:nrow(beta))[-k], 1)
      intr <- as.character(sample(vocab[intr.k, orderbeta[intr.k, 1:thres]], 4,
                                  prob = beta[intr.k, orderbeta[intr.k, 1:thres]]))
      asgn.n.intr <- sample(c(rep(1:3, 4)))
      out[i,] <- c(k, doc,
                   toString(non.intr[asgn.n.intr==1]),
                   toString(non.intr[asgn.n.intr==2]),
                   toString(non.intr[asgn.n.intr==3]),
                   toString(intr))
    }
  } else if (type == "R4WSI"){
    if (is.vector(vocab)){
      vocab <- matrix(vocab, nrow = nrow(beta), ncol = length(vocab), byrow = T)
    }
    if (ncol(vocab) != ncol(beta)){
      stop("beta matrix does not correspond with the vocabulary.")
    }
    orderbeta <- t(apply(beta, 1, order, decreasing = TRUE))
    topic <- rep(1:nrow(beta), length.out = n)
    out <- matrix(NA, ncol = 5, nrow = n)
    colnames(out) <- c('topic', 'opt1', 'opt2', 'opt3', 'optcrt')
    for(i in 1:n){
      k <- topic[i]
      non.intr <- as.character(sample(vocab[k, orderbeta[k, 1:thres]], 12,
                                      prob = beta[k, orderbeta[k, 1:thres]]))
      intr.k <- sample((1:nrow(beta))[-k], 1)
      intr <- as.character(sample(vocab[intr.k, orderbeta[intr.k, 1:thres]], 4,
                                  prob = beta[intr.k, orderbeta[intr.k, 1:thres]]))
      asgn.n.intr <- sample(c(rep(1:3, 4)))
      out[i,] <- c(k,
                   toString(non.intr[asgn.n.intr==1]),
                   toString(non.intr[asgn.n.intr==2]),
                   toString(non.intr[asgn.n.intr==3]),
                   toString(intr))
    }
  } else {
    stop("Please specify a valid task structure.")
  }
  return(out)
}
