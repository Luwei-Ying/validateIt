#' Combine the mass of words with the same root
#'
#' @details
#' Use as a preparing step for validating unstemmed topic models.
#'
#' @param mod Fitted structural topic models.
#' @param vocab A character vector specifying the words in the corpus. Usually, it
#' can be found in topic model output.
#' @param beta A matrix of word probabilities for each topic. Each row represents a
#' topic and each column represents a word. Note this should not be in the logged form.
#'
#' @import tm
#' 
#' @export

combMass <- function(mod = NULL, vocab = NULL, beta = NULL){
  if(class(mod) == "STM"){
    vocab <- mod$vocab
    rawbeta <- exp(mod$beta$logbeta[[1]])
  } else {
    if(is.null(vocab) | is.null(beta)){
      stop("\"vocab\" and \"beta\" must be specified for topic models that are not STM.")
    }
    rawbeta <- beta
  }
  # test: rowSums(rawbeta) == rep(1, nrow(beta))
  stemmed_vocab <- stemDocument(vocab)
  # colnames(rawbeta) <- stemmed_vocab
  # newbeta <- t(rowsum(t(rawbeta), colnames(rawbeta)))
  newvocab <- matrix(NA, nrow = nrow(rawbeta), ncol = length(unique(stemmed_vocab)))
  newbeta <- matrix(NA, nrow = nrow(rawbeta), ncol = length(unique(stemmed_vocab)))
  for (i in 1:nrow(rawbeta)){
    mapping <- cbind.data.frame(vocab, stemmed_vocab, rawbeta[i,], stringsAsFactors = F)
    colnames(mapping)[3] <- "prob"
    maxtable <- aggregate(mapping$prob, by = list(mapping$stemmed_vocab), max)
    sumtable <- aggregate(mapping$prob, by = list(mapping$stemmed_vocab), sum)
    grouptable <- cbind.data.frame(maxtable, sumtable[,2])
    colnames(grouptable) <- c("stemmed_vocab", "prob", "sum.prob")
    mapping <- merge(mapping, grouptable,
                     by = c("stemmed_vocab", "prob"),
                     all.x = T, all.y = F)
    mapping <- mapping[!is.na(mapping$sum.prob) & !duplicated(mapping[,c("stemmed_vocab", "prob")]),]
    newvocab[i,] <- mapping$vocab
    newbeta[i,] <- mapping$sum.prob
    # # super slow
    # for(j in 1:length(unique(stemmed_vocab))){
    #   group <- mapping[mapping$stemmed_vocab == unique(stemmed_vocab)[j],]
    #   newvocab[i, j] <- as.character(group$vocab[which.max(group[,3])])
    #   newbeta[i, j] <- sum(group[,3])
    # }
  }
  return(list(newvocab, newbeta))
}
