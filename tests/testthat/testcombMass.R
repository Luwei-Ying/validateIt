load("../testdata/modtest.rda")

combMass_slow <- function(mod = NULL){
  vocab <- mod$vocab
  rawbeta <- exp(mod$beta$logbeta[[1]])
  stemmed_vocab <- stemDocument(vocab)
  newvocab <- matrix(NA, nrow = nrow(rawbeta), ncol = length(unique(stemmed_vocab)))
  newbeta <- matrix(NA, nrow = nrow(rawbeta), ncol = length(unique(stemmed_vocab)))
  for (i in 1:nrow(rawbeta)){
    mapping <- cbind.data.frame(vocab, stemmed_vocab, rawbeta[i,], stringsAsFactors = F)
    for(j in 1:length(unique(stemmed_vocab))){
      group <- mapping[mapping$stemmed_vocab == unique(stemmed_vocab)[j],]
      newvocab[i, j] <- as.character(group$vocab[which.max(group[,3])])
      newbeta[i, j] <- sum(group[,3])
    }
  }
  return(list(newvocab, newbeta))
}

test_that("combMass() works as expected",{
  out1 <- combMass(modtest)
  out2 <- combMass_slow(modtest)

  expect_equal(nrow(out1[[1]]), nrow(out1[[2]]))
  expect_equal(ncol(out1[[1]]), ncol(out1[[2]]))
  expect_identical(out1[[1]][1,], out1[[1]][1,])
  expect_identical(out1[[1]][2,], out1[[1]][2,])
  expect_identical(out1[[1]][3,], out1[[1]][3,])
  expect_identical(out1[[2]][1,], out1[[2]][1,])
  expect_identical(out1[[2]][2,], out1[[2]][2,])
  expect_identical(out1[[2]][3,], out1[[2]][3,])
})
