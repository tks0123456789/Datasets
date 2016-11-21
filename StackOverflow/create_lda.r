# https://www.kaggle.com/solution/crowdflower-search-relevance/lda-visualization

library(readr)
library(tm)
library(SnowballC)
library(Matrix)
library(lda)
library(LDAvis)
library(servr)

# http://stackoverflow.com/questions/17227294/removing-html-tags-from-a-string-in-r
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

# http://cpsievert.github.io/LDAvis/reviews/reviews.html
stop_words <- stopwords("SMART")
createLDAvis <- function(doc.vec, minWordLen=2, minDf=5,
                         K=20, G=10, alpha=0.02, eta=0.02, seed=123,
                         out.dir="vis", open.browser=FALSE){
  # K, G, alpha, eta: MCMC and model tuning parameters:
  # pre-processing:
  doc.vec <- gsub("'", "", doc.vec)  # remove apostrophes
  doc.vec <- gsub("[[:punct:]]", " ", doc.vec)  # replace punctuation with space
  doc.vec <- gsub("[[:cntrl:]]", " ", doc.vec)  # replace control characters with space
  doc.vec <- gsub("^[[:space:]]+", "", doc.vec) # remove whitespace at beginning of documents
  doc.vec <- gsub("[[:space:]]+$", "", doc.vec) # remove whitespace at end of documents
  doc.vec <- gsub(" [[:digit:]]+", "",  doc.vec) # remove digit only word
  doc.vec <- tolower(doc.vec)  # force to lowercase
  doc.list <- strsplit(doc.vec, "[[:space:]]+")

  # compute the table of terms:
  term.table <- table(unlist(doc.list))
  term.table <- sort(term.table, decreasing = TRUE)
  tnames = names(term.table)
  # remove terms that are stop words or occur fewer than minDf times or
  # term length less than minWordLen:
  # nchar(invalid multibyte string, allowNA=TRUE): NA
  term.nchar <- nchar(tnames, allowNA=TRUE)
  tnames[is.na(term.nchar)] <- ""
  term.nchar[is.na(term.nchar)] <- 0
  del <- (tnames %in% stop_words) | term.table < minDf | term.nchar < minWordLen
  term.table <- term.table[!del]
  vocab <- names(term.table)

  # now put the documents into the format required by the lda package:
  get.terms <- function(x) {
    index <- match(x, vocab)
    index <- index[!is.na(index)]
    rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
  }
  documents <- lapply(doc.list, get.terms)

  # Compute some statistics related to the data set:
  D <- length(documents)  # number of documents
  W <- length(vocab)  # number of terms in the vocab
  doc.length <- sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document
  N <- sum(doc.length)  # total number of tokens in the data
  term.frequency <- as.integer(term.table)  # frequencies of terms in the corpus

  # Fit the model:
  set.seed(seed)
  fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                     num.iterations = G, alpha = alpha, 
                                     eta = eta, initial = NULL, burnin = 0,
                                     compute.log.likelihood = TRUE)
  theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
  phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))

  ForumPosts <- list(phi = phi,
                     theta = theta,
                     doc.length = doc.length,
                     vocab = vocab,
                     term.frequency = term.frequency)

  # create the JSON object to feed the visualization:
  json <- createJSON(phi = ForumPosts$phi, 
                     theta = ForumPosts$theta, 
                     doc.length = ForumPosts$doc.length, 
                     vocab = ForumPosts$vocab, 
                     term.frequency = ForumPosts$term.frequency)

  serVis(json, out.dir = out.dir, open.browser = open.browser)
}

# Q
# 2016/11/19 18h
# 
dataPath <- "your_data_path/"
dnames <- c("pythonquestions", "rquestions", "stacksample", "statsquestions")
minSc <- 3
seed <- 102

result <- c()
t0 = Sys.time()
for(dname in dnames){
  fname <- paste(dataPath, dname, "Questions.csv", sep="/")
  cat(fname, "\n")
  Q <- read.csv(fname, as.is=T)
  Q$Body <- sapply(Q$Body, cleanFun, USE.NAMES=F)
  doc.vec <- as.vector(paste(Q$Title, Q$Body))[Q$Score >= minSc]
  cat("size:", length(doc.vec), "\n")
  for(K in 1:4*10){
    cat("K", K, "\n")
    out.dir <- paste(dname, "K", K, "seed", seed, sep="_")
    lda1k <- createLDAvis(doc.vec, K=K, G=1000, alpha=0.02, eta=0.02,
                          seed=seed, out.dir=out.dir)
    result <- c(result, lda1k)
  }   
  print(Sys.time() - t0)
}
