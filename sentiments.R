library(scrapeR)
library(tm)
library(wordcloud)
library(stringr)
library(openNLP)

scrapePrisjakt <- function(.url){
  s <- 0
  out <- {}
  while(T){
    url <- paste0(.url, "&s=", s)
    print(url)
    page <- getURL(url, .encoding="UTF-8")
    parsedPage <-htmlParse(page)
    reviewNodes <- getNodeSet(parsedPage, "//li[@class='opinion-row']//div[@class='text-truncated']")
    reviews <- lapply(reviewNodes, function(r){paste0(xmlApply(r, xmlValue), collapse="")})
    
    reviews <- lapply(reviews, function(r){
      r <- gsub("(\n)+", " ", r)
      r <- gsub("(\t)+", " ", r)      
      r <- str_trim(r)
      r
    })
    
    if (length(reviews) == 0){ break}
    out <- c(out, reviews)
    s <- s + 50
  }
  print(paste("Scraped", length(out), "reviews for", .url))
  unlist(out)
}

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
   # Parameters
   # sentences: vector of text to score
   # pos.words: vector of words of postive sentiment
   # neg.words: vector of words of negative sentiment
   # .progress: passed to laply() to control of progress bar

   # create simple array of scores with laply
   scores = laply(sentences,
   function(sentence, pos.words, neg.words)
   {
      # remove punctuation
      sentence = gsub("[[:punct:]]", "", sentence)
      # remove control characters
      sentence = gsub("[[:cntrl:]]", "", sentence)
      # remove digits?
      sentence = gsub('\\d+', '', sentence)

      # define error handling function when trying tolower
      tryTolower = function(x)
      {
         # create missing value
         y = NA
         # tryCatch error
         try_error = tryCatch(tolower(x), error=function(e) e)
         # if not an error
         if (!inherits(try_error, "error"))
         y = tolower(x)
         # result
         return(y)
      }
      # use tryTolower with sapply 
      sentence = sapply(sentence, tryTolower)

      # split sentence into words with str_split (stringr package)
      word.list = str_split(sentence, "\\s+")
      words = unlist(word.list)

      # compare words to the dictionaries of positive & negative terms
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)

      # get the position of the matched term or NA
      # we just want a TRUE/FALSE
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)

      # final score
      score = sum(pos.matches) - sum(neg.matches)
      return(score)
      }, pos.words, neg.words, .progress=.progress )

   # data frame with scores for each sentence
   scores.df = data.frame(text=sentences, score=scores)
   return(scores.df)
}

cloud <- function(.sentences, .filter, .palette, .features){
  
  corpus <- Corpus(DataframeSource(data.frame(docs=.sentences)))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, tolower)
  corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("swedish")))
  tdm <- TermDocumentMatrix(corpus)
  
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  #v <- v[!match(names(v), .filter, F)]  
  print(v[1:100])
  v <- v[match(names(v), .features, F)]  
  
  d <- data.frame(word = names(v),freq=v)
  pal <- brewer.pal(9, .palette)
  pal <- pal[-(1:2)]
  wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
}

# import positive and negative words
pos = readLines("positive-se.txt")
neg = readLines("negative-se.txt")

sgs3 <- scrapePrisjakt("http://www.prisjakt.nu/produkt.php?o=1172708")  # SGS3
desire <- scrapePrisjakt("http://www.prisjakt.nu/produkt.php?o=563575") # Desire
sgs2 <- scrapePrisjakt("http://www.prisjakt.nu/produkt.php?o=838917")   # SGSII
iphone4 <- scrapePrisjakt("http://www.prisjakt.nu/produkt.php?o=630392")   # iPhone 4

sentences <- unlist(lapply(iphone4, sentDetect))  
sentences.scored <- score.sentiment(sentences, pos, neg)

bad <- subset(sentences.scored, score<0)$text
good <- subset(sentences.scored, score>0)$text

features <- c("design", "batteritid", "skärm", "kamera")

cloud(bad, neg, "Reds", features)
cloud(good, pos, "BuGn", features)