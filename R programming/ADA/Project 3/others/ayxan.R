# LIBRARY LOADINGS
library("tm")
library("NLP")
library("openNLP")
library("zipfR")
library("wordcloud")
library("syuzhet")
library("quanteda")
library("stringi")

# READING TEXT FILE
chapters <- VCorpus(DirSource("./20000Leagues", ignore.case = TRUE, mode = "text"))
inspect(chapters)
str(chapters)


# PART A
# MODIFYING CONTENT TO MAKE ONE SINGLE LINE
get_content <- function(chp){
  chp.content <- vector()
  index <- 1
  for (line in chp){
    line <- trimws(line)
    # skip header
    if (index == 1){
      chp.content<-paste(chp.content, line, sep = "", quote = ".")
    }
    else{
    chp.content<-paste(chp.content, line, sep = "",quote = " ")
    }
    index <- index + 1
  }
  return(stripWhitespace(chp.content))
}

# COLLECTING WORDS AND SENTENCES FROM CONTENT
get_10_longest <- function(chp){
  chp.content.words <- vector()
  chp.content.sentences <- vector()
  
  for (paragraph in chp){
    words <- strsplit(paragraph, " ")
    sentences <- strsplit(paragraph, "\\.|\\?|\\!|\\. |\\? |\\! ")
    
    for (word in words){
      chp.content.words <- append(chp.content.words, word)
    }
    
    for (sentence in sentences){
      chp.content.sentences <- append(chp.content.sentences, sentence)
    }
  }
  
  chp.10words <- unique(chp.content.words[order(nchar(chp.content.words), 
                                           chp.content.words, decreasing = T)])[1:10]
  chp.10sentences <- unique(chp.content.sentences[order(nchar(chp.content.sentences), 
                                                   chp.content.sentences, decreasing = T)])[1:10]
  print(chp.10words)
  print(chp.10sentences)
  return(chp.10sentences)
}

chp1 <- chapters[[1]]
chp1.content <- get_content(chp1$content)
chp1.10sentences <- get_10_longest(chp1.content)

chp2 <- chapters[[3]]
chp2.content <- get_content(chp2$content)
chp2.10sentences <- get_10_longest(chp2.content)

chp3 <- chapters[[4]]
chp3.content <- get_content(chp3$content)
chp3.10sentences <- get_10_longest(chp3.content)

chp4 <- chapters[[5]]
chp4.content <- get_content(chp4$content)
chp4.10sentences <- get_10_longest(chp4.content)

chp5 <- chapters[[6]]
chp5.content <- get_content(chp5$content)
chp5.10sentences <- get_10_longest(chp5.content)

chp6 <- chapters[[7]]
chp6.content <- get_content(chp6$content)
chp6.10sentences <- get_10_longest(chp6.content)

chp7 <- chapters[[8]]
chp7.content <- get_content(chp7$content)
chp7.10sentences <- get_10_longest(chp7.content)

chp8 <- chapters[[9]]
chp8.content <- get_content(chp8$content)
chp8.10sentences <- get_10_longest(chp8.content)

chp9 <- chapters[[10]]
chp9.content <- get_content(chp9$content)
chp9.10sentences <- get_10_longest(chp9.content)

chp10 <- chapters[[2]]
chp10.content <- get_content(chp10$content)
chp10.10sentences <- get_10_longest(chp10.content)


# PART B
# DocumentTermMatrix
chapters.dtm <- DocumentTermMatrix(chapters)
chapters.dtm
inspect(chapters.dtm)

# TermDocumentMatrix
chapters.tdm <- TermDocumentMatrix(chapters)
chapters.tdm
inspect(chapters.tdm)
chapters.df <- data.frame(chp1[1])
chapters.df[1]
chp1[1]

# Data Cleansing
# Removing numbers and punctuations
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
chapters.lower <- tm_map(chapters, content_transformer(tolower))
chapters.clean <- tm_map(chapters.lower, content_transformer(removeNumPunct))
inspect(chapters.clean)
inspect(chapters.clean[[1]])

# Removing stop words
mystopwords <- c(stopwords("english"))
mystopwords
chapters.stop <- tm_map(chapters.clean, removeWords, mystopwords)
inspect(chapters.stop[[1]])

# New TermDocumentMatrix
chapters.stop.tdm <- TermDocumentMatrix(chapters.stop)
chapters.stop.tdm

# Finding Frequent Terms
freqTerms <- findFreqTerms(chapters.stop.tdm, lowfreq = 0.5)
freqTerms
chapters.tf <- termFreq(chapters.stop[[1]])
chapters.tf

# Dendrogram
chapters.df <- as.data.frame(chapters.stop.tdm[[1]])
chapters.dist <- dist(chapters.df)
chapters.dg <- hclust(chapters.dist, method = "ward.D2")
str(chapters.dg)
plot(chapters.dg)


# PART C
# RETRIEVE WORDS WITH THE GIVEN LENGTH
wordslen <- function(words, wordLength){
  words.size <- vector()
  for (word in words){
    if(nchar(word) == wordLength){
      words.size <- c(words.size, word)
    }
  }
  return(words.size)
}

# REMOVE WORDS WITH THE GIVEN LENGTH AND PLOT DENDOGRAM
removeWordsSized <- function(recent, wordLength){
  # Without Words of Size wordLength
  words.size <- wordslen(chapters.stop.tdm$dimnames$Terms, wordLength)
  chapters.rm <- tm_map(recent, removeWords, words.size)
  
  # New TermDocumentMatrix Without Words of Size wordLength
  chapters.rm.tdm <- TermDocumentMatrix(chapters.rm)
  print(length(chapters.rm.tdm$dimnames$Terms))
  print(chapters.rm.tdm[[1]])
  
  # Dendogram Without Words of Size wordLength
  chapters.df <- as.data.frame(chapters.rm.tdm[[1]])
  chapters.dist <- dist(chapters.df)
  chapters.dg <- hclust(chapters.dist, method = "ward.D2")
  str(chapters.dg)
  plot(chapters.dg)
  
  return(chapters.rm)
}

chapters.rm2 <- removeWordsSized(chapters.stop, 2)
chapters.rm3 <- removeWordsSized(chapters.rm2, 3)
chapters.rm4 <- removeWordsSized(chapters.rm3, 4)
chapters.rm5 <- removeWordsSized(chapters.rm4, 5)
chapters.rm6 <- removeWordsSized(chapters.rm5, 6)
chapters.rm7 <- removeWordsSized(chapters.rm6, 7)
chapters.rm8 <- removeWordsSized(chapters.rm7, 8)
chapters.rm9 <- removeWordsSized(chapters.rm8, 9)
chapters.rm10 <- removeWordsSized(chapters.rm9, 10)
chapters.rm11 <- removeWordsSized(chapters.rm10, 11)
chapters.rm12 <- removeWordsSized(chapters.rm11, 12)

# PART D
# POS
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()

chapters.list <- list(chp1.10sentences, chp2.10sentences, chp3.10sentences, chp4.10sentences, chp5.10sentences,
                      chp6.10sentences, chp7.10sentences, chp8.10sentences, chp9.10sentences, chp10.10sentences)

for(i in 1:10){
  chapter <- chapters.list[i]
  
  print(paste("Noun and Verbs with size >= 5 in 10 longest sentences of Chapter:", i))
  
  for(j in 1:10){
    s1 <- as.String(chapter[j])
    s2 <- annotate(s1, list(sent_token_annotator, word_token_annotator))
    s3 <- annotate(s1, pos_tag_annotator, s2)
    s4 <- as.data.frame(s3)
    
    for(k in 1:dim(s4)[1]){
      type <- s4$type[k]
      POS <- s4$features[k][[1]]$POS
      word.length <- s4$end[k] - s4$start[k] + 1
      
      if(type == "word" && (startsWith(POS, "N") || startsWith(POS, "V")) && word.length >= 5){
        print(substr(s1, s4$start[k], s4$end[k]) + " --- " + POS)
      }
    }
  }
  cat("\n")
}


# PART E
# ZipfR
spc <- tfl2spc(tfl(chapters.tf))
# check sample size and vocabulary and hapax counts
N(spc)
V(spc)
Vm(spc, 1)

# compute binomially interpolated growth curve
ch.vgc <- vgc.interp(spc,(1:10)*80)

# plot it
plot(ch.vgc)

# compute Zipf-Mandelbrot model from data and look at model summary
zm <- lnre("zm",spc)
zm

# plot observed and expected spectrum
zm.spc <- lnre.spc(zm,N(spc))
plot(spc,zm.spc)

# obtain expected V and V1 values at arbitrary sample sizes
EV(zm,1e+8)
EVm(zm,1,1e+8)

# generate expected V and V1 growth curves up to a sample size
# of 10 million tokens and plot them, with vertical line at 
# estimation size
ext.vgc <- lnre.vgc(zm,(1:100)*1e+5,m.max=1)
plot(ext.vgc,N0=N(zm),add.m=1)

# PART F
# Word Cloud
m <- as.matrix(chapters.stop.tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v), freq=v)

pal <- brewer.pal(9, "BuGn")

wordcloud(words=d$word, freq=d$freq, min.freq = 5, colors=brewer.pal(8, "Dark2"))
wordcloud(words=d$word, freq=d$freq, min.freq = 12, colors=pal[-(1:4)])

#Sentiment Analysis
chp = paste(chp1.content, chp2.content, chp3.content, chp4.content, chp5.content,
               chp6.content, chp7.content, chp8.content, chp9.content, chp10.content)

chp.sentences <- get_sentences(chp)

chp.sentiment <- get_sentiment(chp.sentences, "syuzhet")
chp.bing <- get_sentiment(chp.sentences, "bing")
print(chp.sentiment)
print(chp.bing)

dictionary.sentiment <- get_sentiment_dictionary("syuzhet")
dictionary.bing <- get_sentiment_dictionary("bing")
print(dictionary.sentiment)
print(dictionary.bing)

chp.sentiment.sum <- sum(chp.sentiment)
chp.bing.sum <- sum(chp.bing)
print(chp.sentiment.sum)
print(chp.bing.sum)

chp.sentiment.mean <- mean(chp.sentiment)
chp.bing.mean <- mean(chp.bing)
print(chp.sentiment.mean)
print(chp.bing.mean)

plot(chp.sentiment, main="Syuzhet", xlab="Narrative", ylab="Emotional Valence")
plot(chp.bing, main="Bing", xlab="Narrative", ylab="Emotional Valence")

chp.sentiment.pctvalue10 <- get_percentage_values(chp.sentiment, bins = 10)
structure(chp.sentiment.pctvalue10)
plot(chp.sentiment.pctvalue10, main="PctValue 10 bins", xlab="Narrative", ylab="Emotional Valence", col="blue")

chp.sentiment.pctvalue30 <- get_percentage_values(chp.sentiment, bins = 30)
structure(chp.sentiment.pctvalue30)
plot(chp.sentiment.pctvalue30, main="PctValue 30 bins", xlab="Narrative", ylab="Emotional Valence", col="blue")


# PART G
# Quanteda
chp.stop <- character()
for(i in 1:10){
  chp.stop <- append(chp.stop, chapters.stop[[i]]$content)
}

chp.tokens <- tokens(chp.stop)
print(chp.tokens)

chp.dfm <- dfm(chp.tokens)
print(chp.dfm)

chp.docFreq <- docfreq(chp.dfm)
print(chp.docFreq)

chp.weights <- dfm_weight(chp.dfm)
print(chp.weights)

chp.tfidf <- dfm_tfidf(chp.dfm, scheme_tf="count", scheme_df="inverse")
print(chp.tfidf)

# Stringi
test <- paste(chp.sentences[777], chp.sentences[778])
print(test)

test.reversed <- stri_reverse(test)
print(test.reversed)

test.splitted <- stri_split_boundaries(test, type="sentence")
print(test.splitted)

test.padding <- stri_pad_left(test, width = floor(0.9 * getOption("width")), pad = ">")
print(test.padding)

