# Project 3 starts here
install.packages("tm")
library(tm)

setwd("C:/Users/Admin/Documents/p3docs")
getwd()
docs <- VCorpus(DirSource(".", ignore.case = TRUE, mode="text"))
docs

inspect(docs)
str(docs)


text1 <- docs[[1]]
text1
text1[1]

docsDTM <- DocumentTermMatrix(docs)
docsDTM
inspect(docsDTM)
str(docsDTM)

docsTDM <- TermDocumentMatrix(docs)
docsTDM

text1DF <- data.frame(text1[1])
text1DF
str(text1DF)
str(text1[1])
text1[[1]]
text1DF$content[168]

deleteNumPunct <- function(x) gsub("[^[:alpha:][:space:]]", "", x)
deleteNumPunct

docscl <- tm_map(docs, content_transformer(deleteNumPunct))
docscl[[1]]

docsLow<-tm_map(docscl, tm::content_transformer(tolower))
tm::inspect(docsLow)

tm::inspect(docsLow[[1]])

myStopwords <- c(tm::stopwords("english"))
myStopwords

docstop <- tm::tm_map(docsLow, tm::removeWords, myStopwords)

tm::inspect(docstop[[1]])
typeof(myStopwords)

myStopwords[length(myStopwords) + 1] <- "might"
myStopwords[length(myStopwords) + 1] <- "th"
myStopwords[length(myStopwords) + 1] <- "rd"
myStopwords

#dendrogram starts here
docstop <- tm::tm_map(docsLow, tm::removeWords, myStopwords)
tm::inspect(docstop[[1]])


docstopTDM <- TermDocumentMatrix(docstop)
docstopTDM

freqTerms <- tm::findFreqTerms(docstopTDM, lowfreq = 5)
freqTerms

tm::inspect(docstopTDM)

docsDF <- as.data.frame(docstopTDM[[1]])
docsDist <- dist(docsDF)
docsDG <- hclust(docsDist, method="ward.D2")
str(docsDG)
plot(docsDG)

sparseTerms <- tm::findFreqTerms(docstopTDM, highfreq = 45)
length(sparseTerms)

# Find words with length less than or equal to 2
deletedTerms <- docstopTDM$dimnames$Terms[nchar(docstopTDM$dimnames$Terms) <= 2]

# Length 3
deletedTerms <- docstopTDM$dimnames$Terms[nchar(docstopTDM$dimnames$Terms) <= 3]
deletedTerms

# Length 4
deletedTerms <- docstopTDM$dimnames$Terms[nchar(docstopTDM$dimnames$Terms) <= 4]
deletedTerms

# Length 5
deletedTerms <- docstopTDM$dimnames$Terms[nchar(docstopTDM$dimnames$Terms) <= 5]
deletedTerms

# Length 6
deletedTerms <- docstopTDM$dimnames$Terms[nchar(docstopTDM$dimnames$Terms) <= 6]
deletedTerms

# Length 7
deletedTerms <- docstopTDM$dimnames$Terms[nchar(docstopTDM$dimnames$Terms) <= 7]
deletedTerms

# Length 8
deletedTerms <- docstopTDM$dimnames$Terms[nchar(docstopTDM$dimnames$Terms) <= 8]
deletedTerms

# Length 9
deletedTerms <- docstopTDM$dimnames$Terms[nchar(docstopTDM$dimnames$Terms) <= 10]
deletedTerms

# Length 10
deletedTerms <- docstopTDM$dimnames$Terms[nchar(docstopTDM$dimnames$Terms) <= 10]
deletedTerms

# Length 11
deletedTerms <- docstopTDM$dimnames$Terms[nchar(docstopTDM$dimnames$Terms) <= 11]
deletedTerms

# Length 12
deletedTerms <- docstopTDM$dimnames$Terms[nchar(docstopTDM$dimnames$Terms) <= 12]
deletedTerms

docstop2 <- tm::tm_map(docstop, tm::removeWords, deletedTerms)
str(docstop2)

docs2TDM <- tm::TermDocumentMatrix(docstop2)
docsdf2 <- as.data.frame(docs2TDM[[1]])
docsDist2 <- dist(docsdf2)
docsDG2 <- hclust(docsDist2, method = "ward.D2")
str(docsDG2)
plot(docsDG2)

# Wordcloud
install.packages("wordcloud")
library(wordcloud)

docstf <- tm::termFreq(docstop[[1]])
words <- names(docstf)
words

pal <- brewer.pal(9, "BuGn")
str(pal)

docsWC <- wordcloud(words, docstf, colors = pal[-(1:4)])
str(docsWC)

pal2 <- brewer.pal(9, "Spectral")
docsWC <- wordcloud(words, docstf, colors = pal2)

# zipfR

install.packages("zipfR")
library(zipfR)

install.packages("languageR")
library(languageR)

text2spc.fnc()


terms <- as.data.frame(docsTDM$dimnames$Terms)
terms
freq <- table(terms)
freq

terms.spc <- spc(sort(freq,decreasing = TRUE))
summary(terms.spc)

# N, V and Vm are generic methods that can (and should) be used to access observed frequency data
# N returns the sample size, V the vocabulary size, and Vm one or more selected elements of the frequency spectrum
N(terms.spc)
V(terms.spc)
Vm(terms.spc,1)

?N

terms.spc <- text2spc.fnc(terms.spc)

# type(zm, fzm, gigp)	class of LNRE model to use
# The Zipf-Mandelbrot (ZM) LNRE model
zm <- lnre("zm", terms.spc)
zm

# The finite Zipf-Mandelbrot (fZM) LNRE model
fzm <- lnre("fzm", terms.spc)
fzm

# The Generalized Inverse Gauss-Poisson (GIGP) LNRE model
gigp <- lnre("zm", terms.spc)
gigp


# tf-idf using quanteda
install.packages("quanteda")
library(quanteda)

docstext <- docscl[[1]]
docstext$content[1:10]

docscorp <- quanteda::corpus(docscl)
docscorp

docstokens <- quanteda::tokens(docscorp)
str(docstokens)

docstokens <- tokens_remove(docstokens, pattern = stopwords("en"))
docstokens

docsDFM <- quanteda::dfm(docstokens)
str(docsDFM)
?quanteda::dfm


docsFreq <- quanteda::docfreq(docsDFM)
str(docsFreq)
docsFreq

docsweights <- quanteda::dfm_weight(docsDFM)
str(docsweights)

docsweights

docsTFIDF <- quanteda::dfm_tfidf(docsDFM, scheme_tf = "count", scheme_df = "inverse")
str(docsTFIDF)

kwic_context <- kwic(docstokens, pattern = "believe")
kwic_context

top_features <- topfeatures(docsDFM, 20)
top_features

find_features <- dfm_match(docsDFM, c("captain", "land"))
find_features

# sentiment analysis using syuzhet package
install.packages("syuzhet")
library(syuzhet)

c1 <- get_text_as_string("CHAPTER1.txt")
c2 <- get_text_as_string("CHAPTER2.txt")
c3 <- get_text_as_string("CHAPTER3.txt")
c4 <- get_text_as_string("CHAPTER4.txt")
c5 <- get_text_as_string("CHAPTER5.txt")
c6 <- get_text_as_string("CHAPTER6.txt")
c7 <- get_text_as_string("CHAPTER7.txt")
c8 <- get_text_as_string("CHAPTER8.txt")
c9 <- get_text_as_string("CHAPTER9.txt")
c10 <- get_text_as_string("CHAPTER10.txt")

docsSTR <- paste(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, sep = " ")
docsSTR

ds <- get_sentences(docsSTR)
head(ds)

dsSent <- get_sentiment(ds, "syuzhet")
dsSent

dsBing <- get_sentiment(ds, "bing")
dsBing

dsDict <- get_sentiment_dictionary()
dsDict

dsDictBing <- get_sentiment_dictionary("bing")
dsDictBing

dsSum <- sum(dsSent)
dsSum


dsSumBing <- sum(dsBing)
dsSumBing

dsMean <- mean(dsSent)
dsMean


dsMeanBing <- mean(dsBing)
dsMeanBing

plot(dsSent, main = "Twenty Thousand Leagues Under The Sea Plot Trajectory: syuzhet", xlab = "Narrative", ylab = "Emotional valence")

plot(dsBing, main = "Twenty Thousand Leagues Under The Sea Plot Trajectory: Bing", xlab = "Narrative", ylab = "Emotional valence")

dsSentPVal <- get_percentage_values(dsSent, bins = 10)
structure(dsSentPVal)
plot(dsSentPVal, main = "Twenty Thousand Leagues Under The Sea PCTValue 10 bins", xlab = "Narrative", ylab = "Emotional valence")

dsSentPVal <- get_percentage_values(dsSent, bins = 20)
structure(dsSentPVal)
plot(dsSentPVal, main = "Twenty Thousand Leagues Under The Sea PCTValue 20 bins", xlab = "Narrative", ylab = "Emotional valence")


install.packages("tokenizers")
library(tokenizers)

longest_words <- function(doc, limit = 10){
  words_doc = c()
  
  w <- tokenizers::tokenize_words(doc$content)
  
  for (i in 1:length(w)) {
    l <- w[[i]]
    if(length(l) != 0){
      for(j in 1:length(l)){
        words_doc[length(words_doc) + 1] <- l[j]
      }
    }
  }
  
  uwords <- unique(words_doc)
  uwords[order(-nchar(uwords), uwords)][1:limit]
}

longest_sentences <- function(doc, limit = 10){
  sentences <- tokenizers::tokenize_sentences(paste(doc$content,collapse=" "))[[1]]
  
  sentences[order(-nchar(sentences), sentences)][1:limit]
}

longest_sentences(docs[[10]])

# Words table
chapter_names <- c("Chapter 1", "Chapter 2", "Chapter 3", "Chapter 4", "Chapter 5",
                   "Chapter 6", "Chapter 7", "Chapter 8", "Chapter 9", "Chapter 10")

words_table <- data.frame("Chapters" = character(0), "Word 1" = character(0), "Word 2" = character(0),
           "Word 3" = character(0), "Word 4" = character(0), "Word 5" = character(0),
           "Word 6" = character(0), "Word 7" = character(0), "Word 8" = character(0),
           "Word 9" = character(0), "Word 10" = character(0))



for (i in 1:length(docs)) {
  words_table[nrow(words_table) + 1,] = c(chapter_names[i],longest_words(docs[[i]]))
}

words_table
#install.packages("gridExtra")
#library(gridExtra)
gridExtra::grid.table(words_table)

# sentences table
sentences_table <- data.frame("Chapters" = character(0), "Sentence 1" = character(0), "Sentence 2" = character(0),
                          "Sentence 3" = character(0), "Sentence 4" = character(0), "Sentence 5" = character(0),
                          "Sentence 6" = character(0), "Sentence 7" = character(0), "Sentence 8" = character(0),
                          "Sentence 9" = character(0), "Sentence 10" = character(0))


for (i in 1:length(docs)) {
  sentences_table[nrow(sentences_table) + 1,] = c(chapter_names[i],longest_sentences(docs[[i]]))
}

sentences_table

install.packages("ggplot2")
library(ggplot2)
# openNLP
install.packages("openNLP")
library(openNLP)

s <- longest_sentences(docs[[1]])[1]
nchar(s)
s
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))

pos_tag_annotator <- Maxent_POS_Tag_Annotator()
pos_tag_annotator
a3 <- annotate(s, pos_tag_annotator, a2)
a4 <- a3[2:length(a3)]
a4<-a4[(a4$end - a4$start + 1) >= 5 & (a4$features[[1]]$POS == "VBG" | a4$features[[1]]$POS == "VBP")]
length(a4)
typeof(a4[[1]])
str(a4)
df1 <- as.data.frame(a3)

nrow(df1)
nouns = c()
df2 <- data.frame()
for (i in 1:length(a4)) {
  if(a4[i]$features[[1]]$POS == "NN"){
    nouns[length(nouns) + 1] <- a4[i]
  }
}
df1[20,]$features[[1]]$POS
as.character(a4[1]$features)
nouns
a4[1]
length(a4)
a4[2]$features[[1]]$POS == "VBG"
a4[a4$features[[1]]$POS == "VBG"]
   
as.data.frame(a3)$features

a4<-a4[(a4$end - a4$start + 1) >= 5]
length(a4)
typeof(a4)
str(a4)
wordTagsFinal <-sapply(a4$features, '[[', "POS")
a5 <- cbind(a4, wordTagsFinal)
a5
str(a4)
a4[[1]]

lapply(a4$features, `[[`, "POS")
pos <- sapply(a4$features, "[[", "POS")

words <- vector("list", 20)
words
a5 <- a3
e = 1
for (i in 1:length(a4)) {
  if(a4[i]$features[[1]]$POS != "NN"){
    words[[e]] <- a4[[i]]
    e = e + 1
  }
}


install.packages("dplyr")
library(dplyr)


install.packages("stringr")
library(stringr)

find_nouns <-function(s){
  z=1
  s<-str_replace_all(s,"--"," ")
  start_noun<-c()
  end_noun<-c()
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  #pos_tag_annotator
  a3 <- annotate(s, pos_tag_annotator, a2)
  a4 <- a3[2:length(a3)]
  print(length(a4))
  for(i in 1:length(a4)) {
    if( ((a4[i]$features[[1]]$POS=="NN")==TRUE) | ((a4[i]$features[[1]]$POS=="NNS")==TRUE) | ((a4[i]$features[[1]]$POS=="NNP")==TRUE)  
        | ((a4[i]$features[[1]]$POS=="NNPS")==TRUE)  ) {
      start_noun[z] <- a4[i]$start
      end_noun[z]<- a4[i]$end
      z=z+1
    }
  }
  noun_of_sentence<-c()
  q=1
  #length(start_noun)
  for(i in 1:length(start_noun)){
    #print("YESS")
    if(end_noun[i]-start_noun[i]+1>=5){
      noun_of_sentence[q] <- substr(s, start_noun[i],end_noun[i])
      q=q+1
    }
  }
  noun_of_sentence
}
s <- longest_sentences(docs[[1]])[1]
find_nouns("hello hahah")

find_verbs <-function(s){
  z=1
  s<-str_replace_all(s,"--"," ")
  start_verb<-c()
  end_verb<-c()
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  #pos_tag_annotator
  a3 <- annotate(s, pos_tag_annotator, a2)
  a4 <- a3[2:length(a3)]
  for(i in 1:length(a4)) {
    if( ((a4[i]$features[[1]]$POS=="VB")==TRUE) | ((a4[i]$features[[1]]$POS=="VBD")==TRUE) | ((a4[i]$features[[1]]$POS=="VBG")==TRUE)  
        | ((a4[i]$features[[1]]$POS=="VBN")==TRUE) |  ((a4[i]$features[[1]]$POS=="VBP")==TRUE) | ((a4[i]$features[[1]]$POS=="VBZ")==TRUE) |
        ((a4[i]$features[[1]]$POS=="MD")==TRUE) | ((a4[i]$features[[1]]$POS=="TO")==TRUE)) {
      start_verb[z] <- a4[i]$start
      end_verb[z]<- a4[i]$end
      z=z+1
    }
  }
  verb_of_sentence<-c()
  q=1
  for(i in 1:length(start_verb)){
    if(end_verb[i]-start_verb[i]+1>=5){
      verb_of_sentence[q] <- substr(s, start_verb[i],end_verb[i])
      q=q+1
    }
  }
  print(verb_of_sentence)
}
s <- longest_sentences(docs[[5]])[7]
s
find_nouns(s)
find_verbs(s)
s
a
nouns <- array(array())
nouns[1] = 2
nouns

l <- vector("list", 100)
i = 1
for (d in 1:length(docs)) {
  lsentences <- longest_sentences(docs[[d]])
  for(s in 1:length(lsentences)){
    l[[i]] <- find_nouns(lsentences[s])
    print(i)
    i = i + 1
  }
}

l

l <- vector("list", 100)
i = 1
for (d in 1:length(docs)) {
lsentences <- longest_sentences(docs[[d]])
for(s in 1:length(lsentences)){
  l[[i]] <- find_verbs(lsentences[s])
  print(i)
  i = i + 1
}
}

l

# quanteda
# Function 1 # dfm
# Construct a sparse document-feature matrix, from a character, corpus, tokens, or even other dfm
# object.
dfm(clChapter1)
# Function 2 # tokens
#Tokenize the texts from a character vector or from a corpus.
clChapter1Tokens <- quanteda::tokens(clChapter1)
clChapter1Tokens
# Function 3 #
# tokens_tolower and tokens_toupper convert the features of a tokens object and re-index the
# types.
toks <- tokens_tolower(clChapter1Tokens)


# stringi
# Function 1 
# count number of occurences of pattern in a string
stri_count(clChapter1, fixed="the")
# Function 2 
# detecting char set and language
stri_enc_detect(clChapter1)
# Function 3
# padding and wrapping strings
cat(stri_pad(stri_wrap(clChapter1), side='both'), sep="\n")
