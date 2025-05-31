install.packages("tm")
install.packages("readtext")
install.packages("stringi")
install.packages("quanteda")
Yinstall.packages("openNLP")
install.packages("wordcloud")
install.packages("stringr")
install.packages("tokenizers")
install.packages("data.table")
install.packages("gridExtra")
install.packages("grid")
install.packages("syuzhet")
install.packages("zipfR")
install.packages("writexl")
install.packages("readxl")

library(tm)
library(readtext)
library(stringi)
library(quanteda)
library(openNLP)
library(wordcloud)
library(stringr)
library(tokenizers)
library(data.table)
library(gridExtra)
library(grid)
library(syuzhet)
library(zipfR)
library(writexl)
library(readxl)

# setting current directory to the working folder
setwd("/Users/leylaaliyeva/Desktop/ley3")
getwd()

# reading data as corpus
gutenberg_corpus <- VCorpus(DirSource("Data/",ignore.case = TRUE, mode = "text"))
gutenberg_corpus


# *** task (1.a) *** 

# writing function to find 10 longest words 
findTenLongestWords <- function(tokens) {
  # creating empty vector
  words_list <- c()
  
  # reading all tokens one by one
  for(each in tokens){
    if(length(each)>0){
      for (word in each) {
        # adding all tokenized words into temporary word list vector
        words_list[length(words_list)+1] <- word;
      }
    }
  }
  
  # sorting words by their length
  tmp_sorted <- words_list[order(nchar(words_list), words_list)] 
  
  # removing duplicate words in the list
  tmp_sorted <- unique(tmp_sorted)
  
  # getting the last 10 elements from sorted list
  return(tail(tmp_sorted, 10));
} 

# writing function to assign 10 longest words from corpus
findLongestWordsFromCorpus <- function(){
  
  # finding 10 longest words in corpus 1
  corpus1 <- list("id" = gutenberg_corpus[[1]]$meta$id, "words" = findTenLongestWords(tokenize_words(gutenberg_corpus[[1]]$content)))
  # finding 10 longest words in corpus 2
  corpus2 <- list("id" = gutenberg_corpus[[2]]$meta$id, "words" = findTenLongestWords(tokenize_words(gutenberg_corpus[[2]]$content)))
  # finding 10 longest words in corpus 3
  corpus3 <- list("id" = gutenberg_corpus[[3]]$meta$id, "words" = findTenLongestWords(tokenize_words(gutenberg_corpus[[3]]$content)))
  # finding 10 longest words in corpus 4
  corpus4 <- list("id" = gutenberg_corpus[[4]]$meta$id, "words" = findTenLongestWords(tokenize_words(gutenberg_corpus[[4]]$content)))
  # finding 10 longest words in corpus 5
  corpus5 <- list("id" = gutenberg_corpus[[5]]$meta$id, "words" = findTenLongestWords(tokenize_words(gutenberg_corpus[[5]]$content)))
  # finding 10 longest words in corpus 6
  corpus6 <- list("id" = gutenberg_corpus[[6]]$meta$id, "words" = findTenLongestWords(tokenize_words(gutenberg_corpus[[6]]$content)))
  # finding 10 longest words in corpus 7
  corpus7 <- list("id" = gutenberg_corpus[[7]]$meta$id, "words" = findTenLongestWords(tokenize_words(gutenberg_corpus[[7]]$content)))
  # finding 10 longest words in corpus 8
  corpus8 <- list("id" = gutenberg_corpus[[8]]$meta$id, "words" = findTenLongestWords(tokenize_words(gutenberg_corpus[[8]]$content)))
  # finding 10 longest words in corpus 9
  corpus9 <- list("id" = gutenberg_corpus[[9]]$meta$id, "words" = findTenLongestWords(tokenize_words(gutenberg_corpus[[9]]$content)))
  # finding 10 longest words in corpus 10
  corpus10 <- list("id" = gutenberg_corpus[[10]]$meta$id, "words" = findTenLongestWords(tokenize_words(gutenberg_corpus[[10]]$content)))
  
  # preparing a table 
  table <- c(c1 = corpus1$words, c2 = corpus2$words,c3 = corpus3$words,c4 = corpus4$words,c5 = corpus5$words,c6 = corpus6$words,c7 = corpus7$words,c8 = corpus8$words,c9 = corpus9$words,c10 = corpus10$words)
  
  # creating a table of data
  data_table <- data.table(c1 = corpus1$words, c2 = corpus2$words,c3 = corpus3$words,c4 = corpus4$words,c5 = corpus5$words,c6 = corpus6$words,c7 = corpus7$words,c8 = corpus8$words,c9 = corpus9$words,c10 = corpus10$words)
  
  # visualizing table in grid view 
  grid.table(data_table)

  return(table)
}

# writing a fuction to truncate sentences 
truncSent <- function(tmp_table){
  #creating an empty vector
  dummy_list <- c()
  #fetching each value of the table
  for(each in tmp_table){
    dummy_list[length(dummy_list) + 1] <-  str_trunc(each,15,)
  }
  return(dummy_list)
}

# finding 10 longest sentenctes
findLongestSentencesFromCorpus <- function(){
  # getting 10 longest sentences from corpus 1 
  corpus1_sen <- list("id" = gutenberg_corpus[[1]]$meta$id, "sentences" = findTenLongestWords(tokenize_sentences(paste(gutenberg_corpus[[1]]$content,collapse = " "))))
  # getting 10 longest sentences from corpus 2 
  corpus2_sen <- list("id" = gutenberg_corpus[[2]]$meta$id, "sentences" = findTenLongestWords(tokenize_sentences(paste(gutenberg_corpus[[2]]$content,collapse = " "))))
  # getting 10 longest sentences from corpus 3 
  corpus3_sen <- list("id" = gutenberg_corpus[[3]]$meta$id, "sentences" = findTenLongestWords(tokenize_sentences(paste(gutenberg_corpus[[3]]$content,collapse = " "))))
  # getting 10 longest sentences from corpus 4 
  corpus4_sen <- list("id" = gutenberg_corpus[[4]]$meta$id, "sentences" = findTenLongestWords(tokenize_sentences(paste(gutenberg_corpus[[4]]$content,collapse = " "))))
  # getting 10 longest sentences from corpus 5 
  corpus5_sen <- list("id" = gutenberg_corpus[[5]]$meta$id, "sentences" = findTenLongestWords(tokenize_sentences(paste(gutenberg_corpus[[5]]$content,collapse = " "))))
  # getting 10 longest sentences from corpus 6 
  corpus6_sen <- list("id" = gutenberg_corpus[[6]]$meta$id, "sentences" = findTenLongestWords(tokenize_sentences(paste(gutenberg_corpus[[6]]$content,collapse = " "))))
  # getting 10 longest sentences from corpus 7 
  corpus7_sen <- list("id" = gutenberg_corpus[[7]]$meta$id, "sentences" = findTenLongestWords(tokenize_sentences(paste(gutenberg_corpus[[7]]$content,collapse = " "))))
  # getting 10 longest sentences from corpus 8 
  corpus8_sen <- list("id" = gutenberg_corpus[[8]]$meta$id, "sentences" = findTenLongestWords(tokenize_sentences(paste(gutenberg_corpus[[8]]$content,collapse = " "))))
  # getting 10 longest sentences from corpus 9 
  corpus9_sen <- list("id" = gutenberg_corpus[[9]]$meta$id, "sentences" = findTenLongestWords(tokenize_sentences(paste(gutenberg_corpus[[9]]$content,collapse = " "))))
  # getting 10 longest sentences from corpus 10 
  corpus10_sen <- list("id" = gutenberg_corpus[[10]]$meta$id, "sentences" = findTenLongestWords(tokenize_sentences(paste(gutenberg_corpus[[10]]$content,collapse = " "))))
  
  # adding all the elements to one table
  table <- list(corpus1_sen, corpus2_sen, corpus3_sen, corpus4_sen, corpus5_sen, corpus6_sen, corpus7_sen, corpus8_sen, corpus9_sen, corpus10_sen)
  
  # truncating longest sentences accordingly
  corpus1_t <- truncSent(corpus1_sen$sentences)
  corpus2_t <- truncSent(corpus2_sen$sentences)
  corpus3_t <- truncSent(corpus3_sen$sentences)
  corpus4_t <- truncSent(corpus4_sen$sentences)
  corpus5_t <- truncSent(corpus5_sen$sentences)
  corpus6_t <- truncSent(corpus6_sen$sentences)
  corpus7_t <- truncSent(corpus7_sen$sentences)
  corpus8_t <- truncSent(corpus8_sen$sentences)
  corpus9_t <- truncSent(corpus9_sen$sentences)
  corpus10_t <- truncSent(corpus10_sen$sentences)
  
  # creating a table of data
  data_table <- data.table(c1 = corpus1_t, c2 = corpus2_t, c3 = corpus3_t, c4 = corpus4_t, c5 = corpus5_t, c6 = corpus6_t, c7 = corpus7_t, c8 = corpus8_t, c9 = corpus9_t, c10 = corpus10_t)
  
  # visualizing generated table
  grid.table(data_table)
  
  return(table)
}

# finding 10 longest word of each chapter
gutenbergTenLongestWords <- findLongestWordsFromCorpus()
gutenbergTenLongestSentences <- findLongestSentencesFromCorpus()

gutenbergTenLongestWords
gutenbergTenLongestSentences

# *** task (1.b) *** 

# displaying summary elements for each corpus
inspect(gutenberg_corpus)
gutenberg_corpus[[1]]
gutenberg_corpus[[1]]$meta

# extracting the first document from corpus
doc1 <- gutenberg_corpus[[1]]
#displaying the content and meta data of document
doc1
# displaying the content of document 1
doc1$content

#calculating Document Term Matrix (DTM) where documents are rows, and terms are columns
gutenberg_DTM <- DocumentTermMatrix(gutenberg_corpus)
#displaying the sparsity, maximal length, weights of DTM
gutenberg_DTM
#displaying DTM as well
inspect(gutenberg_DTM)
#displaying structure
str(gutenberg_DTM)

# calculating Term Document Matrix TDM where rows are words and columns are documents
gutenberg_TDM <- TermDocumentMatrix(gutenberg_corpus)
gutenberg_TDM
inspect(gutenberg_TDM)

# creating data frame from the corpus
doc1_df <- data.frame(doc1$content)
doc1_df[1]

# Corpus Cleansing – Data Wrangling

# defining a function to remove numbers and punctuation
removeNumPunct <-function(x) gsub("[^[:alpha:][:space:]]*","",x)
removeNumPunct

# remove numbers and punctuation, to make clean corpus
gutenberg_corpus_clean <- tm_map(gutenberg_corpus, content_transformer(removeNumPunct))
#printing both versions of corpus to compare
gutenberg_corpus_clean[[2]]
gutenberg_corpus[[2]]

# making everything in lower case
gutenberg_corpus_clean_low <- tm_map(gutenberg_corpus_clean, content_transformer(tolower))
inspect(gutenberg_corpus_clean_low)
inspect(gutenberg_corpus_clean_low[[2]])

# getting the stop words for english lang from tm library
myStopWords <- c(stopwords("en"))
myStopWords

# removing stop words from chapters
gutenberg_corpus_stop <- tm_map(gutenberg_corpus_clean_low, removeWords, myStopWords)
inspect(gutenberg_corpus_stop[[2]])

# creating TDM again
gutenberg_corpus_stop_TDM <- TermDocumentMatrix(gutenberg_corpus_stop)
gutenberg_corpus_stop_TDM

# finding frequent terms
freqTerms <- findFreqTerms(gutenberg_corpus_stop_TDM, lowfreq = 5)
freqTerms

# creating term frequencies of corpis that is cleaned from stopwords
gutenberg_corpus_stop_tf <- termFreq(gutenberg_corpus_stop[[1]])
gutenberg_corpus_stop_tf

# inspecting TDM cleaned from stopwords
inspect(gutenberg_corpus_stop_TDM)

# Drawing a dendogram

# creating a data frame
gutenberg_corpus_stop_df <- as.data.frame(gutenberg_corpus_stop_TDM[[1]])
gutenberg_corpus_stop_df

# calculating distances
gutenberg_corpus_stop_dist <- dist(gutenberg_corpus_stop_df)
gutenberg_corpus_stop_dist

# clustering the data for our dendrogram
gutenberg_corpus_stop_dend <- hclust(gutenberg_corpus_stop_dist, method = "ward.D2")

# getting the structure of object
str(gutenberg_corpus_stop_dend)

# visualizing via plot function
plot(gutenberg_corpus_stop_dend)

# eliminating some more sparse words as graphics are not appropriate
newStopWords = c("even", "a", "will", "may", "soon", "can", "as", "one", "much", "just","now", "quite","merely", "shall","take","certain","well")

# removing more sparse words
gutenberg_corpus_stop_2 <- tm_map(gutenberg_corpus_stop, removeWords, newStopWords)
str(gutenberg_corpus_stop_2)

# recreating a new TDM, data frame, and distance matrix.
gutenberg_corpus_stop_2_TDM <- TermDocumentMatrix(gutenberg_corpus_stop_2)
#making a data frame from newly created TDM
gutenberg_corpus_stop_2_df <- as.data.frame(gutenberg_corpus_stop_2_TDM[[1]])

# calculating distances
gutenberg_corpus_stop_2_dist <- dist(gutenberg_corpus_stop_2_df)

# cluster the data for dendrogram
gutenberg_corpus_stop_2_den <- hclust(gutenberg_corpus_stop_2_dist, method = "ward.D2")
# getting the structure of object
str(gutenberg_corpus_stop_2_den)
# visualizing via plot function
plot(gutenberg_corpus_stop_2_den)

#pretty much the same thing, therefore, continue cleansing

# removing more sparse terms, just make sparsity negligible 
# to see the dendrogram
gutenberg_sparse <- removeSparseTerms(gutenberg_corpus_stop_2_TDM,sparse = 0.2)
inspect(gutenberg_sparse)
gutenberg_sparse_df <- as.data.frame(gutenberg_sparse[[1]])
gutenberg_sparse_dist <- dist(gutenberg_sparse_df)
gutenberg_sparse_den <- hclust(gutenberg_sparse_dist, method = "ward.D2")
#getting the structure of cleaned object
str(gutenberg_sparse_den)
#visualizing the cleaned object
plot(gutenberg_sparse_den)

#the rest of the functions from the filee are used in the below tasks


# *** task (1.c) *** 

# functions to remove words of length n

# remove words shorter than 2
removeWordsSmaller2 <- function(x) gsub("\\b\\w{1,2}\\b\\s+","",x)
# apply the function to remove words shorter than 2
tmp_new_2 <- tm_map(gutenberg_corpus_stop, content_transformer(removeWordsSmaller2))
tmp_new_2[[1]]$content
# creating TDM
gutenberg_new_TDM_2 <- TermDocumentMatrix(tmp_new_2)
# making data frame
gutenberg_new_2_df <- as.data.frame(gutenberg_new_TDM_2[[1]])
# calculating distances
gutenberg_new_2_dist <- dist(gutenberg_new_2_df)
# clustering the data for dendrogram
gutenberg_new_2_den <- hclust(gutenberg_new_2_dist, method = "ward.D2")
# visualize via plot function
plot(gutenberg_new_2_den)

# remove words shorter than 3, repeating the same operations with 3
removeWordsSmaller3 <- function(x) gsub("\\b\\w{1,3}\\b\\s+","",x)
tmp_new_3 <- tm_map(gutenberg_corpus_stop, content_transformer(removeWordsSmaller3))
gutenberg_new_TDM_3 <- TermDocumentMatrix(tmp_new_3)
gutenberg_new_3_df <- as.data.frame(gutenberg_new_TDM_3[[1]])
gutenberg_new_3_dist <- dist(gutenberg_new_3_df)
gutenberg_new_3_den <- hclust(gutenberg_new_3_dist, method = "ward.D2")
plot(gutenberg_new_3_den)

# remove words shorter than 4, repeating the same operations with 4
removeWordsSmaller4 <- function(x) gsub("\\b\\w{1,4}\\b\\s+","",x)
tmp_new_4 <- tm_map(gutenberg_corpus_stop, content_transformer(removeWordsSmaller4))
gutenberg_new_TDM_4 <- TermDocumentMatrix(tmp_new_4)
gutenberg_new_4_df <- as.data.frame(gutenberg_new_TDM_4[[1]])
gutenberg_new_4_dist <- dist(gutenberg_new_4_df)
gutenberg_new_4_den <- hclust(gutenberg_new_4_dist, method = "ward.D2")
plot(gutenberg_new_4_den)

# remove words shorter than 5, repeating the same operations with 5
removeWordsSmaller5 <- function(x) gsub("\\b\\w{1,5}\\b\\s+","",x)
tmp_new_5 <- tm_map(gutenberg_corpus_stop, content_transformer(removeWordsSmaller5))
gutenberg_new_TDM_5 <- TermDocumentMatrix(tmp_new_5)
gutenberg_new_5_df <- as.data.frame(gutenberg_new_TDM_5[[1]])
gutenberg_new_5_dist <- dist(gutenberg_new_5_df)
gutenberg_new_5_den <- hclust(gutenberg_new_5_dist, method = "ward.D2")
plot(gutenberg_new_5_den)


# remove words shorter than 6, repeating the same operations with 6
removeWordsSmaller6 <- function(x) gsub("\\b\\w{1,6}\\b\\s+","",x)
tmp_new_6 <- tm_map(gutenberg_corpus_stop, content_transformer(removeWordsSmaller6))
gutenberg_new_TDM_6 <- TermDocumentMatrix(tmp_new_6)
gutenberg_new_6_df <- as.data.frame(gutenberg_new_TDM_6[[1]])
gutenberg_new_6_dist <- dist(gutenberg_new_6_df)
gutenberg_new_6_den <- hclust(gutenberg_new_6_dist, method = "ward.D2")
plot(gutenberg_new_6_den)

# remove words shorter than 7, repeating the same operations with 7
removeWordsSmaller7 <- function(x) gsub("\\b\\w{1,7}\\b\\s+","",x)
tmp_new_7 <- tm_map(gutenberg_corpus_stop, content_transformer(removeWordsSmaller7))
gutenberg_new_TDM_7 <- TermDocumentMatrix(tmp_new_7)
gutenberg_new_7_df <- as.data.frame(gutenberg_new_TDM_7[[1]])
gutenberg_new_7_dist <- dist(gutenberg_new_7_df)
gutenberg_new_7_den <- hclust(gutenberg_new_7_dist, method = "ward.D2")
plot(gutenberg_new_7_den)

# remove words shorter than 8, repeating the same operations with 8
removeWordsSmaller8 <- function(x) gsub("\\b\\w{1,8}\\b\\s+","",x)
tmp_new_8 <- tm_map(gutenberg_corpus_stop, content_transformer(removeWordsSmaller8))
gutenberg_new_TDM_8 <- TermDocumentMatrix(tmp_new_8)
gutenberg_new_8_df <- as.data.frame(gutenberg_new_TDM_8[[1]])
gutenberg_new_8_dist <- dist(gutenberg_new_8_df)
gutenberg_new_8_den <- hclust(gutenberg_new_8_dist, method = "ward.D2")
plot(gutenberg_new_8_den)

# remove words shorter than 9, repeating the same operations with 9
removeWordsSmaller9 <- function(x) gsub("\\b\\w{1,9}\\b","",x)
tmp_new_9 <- tm_map(gutenberg_corpus_stop, content_transformer(removeWordsSmaller9))
gutenberg_new_TDM_9 <- TermDocumentMatrix(tmp_new_9)
gutenberg_new_9_df <- as.data.frame(gutenberg_new_TDM_9[[1]])
gutenberg_new_9_dist <- dist(gutenberg_new_9_df)
gutenberg_new_9_den <- hclust(gutenberg_new_9_dist, method = "ward.D2")
plot(gutenberg_new_9_den)

# remove words shorter than 10, repeating the same operations with 10
removeWordsSmaller10 <- function(x) gsub("\\b\\w{1,10}\\b","",x)
tmp_new_10 <- tm_map(gutenberg_corpus_stop, content_transformer(removeWordsSmaller10))
gutenberg_new_TDM_10 <- TermDocumentMatrix(tmp_new_10)
gutenberg_new_10_df <- as.data.frame(gutenberg_new_TDM_10[[1]])
gutenberg_new_10_dist <- dist(gutenberg_new_10_df)
gutenberg_new_10_den <- hclust(gutenberg_new_10_dist, method = "ward.D2")
plot(gutenberg_new_10_den)

#remove words shorter than 11, repeating the same operations with 11
removeWordsSmaller11 <- function(x) gsub("\\b\\w{1,11}\\b","",x)
tmp_new_11 <- tm_map(gutenberg_corpus_stop, content_transformer(removeWordsSmaller11))
gutenberg_new_TDM_11 <- TermDocumentMatrix(tmp_new_11)
gutenberg_new_11_df <- as.data.frame(gutenberg_new_TDM_11[[1]])
gutenberg_new_11_dist <- dist(gutenberg_new_11_df)
gutenberg_new_11_den <- hclust(gutenberg_new_11_dist, method = "ward.D2")
plot(gutenberg_new_11_den)

#remove words shorter than 12, repeating the same operations with 12
removeWordsSmaller12 <- function(x) gsub("\\b\\w{1,12}\\b","",x)
tmp_new_12 <- tm_map(gutenberg_corpus_stop, content_transformer(removeWordsSmaller12))
gutenberg_new_TDM_12 <- TermDocumentMatrix(tmp_new_12)
gutenberg_new_12_df <- as.data.frame(gutenberg_new_TDM_12[[1]])
gutenberg_new_12_dist <- dist(gutenberg_new_12_df)
gutenberg_new_12_den <- hclust(gutenberg_new_12_dist, method = "ward.D2")
plot(gutenberg_new_12_den)


# *** Task (1.d) ***
#defining part of speechs of 10 longest sentences found in part a for nouns and verbs having a length of 5 or greater
# converting previously found 10 longest sentences to Vcorpus
corpus_longestSent <- as.VCorpus(gutenbergTenLongestSentences)
corpus_longestSent

# getting strings separately from the sentences 
getStrings = function(x){ 
  my_str = ''
  for (val in 1:10)
  {
    my_str = my_str + as.String(x[[val]]$sentences)
  }
  return (my_str)}

corpus_longestSent.string <- getStrings(corpus_longestSent)  

# this section includes a funtion to get pos taggings of words for specific regex

# first agrument x is a string (10 sentences), second argument is regex for POS. 
# the return values will be used to make our table and omit words with length <5
get_POS_words  <-  function(x,POSregex) {
  wordAnnotation <- annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
  POSAnnotation <- annotate(x, Maxent_POS_Tag_Annotator(), wordAnnotation)
  POSwords <- subset(POSAnnotation, type == "word")
  tags <- sapply(POSwords$features, '[[', "POS")
  POSindex <- grep(POSregex, tags)
  words = c(x[POSwords][POSindex])
  tags =  c(tags[POSindex])
  output<-list(words,tags)
  return (output) }

# getting nouns and tags of them, assigning "NN" for nouns
my_nouns <- get_POS_words(corpus_longestSent.string,'NN')
noun_words <- my_nouns[1]
noun_tags <- my_nouns[2]
#printing the results
noun_words
noun_tags

# getting verbs and tags of them, , assigning "VB" for verbs
my_verbs <- get_POS_words(corpus_longestSent.string,'VB')
verb_words <- my_verbs[1]
verb_tags <- my_verbs[2]
#printing the results
verb_words
verb_tags

# as tags also has additional information such as if noun is plural etc
noun_type <-rep(c('Noun'),each=lengths(noun_words))
# as tags also has additional information such as if verb is in present or past etc 
verb_type <-rep(c('Verb'),each=lengths(verb_words))


# combining previously created vectors of words to add to data frame
all_words <- c(noun_words, verb_words)
all_words <- Reduce(c,all_words)
# combining previously created vectors of tags to add to data frame
all_tags = c(noun_tags, verb_tags)
all_tags <- Reduce(c,all_tags)
# combining previously created vectors of types to add to data frame
all_types = c(noun_type,verb_type)
all_types = Reduce(c,all_types)

# creating data frame from words,tags, and their types
corpus_longestSent.df <- data.frame (Word  = all_words, Tag = all_tags, Type = all_types, stringsAsFactors = FALSE )
corpus_longestSent.df

# removing the words with the length less than five
corpus_longestSent.filtered <- corpus_longestSent.df[(nchar(corpus_longestSent.df$Word) > 5),]
corpus_longestSent.filtered

# converting it to data table
corpus_longestSent.dt <- as.data.table(corpus_longestSent.filtered)
# printing head and tail of data table 
corpus_longestSent.dt

# store the table in local working folder
write_xlsx(corpus_longestSent.filtered,"table.xlsx")

# reading the savev file 
table <- read_excel("table.xlsx")
#printing the previous file
table

# *** Task (1.e) ***
task_e <- gutenberg_corpus_stop
task_e

# analyzing word frequency using functions from package zipfR
#creating function to use for each chapter
getZiprf <- function(doc){
  freq <- termFreq(doc)
  task_e.spc <- spc(sort(freq,decreasing = TRUE))
  #getting Zipf-Mandelbrot LNRE model parameters
  zm <- lnre("zm",task_e.spc)
  #printing shape, upper cutoff, normalization parameters
  print(zm)
  print("***************************************************")
  
  #getting finite Zipf-Mandelbrot LNRE model parameters
  fzm <- lnre("fzm",task_e.spc)
  #printing shape, upper cutoff, normalization parameters
  print(fzm)
  print("***************************************************")
  
  #getting Generalized Inverse Gauss-Poisson (GIGP) LNRE model
  fzm <- lnre("gigp",task_e.spc)
  #printing shape, lower decay,upper decay, zipf size parameters
  print(fzm)
}

#applying Ziprf function to each chapter and getting the result parameters
getZiprf(task_e[[1]])
getZiprf(task_e[[2]])
getZiprf(task_e[[3]])
getZiprf(task_e[[4]])
getZiprf(task_e[[5]])
getZiprf(task_e[[6]])
getZiprf(task_e[[7]])
getZiprf(task_e[[8]])
getZiprf(task_e[[9]])
getZiprf(task_e[[10]])


# *** Task (1.f) *** 

gutenberg_corpus_stop_tf <- termFreq(gutenberg_corpus_stop[[3]])
gutenberg_corpus_stop_tf

# getting words of 3rd corpus from term frequencies 
gutenberg_words_3 <- names(termFreq(gutenberg_corpus_stop[[3]]))
gutenberg_words_3

# creating palette of colors
pal <- brewer.pal(9,"BuGn")
str(pal)
# generating word cloud view 
gutenberg_words_3_wc <- wordcloud(gutenberg_words_3, gutenberg_corpus_stop_tf, colors = pal[-(1:4)]) 

# creating another color palette
pal2 <- brewer.pal(9,"Spectral")
# generate word cloud view 
gutenberg_words_6_wc <- wordcloud(names(termFreq(gutenberg_corpus_stop[[6]])), termFreq(gutenberg_corpus_stop[[6]]), colors = pal2, min.freq = 2) 

# getting data for applying quanteda package
gutenberg_doc2_text <- gutenberg_corpus_clean[[2]] 
gutenberg_doc2_text$content

# applying tokenization via the tokens function of quanteda package
gutenberg_doc2_tokens <- quanteda::tokens(gutenberg_doc2_text$content) 
str(gutenberg_doc2_tokens)

# creating a sparse Document Feature Matrix (DFM)
gutenberg_doc2_DFM <- dfm(gutenberg_doc2_tokens)
str(gutenberg_doc2_DFM)

# getting frequency of the terms of DFM object
gutenberg_doc2_freq <- docfreq(gutenberg_doc2_DFM)
gutenberg_doc2_freq

# assigning weights to these words
gutenberg_doc2_weights <- dfm_weight(gutenberg_doc2_DFM,smoothing = 0.75)
gutenberg_doc2_weights

# computing the tf-idf score for feature extraction
gutenberg_doc2_TFIDF <- dfm_tfidf(gutenberg_doc2_DFM, scheme_tf = "count", scheme_df = "inverse")
gutenberg_doc2_TFIDF
str(gutenberg_doc2_TFIDF)

# extracting as a data frame
gutenberg_doc2_text_df <- as.data.frame(gutenberg_doc2_text$content)
gutenberg_doc2_text_df

# *** sentiment analysis ***

# reading file as string, Chapter10 because it is the corpus 2 in VCorpus object
gutenberg_doc2_as_string <- get_text_as_string("Data/Chapter10.txt")
gutenberg_doc2_as_string

# getting sentences from string 
gutenberg_doc2_sentences <- get_sentences(gutenberg_doc2_as_string)
gutenberg_doc2_sentences
# printing the structure of an object
str(gutenberg_doc2_sentences)

# getting sentiment of words or sentences, using method = syuzhet
gutenberg_doc2_syuz <- get_sentiment(gutenberg_doc2_sentences, method = "syuzhet")
gutenberg_doc2_syuz

# getting sentiment of words or sentences, using method = bing
gutenberg_doc2_bin <- get_sentiment(gutenberg_doc2_sentences, method = "bing")
gutenberg_doc2_bin

#printing dictionary for method syuzhet
syuzhet_dictionary <- get_sentiment_dictionary()
syuzhet_dictionary

#printing dictionary for method bing
bing_dictionary <- get_sentiment_dictionary(dictionary = "bing")
bing_dictionary

# summing the values of sentiments for syuzhet
gutenberg_doc2_sentiment_sum <- sum(gutenberg_doc2_syuz)
gutenberg_doc2_sentiment_sum

# summing the values of sentiments for bing
gutenberg_doc2_sentiment_bing <- sum(gutenberg_doc2_bin)
gutenberg_doc2_sentiment_bing

# calculating means of sentiment values for syuzhet
gutenberg_doc2_sentiment_mean <- mean(gutenberg_doc2_syuz)
gutenberg_doc2_sentiment_mean

# calculating means of sentiment values for bing method
gutenberg_doc2_bing_mean <- mean(gutenberg_doc2_bin)
gutenberg_doc2_bing_mean

# returning a data frame in which each row represents a sentence from the original file
# the columns include one for each emotion type was well as the positive or negative sentiment valence
gutenberg_doc2_sent_nrc <- get_nrc_sentiment(gutenberg_doc2_sentences)
gutenberg_doc2_sent_nrc

# suming up row values to see overal values of each sentence
gutenberg_rowSum <- rowSums(gutenberg_doc2_sent_nrc)
gutenberg_rowSum

# suming up row values to see how much of each emotion/sentiment is used
gutenberg_colSum <- colSums(gutenberg_doc2_sent_nrc)
gutenberg_colSum

# visualizing previously generated data for Syuzhet method
plot(gutenberg_doc2_syuz, main = "\n\nChapter 10, plot (Syuzhet)", xlab = "Narrative", ylab = "words")
# visualizing previously generated data for Bing method
plot(gutenberg_doc2_bin, main = "\n\nChapter 10, plot (Bing)", xlab = "Narrative", ylab = "words")

# comparing the shape of one trajectory to another
gutenberg_doc2_sent_pct_value <- get_percentage_values(gutenberg_doc2_syuz, bins = 25)
structure(gutenberg_doc2_sent_pct_value)

# visualizing new data
plot(gutenberg_doc2_sent_pct_value, main = "\n\nChapter 10, plot (Syuzhet)", xlab = "Narrative", ylab = "Emotional valence", col = "red")

# comparing the shape of one trajectory to another, updated bins value
gutenberg_doc2_sent_pct_value <- get_percentage_values(gutenberg_doc2_syuz, bins = 75)
structure(gutenberg_doc2_sent_pct_value)

# visualizing new data
plot(gutenberg_doc2_sent_pct_value, main = "\n\nChapter 10, plot (Syuzhet)", xlab = "Narrative", ylab = "Emotional valence", col = "red",axes = TRUE)

#more granularity gives a better view of emotional intensity about data


# *** Task (1.g) *** 

# stringi and quanteda package is already installed in the beginning of the document
#here is the commented code
# install.packages("stringi")
# install.packages("quanteda")
# library(stringi)
# library(quanteda)

# *** additional methods from quanteda ***

# constructor for corpus objects
corpus_gut <- corpus(gutenberg_doc2_sentences)
corpus_gut

#counting number of tokens in the doc
corpus_tokens <- ntoken(gutenberg_doc2_sentences)
corpus_tokens

#creating set of bigrams (when n=2)
corpus_bigram <- ngrams(corpus_gut, 2)
corpus_bigram

# splitting corpus on a pattern match
corpus_segment(corpus_gut, pattern = ",*")

# reshaping to some aggregated forms
corpus_reshape(corpus_gut, to = "sentences")

# creating set of ngrams 
tokens_ngrams(tokens(corpus_gut, remove_punct = TRUE), n = 2:5)


# *** additional methods from stringi ***
tmp <- gutenberg_doc2_sentences

#removing all empty strings from a character vector
stri_remove_empty(tmp)

#splitting a string at text boundaries
stri_split_boundaries(tmp)

# printing statistics for a character vector containing LaTeX commands where each text line is represented by a separate string
stri_stats_latex(gutenberg_doc2_as_string)

#trimming characters from the Left and/or Right side of a string
stri_trim_both(gutenberg_doc2_as_string)

# printing general statistics like lines and chars
stri_stats_general(gutenberg_doc2_sentences)

# detect encoding of character vector
stri_enc_detect(gutenberg_doc2_sentences[1:10])

