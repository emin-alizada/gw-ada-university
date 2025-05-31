# NECESSARY PACKAGES TO BE INSTALLED
# install.packages("tm")
# install.packages("tokenizers")
# install.packages("wordcloud")
# install.packages('RColorBrewer')
# install.packages("quanteda")
# install.packages("syuzhet")
# install.packages("openNLP")
# install.packages("zipfR")
# installed.packages("languageR")
# install.packages("stringi")

# NECESSARY PACKAGES TO BE ACTIVATED
library("tm")
library("tokenizers")
library("wordcloud")
library("RColorBrewer")
library("quanteda")
library("syuzhet")
library("openNLP")
library("dplyr")
library("zipfR")
library("languageR")
library("stringi")

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# change directory
setwd("C:/Users/abagiyev/OneDrive - ADA University/Courses/ADA/Master/Spring Term - 2021/Intro. to Big Data & Analytics/Assignments/Class Project 3/20000Leagues")

# ------------------------------------------------------------------ EARLY SETUP

# creating VCorpus
leagues <- VCorpus(DirSource(".", ignore.case=TRUE, mode="text"))
str(leagues)

# printing VCorpus
leagues

# extracting the chapter 1 from the corpus
chapter_1 <- leagues[[1]]
chapter_1

# exploring content of the document
chapter_1$content

# extracting the chapter 2 from the corpus
chapter_2 <- leagues[[2]]
chapter_2

# exploring content of the document
chapter_2$content


# extracting the chapter 3 from the corpus
chapter_3 <- leagues[[3]]
chapter_3

# exploring content of the document
chapter_3$content


# extracting the chapter 4 from the corpus
chapter_4 <- leagues[[4]]
chapter_4

# exploring content of the document
chapter_4$content


# extracting the chapter 5 from the corpus
chapter_5 <- leagues[[5]]
chapter_5

# exploring content of the document
chapter_5$content


# extracting the chapter 6 from the corpus
chapter_6 <- leagues[[6]]
chapter_6

# exploring content of the document
chapter_6$content


# extracting the chapter 7 from the corpus
chapter_7 <- leagues[[7]]
chapter_7

# exploring content of the document
chapter_7$content


# extracting the chapter 8 from the corpus
chapter_8 <- leagues[[8]]
chapter_8

# exploring content of the document
chapter_8$content


# extracting the chapter 9 from the corpus
chapter_9 <- leagues[[9]]
chapter_9

# exploring content of the document
chapter_9$content


# extracting the chapter 10 from the corpus
chapter_10 <- leagues[[10]]
chapter_10

# exploring content of the document
chapter_10$content

# -------------------------------------------------------------- END EARLY SETUP

# ----------------------------------------------------------------------- TASK A

# creating a method to get the 10 longest words

get_10_longest_words <- function(chapter) {
  
  # tokenizing content to words
  tokens = tokenize_words(chapter$content)
  
  # combining all elements within single vector/list
  unlisted_tokens = unlist(tokens)
  
  # remove duplicates
  unique_tokens = unique(unlisted_tokens)
  
  # sort in decreasing order by length
  unique_tokens = unique_tokens[order(nchar(unique_tokens), unique_tokens, decreasing = TRUE)]
  
  return (unique_tokens[1:10])
}


# creating a method to get the 10 longest words
get_10_longest_sentences <- function(chapter) {
  
  # tokenizing content to sentences
  tokens = tokenize_sentences(chapter$content)
  
  # combining all elements within single vector/list
  unlisted_tokens = unlist(tokens)
  
  # remove duplicates
  unique_tokens = unique(unlisted_tokens)
  
  # sort in decreasing order by length
  unique_tokens = unique_tokens[order(nchar(unique_tokens), unique_tokens, decreasing = TRUE)]
  
  return (unique_tokens[1:10])
}


# getting longest 10 words in all chapters
for (i in 1:10) {
  print(paste0("Chapter ", i, " longest 10 words"))
  print(get_10_longest_words(leagues[[i]]))
  cat('\n')
}


# getting longest 10 sentences in all chapters
for (i in 1:10) {
  print(paste0("Chapter ", i, " longest 10 sentences"))
  print(get_10_longest_sentences(leagues[[i]]))
  cat('\n')
}

# ------------------------------------------------------------------- END TASK A

# ----------------------------------------------------------------------- TASK B
# beginning of the tutorial has been applied in the EARLY SETUP section

# generating - DTM - Document Term Matrix
leagues_DTM <- DocumentTermMatrix(leagues)
inspect(leaguesDTM)
str(leaguesDTM)

# generating - TDM - Term Document Matrix
leagues_TDM <- TermDocumentMatrix(leagues)
inspect(leaguesTDM)
str(leaguesTDM)


# generate a data frame
leagues_DF <- data.frame(chapter_1[1])
leagues_DF[1]


# +++++++++++++++++++++ Corpus Cleansing – Data Wrangling +++++++++++++++++++++

# ---------------------------------------------- FUNCTIONS
# creating function to remove numbers and punctuation
remove_num_punct <- function(chapter) gsub("[^[:alpha:][:space:]]*", "", chapter)

# creating a function to inspect all chapters in once
inspect_vcorpus = function(vcorpus) {
  for (i in 1:10) {
    print(paste0("Chapter ", i))
    print(inspect(vcorpus[[i]]))
    cat('\n')
  }
}

# methods stores all term frequencies in a list
get_term_frequencies = function(vcorpus) {
  # creating a list to hold each chapter term frequencies
  all_term_frequencies_list = list()
  
  for (i in 1:10) { 
    all_term_frequencies_list[i] = termFreq(vcorpus[[i]])
  }
  
  return (all_term_frequencies_list)
}

# look either all or particular chapter term frequency
look_term_frequencies = function(all_term_frequencies_list, chapter=NULL) {
  
  if (is.null(chapter)) {
    for (i in 1:10) {
      print(paste0("Chapter ", i, " term frequencies"))
      print(all_term_frequencies_list[i])
      cat('\n')
    }
  }else{
    print(paste0("Chapter ", i, " term frequencies"))
    print(all_term_frequencies_list[i])
  }
}


# function to extract name of the words from the frequencies
get_words = function(vcorpus, chapter=NULL) {
  
  if (is.null(chapter)) {
    all_words = c()
    
    for (i in 1:10) {
      all_words = c(all_words, names(termFreq(leagues_stopsless[[i]])))
    }
    
    return (all_words)
    
  }else{
    return (names(termFreq(leagues_stopsless[[chapter]])))
  }

}

print_str_and_plotting_dendograms = function(TDM) {
  leagues_stopsless_ch_DF <-as.data.frame(TDM[[1]])
  leagues_stopsless_ch_dist <- dist(leagues_stopsless_ch_DF)
  leagues_stopsless_ch_DG <- hclust(leagues_stopsless_ch_dist, method="ward.D2")
  str(leagues_stopsless_ch_DG)
  plot(leagues_stopsless_ch_DG)
}

# ---------------------------------------------- END FUNCTIONS
# cleaning corpus from numbers and punctuation
leagues_cleaned <- tm_map(leagues, content_transformer(remove_num_punct))
leagues_cleaned[[1]]

# converting every instance to lowercase
leagues_lowered <- tm_map(leagues_cleaned, content_transformer(tolower))
inspect_vcorpus(leagues_lowered)

# getting all stopwords from English language
english_stopwords = c(stopwords('English'))

# removing stopwords from lowered VCorpus
leagues_stopsless = tm_map(leagues_lowered, removeWords, english_stopwords)
inspect_vcorpus(leagues_stopsless)

# creating TDM again without stopwords
leagues_stopsless_TDM = TermDocumentMatrix(leagues_stopsless)

# finding frequency terms
leagues_stopless_frequency_terms = findFreqTerms(leagues_stopsless_TDM, lowfreq = 5)
leagues_stopless_frequency_terms

# finding term frequencies for all chapters
leagues_stopsless_term_freq_ch_1 = termFreq(leagues_stopsless[[1]])
leagues_stopsless_term_freq_ch_1

leagues_stopsless_term_freq_ch_2 = termFreq(leagues_stopsless[[2]])
leagues_stopsless_term_freq_ch_2

leagues_stopsless_term_freq_ch_3 = termFreq(leagues_stopsless[[3]])
leagues_stopsless_term_freq_ch_3

leagues_stopsless_term_freq_ch_4 = termFreq(leagues_stopsless[[4]])
leagues_stopsless_term_freq_ch_4

leagues_stopsless_term_freq_ch_5 = termFreq(leagues_stopsless[[5]])
leagues_stopsless_term_freq_ch_5

leagues_stopsless_term_freq_ch_6 = termFreq(leagues_stopsless[[6]])
leagues_stopsless_term_freq_ch_6

leagues_stopsless_term_freq_ch_7 = termFreq(leagues_stopsless[[7]])
leagues_stopsless_term_freq_ch_7

leagues_stopsless_term_freq_ch_8 = termFreq(leagues_stopsless[[8]])
leagues_stopsless_term_freq_ch_8

leagues_stopsless_term_freq_ch_9 = termFreq(leagues_stopsless[[9]])
leagues_stopsless_term_freq_ch_9

leagues_stopsless_term_freq_ch_10 = termFreq(leagues_stopsless[[10]])
leagues_stopsless_term_freq_ch_10

# inspecting stopless TDM again
inspect(leagues_stopsless_TDM)


# forming and plotting dendograms
print_str_and_plotting_dendograms(leagues_stopsless_TDM)


# creating custom stopwords to eliminate
custom_stopwords = c("even", "a", "will", "may", "soon", "can", "as", "one", "much", "just", "now", "quite", "merely", "shall", "take", "will", "certain", "well")

# removing custom stopwords from vcorpus
league_stopless_updated = tm_map(leagues_stopsless, removeWords, custom_stopwords)
str(league_stopless_updated)

# creating new TDM, data frame and distance matrix on updated corpus
league_stopless_updated_TDM = TermDocumentMatrix(league_stopless_updated)
league_stopless_updated_DF  = as.data.frame(league_stopless_updated_TDM[[1]])
league_stopless_updated_dist = dist(league_stopless_updated_DF)

# extract name of the words from the frequencies
leagues_stopsless_term_freq_ch_1_words = get_words(leagues_stopsless, 1)

leagues_stopsless_term_freq_ch_2_words = get_words(leagues_stopsless, 2)

leagues_stopsless_term_freq_ch_3_words = get_words(leagues_stopsless, 3)

leagues_stopsless_term_freq_ch_4_words = get_words(leagues_stopsless, 4)

leagues_stopsless_term_freq_ch_5_words = get_words(leagues_stopsless, 5)

leagues_stopsless_term_freq_ch_6_words = get_words(leagues_stopsless, 6)

leagues_stopsless_term_freq_ch_7_words = get_words(leagues_stopsless, 7)

leagues_stopsless_term_freq_ch_8_words = get_words(leagues_stopsless, 8)

leagues_stopsless_term_freq_ch_9_words = get_words(leagues_stopsless, 9)

leagues_stopsless_term_freq_ch_10_words = get_words(leagues_stopsless, 10)


# creating paletter of colors
palette_1 = brewer.pal(9, "BuGn")
palette_2 = brewer.pal(9, "Spectral")
str(palette_1)
str(palette_2)


# creating wordclouds with patette 1 
leagues_stopsless_term_freq_ch_1_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_1_words, leagues_stopsless_term_freq_ch_1, colors=palette_1)

leagues_stopsless_term_freq_ch_2_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_2_words, leagues_stopsless_term_freq_ch_2, colors=palette_1)

leagues_stopsless_term_freq_ch_3_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_3_words, leagues_stopsless_term_freq_ch_3, colors=palette_1)

leagues_stopsless_term_freq_ch_4_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_4_words, leagues_stopsless_term_freq_ch_4, colors=palette_1)

leagues_stopsless_term_freq_ch_5_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_5_words, leagues_stopsless_term_freq_ch_5, colors=palette_1)

leagues_stopsless_term_freq_ch_6_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_6_words, leagues_stopsless_term_freq_ch_6, colors=palette_1)

leagues_stopsless_term_freq_ch_7_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_7_words, leagues_stopsless_term_freq_ch_7, colors=palette_1)

leagues_stopsless_term_freq_ch_8_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_8_words, leagues_stopsless_term_freq_ch_8, colors=palette_1)

leagues_stopsless_term_freq_ch_9_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_9_words, leagues_stopsless_term_freq_ch_9, colors=palette_1)

leagues_stopsless_term_freq_ch_10_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_10_words, leagues_stopsless_term_freq_ch_10, colors=palette_1)


# creating wordclouds with patette 2
leagues_stopsless_term_freq_ch_1_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_1_words, leagues_stopsless_term_freq_ch_1, colors=palette_2)

leagues_stopsless_term_freq_ch_2_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_2_words, leagues_stopsless_term_freq_ch_2, colors=palette_2)

leagues_stopsless_term_freq_ch_3_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_3_words, leagues_stopsless_term_freq_ch_3, colors=palette_2)

leagues_stopsless_term_freq_ch_4_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_4_words, leagues_stopsless_term_freq_ch_4, colors=palette_2)

leagues_stopsless_term_freq_ch_5_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_5_words, leagues_stopsless_term_freq_ch_5, colors=palette_2)

leagues_stopsless_term_freq_ch_6_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_6_words, leagues_stopsless_term_freq_ch_6, colors=palette_2)

leagues_stopsless_term_freq_ch_7_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_7_words, leagues_stopsless_term_freq_ch_7, colors=palette_2)

leagues_stopsless_term_freq_ch_8_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_8_words, leagues_stopsless_term_freq_ch_8, colors=palette_2)

leagues_stopsless_term_freq_ch_9_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_9_words, leagues_stopsless_term_freq_ch_9, colors=palette_2)

leagues_stopsless_term_freq_ch_10_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_10_words, leagues_stopsless_term_freq_ch_10, colors=palette_2)

# lets look at 1st few lines of the cleaned vcorpus (this will be shown )
leagues_cleaned_ch_1_text <- leagues_cleaned[[1]]
leagues_cleaned_ch_1_text$content[1:10]

leagues_cleaned_ch_2_text <- leagues_cleaned[[2]]
leagues_cleaned_ch_2_text$content[1:10]

leagues_cleaned_ch_3_text <- leagues_cleaned[[3]]
leagues_cleaned_ch_3_text$content[1:10]

leagues_cleaned_ch_4_text <- leagues_cleaned[[4]]
leagues_cleaned_ch_4_text$content[1:10]

leagues_cleaned_ch_5_text <- leagues_cleaned[[5]]
leagues_cleaned_ch_5_text$content[1:10]

leagues_cleaned_ch_6_text <- leagues_cleaned[[6]]
leagues_cleaned_ch_6_text$content[1:10]

leagues_cleaned_ch_7_text <- leagues_cleaned[[7]]
leagues_cleaned_ch_7_text$content[1:10]

leagues_cleaned_ch_8_text <- leagues_cleaned[[8]]
leagues_cleaned_ch_8_text$content[1:10]

leagues_cleaned_ch_9_text <- leagues_cleaned[[9]]
leagues_cleaned_ch_9_text$content[1:10]

leagues_cleaned_ch_10_text <- leagues_cleaned[[10]]
leagues_cleaned_ch_10_text$content[1:10]


# applying tokens for chapter contents
leagues_cleaned_ch_1_text_tokens = tokens(leagues_cleaned_ch_1_text$content)
str(leagues_cleaned_ch_1_text_tokens)

leagues_cleaned_ch_2_text_tokens = tokens(leagues_cleaned_ch_2_text$content)
str(leagues_cleaned_ch_2_text_tokens)

leagues_cleaned_ch_3_text_tokens = tokens(leagues_cleaned_ch_3_text$content)
str(leagues_cleaned_ch_3_text_tokens)

leagues_cleaned_ch_4_text_tokens = tokens(leagues_cleaned_ch_4_text$content)
str(leagues_cleaned_ch_4_text_tokens)

leagues_cleaned_ch_5_text_tokens = tokens(leagues_cleaned_ch_5_text$content)
str(leagues_cleaned_ch_5_text_tokens)

leagues_cleaned_ch_6_text_tokens = tokens(leagues_cleaned_ch_6_text$content)
str(leagues_cleaned_ch_6_text_tokens)

leagues_cleaned_ch_7_text_tokens = tokens(leagues_cleaned_ch_7_text$content)
str(leagues_cleaned_ch_7_text_tokens)

leagues_cleaned_ch_8_text_tokens = tokens(leagues_cleaned_ch_8_text$content)
str(leagues_cleaned_ch_8_text_tokens)

leagues_cleaned_ch_9_text_tokens = tokens(leagues_cleaned_ch_9_text$content)
str(leagues_cleaned_ch_9_text_tokens)

leagues_cleaned_ch_10_text_tokens = tokens(leagues_cleaned_ch_10_text$content)
str(leagues_cleaned_ch_10_text_tokens)


# construct a sparse document-feature matrix
leagues_cleaned_ch_1_text_tokens_dfm <- quanteda::dfm(leagues_cleaned_ch_1_text_tokens, remove=c(english_stopwords, custom_stopwords))
str(leagues_cleaned_ch_1_text_tokens_dfm)

leagues_cleaned_ch_2_text_tokens_dfm <- quanteda::dfm(leagues_cleaned_ch_2_text_tokens, remove=c(english_stopwords, custom_stopwords))
str(leagues_cleaned_ch_2_text_tokens_dfm)

leagues_cleaned_ch_3_text_tokens_dfm <- quanteda::dfm(leagues_cleaned_ch_3_text_tokens, remove=c(english_stopwords, custom_stopwords))
str(leagues_cleaned_ch_3_text_tokens_dfm)

leagues_cleaned_ch_4_text_tokens_dfm <- quanteda::dfm(leagues_cleaned_ch_4_text_tokens, remove=c(english_stopwords, custom_stopwords))
str(leagues_cleaned_ch_4_text_tokens_dfm)

leagues_cleaned_ch_5_text_tokens_dfm <- quanteda::dfm(leagues_cleaned_ch_5_text_tokens, remove=c(english_stopwords, custom_stopwords))
str(leagues_cleaned_ch_5_text_tokens_dfm)

leagues_cleaned_ch_6_text_tokens_dfm <- quanteda::dfm(leagues_cleaned_ch_6_text_tokens, remove=c(english_stopwords, custom_stopwords))
str(leagues_cleaned_ch_6_text_tokens_dfm)

leagues_cleaned_ch_7_text_tokens_dfm <- dfm(leagues_cleaned_ch_7_text_tokens, remove=c(english_stopwords, custom_stopwords))
str(leagues_cleaned_ch_7_text_tokens_dfm)

leagues_cleaned_ch_8_text_tokens_dfm <- quanteda::dfm(leagues_cleaned_ch_8_text_tokens, remove=c(english_stopwords, custom_stopwords))
str(leagues_cleaned_ch_8_text_tokens_dfm)

leagues_cleaned_ch_9_text_tokens_dfm <- quanteda::dfm(leagues_cleaned_ch_9_text_tokens, remove=c(english_stopwords, custom_stopwords))
str(leagues_cleaned_ch_9_text_tokens_dfm)

leagues_cleaned_ch_10_text_tokens_dfm <- quanteda::dfm(leagues_cleaned_ch_10_text_tokens, remove=c(english_stopwords, custom_stopwords))
str(leagues_cleaned_ch_10_text_tokens_dfm)


# let’s get the frequency of terms in the dfm
leagues_cleaned_ch_1_text_tokens_dfm_freq <- quanteda::docfreq(leagues_cleaned_ch_1_text_tokens_dfm)
str(leagues_cleaned_ch_1_text_tokens_dfm_freq)

leagues_cleaned_ch_2_text_tokens_dfm_freq <- quanteda::docfreq(leagues_cleaned_ch_2_text_tokens_dfm)
str(leagues_cleaned_ch_2_text_tokens_dfm_freq)

leagues_cleaned_ch_3_text_tokens_dfm_freq <- quanteda::docfreq(leagues_cleaned_ch_3_text_tokens_dfm)
str(leagues_cleaned_ch_3_text_tokens_dfm_freq)

leagues_cleaned_ch_4_text_tokens_dfm_freq <- quanteda::docfreq(leagues_cleaned_ch_4_text_tokens_dfm)
str(leagues_cleaned_ch_4_text_tokens_dfm_freq)

leagues_cleaned_ch_5_text_tokens_dfm_freq <- quanteda::docfreq(leagues_cleaned_ch_5_text_tokens_dfm)
str(leagues_cleaned_ch_5_text_tokens_dfm_freq)

leagues_cleaned_ch_6_text_tokens_dfm_freq <- quanteda::docfreq(leagues_cleaned_ch_6_text_tokens_dfm)
str(leagues_cleaned_ch_6_text_tokens_dfm_freq)

leagues_cleaned_ch_7_text_tokens_dfm_freq <- quanteda::docfreq(leagues_cleaned_ch_7_text_tokens_dfm)
str(leagues_cleaned_ch_7_text_tokens_dfm_freq)

leagues_cleaned_ch_8_text_tokens_dfm_freq <- quanteda::docfreq(leagues_cleaned_ch_8_text_tokens_dfm)
str(leagues_cleaned_ch_8_text_tokens_dfm_freq)

leagues_cleaned_ch_9_text_tokens_dfm_freq <- quanteda::docfreq(leagues_cleaned_ch_9_text_tokens_dfm)
str(leagues_cleaned_ch_9_text_tokens_dfm_freq)

leagues_cleaned_ch_10_text_tokens_dfm_freq <- quanteda::docfreq(leagues_cleaned_ch_10_text_tokens_dfm)
str(leagues_cleaned_ch_10_text_tokens_dfm_freq)

# let’s assign weights to these words in the dfm
leagues_cleaned_ch_1_text_tokens_dfm_weights <- quanteda::dfm_weight(leagues_cleaned_ch_1_text_tokens_dfm)
str(leagues_cleaned_ch_1_text_tokens_dfm_weights)
leagues_cleaned_ch_1_text_tokens_dfm_weights

leagues_cleaned_ch_2_text_tokens_dfm_weights <- quanteda::dfm_weight(leagues_cleaned_ch_2_text_tokens_dfm)
str(leagues_cleaned_ch_2_text_tokens_dfm_weights)
leagues_cleaned_ch_2_text_tokens_dfm_weights

leagues_cleaned_ch_3_text_tokens_dfm_weights <- quanteda::dfm_weight(leagues_cleaned_ch_3_text_tokens_dfm)
str(leagues_cleaned_ch_3_text_tokens_dfm_weights)
leagues_cleaned_ch_3_text_tokens_dfm_weights

leagues_cleaned_ch_4_text_tokens_dfm_weights <- quanteda::dfm_weight(leagues_cleaned_ch_4_text_tokens_dfm)
str(leagues_cleaned_ch_4_text_tokens_dfm_weights)
leagues_cleaned_ch_4_text_tokens_dfm_weights

leagues_cleaned_ch_5_text_tokens_dfm_weights <- quanteda::dfm_weight(leagues_cleaned_ch_5_text_tokens_dfm)
str(leagues_cleaned_ch_5_text_tokens_dfm_weights)
leagues_cleaned_ch_5_text_tokens_dfm_weights

leagues_cleaned_ch_6_text_tokens_dfm_weights <- quanteda::dfm_weight(leagues_cleaned_ch_6_text_tokens_dfm)
str(leagues_cleaned_ch_6_text_tokens_dfm_weights)
leagues_cleaned_ch_6_text_tokens_dfm_weights

leagues_cleaned_ch_7_text_tokens_dfm_weights <- quanteda::dfm_weight(leagues_cleaned_ch_7_text_tokens_dfm)
str(leagues_cleaned_ch_7_text_tokens_dfm_weights)
leagues_cleaned_ch_7_text_tokens_dfm_weights

leagues_cleaned_ch_8_text_tokens_dfm_weights <- quanteda::dfm_weight(leagues_cleaned_ch_8_text_tokens_dfm)
str(leagues_cleaned_ch_8_text_tokens_dfm_weights)
leagues_cleaned_ch_8_text_tokens_dfm_weights

leagues_cleaned_ch_9_text_tokens_dfm_weights <- quanteda::dfm_weight(leagues_cleaned_ch_9_text_tokens_dfm)
str(leagues_cleaned_ch_9_text_tokens_dfm_weights)
leagues_cleaned_ch_9_text_tokens_dfm_weights

leagues_cleaned_ch_10_text_tokens_dfm_weights <- quanteda::dfm_weight(leagues_cleaned_ch_10_text_tokens_dfm)
str(leagues_cleaned_ch_10_text_tokens_dfm_weights)
leagues_cleaned_ch_10_text_tokens_dfm_weights


# let’s compute the tf-idf score
leagues_cleaned_ch_1_text_tokens_dfm_tfidf <- quanteda::dfm_tfidf(leagues_cleaned_ch_1_text_tokens_dfm, scheme_tf="count", scheme_df="inverse")
str(leagues_cleaned_ch_1_text_tokens_dfm_tfidf)

leagues_cleaned_ch_2_text_tokens_dfm_tfidf <- quanteda::dfm_tfidf(leagues_cleaned_ch_2_text_tokens_dfm, scheme_tf="count", scheme_df="inverse")
str(leagues_cleaned_ch_2_text_tokens_dfm_tfidf)

leagues_cleaned_ch_3_text_tokens_dfm_tfidf <- quanteda::dfm_tfidf(leagues_cleaned_ch_3_text_tokens_dfm, scheme_tf="count", scheme_df="inverse")
str(leagues_cleaned_ch_3_text_tokens_dfm_tfidf)

leagues_cleaned_ch_4_text_tokens_dfm_tfidf <- quanteda::dfm_tfidf(leagues_cleaned_ch_4_text_tokens_dfm, scheme_tf="count", scheme_df="inverse")
str(leagues_cleaned_ch_4_text_tokens_dfm_tfidf)

leagues_cleaned_ch_5_text_tokens_dfm_tfidf <- quanteda::dfm_tfidf(leagues_cleaned_ch_5_text_tokens_dfm, scheme_tf="count", scheme_df="inverse")
str(leagues_cleaned_ch_5_text_tokens_dfm_tfidf)

leagues_cleaned_ch_6_text_tokens_dfm_tfidf <- quanteda::dfm_tfidf(leagues_cleaned_ch_6_text_tokens_dfm, scheme_tf="count", scheme_df="inverse")
str(leagues_cleaned_ch_6_text_tokens_dfm_tfidf)

leagues_cleaned_ch_7_text_tokens_dfm_tfidf <- quanteda::dfm_tfidf(leagues_cleaned_ch_7_text_tokens_dfm, scheme_tf="count", scheme_df="inverse")
str(leagues_cleaned_ch_7_text_tokens_dfm_tfidf)

leagues_cleaned_ch_8_text_tokens_dfm_tfidf <- quanteda::dfm_tfidf(leagues_cleaned_ch_8_text_tokens_dfm, scheme_tf="count", scheme_df="inverse")
str(leagues_cleaned_ch_8_text_tokens_dfm_tfidf)

leagues_cleaned_ch_9_text_tokens_dfm_tfidf <- quanteda::dfm_tfidf(leagues_cleaned_ch_9_text_tokens_dfm, scheme_tf="count", scheme_df="inverse")
str(leagues_cleaned_ch_9_text_tokens_dfm_tfidf)

leagues_cleaned_ch_10_text_tokens_dfm_tfidf <- quanteda::dfm_tfidf(leagues_cleaned_ch_10_text_tokens_dfm, scheme_tf="count", scheme_df="inverse")
str(leagues_cleaned_ch_10_text_tokens_dfm_tfidf)


# extracting text of the documents as data frame
leagues_cleaned_ch_1_text_DF = as.data.frame(leagues_cleaned_ch_1_text$content)
leagues_cleaned_ch_1_text_DF

leagues_cleaned_ch_2_text_DF = as.data.frame(leagues_cleaned_ch_2_text$content)
leagues_cleaned_ch_2_text_DF

leagues_cleaned_ch_3_text_DF = as.data.frame(leagues_cleaned_ch_3_text$content)
leagues_cleaned_ch_3_text_DF

leagues_cleaned_ch_4_text_DF = as.data.frame(leagues_cleaned_ch_4_text$content)
leagues_cleaned_ch_4_text_DF

leagues_cleaned_ch_5_text_DF = as.data.frame(leagues_cleaned_ch_5_text$content)
leagues_cleaned_ch_5_text_DF

leagues_cleaned_ch_6_text_DF = as.data.frame(leagues_cleaned_ch_6_text$content)
leagues_cleaned_ch_6_text_DF

leagues_cleaned_ch_7_text_DF = as.data.frame(leagues_cleaned_ch_7_text$content)
leagues_cleaned_ch_7_text_DF

leagues_cleaned_ch_8_text_DF = as.data.frame(leagues_cleaned_ch_8_text$content)
leagues_cleaned_ch_8_text_DF

leagues_cleaned_ch_9_text_DF = as.data.frame(leagues_cleaned_ch_9_text$content)
leagues_cleaned_ch_9_text_DF

leagues_cleaned_ch_10_text_DF = as.data.frame(leagues_cleaned_ch_10_text$content)
leagues_cleaned_ch_10_text_DF

# lets get the sentences in each chapter
leagues_ch_1_as_string = get_text_as_string("CHAPTER1.txt")
leagues_ch_1_as_string

leagues_ch_2_as_string = get_text_as_string("CHAPTER2.txt")
leagues_ch_2_as_string

leagues_ch_3_as_string = get_text_as_string("CHAPTER3.txt")
leagues_ch_3_as_string

leagues_ch_4_as_string = get_text_as_string("CHAPTER4.txt")
leagues_ch_4_as_string

leagues_ch_5_as_string = get_text_as_string("CHAPTER5.txt")
leagues_ch_6_as_string

leagues_ch_6_as_string = get_text_as_string("CHAPTER6.txt")
leagues_ch_7_as_string

leagues_ch_7_as_string = get_text_as_string("CHAPTER7.txt")
leagues_ch_7_as_string

leagues_ch_8_as_string = get_text_as_string("CHAPTER8.txt")
leagues_ch_8_as_string

leagues_ch_9_as_string = get_text_as_string("CHAPTER9.txt")
leagues_ch_9_as_string

leagues_ch_10_as_string = get_text_as_string("CHAPTER10.txt")
leagues_ch_10_as_string

# let's get sentences now
leagues_ch_1_as_string_sentences = get_sentences(leagues_ch_1_as_string)
str(leagues_ch_1_as_string_sentences)
leagues_ch_1_as_string_sentences

leagues_ch_2_as_string_sentences = get_sentences(leagues_ch_2_as_string)
str(leagues_ch_2_as_string_sentences)
leagues_ch_2_as_string_sentences

leagues_ch_3_as_string_sentences = get_sentences(leagues_ch_3_as_string)
str(leagues_ch_3_as_string_sentences)
leagues_ch_3_as_string_sentences

leagues_ch_4_as_string_sentences = get_sentences(leagues_ch_4_as_string)
str(leagues_ch_5_as_string_sentences)
leagues_ch_5_as_string_sentences

leagues_ch_5_as_string_sentences = get_sentences(leagues_ch_5_as_string)
str(leagues_ch_6_as_string_sentences)
leagues_ch_6_as_string_sentences

leagues_ch_6_as_string_sentences = get_sentences(leagues_ch_6_as_string)
str(leagues_ch_7_as_string_sentences)
leagues_ch_7_as_string_sentences

leagues_ch_7_as_string_sentences = get_sentences(leagues_ch_7_as_string)
str(leagues_ch_7_as_string_sentences)
leagues_ch_7_as_string_sentences

leagues_ch_8_as_string_sentences = get_sentences(leagues_ch_8_as_string)
str(leagues_ch_8_as_string_sentences)
leagues_ch_8_as_string_sentences

leagues_ch_9_as_string_sentences = get_sentences(leagues_ch_9_as_string)
str(leagues_ch_9_as_string_sentences)
leagues_ch_9_as_string_sentences

leagues_ch_10_as_string_sentences = get_sentences(leagues_ch_10_as_string)
str(leagues_ch_10_as_string_sentences)
leagues_ch_10_as_string_sentences

# let’s call get_sentiment and get_sentiment_dictionary with the default value of “syuzhet”
leagues_ch_1_as_string_sentences_sentiment = get_sentiment(leagues_ch_1_as_string_sentences, "syuzhet")
leagues_ch_1_as_string_sentences_sentiment

leagues_ch_2_as_string_sentences_sentiment = get_sentiment(leagues_ch_2_as_string_sentences, "syuzhet")
leagues_ch_2_as_string_sentences_sentiment

leagues_ch_3_as_string_sentences_sentiment = get_sentiment(leagues_ch_3_as_string_sentences, "syuzhet")
leagues_ch_3_as_string_sentences_sentiment

leagues_ch_4_as_string_sentences_sentiment = get_sentiment(leagues_ch_4_as_string_sentences, "syuzhet")
leagues_ch_4_as_string_sentences_sentiment

leagues_ch_5_as_string_sentences_sentiment = get_sentiment(leagues_ch_5_as_string_sentences, "syuzhet")
leagues_ch_5_as_string_sentences_sentiment

leagues_ch_6_as_string_sentences_sentiment = get_sentiment(leagues_ch_6_as_string_sentences, "syuzhet")
leagues_ch_6_as_string_sentences_sentiment

leagues_ch_7_as_string_sentences_sentiment = get_sentiment(leagues_ch_7_as_string_sentences, "syuzhet")
leagues_ch_8_as_string_sentences_sentiment

leagues_ch_8_as_string_sentences_sentiment = get_sentiment(leagues_ch_8_as_string_sentences, "syuzhet")
leagues_ch_8_as_string_sentences_sentiment

leagues_ch_9_as_string_sentences_sentiment = get_sentiment(leagues_ch_9_as_string_sentences, "syuzhet")
leagues_ch_9_as_string_sentences_sentiment

leagues_ch_10_as_string_sentences_sentiment = get_sentiment(leagues_ch_10_as_string_sentences, "syuzhet")
leagues_ch_10_as_string_sentences_sentiment


# let’s call get_sentiment with the default value of “bing”
leagues_ch_1_as_string_sentences_sentiment_bing = get_sentiment(leagues_ch_1_as_string_sentences, "bing")
leagues_ch_1_as_string_sentences_sentiment_bing

leagues_ch_2_as_string_sentences_sentiment_bing = get_sentiment(leagues_ch_2_as_string_sentences, "bing")
leagues_ch_2_as_string_sentences_sentiment_bing

leagues_ch_3_as_string_sentences_sentiment_bing = get_sentiment(leagues_ch_3_as_string_sentences, "bing")
leagues_ch_3_as_string_sentences_sentiment_bing

leagues_ch_4_as_string_sentences_sentiment_bing = get_sentiment(leagues_ch_4_as_string_sentences, "bing")
leagues_ch_4_as_string_sentences_sentiment_bing

leagues_ch_5_as_string_sentences_sentiment_bing = get_sentiment(leagues_ch_5_as_string_sentences, "bing")
leagues_ch_5_as_string_sentences_sentiment_bing

leagues_ch_6_as_string_sentences_sentiment_bing = get_sentiment(leagues_ch_6_as_string_sentences, "bing")
leagues_ch_6_as_string_sentences_sentiment_bing

leagues_ch_7_as_string_sentences_sentiment_bing = get_sentiment(leagues_ch_7_as_string_sentences, "bing")
leagues_ch_8_as_string_sentences_sentiment_bing

leagues_ch_8_as_string_sentences_sentiment_bing = get_sentiment(leagues_ch_8_as_string_sentences, "bing")
leagues_ch_8_as_string_sentences_sentiment_bing

leagues_ch_9_as_string_sentences_sentiment_bing = get_sentiment(leagues_ch_9_as_string_sentences, "bing")
leagues_ch_9_as_string_sentences_sentiment_bing

leagues_ch_10_as_string_sentences_sentiment_bing = get_sentiment(leagues_ch_10_as_string_sentences, "bing")
leagues_ch_10_as_string_sentences_sentiment_bing

# let’s look at the sentiment dictionary for “syuzhet”
leagues_chapters_as_string_sentences_sentiment_dictionary = get_sentiment_dictionary()
leagues_chapters_as_string_sentences_sentiment_dictionary

# let’s look at the sentiment dictionary for “bing”
leagues_chapters_as_string_sentences_sentiment_dictionary_bing = get_sentiment_dictionary('bing')
leagues_chapters_as_string_sentences_sentiment_dictionary_bing

# sum the values of the sentiment vector in order to get a measure of the overall emotional valence in the text
leagues_ch_1_as_string_sentences_sentiment_sum = sum(leagues_ch_1_as_string_sentences_sentiment)
leagues_ch_1_as_string_sentences_sentiment_sum

leagues_ch_2_as_string_sentences_sentiment_sum = sum(leagues_ch_2_as_string_sentences_sentiment)
leagues_ch_2_as_string_sentences_sentiment_sum

leagues_ch_3_as_string_sentences_sentiment_sum = sum(leagues_ch_3_as_string_sentences_sentiment)
leagues_ch_3_as_string_sentences_sentiment_sum

leagues_ch_4_as_string_sentences_sentiment_sum = sum(leagues_ch_4_as_string_sentences_sentiment)
leagues_ch_4_as_string_sentences_sentiment_sum

leagues_ch_5_as_string_sentences_sentiment_sum = sum(leagues_ch_5_as_string_sentences_sentiment)
leagues_ch_5_as_string_sentences_sentiment_sum

leagues_ch_6_as_string_sentences_sentiment_sum = sum(leagues_ch_6_as_string_sentences_sentiment)
leagues_ch_6_as_string_sentences_sentiment_sum

leagues_ch_7_as_string_sentences_sentiment_sum = sum(leagues_ch_7_as_string_sentences_sentiment)
leagues_ch_7_as_string_sentences_sentiment_sum

leagues_ch_8_as_string_sentences_sentiment_sum = sum(leagues_ch_8_as_string_sentences_sentiment)
leagues_ch_8_as_string_sentences_sentiment_sum

leagues_ch_9_as_string_sentences_sentiment_sum = sum(leagues_ch_9_as_string_sentences_sentiment)
leagues_ch_9_as_string_sentences_sentiment_sum

leagues_ch_10_as_string_sentences_sentiment_sum = sum(leagues_ch_10_as_string_sentences_sentiment)
leagues_ch_10_as_string_sentences_sentiment_sum


# sum the values of the sentiment(bing) vector in order to get a measure of the overall emotional valence in the text
leagues_ch_1_as_string_sentences_sentiment_bing_sum = sum(leagues_ch_1_as_string_sentences_sentiment_bing)
leagues_ch_1_as_string_sentences_sentiment_bing_sum

leagues_ch_2_as_string_sentences_sentiment_bing_sum = sum(leagues_ch_2_as_string_sentences_sentiment_bing)
leagues_ch_2_as_string_sentences_sentiment_bing_sum

leagues_ch_3_as_string_sentences_sentiment_bing_sum = sum(leagues_ch_3_as_string_sentences_sentiment_bing)
leagues_ch_3_as_string_sentences_sentiment_bing_sum

leagues_ch_4_as_string_sentences_sentiment_bing_sum = sum(leagues_ch_4_as_string_sentences_sentiment_bing)
leagues_ch_4_as_string_sentences_sentiment_bing_sum

leagues_ch_5_as_string_sentences_sentiment_bing_sum = sum(leagues_ch_5_as_string_sentences_sentiment_bing)
leagues_ch_5_as_string_sentences_sentiment_bing_sum

leagues_ch_6_as_string_sentences_sentiment_bing_sum = sum(leagues_ch_6_as_string_sentences_sentiment_bing)
leagues_ch_6_as_string_sentences_sentiment_bing_sum

leagues_ch_7_as_string_sentences_sentiment_bing_sum = sum(leagues_ch_7_as_string_sentences_sentiment_bing)
leagues_ch_7_as_string_sentences_sentiment_bing_sum

leagues_ch_8_as_string_sentences_sentiment_bing_sum = sum(leagues_ch_8_as_string_sentences_sentiment_bing)
leagues_ch_8_as_string_sentences_sentiment_bing_sum

leagues_ch_9_as_string_sentences_sentiment_bing_sum = sum(leagues_ch_9_as_string_sentences_sentiment_bing)
leagues_ch_9_as_string_sentences_sentiment_bing_sum

leagues_ch_10_as_string_sentences_sentiment_bing_sum = sum(leagues_ch_10_as_string_sentences_sentiment_bing)
leagues_ch_10_as_string_sentences_sentiment_bing_sum


# let’s look at the means
leagues_ch_1_as_string_sentences_sentiment_mean = mean(leagues_ch_1_as_string_sentences_sentiment)
leagues_ch_1_as_string_sentences_sentiment_mean

leagues_ch_2_as_string_sentences_sentiment_mean = mean(leagues_ch_2_as_string_sentences_sentiment)
leagues_ch_2_as_string_sentences_sentiment_mean

leagues_ch_3_as_string_sentences_sentiment_mean = mean(leagues_ch_3_as_string_sentences_sentiment)
leagues_ch_3_as_string_sentences_sentiment_mean

leagues_ch_4_as_string_sentences_sentiment_mean = mean(leagues_ch_4_as_string_sentences_sentiment)
leagues_ch_4_as_string_sentences_sentiment_mean

leagues_ch_5_as_string_sentences_sentiment_mean = mean(leagues_ch_5_as_string_sentences_sentiment)
leagues_ch_5_as_string_sentences_sentiment_mean

leagues_ch_6_as_string_sentences_sentiment_mean = mean(leagues_ch_6_as_string_sentences_sentiment)
leagues_ch_6_as_string_sentences_sentiment_mean

leagues_ch_7_as_string_sentences_sentiment_mean = mean(leagues_ch_7_as_string_sentences_sentiment)
leagues_ch_7_as_string_sentences_sentiment_mean

leagues_ch_8_as_string_sentences_sentiment_mean = mean(leagues_ch_8_as_string_sentences_sentiment)
leagues_ch_8_as_string_sentences_sentiment_mean

leagues_ch_9_as_string_sentences_sentiment_mean = mean(leagues_ch_9_as_string_sentences_sentiment)
leagues_ch_9_as_string_sentences_sentiment_mean

leagues_ch_10_as_string_sentences_sentiment_mean = mean(leagues_ch_10_as_string_sentences_sentiment)
leagues_ch_10_as_string_sentences_sentiment_mean

# let’s look at the means(bing)
leagues_ch_1_as_string_sentences_sentiment_bing_mean = mean(leagues_ch_1_as_string_sentences_sentiment_bing)
leagues_ch_1_as_string_sentences_sentiment_bing_mean

leagues_ch_2_as_string_sentences_sentiment_bing_mean = mean(leagues_ch_2_as_string_sentences_sentiment_bing)
leagues_ch_2_as_string_sentences_sentiment_bing_mean

leagues_ch_3_as_string_sentences_sentiment_bing_mean = mean(leagues_ch_3_as_string_sentences_sentiment_bing)
leagues_ch_3_as_string_sentences_sentiment_bing_mean

leagues_ch_4_as_string_sentences_sentiment_bing_mean = mean(leagues_ch_4_as_string_sentences_sentiment_bing)
leagues_ch_4_as_string_sentences_sentiment_bing_mean

leagues_ch_5_as_string_sentences_sentiment_bing_mean = mean(leagues_ch_5_as_string_sentences_sentiment_bing)
leagues_ch_5_as_string_sentences_sentiment_bing_mean

leagues_ch_6_as_string_sentences_sentiment_bing_mean = mean(leagues_ch_6_as_string_sentences_sentiment_bing)
leagues_ch_6_as_string_sentences_sentiment_bing_mean

leagues_ch_7_as_string_sentences_sentiment_bing_mean = mean(leagues_ch_7_as_string_sentences_sentiment_bing)
leagues_ch_7_as_string_sentences_sentiment_bing_mean

leagues_ch_8_as_string_sentences_sentiment_bing_mean = mean(leagues_ch_8_as_string_sentences_sentiment_bing)
leagues_ch_8_as_string_sentences_sentiment_bing_mean

leagues_ch_9_as_string_sentences_sentiment_bing_mean = mean(leagues_ch_9_as_string_sentences_sentiment_bing)
leagues_ch_9_as_string_sentences_sentiment_bing_mean

leagues_ch_10_as_string_sentences_sentiment_bing_mean = mean(leagues_ch_10_as_string_sentences_sentiment_bing)
leagues_ch_10_as_string_sentences_sentiment_bing_mean

# plot the values in a graph where the x-axis represents the passage of time from the beginning to the end of the text, and the y-axis measures 
# the degrees of positive and negative sentiment.
plot(leagues_ch_1_as_string_sentences_sentiment, main="Chapter 1 Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_2_as_string_sentences_sentiment, main="Chapter 2 Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_3_as_string_sentences_sentiment, main="Chapter 3 Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_4_as_string_sentences_sentiment, main="Chapter 4 Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_5_as_string_sentences_sentiment, main="Chapter 5 Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_6_as_string_sentences_sentiment, main="Chapter 6 Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_7_as_string_sentences_sentiment, main="Chapter 7 Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_8_as_string_sentences_sentiment, main="Chapter 8 Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_9_as_string_sentences_sentiment, main="Chapter 9 Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_10_as_string_sentences_sentiment, main="Chapter 10 Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")

# the same plotting for bing sentiments
plot(leagues_ch_1_as_string_sentences_sentiment_bing, main="Chapter 1 Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_2_as_string_sentences_sentiment_bing, main="Chapter 2 Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_3_as_string_sentences_sentiment_bing, main="Chapter 3 Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_4_as_string_sentences_sentiment_bing, main="Chapter 4 Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_5_as_string_sentences_sentiment_bing, main="Chapter 5 Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_6_as_string_sentences_sentiment_bing, main="Chapter 6 Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_7_as_string_sentences_sentiment_bing, main="Chapter 7 Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_8_as_string_sentences_sentiment_bing, main="Chapter 8 Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_9_as_string_sentences_sentiment_bing, main="Chapter 9 Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_10_as_string_sentences_sentiment_bing, main="Chapter 10 Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

# let's use get_percentage_values function to calculate mean sentiment valence for each divided text chunk (bin size = 10)
leagues_ch_1_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_1_as_string_sentences_sentiment, bins = 10)

leagues_ch_2_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_2_as_string_sentences_sentiment, bins = 10)

leagues_ch_3_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_3_as_string_sentences_sentiment, bins = 10)

leagues_ch_4_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_4_as_string_sentences_sentiment, bins = 10)

leagues_ch_5_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_5_as_string_sentences_sentiment, bins = 10)

leagues_ch_6_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_6_as_string_sentences_sentiment, bins = 10)

leagues_ch_7_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_7_as_string_sentences_sentiment, bins = 10)

leagues_ch_8_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_8_as_string_sentences_sentiment, bins = 10)

leagues_ch_9_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_9_as_string_sentences_sentiment, bins = 10)

leagues_ch_10_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_10_as_string_sentences_sentiment, bins = 10)


# let's use get_percentage_values function to calculate mean sentiment valence for each divided text chunk (bin size = 20)
leagues_ch_1_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_1_as_string_sentences_sentiment, bins = 20)

leagues_ch_2_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_2_as_string_sentences_sentiment, bins = 20)

leagues_ch_3_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_3_as_string_sentences_sentiment, bins = 20)

leagues_ch_4_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_4_as_string_sentences_sentiment, bins = 20)

leagues_ch_5_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_5_as_string_sentences_sentiment, bins = 20)

leagues_ch_6_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_6_as_string_sentences_sentiment, bins = 20)

leagues_ch_7_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_7_as_string_sentences_sentiment, bins = 20)

leagues_ch_8_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_8_as_string_sentences_sentiment, bins = 20)

leagues_ch_9_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_9_as_string_sentences_sentiment, bins = 20)

leagues_ch_10_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_10_as_string_sentences_sentiment, bins = 20)

# ------------------------------------------------------------------- END TASK B



# ----------------------------------------------------------------------- TASK C

# -------------------------------------------------- FUNCTIONS

# function to get given length sized words
get_n_sized_words = function(chapter, size=2) {
  # tokenizing content to words
  tokens = tokenize_words(chapter$content)
  
  # combining all elements within single vector/list
  unlisted_tokens = unlist(tokens)
  
  # getting all words with the required size
  selected_words = unlisted_tokens[which(nchar(unlisted_tokens) == size)]
  
  return (selected_words)
}



# ---------------------------------------------- END FUNCTIONS

# getting all chapters 2 length words
leagues_stopsless_ch_1_size_2_words = get_n_sized_words(leagues_stopsless[[1]], 2)

leagues_stopsless_ch_2_size_2_words = get_n_sized_words(leagues_stopsless[[2]], 2)

leagues_stopsless_ch_3_size_2_words = get_n_sized_words(leagues_stopsless[[3]], 2)

leagues_stopsless_ch_4_size_2_words = get_n_sized_words(leagues_stopsless[[4]], 2)

leagues_stopsless_ch_5_size_2_words = get_n_sized_words(leagues_stopsless[[5]], 2)

leagues_stopsless_ch_6_size_2_words = get_n_sized_words(leagues_stopsless[[6]], 2)

leagues_stopsless_ch_7_size_2_words = get_n_sized_words(leagues_stopsless[[7]], 2)

leagues_stopsless_ch_8_size_2_words = get_n_sized_words(leagues_stopsless[[8]], 2)

leagues_stopsless_ch_9_size_2_words = get_n_sized_words(leagues_stopsless[[9]], 2)

leagues_stopsless_ch_10_size_2_words = get_n_sized_words(leagues_stopsless[[10]], 2)


# getting all chapters 3 length words
leagues_stopsless_ch_1_size_3_words = get_n_sized_words(leagues_stopsless[[1]], 3)

leagues_stopsless_ch_2_size_3_words = get_n_sized_words(leagues_stopsless[[2]], 3)

leagues_stopsless_ch_3_size_3_words = get_n_sized_words(leagues_stopsless[[3]], 3)

leagues_stopsless_ch_4_size_3_words = get_n_sized_words(leagues_stopsless[[4]], 3)

leagues_stopsless_ch_5_size_3_words = get_n_sized_words(leagues_stopsless[[5]], 3)

leagues_stopsless_ch_6_size_3_words = get_n_sized_words(leagues_stopsless[[6]], 3)

leagues_stopsless_ch_7_size_3_words = get_n_sized_words(leagues_stopsless[[7]], 3)

leagues_stopsless_ch_8_size_3_words = get_n_sized_words(leagues_stopsless[[8]], 3)

leagues_stopsless_ch_9_size_3_words = get_n_sized_words(leagues_stopsless[[9]], 3)

leagues_stopsless_ch_10_size_3_words = get_n_sized_words(leagues_stopsless[[10]], 3)


# getting all chapters 4 length words
leagues_stopsless_ch_1_size_4_words = get_n_sized_words(leagues_stopsless[[1]], 4)

leagues_stopsless_ch_2_size_4_words = get_n_sized_words(leagues_stopsless[[2]], 4)

leagues_stopsless_ch_3_size_4_words = get_n_sized_words(leagues_stopsless[[3]], 4)

leagues_stopsless_ch_4_size_4_words = get_n_sized_words(leagues_stopsless[[4]], 4)

leagues_stopsless_ch_5_size_4_words = get_n_sized_words(leagues_stopsless[[5]], 4)

leagues_stopsless_ch_6_size_4_words = get_n_sized_words(leagues_stopsless[[6]], 4)

leagues_stopsless_ch_7_size_4_words = get_n_sized_words(leagues_stopsless[[7]], 4)

leagues_stopsless_ch_8_size_4_words = get_n_sized_words(leagues_stopsless[[8]], 4)

leagues_stopsless_ch_9_size_4_words = get_n_sized_words(leagues_stopsless[[9]], 4)

leagues_stopsless_ch_10_size_4_words = get_n_sized_words(leagues_stopsless[[10]], 4)

# combining all 2 lenght words together
leagues_stopsless_chapters_size_2_words = c(leagues_stopsless_ch_1_size_2_words, 
                                        leagues_stopsless_ch_2_size_2_words, 
                                        leagues_stopsless_ch_3_size_2_words, 
                                        leagues_stopsless_ch_4_size_2_words, 
                                        leagues_stopsless_ch_5_size_2_words, 
                                        leagues_stopsless_ch_6_size_2_words, 
                                        leagues_stopsless_ch_7_size_2_words, 
                                        leagues_stopsless_ch_8_size_2_words, 
                                        leagues_stopsless_ch_9_size_2_words, 
                                        leagues_stopsless_ch_10_size_2_words)


# combining all 3 lenght words together
leagues_stopsless_chapters_size_3_words = c(leagues_stopsless_ch_1_size_3_words, 
                                            leagues_stopsless_ch_2_size_3_words, 
                                            leagues_stopsless_ch_3_size_3_words, 
                                            leagues_stopsless_ch_4_size_3_words, 
                                            leagues_stopsless_ch_5_size_3_words, 
                                            leagues_stopsless_ch_6_size_3_words, 
                                            leagues_stopsless_ch_7_size_3_words, 
                                            leagues_stopsless_ch_8_size_3_words, 
                                            leagues_stopsless_ch_9_size_3_words, 
                                            leagues_stopsless_ch_10_size_3_words)

# combining all 4 lenght words together
leagues_stopsless_chapters_size_4_words = c(leagues_stopsless_ch_1_size_4_words, 
                                            leagues_stopsless_ch_2_size_4_words, 
                                            leagues_stopsless_ch_3_size_4_words, 
                                            leagues_stopsless_ch_4_size_4_words, 
                                            leagues_stopsless_ch_5_size_4_words, 
                                            leagues_stopsless_ch_6_size_4_words, 
                                            leagues_stopsless_ch_7_size_4_words, 
                                            leagues_stopsless_ch_8_size_4_words, 
                                            leagues_stopsless_ch_9_size_4_words, 
                                            leagues_stopsless_ch_10_size_4_words)

# removing 2 length words from stopwords removed and lowered VCorpus
leagues_stopsless_2_lengthed_words_removed = tm_map(leagues_stopsless, removeWords, leagues_stopsless_chapters_size_2_words)
inspect_vcorpus(leagues_stopsless_2_lengthed_words_removed)

# creating TDM again without lenght 2 words
leagues_stopsless_2_lengthed_words_removed_TDM = TermDocumentMatrix(leagues_stopsless_2_lengthed_words_removed)

# forming and plotting dendograms
print_str_and_plotting_dendograms(leagues_stopsless_2_lengthed_words_removed_TDM)


# removing 3 length words from stopwords removed and lowered VCorpus
leagues_stopsless_3_lengthed_words_removed = tm_map(leagues_stopsless, removeWords, leagues_stopsless_chapters_size_3_words)
inspect_vcorpus(leagues_stopsless_3_lengthed_words_removed)

# creating TDM again without lenght 3 words
leagues_stopsless_3_lengthed_words_removed_TDM = TermDocumentMatrix(leagues_stopsless_3_lengthed_words_removed)

# forming and plotting dendograms
print_str_and_plotting_dendograms(leagues_stopsless_3_lengthed_words_removed_TDM)



# removing 4 length words from stopwords removed and lowered VCorpus
leagues_stopsless_4_lengthed_words_removed = tm_map(leagues_stopsless, removeWords, leagues_stopsless_chapters_size_4_words)
inspect_vcorpus(leagues_stopsless_4_lengthed_words_removed)

# creating TDM again without lenght 4 words
leagues_stopsless_4_lengthed_words_removed_TDM = TermDocumentMatrix(leagues_stopsless_4_lengthed_words_removed)

# forming and plotting dendograms
print_str_and_plotting_dendograms(leagues_stopsless_4_lengthed_words_removed_TDM)

# --------------------------------------------------------------------END TASK C

# ----------------------------------------------------------------------- TASK D

# ---------------------------------------- FUNCTION

# function to create dataframe having a length of 5 or greater words and tags
get_length_over_5_words_dataframe = function(sentence) {
  sentence = as.String(sentence)
  
  # chunking needs word token annotations with POS tags
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  a2 <- annotate(sentence, list(sent_token_annotator, word_token_annotator))
  
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  a3 <- annotate(sentence, pos_tag_annotator, a2)
  
  # determine the distribution of POS tags for word tokens
  a3w <- subset(a3, type == "word")
  tags <- sapply(a3w$features, '[[', "POS")
  sentence[a3w]
  
  df = data.frame(sentence[a3w], tags)
  df
  
  df = filter(df, nchar(df[,1]) > 5)
  
  return (df)
}

# filter nouns
get_length_over_5_words_nouns = function(sentence) {
  
  df = get_length_over_5_words_dataframe(sentence)
  
  # filter only nouns
  nouns = filter(df, substring(df[,2], 0, 1) == 'N')
  
  return (nouns)
}

get_length_over_5_words_verbs = function(sentence) {
  df = get_length_over_5_words_dataframe(sentence)
  
  # filter only verbs
  verbs = filter(df, substring(df[,2], 0, 1) == 'V')
  
  return (verbs)
}

# ------------------------------------ END FUNCTION

# get 10 longest sentences for each chapter
# chapter 1
get_length_over_5_words_nouns(get_10_longest_sentences(leagues_stopsless[[1]]))
get_length_over_5_words_verbs(get_10_longest_sentences(leagues_stopsless[[1]]))

# chapter 2
get_length_over_5_words_nouns(get_10_longest_sentences(leagues_stopsless[[2]]))
get_length_over_5_words_verbs(get_10_longest_sentences(leagues_stopsless[[2]]))

# chapter 3
get_length_over_5_words_nouns(get_10_longest_sentences(leagues_stopsless[[3]]))
get_length_over_5_words_verbs(get_10_longest_sentences(leagues_stopsless[[3]]))

# chapter 4
get_length_over_5_words_nouns(get_10_longest_sentences(leagues_stopsless[[4]]))
get_length_over_5_words_verbs(get_10_longest_sentences(leagues_stopsless[[4]]))

# chapter 5
get_length_over_5_words_nouns(get_10_longest_sentences(leagues_stopsless[[5]]))
get_length_over_5_words_verbs(get_10_longest_sentences(leagues_stopsless[[5]]))

# chapter 6
get_length_over_5_words_nouns(get_10_longest_sentences(leagues_stopsless[[6]]))
get_length_over_5_words_verbs(get_10_longest_sentences(leagues_stopsless[[6]]))

# chapter 7
get_length_over_5_words_nouns(get_10_longest_sentences(leagues_stopsless[[7]]))
get_length_over_5_words_verbs(get_10_longest_sentences(leagues_stopsless[[7]]))

# chapter 8
get_length_over_5_words_nouns(get_10_longest_sentences(leagues_stopsless[[8]]))
get_length_over_5_words_verbs(get_10_longest_sentences(leagues_stopsless[[8]]))

# chapter 9
get_length_over_5_words_nouns(get_10_longest_sentences(leagues_stopsless[[9]]))
get_length_over_5_words_verbs(get_10_longest_sentences(leagues_stopsless[[9]]))

# chapter 10
get_length_over_5_words_nouns(get_10_longest_sentences(leagues_stopsless[[10]]))
get_length_over_5_words_verbs(get_10_longest_sentences(leagues_stopsless[[10]]))

# ------------------------------------------------------------------- END TASK D

# ----------------------------------------------------------------------- TASK E
# creating a spc object for each chapter to use with zipfR functions
leagues_stopsless_term_freq_ch_1.spc = text2spc.fnc(leagues_stopsless_term_freq_ch_1)
leagues_stopsless_term_freq_ch_1.spc

leagues_stopsless_term_freq_ch_2.spc = text2spc.fnc(leagues_stopsless_term_freq_ch_2)
leagues_stopsless_term_freq_ch_2.spc

leagues_stopsless_term_freq_ch_3.spc = text2spc.fnc(leagues_stopsless_term_freq_ch_3)
leagues_stopsless_term_freq_ch_3.spc

leagues_stopsless_term_freq_ch_4.spc = text2spc.fnc(leagues_stopsless_term_freq_ch_4)
leagues_stopsless_term_freq_ch_4.spc

leagues_stopsless_term_freq_ch_5.spc = text2spc.fnc(leagues_stopsless_term_freq_ch_5)
leagues_stopsless_term_freq_ch_5.spc

leagues_stopsless_term_freq_ch_6.spc = text2spc.fnc(leagues_stopsless_term_freq_ch_6)
leagues_stopsless_term_freq_ch_6.spc

leagues_stopsless_term_freq_ch_7.spc = text2spc.fnc(leagues_stopsless_term_freq_ch_7)
leagues_stopsless_term_freq_ch_7.spc

leagues_stopsless_term_freq_ch_8.spc = text2spc.fnc(leagues_stopsless_term_freq_ch_8)
leagues_stopsless_term_freq_ch_8.spc

leagues_stopsless_term_freq_ch_9.spc = text2spc.fnc(leagues_stopsless_term_freq_ch_9)
leagues_stopsless_term_freq_ch_9.spc

leagues_stopsless_term_freq_ch_10.spc = text2spc.fnc(leagues_stopsless_term_freq_ch_10)
leagues_stopsless_term_freq_ch_10.spc


# getting summary: summary applied to a spectrum
# data structure returns a useful sketch of the spectrum (N, V , the type frequencies of the first few classes of the spectrum)
summary(leagues_stopsless_term_freq_ch_1.spc)

summary(leagues_stopsless_term_freq_ch_2.spc)

summary(leagues_stopsless_term_freq_ch_3.spc)

summary(leagues_stopsless_term_freq_ch_4.spc)

summary(leagues_stopsless_term_freq_ch_5.spc)

summary(leagues_stopsless_term_freq_ch_6.spc)

summary(leagues_stopsless_term_freq_ch_7.spc)

summary(leagues_stopsless_term_freq_ch_8.spc)

summary(leagues_stopsless_term_freq_ch_9.spc)

summary(leagues_stopsless_term_freq_ch_10.spc)


# the function N returns the sample size N, V returns the vocabulary size V
N(leagues_stopsless_term_freq_ch_1.spc)
V(leagues_stopsless_term_freq_ch_1.spc)

N(leagues_stopsless_term_freq_ch_2.spc)
V(leagues_stopsless_term_freq_ch_2.spc)

N(leagues_stopsless_term_freq_ch_3.spc)
V(leagues_stopsless_term_freq_ch_3.spc)

N(leagues_stopsless_term_freq_ch_4.spc)
V(leagues_stopsless_term_freq_ch_4.spc)

N(leagues_stopsless_term_freq_ch_5.spc)
V(leagues_stopsless_term_freq_ch_5.spc)

N(leagues_stopsless_term_freq_ch_6.spc)
V(leagues_stopsless_term_freq_ch_6.spc)

N(leagues_stopsless_term_freq_ch_7.spc)
V(leagues_stopsless_term_freq_ch_7.spc)

N(leagues_stopsless_term_freq_ch_8.spc)
V(leagues_stopsless_term_freq_ch_8.spc)

N(leagues_stopsless_term_freq_ch_9.spc)
V(leagues_stopsless_term_freq_ch_9.spc)

N(leagues_stopsless_term_freq_ch_10.spc)
V(leagues_stopsless_term_freq_ch_10.spc)


# plotting the spc files of each chapter
with(leagues_stopsless_term_freq_ch_1.spc, plot(m, Vm, main="Chapter 1: Frequency Spectrum"))

with(leagues_stopsless_term_freq_ch_2.spc, plot(m, Vm, main="Chapter 2: Frequency Spectrum"))

with(leagues_stopsless_term_freq_ch_3.spc, plot(m, Vm, main="Chapter 3: Frequency Spectrum"))

with(leagues_stopsless_term_freq_ch_4.spc, plot(m, Vm, main="Chapter 4: Frequency Spectrum"))

with(leagues_stopsless_term_freq_ch_5.spc, plot(m, Vm, main="Chapter 5: Frequency Spectrum"))

with(leagues_stopsless_term_freq_ch_6.spc, plot(m, Vm, main="Chapter 6: Frequency Spectrum"))

with(leagues_stopsless_term_freq_ch_7.spc, plot(m, Vm, main="Chapter 7: Frequency Spectrum"))

with(leagues_stopsless_term_freq_ch_8.spc, plot(m, Vm, main="Chapter 8: Frequency Spectrum"))

with(leagues_stopsless_term_freq_ch_9.spc, plot(m, Vm, main="Chapter 9: Frequency Spectrum"))

with(leagues_stopsless_term_freq_ch_10.spc, plot(m, Vm, main="Chapter 10: Frequency Spectrum"))


# now, use chapter scps to estimate a fZM model
leagues_stopsless_term_freq_ch_1.fzm <- lnre("fzm", leagues_stopsless_term_freq_ch_1.spc, exact=FALSE)
summary(leagues_stopsless_term_freq_ch_1.fzm)

leagues_stopsless_term_freq_ch_2.fzm <- lnre("fzm", leagues_stopsless_term_freq_ch_2.spc, exact=FALSE)
summary(leagues_stopsless_term_freq_ch_2.fzm)

leagues_stopsless_term_freq_ch_3.fzm <- lnre("fzm", leagues_stopsless_term_freq_ch_3.spc, exact=FALSE)
summary(leagues_stopsless_term_freq_ch_3.fzm)

leagues_stopsless_term_freq_ch_4.fzm <- lnre("fzm", leagues_stopsless_term_freq_ch_4.spc, exact=FALSE)
summary(leagues_stopsless_term_freq_ch_4.fzm)

leagues_stopsless_term_freq_ch_5.fzm <- lnre("fzm", leagues_stopsless_term_freq_ch_5.spc, exact=FALSE)
summary(leagues_stopsless_term_freq_ch_5.fzm)

leagues_stopsless_term_freq_ch_6.fzm <- lnre("fzm", leagues_stopsless_term_freq_ch_6.spc, exact=FALSE)
summary(leagues_stopsless_term_freq_ch_6.fzm)

leagues_stopsless_term_freq_ch_7.fzm <- lnre("fzm", leagues_stopsless_term_freq_ch_7.spc, exact=FALSE)
summary(leagues_stopsless_term_freq_ch_7.fzm)

leagues_stopsless_term_freq_ch_8.fzm <- lnre("fzm", leagues_stopsless_term_freq_ch_8.spc, exact=FALSE)
summary(leagues_stopsless_term_freq_ch_8.fzm)

leagues_stopsless_term_freq_ch_9.fzm <- lnre("fzm", leagues_stopsless_term_freq_ch_9.spc, exact=FALSE)
summary(leagues_stopsless_term_freq_ch_9.fzm)

leagues_stopsless_term_freq_ch_10.fzm <- lnre("fzm", leagues_stopsless_term_freq_ch_10.spc, exact=FALSE)
summary(leagues_stopsless_term_freq_ch_10.fzm)
# --------------------------------------------------------------------END TASK E


# ----------------------------------------------------------------------- TASK F
# creating paletter of colors
palette_1 = brewer.pal(9, "BuGn")
palette_2 = brewer.pal(9, "Spectral")
str(palette_1)
str(palette_2)


# creating wordclouds with patette 1 
leagues_stopsless_term_freq_ch_1_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_1_words, leagues_stopsless_term_freq_ch_1, colors=palette_1)

leagues_stopsless_term_freq_ch_2_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_2_words, leagues_stopsless_term_freq_ch_2, colors=palette_1)

leagues_stopsless_term_freq_ch_3_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_3_words, leagues_stopsless_term_freq_ch_3, colors=palette_1)

leagues_stopsless_term_freq_ch_4_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_4_words, leagues_stopsless_term_freq_ch_4, colors=palette_1)

leagues_stopsless_term_freq_ch_5_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_5_words, leagues_stopsless_term_freq_ch_5, colors=palette_1)

leagues_stopsless_term_freq_ch_6_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_6_words, leagues_stopsless_term_freq_ch_6, colors=palette_1)

leagues_stopsless_term_freq_ch_7_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_7_words, leagues_stopsless_term_freq_ch_7, colors=palette_1)

leagues_stopsless_term_freq_ch_8_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_8_words, leagues_stopsless_term_freq_ch_8, colors=palette_1)

leagues_stopsless_term_freq_ch_9_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_9_words, leagues_stopsless_term_freq_ch_9, colors=palette_1)

leagues_stopsless_term_freq_ch_10_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_10_words, leagues_stopsless_term_freq_ch_10, colors=palette_1)


# creating wordclouds with patette 2
leagues_stopsless_term_freq_ch_1_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_1_words, leagues_stopsless_term_freq_ch_1, colors=palette_2)

leagues_stopsless_term_freq_ch_2_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_2_words, leagues_stopsless_term_freq_ch_2, colors=palette_2)

leagues_stopsless_term_freq_ch_3_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_3_words, leagues_stopsless_term_freq_ch_3, colors=palette_2)

leagues_stopsless_term_freq_ch_4_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_4_words, leagues_stopsless_term_freq_ch_4, colors=palette_2)

leagues_stopsless_term_freq_ch_5_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_5_words, leagues_stopsless_term_freq_ch_5, colors=palette_2)

leagues_stopsless_term_freq_ch_6_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_6_words, leagues_stopsless_term_freq_ch_6, colors=palette_2)

leagues_stopsless_term_freq_ch_7_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_7_words, leagues_stopsless_term_freq_ch_7, colors=palette_2)

leagues_stopsless_term_freq_ch_8_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_8_words, leagues_stopsless_term_freq_ch_8, colors=palette_2)

leagues_stopsless_term_freq_ch_9_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_9_words, leagues_stopsless_term_freq_ch_9, colors=palette_2)

leagues_stopsless_term_freq_ch_10_words_WC = wordcloud::wordcloud(leagues_stopsless_term_freq_ch_10_words, leagues_stopsless_term_freq_ch_10, colors=palette_2)

# lets look at 1st few lines of the cleaned vcorpus (this will be shown )
leagues_cleaned_ch_1_text <- leagues_cleaned[[1]]
leagues_cleaned_ch_1_text$content[1:10]

leagues_cleaned_ch_2_text <- leagues_cleaned[[2]]
leagues_cleaned_ch_2_text$content[1:10]

leagues_cleaned_ch_3_text <- leagues_cleaned[[3]]
leagues_cleaned_ch_3_text$content[1:10]

leagues_cleaned_ch_4_text <- leagues_cleaned[[4]]
leagues_cleaned_ch_4_text$content[1:10]

leagues_cleaned_ch_5_text <- leagues_cleaned[[5]]
leagues_cleaned_ch_5_text$content[1:10]

leagues_cleaned_ch_6_text <- leagues_cleaned[[6]]
leagues_cleaned_ch_6_text$content[1:10]

leagues_cleaned_ch_7_text <- leagues_cleaned[[7]]
leagues_cleaned_ch_7_text$content[1:10]

leagues_cleaned_ch_8_text <- leagues_cleaned[[8]]
leagues_cleaned_ch_8_text$content[1:10]

leagues_cleaned_ch_9_text <- leagues_cleaned[[9]]
leagues_cleaned_ch_9_text$content[1:10]

leagues_cleaned_ch_10_text <- leagues_cleaned[[10]]
leagues_cleaned_ch_10_text$content[1:10]


# applying tokens for chapter contents
leagues_cleaned_ch_1_text_tokens = tokens(leagues_cleaned_ch_1_text$content)
str(leagues_cleaned_ch_1_text_tokens)

leagues_cleaned_ch_2_text_tokens = tokens(leagues_cleaned_ch_2_text$content)
str(leagues_cleaned_ch_2_text_tokens)

leagues_cleaned_ch_3_text_tokens = tokens(leagues_cleaned_ch_3_text$content)
str(leagues_cleaned_ch_3_text_tokens)

leagues_cleaned_ch_4_text_tokens = tokens(leagues_cleaned_ch_4_text$content)
str(leagues_cleaned_ch_4_text_tokens)

leagues_cleaned_ch_5_text_tokens = tokens(leagues_cleaned_ch_5_text$content)
str(leagues_cleaned_ch_5_text_tokens)

leagues_cleaned_ch_6_text_tokens = tokens(leagues_cleaned_ch_6_text$content)
str(leagues_cleaned_ch_6_text_tokens)

leagues_cleaned_ch_7_text_tokens = tokens(leagues_cleaned_ch_7_text$content)
str(leagues_cleaned_ch_7_text_tokens)

leagues_cleaned_ch_8_text_tokens = tokens(leagues_cleaned_ch_8_text$content)
str(leagues_cleaned_ch_8_text_tokens)

leagues_cleaned_ch_9_text_tokens = tokens(leagues_cleaned_ch_9_text$content)
str(leagues_cleaned_ch_9_text_tokens)

leagues_cleaned_ch_10_text_tokens = tokens(leagues_cleaned_ch_10_text$content)
str(leagues_cleaned_ch_10_text_tokens)


# construct a sparse document-feature matrix
leagues_cleaned_ch_1_text_tokens_dfm <- quanteda::dfm(leagues_cleaned_ch_1_text_tokens, remove=c(english_stopwords, custom_stopwords))
str(leagues_cleaned_ch_1_text_tokens_dfm)

leagues_cleaned_ch_2_text_tokens_dfm <- quanteda::dfm(leagues_cleaned_ch_2_text_tokens, remove=c(english_stopwords, custom_stopwords))
str(leagues_cleaned_ch_2_text_tokens_dfm)

leagues_cleaned_ch_3_text_tokens_dfm <- quanteda::dfm(leagues_cleaned_ch_3_text_tokens, remove=c(english_stopwords, custom_stopwords))
str(leagues_cleaned_ch_3_text_tokens_dfm)

leagues_cleaned_ch_4_text_tokens_dfm <- quanteda::dfm(leagues_cleaned_ch_4_text_tokens, remove=c(english_stopwords, custom_stopwords))
str(leagues_cleaned_ch_4_text_tokens_dfm)

leagues_cleaned_ch_5_text_tokens_dfm <- quanteda::dfm(leagues_cleaned_ch_5_text_tokens, remove=c(english_stopwords, custom_stopwords))
str(leagues_cleaned_ch_5_text_tokens_dfm)

leagues_cleaned_ch_6_text_tokens_dfm <- quanteda::dfm(leagues_cleaned_ch_6_text_tokens, remove=c(english_stopwords, custom_stopwords))
str(leagues_cleaned_ch_6_text_tokens_dfm)

leagues_cleaned_ch_7_text_tokens_dfm <- dfm(leagues_cleaned_ch_7_text_tokens, remove=c(english_stopwords, custom_stopwords))
str(leagues_cleaned_ch_7_text_tokens_dfm)

leagues_cleaned_ch_8_text_tokens_dfm <- quanteda::dfm(leagues_cleaned_ch_8_text_tokens, remove=c(english_stopwords, custom_stopwords))
str(leagues_cleaned_ch_8_text_tokens_dfm)

leagues_cleaned_ch_9_text_tokens_dfm <- quanteda::dfm(leagues_cleaned_ch_9_text_tokens, remove=c(english_stopwords, custom_stopwords))
str(leagues_cleaned_ch_9_text_tokens_dfm)

leagues_cleaned_ch_10_text_tokens_dfm <- quanteda::dfm(leagues_cleaned_ch_10_text_tokens, remove=c(english_stopwords, custom_stopwords))
str(leagues_cleaned_ch_10_text_tokens_dfm)


# let’s get the frequency of terms in the dfm
leagues_cleaned_ch_1_text_tokens_dfm_freq <- quanteda::docfreq(leagues_cleaned_ch_1_text_tokens_dfm)
str(leagues_cleaned_ch_1_text_tokens_dfm_freq)

leagues_cleaned_ch_2_text_tokens_dfm_freq <- quanteda::docfreq(leagues_cleaned_ch_2_text_tokens_dfm)
str(leagues_cleaned_ch_2_text_tokens_dfm_freq)

leagues_cleaned_ch_3_text_tokens_dfm_freq <- quanteda::docfreq(leagues_cleaned_ch_3_text_tokens_dfm)
str(leagues_cleaned_ch_3_text_tokens_dfm_freq)

leagues_cleaned_ch_4_text_tokens_dfm_freq <- quanteda::docfreq(leagues_cleaned_ch_4_text_tokens_dfm)
str(leagues_cleaned_ch_4_text_tokens_dfm_freq)

leagues_cleaned_ch_5_text_tokens_dfm_freq <- quanteda::docfreq(leagues_cleaned_ch_5_text_tokens_dfm)
str(leagues_cleaned_ch_5_text_tokens_dfm_freq)

leagues_cleaned_ch_6_text_tokens_dfm_freq <- quanteda::docfreq(leagues_cleaned_ch_6_text_tokens_dfm)
str(leagues_cleaned_ch_6_text_tokens_dfm_freq)

leagues_cleaned_ch_7_text_tokens_dfm_freq <- quanteda::docfreq(leagues_cleaned_ch_7_text_tokens_dfm)
str(leagues_cleaned_ch_7_text_tokens_dfm_freq)

leagues_cleaned_ch_8_text_tokens_dfm_freq <- quanteda::docfreq(leagues_cleaned_ch_8_text_tokens_dfm)
str(leagues_cleaned_ch_8_text_tokens_dfm_freq)

leagues_cleaned_ch_9_text_tokens_dfm_freq <- quanteda::docfreq(leagues_cleaned_ch_9_text_tokens_dfm)
str(leagues_cleaned_ch_9_text_tokens_dfm_freq)

leagues_cleaned_ch_10_text_tokens_dfm_freq <- quanteda::docfreq(leagues_cleaned_ch_10_text_tokens_dfm)
str(leagues_cleaned_ch_10_text_tokens_dfm_freq)

# let’s assign weights to these words in the dfm
leagues_cleaned_ch_1_text_tokens_dfm_weights <- quanteda::dfm_weight(leagues_cleaned_ch_1_text_tokens_dfm)
str(leagues_cleaned_ch_1_text_tokens_dfm_weights)
leagues_cleaned_ch_1_text_tokens_dfm_weights

leagues_cleaned_ch_2_text_tokens_dfm_weights <- quanteda::dfm_weight(leagues_cleaned_ch_2_text_tokens_dfm)
str(leagues_cleaned_ch_2_text_tokens_dfm_weights)
leagues_cleaned_ch_2_text_tokens_dfm_weights

leagues_cleaned_ch_3_text_tokens_dfm_weights <- quanteda::dfm_weight(leagues_cleaned_ch_3_text_tokens_dfm)
str(leagues_cleaned_ch_3_text_tokens_dfm_weights)
leagues_cleaned_ch_3_text_tokens_dfm_weights

leagues_cleaned_ch_4_text_tokens_dfm_weights <- quanteda::dfm_weight(leagues_cleaned_ch_4_text_tokens_dfm)
str(leagues_cleaned_ch_4_text_tokens_dfm_weights)
leagues_cleaned_ch_4_text_tokens_dfm_weights

leagues_cleaned_ch_5_text_tokens_dfm_weights <- quanteda::dfm_weight(leagues_cleaned_ch_5_text_tokens_dfm)
str(leagues_cleaned_ch_5_text_tokens_dfm_weights)
leagues_cleaned_ch_5_text_tokens_dfm_weights

leagues_cleaned_ch_6_text_tokens_dfm_weights <- quanteda::dfm_weight(leagues_cleaned_ch_6_text_tokens_dfm)
str(leagues_cleaned_ch_6_text_tokens_dfm_weights)
leagues_cleaned_ch_6_text_tokens_dfm_weights

leagues_cleaned_ch_7_text_tokens_dfm_weights <- quanteda::dfm_weight(leagues_cleaned_ch_7_text_tokens_dfm)
str(leagues_cleaned_ch_7_text_tokens_dfm_weights)
leagues_cleaned_ch_7_text_tokens_dfm_weights

leagues_cleaned_ch_8_text_tokens_dfm_weights <- quanteda::dfm_weight(leagues_cleaned_ch_8_text_tokens_dfm)
str(leagues_cleaned_ch_8_text_tokens_dfm_weights)
leagues_cleaned_ch_8_text_tokens_dfm_weights

leagues_cleaned_ch_9_text_tokens_dfm_weights <- quanteda::dfm_weight(leagues_cleaned_ch_9_text_tokens_dfm)
str(leagues_cleaned_ch_9_text_tokens_dfm_weights)
leagues_cleaned_ch_9_text_tokens_dfm_weights

leagues_cleaned_ch_10_text_tokens_dfm_weights <- quanteda::dfm_weight(leagues_cleaned_ch_10_text_tokens_dfm)
str(leagues_cleaned_ch_10_text_tokens_dfm_weights)
leagues_cleaned_ch_10_text_tokens_dfm_weights


# let’s compute the tf-idf score
leagues_cleaned_ch_1_text_tokens_dfm_tfidf <- quanteda::dfm_tfidf(leagues_cleaned_ch_1_text_tokens_dfm, scheme_tf="count", scheme_df="inverse")
str(leagues_cleaned_ch_1_text_tokens_dfm_tfidf)

leagues_cleaned_ch_2_text_tokens_dfm_tfidf <- quanteda::dfm_tfidf(leagues_cleaned_ch_2_text_tokens_dfm, scheme_tf="count", scheme_df="inverse")
str(leagues_cleaned_ch_2_text_tokens_dfm_tfidf)

leagues_cleaned_ch_3_text_tokens_dfm_tfidf <- quanteda::dfm_tfidf(leagues_cleaned_ch_3_text_tokens_dfm, scheme_tf="count", scheme_df="inverse")
str(leagues_cleaned_ch_3_text_tokens_dfm_tfidf)

leagues_cleaned_ch_4_text_tokens_dfm_tfidf <- quanteda::dfm_tfidf(leagues_cleaned_ch_4_text_tokens_dfm, scheme_tf="count", scheme_df="inverse")
str(leagues_cleaned_ch_4_text_tokens_dfm_tfidf)

leagues_cleaned_ch_5_text_tokens_dfm_tfidf <- quanteda::dfm_tfidf(leagues_cleaned_ch_5_text_tokens_dfm, scheme_tf="count", scheme_df="inverse")
str(leagues_cleaned_ch_5_text_tokens_dfm_tfidf)

leagues_cleaned_ch_6_text_tokens_dfm_tfidf <- quanteda::dfm_tfidf(leagues_cleaned_ch_6_text_tokens_dfm, scheme_tf="count", scheme_df="inverse")
str(leagues_cleaned_ch_6_text_tokens_dfm_tfidf)

leagues_cleaned_ch_7_text_tokens_dfm_tfidf <- quanteda::dfm_tfidf(leagues_cleaned_ch_7_text_tokens_dfm, scheme_tf="count", scheme_df="inverse")
str(leagues_cleaned_ch_7_text_tokens_dfm_tfidf)

leagues_cleaned_ch_8_text_tokens_dfm_tfidf <- quanteda::dfm_tfidf(leagues_cleaned_ch_8_text_tokens_dfm, scheme_tf="count", scheme_df="inverse")
str(leagues_cleaned_ch_8_text_tokens_dfm_tfidf)

leagues_cleaned_ch_9_text_tokens_dfm_tfidf <- quanteda::dfm_tfidf(leagues_cleaned_ch_9_text_tokens_dfm, scheme_tf="count", scheme_df="inverse")
str(leagues_cleaned_ch_9_text_tokens_dfm_tfidf)

leagues_cleaned_ch_10_text_tokens_dfm_tfidf <- quanteda::dfm_tfidf(leagues_cleaned_ch_10_text_tokens_dfm, scheme_tf="count", scheme_df="inverse")
str(leagues_cleaned_ch_10_text_tokens_dfm_tfidf)


# extracting text of the documents as data frame
leagues_cleaned_ch_1_text_DF = as.data.frame(leagues_cleaned_ch_1_text$content)
leagues_cleaned_ch_1_text_DF

leagues_cleaned_ch_2_text_DF = as.data.frame(leagues_cleaned_ch_2_text$content)
leagues_cleaned_ch_2_text_DF

leagues_cleaned_ch_3_text_DF = as.data.frame(leagues_cleaned_ch_3_text$content)
leagues_cleaned_ch_3_text_DF

leagues_cleaned_ch_4_text_DF = as.data.frame(leagues_cleaned_ch_4_text$content)
leagues_cleaned_ch_4_text_DF

leagues_cleaned_ch_5_text_DF = as.data.frame(leagues_cleaned_ch_5_text$content)
leagues_cleaned_ch_5_text_DF

leagues_cleaned_ch_6_text_DF = as.data.frame(leagues_cleaned_ch_6_text$content)
leagues_cleaned_ch_6_text_DF

leagues_cleaned_ch_7_text_DF = as.data.frame(leagues_cleaned_ch_7_text$content)
leagues_cleaned_ch_7_text_DF

leagues_cleaned_ch_8_text_DF = as.data.frame(leagues_cleaned_ch_8_text$content)
leagues_cleaned_ch_8_text_DF

leagues_cleaned_ch_9_text_DF = as.data.frame(leagues_cleaned_ch_9_text$content)
leagues_cleaned_ch_9_text_DF

leagues_cleaned_ch_10_text_DF = as.data.frame(leagues_cleaned_ch_10_text$content)
leagues_cleaned_ch_10_text_DF

# lets get the sentences in each chapter
leagues_ch_1_as_string = get_text_as_string("CHAPTER1.txt")
leagues_ch_1_as_string

leagues_ch_2_as_string = get_text_as_string("CHAPTER2.txt")
leagues_ch_2_as_string

leagues_ch_3_as_string = get_text_as_string("CHAPTER3.txt")
leagues_ch_3_as_string

leagues_ch_4_as_string = get_text_as_string("CHAPTER4.txt")
leagues_ch_4_as_string

leagues_ch_5_as_string = get_text_as_string("CHAPTER5.txt")
leagues_ch_6_as_string

leagues_ch_6_as_string = get_text_as_string("CHAPTER6.txt")
leagues_ch_7_as_string

leagues_ch_7_as_string = get_text_as_string("CHAPTER7.txt")
leagues_ch_7_as_string

leagues_ch_8_as_string = get_text_as_string("CHAPTER8.txt")
leagues_ch_8_as_string

leagues_ch_9_as_string = get_text_as_string("CHAPTER9.txt")
leagues_ch_9_as_string

leagues_ch_10_as_string = get_text_as_string("CHAPTER10.txt")
leagues_ch_10_as_string

# let's get sentences now
leagues_ch_1_as_string_sentences = get_sentences(leagues_ch_1_as_string)
str(leagues_ch_1_as_string_sentences)
leagues_ch_1_as_string_sentences

leagues_ch_2_as_string_sentences = get_sentences(leagues_ch_2_as_string)
str(leagues_ch_2_as_string_sentences)
leagues_ch_2_as_string_sentences

leagues_ch_3_as_string_sentences = get_sentences(leagues_ch_3_as_string)
str(leagues_ch_3_as_string_sentences)
leagues_ch_3_as_string_sentences

leagues_ch_4_as_string_sentences = get_sentences(leagues_ch_4_as_string)
str(leagues_ch_5_as_string_sentences)
leagues_ch_5_as_string_sentences

leagues_ch_5_as_string_sentences = get_sentences(leagues_ch_5_as_string)
str(leagues_ch_6_as_string_sentences)
leagues_ch_6_as_string_sentences

leagues_ch_6_as_string_sentences = get_sentences(leagues_ch_6_as_string)
str(leagues_ch_7_as_string_sentences)
leagues_ch_7_as_string_sentences

leagues_ch_7_as_string_sentences = get_sentences(leagues_ch_7_as_string)
str(leagues_ch_7_as_string_sentences)
leagues_ch_7_as_string_sentences

leagues_ch_8_as_string_sentences = get_sentences(leagues_ch_8_as_string)
str(leagues_ch_8_as_string_sentences)
leagues_ch_8_as_string_sentences

leagues_ch_9_as_string_sentences = get_sentences(leagues_ch_9_as_string)
str(leagues_ch_9_as_string_sentences)
leagues_ch_9_as_string_sentences

leagues_ch_10_as_string_sentences = get_sentences(leagues_ch_10_as_string)
str(leagues_ch_10_as_string_sentences)
leagues_ch_10_as_string_sentences

# let’s call get_sentiment and get_sentiment_dictionary with the default value of “syuzhet”
leagues_ch_1_as_string_sentences_sentiment = get_sentiment(leagues_ch_1_as_string_sentences, "syuzhet")
leagues_ch_1_as_string_sentences_sentiment

leagues_ch_2_as_string_sentences_sentiment = get_sentiment(leagues_ch_2_as_string_sentences, "syuzhet")
leagues_ch_2_as_string_sentences_sentiment

leagues_ch_3_as_string_sentences_sentiment = get_sentiment(leagues_ch_3_as_string_sentences, "syuzhet")
leagues_ch_3_as_string_sentences_sentiment

leagues_ch_4_as_string_sentences_sentiment = get_sentiment(leagues_ch_4_as_string_sentences, "syuzhet")
leagues_ch_4_as_string_sentences_sentiment

leagues_ch_5_as_string_sentences_sentiment = get_sentiment(leagues_ch_5_as_string_sentences, "syuzhet")
leagues_ch_5_as_string_sentences_sentiment

leagues_ch_6_as_string_sentences_sentiment = get_sentiment(leagues_ch_6_as_string_sentences, "syuzhet")
leagues_ch_6_as_string_sentences_sentiment

leagues_ch_7_as_string_sentences_sentiment = get_sentiment(leagues_ch_7_as_string_sentences, "syuzhet")
leagues_ch_8_as_string_sentences_sentiment

leagues_ch_8_as_string_sentences_sentiment = get_sentiment(leagues_ch_8_as_string_sentences, "syuzhet")
leagues_ch_8_as_string_sentences_sentiment

leagues_ch_9_as_string_sentences_sentiment = get_sentiment(leagues_ch_9_as_string_sentences, "syuzhet")
leagues_ch_9_as_string_sentences_sentiment

leagues_ch_10_as_string_sentences_sentiment = get_sentiment(leagues_ch_10_as_string_sentences, "syuzhet")
leagues_ch_10_as_string_sentences_sentiment


# let’s call get_sentiment with the default value of “bing”
leagues_ch_1_as_string_sentences_sentiment_bing = get_sentiment(leagues_ch_1_as_string_sentences, "bing")
leagues_ch_1_as_string_sentences_sentiment_bing

leagues_ch_2_as_string_sentences_sentiment_bing = get_sentiment(leagues_ch_2_as_string_sentences, "bing")
leagues_ch_2_as_string_sentences_sentiment_bing

leagues_ch_3_as_string_sentences_sentiment_bing = get_sentiment(leagues_ch_3_as_string_sentences, "bing")
leagues_ch_3_as_string_sentences_sentiment_bing

leagues_ch_4_as_string_sentences_sentiment_bing = get_sentiment(leagues_ch_4_as_string_sentences, "bing")
leagues_ch_4_as_string_sentences_sentiment_bing

leagues_ch_5_as_string_sentences_sentiment_bing = get_sentiment(leagues_ch_5_as_string_sentences, "bing")
leagues_ch_5_as_string_sentences_sentiment_bing

leagues_ch_6_as_string_sentences_sentiment_bing = get_sentiment(leagues_ch_6_as_string_sentences, "bing")
leagues_ch_6_as_string_sentences_sentiment_bing

leagues_ch_7_as_string_sentences_sentiment_bing = get_sentiment(leagues_ch_7_as_string_sentences, "bing")
leagues_ch_8_as_string_sentences_sentiment_bing

leagues_ch_8_as_string_sentences_sentiment_bing = get_sentiment(leagues_ch_8_as_string_sentences, "bing")
leagues_ch_8_as_string_sentences_sentiment_bing

leagues_ch_9_as_string_sentences_sentiment_bing = get_sentiment(leagues_ch_9_as_string_sentences, "bing")
leagues_ch_9_as_string_sentences_sentiment_bing

leagues_ch_10_as_string_sentences_sentiment_bing = get_sentiment(leagues_ch_10_as_string_sentences, "bing")
leagues_ch_10_as_string_sentences_sentiment_bing

# let’s look at the sentiment dictionary for “syuzhet”
leagues_chapters_as_string_sentences_sentiment_dictionary = get_sentiment_dictionary()
leagues_chapters_as_string_sentences_sentiment_dictionary

# let’s look at the sentiment dictionary for “bing”
leagues_chapters_as_string_sentences_sentiment_dictionary_bing = get_sentiment_dictionary('bing')
leagues_chapters_as_string_sentences_sentiment_dictionary_bing

# sum the values of the sentiment vector in order to get a measure of the overall emotional valence in the text
leagues_ch_1_as_string_sentences_sentiment_sum = sum(leagues_ch_1_as_string_sentences_sentiment)
leagues_ch_1_as_string_sentences_sentiment_sum

leagues_ch_2_as_string_sentences_sentiment_sum = sum(leagues_ch_2_as_string_sentences_sentiment)
leagues_ch_2_as_string_sentences_sentiment_sum

leagues_ch_3_as_string_sentences_sentiment_sum = sum(leagues_ch_3_as_string_sentences_sentiment)
leagues_ch_3_as_string_sentences_sentiment_sum

leagues_ch_4_as_string_sentences_sentiment_sum = sum(leagues_ch_4_as_string_sentences_sentiment)
leagues_ch_4_as_string_sentences_sentiment_sum

leagues_ch_5_as_string_sentences_sentiment_sum = sum(leagues_ch_5_as_string_sentences_sentiment)
leagues_ch_5_as_string_sentences_sentiment_sum

leagues_ch_6_as_string_sentences_sentiment_sum = sum(leagues_ch_6_as_string_sentences_sentiment)
leagues_ch_6_as_string_sentences_sentiment_sum

leagues_ch_7_as_string_sentences_sentiment_sum = sum(leagues_ch_7_as_string_sentences_sentiment)
leagues_ch_7_as_string_sentences_sentiment_sum

leagues_ch_8_as_string_sentences_sentiment_sum = sum(leagues_ch_8_as_string_sentences_sentiment)
leagues_ch_8_as_string_sentences_sentiment_sum

leagues_ch_9_as_string_sentences_sentiment_sum = sum(leagues_ch_9_as_string_sentences_sentiment)
leagues_ch_9_as_string_sentences_sentiment_sum

leagues_ch_10_as_string_sentences_sentiment_sum = sum(leagues_ch_10_as_string_sentences_sentiment)
leagues_ch_10_as_string_sentences_sentiment_sum


# sum the values of the sentiment(bing) vector in order to get a measure of the overall emotional valence in the text
leagues_ch_1_as_string_sentences_sentiment_bing_sum = sum(leagues_ch_1_as_string_sentences_sentiment_bing)
leagues_ch_1_as_string_sentences_sentiment_bing_sum

leagues_ch_2_as_string_sentences_sentiment_bing_sum = sum(leagues_ch_2_as_string_sentences_sentiment_bing)
leagues_ch_2_as_string_sentences_sentiment_bing_sum

leagues_ch_3_as_string_sentences_sentiment_bing_sum = sum(leagues_ch_3_as_string_sentences_sentiment_bing)
leagues_ch_3_as_string_sentences_sentiment_bing_sum

leagues_ch_4_as_string_sentences_sentiment_bing_sum = sum(leagues_ch_4_as_string_sentences_sentiment_bing)
leagues_ch_4_as_string_sentences_sentiment_bing_sum

leagues_ch_5_as_string_sentences_sentiment_bing_sum = sum(leagues_ch_5_as_string_sentences_sentiment_bing)
leagues_ch_5_as_string_sentences_sentiment_bing_sum

leagues_ch_6_as_string_sentences_sentiment_bing_sum = sum(leagues_ch_6_as_string_sentences_sentiment_bing)
leagues_ch_6_as_string_sentences_sentiment_bing_sum

leagues_ch_7_as_string_sentences_sentiment_bing_sum = sum(leagues_ch_7_as_string_sentences_sentiment_bing)
leagues_ch_7_as_string_sentences_sentiment_bing_sum

leagues_ch_8_as_string_sentences_sentiment_bing_sum = sum(leagues_ch_8_as_string_sentences_sentiment_bing)
leagues_ch_8_as_string_sentences_sentiment_bing_sum

leagues_ch_9_as_string_sentences_sentiment_bing_sum = sum(leagues_ch_9_as_string_sentences_sentiment_bing)
leagues_ch_9_as_string_sentences_sentiment_bing_sum

leagues_ch_10_as_string_sentences_sentiment_bing_sum = sum(leagues_ch_10_as_string_sentences_sentiment_bing)
leagues_ch_10_as_string_sentences_sentiment_bing_sum


# let’s look at the means
leagues_ch_1_as_string_sentences_sentiment_mean = mean(leagues_ch_1_as_string_sentences_sentiment)
leagues_ch_1_as_string_sentences_sentiment_mean

leagues_ch_2_as_string_sentences_sentiment_mean = mean(leagues_ch_2_as_string_sentences_sentiment)
leagues_ch_2_as_string_sentences_sentiment_mean

leagues_ch_3_as_string_sentences_sentiment_mean = mean(leagues_ch_3_as_string_sentences_sentiment)
leagues_ch_3_as_string_sentences_sentiment_mean

leagues_ch_4_as_string_sentences_sentiment_mean = mean(leagues_ch_4_as_string_sentences_sentiment)
leagues_ch_4_as_string_sentences_sentiment_mean

leagues_ch_5_as_string_sentences_sentiment_mean = mean(leagues_ch_5_as_string_sentences_sentiment)
leagues_ch_5_as_string_sentences_sentiment_mean

leagues_ch_6_as_string_sentences_sentiment_mean = mean(leagues_ch_6_as_string_sentences_sentiment)
leagues_ch_6_as_string_sentences_sentiment_mean

leagues_ch_7_as_string_sentences_sentiment_mean = mean(leagues_ch_7_as_string_sentences_sentiment)
leagues_ch_7_as_string_sentences_sentiment_mean

leagues_ch_8_as_string_sentences_sentiment_mean = mean(leagues_ch_8_as_string_sentences_sentiment)
leagues_ch_8_as_string_sentences_sentiment_mean

leagues_ch_9_as_string_sentences_sentiment_mean = mean(leagues_ch_9_as_string_sentences_sentiment)
leagues_ch_9_as_string_sentences_sentiment_mean

leagues_ch_10_as_string_sentences_sentiment_mean = mean(leagues_ch_10_as_string_sentences_sentiment)
leagues_ch_10_as_string_sentences_sentiment_mean

# let’s look at the means(bing)
leagues_ch_1_as_string_sentences_sentiment_bing_mean = mean(leagues_ch_1_as_string_sentences_sentiment_bing)
leagues_ch_1_as_string_sentences_sentiment_bing_mean

leagues_ch_2_as_string_sentences_sentiment_bing_mean = mean(leagues_ch_2_as_string_sentences_sentiment_bing)
leagues_ch_2_as_string_sentences_sentiment_bing_mean

leagues_ch_3_as_string_sentences_sentiment_bing_mean = mean(leagues_ch_3_as_string_sentences_sentiment_bing)
leagues_ch_3_as_string_sentences_sentiment_bing_mean

leagues_ch_4_as_string_sentences_sentiment_bing_mean = mean(leagues_ch_4_as_string_sentences_sentiment_bing)
leagues_ch_4_as_string_sentences_sentiment_bing_mean

leagues_ch_5_as_string_sentences_sentiment_bing_mean = mean(leagues_ch_5_as_string_sentences_sentiment_bing)
leagues_ch_5_as_string_sentences_sentiment_bing_mean

leagues_ch_6_as_string_sentences_sentiment_bing_mean = mean(leagues_ch_6_as_string_sentences_sentiment_bing)
leagues_ch_6_as_string_sentences_sentiment_bing_mean

leagues_ch_7_as_string_sentences_sentiment_bing_mean = mean(leagues_ch_7_as_string_sentences_sentiment_bing)
leagues_ch_7_as_string_sentences_sentiment_bing_mean

leagues_ch_8_as_string_sentences_sentiment_bing_mean = mean(leagues_ch_8_as_string_sentences_sentiment_bing)
leagues_ch_8_as_string_sentences_sentiment_bing_mean

leagues_ch_9_as_string_sentences_sentiment_bing_mean = mean(leagues_ch_9_as_string_sentences_sentiment_bing)
leagues_ch_9_as_string_sentences_sentiment_bing_mean

leagues_ch_10_as_string_sentences_sentiment_bing_mean = mean(leagues_ch_10_as_string_sentences_sentiment_bing)
leagues_ch_10_as_string_sentences_sentiment_bing_mean

# plot the values in a graph where the x-axis represents the passage of time from the beginning to the end of the text, and the y-axis measures 
# the degrees of positive and negative sentiment.
plot(leagues_ch_1_as_string_sentences_sentiment, main="Chapter 1 Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_2_as_string_sentences_sentiment, main="Chapter 2 Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_3_as_string_sentences_sentiment, main="Chapter 3 Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_4_as_string_sentences_sentiment, main="Chapter 4 Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_5_as_string_sentences_sentiment, main="Chapter 5 Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_6_as_string_sentences_sentiment, main="Chapter 6 Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_7_as_string_sentences_sentiment, main="Chapter 7 Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_8_as_string_sentences_sentiment, main="Chapter 8 Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_9_as_string_sentences_sentiment, main="Chapter 9 Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_10_as_string_sentences_sentiment, main="Chapter 10 Plot Trajectory", xlab="Narrative", ylab="Emotional Valence")

# the same plotting for bing sentiments
plot(leagues_ch_1_as_string_sentences_sentiment_bing, main="Chapter 1 Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_2_as_string_sentences_sentiment_bing, main="Chapter 2 Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_3_as_string_sentences_sentiment_bing, main="Chapter 3 Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_4_as_string_sentences_sentiment_bing, main="Chapter 4 Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_5_as_string_sentences_sentiment_bing, main="Chapter 5 Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_6_as_string_sentences_sentiment_bing, main="Chapter 6 Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_7_as_string_sentences_sentiment_bing, main="Chapter 7 Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_8_as_string_sentences_sentiment_bing, main="Chapter 8 Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_9_as_string_sentences_sentiment_bing, main="Chapter 9 Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

plot(leagues_ch_10_as_string_sentences_sentiment_bing, main="Chapter 10 Plot Trajectory: Bing", xlab="Narrative", ylab="Emotional Valence")

# let's use get_percentage_values function to calculate mean sentiment valence for each divided text chunk (bin size = 10)
leagues_ch_1_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_1_as_string_sentences_sentiment, bins = 10)

leagues_ch_2_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_2_as_string_sentences_sentiment, bins = 10)

leagues_ch_3_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_3_as_string_sentences_sentiment, bins = 10)

leagues_ch_4_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_4_as_string_sentences_sentiment, bins = 10)

leagues_ch_5_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_5_as_string_sentences_sentiment, bins = 10)

leagues_ch_6_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_6_as_string_sentences_sentiment, bins = 10)

leagues_ch_7_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_7_as_string_sentences_sentiment, bins = 10)

leagues_ch_8_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_8_as_string_sentences_sentiment, bins = 10)

leagues_ch_9_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_9_as_string_sentences_sentiment, bins = 10)

leagues_ch_10_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_10_as_string_sentences_sentiment, bins = 10)


# let's use get_percentage_values function to calculate mean sentiment valence for each divided text chunk (bin size = 20)
leagues_ch_1_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_1_as_string_sentences_sentiment, bins = 20)

leagues_ch_2_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_2_as_string_sentences_sentiment, bins = 20)

leagues_ch_3_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_3_as_string_sentences_sentiment, bins = 20)

leagues_ch_4_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_4_as_string_sentences_sentiment, bins = 20)

leagues_ch_5_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_5_as_string_sentences_sentiment, bins = 20)

leagues_ch_6_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_6_as_string_sentences_sentiment, bins = 20)

leagues_ch_7_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_7_as_string_sentences_sentiment, bins = 20)

leagues_ch_8_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_8_as_string_sentences_sentiment, bins = 20)

leagues_ch_9_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_9_as_string_sentences_sentiment, bins = 20)

leagues_ch_10_as_string_sentences_sentiment_pct_value = get_percentage_values(leagues_ch_10_as_string_sentences_sentiment, bins = 20)

# ------------------------------------------------------------------- END TASK F

# ----------------------------------------------------------------------- TASK G

# ----------------------------- STRINGI

# ------ 1 method

# the function checks whether given sequences of bytes forms a proper UTF-8 string.
# returns a logical vector. Its i-th element indicates whether the i-th string corresponds to a valid UTF-8 byte sequence.
leagues_ch_1_as_string_sentences_utf8 = stri_enc_isutf8(leagues_ch_1_as_string_sentences)
leagues_ch_1_as_string_sentences_utf8

# ------ end 1 method


# ------ 2 method

# function counts the number of occurrences of a pattern in a string.
stri_count(leagues_ch_1_as_string_sentences, fixed='m')

# ------ end 2 method

# ------ 3 method

# function concatenate all the strings in each character vector in a given list
words_joined = stri_join_list(list('hello', 'moon', 'money'), sep = " ", collapse = NULL)
words_joined

# ------ end 3 method

# ------------------------- END STRINGI


# ----------------------------- QUANTEDA

# ------ 1 method

# coercion functions to and from tokens objects, checks for whether an object is a tokens object, and functions to combine tokens objects.
dict <- dictionary(list(country = "United States", sea = c("Atlantic Ocean", "Pacific Ocean")))
lis  <- list(c("The", "United-States", "has", "the", "Atlantic-Ocean", "and", "the", "Pacific-Ocean", "."))
toks <- as.tokens(lis, concatenator = "-")
tokens_lookup(toks, dict)

# ------ end 1 method


# ------ 2 method

# take a random sample of documents of the specified size from a corpus, with or without replacement, optionally by grouping variables or with probability weights
set.seed(123)

# sampling from a corpus
summary(corpus_sample(data_corpus_inaugural, size = 5))
summary(corpus_sample(data_corpus_inaugural, size = 10, replace = TRUE))

# sampling with by
corp <- data_corpus_inaugural
corp$century <- paste(floor(corp$Year / 100) + 1)
corp$century <- paste0(corp$century, ifelse(corp$century < 21, "th", "st"))
corpus_sample(corp, size = 2, by = century) %>% summary()
# needs drop = TRUE to avoid empty interactions
corpus_sample(corp, size = 1, by = interaction(Party, century, drop = TRUE), replace = TRUE) %>%
  summary()

# sampling sentences by document
corp <- corpus(c(one = "Sentence one.  Sentence two.  Third sentence.",
                 two = "First sentence, doc2.  Second sentence, doc2."),
               docvars = data.frame(var1 = c("a", "a"), var2 = c(1, 2)))
corpus_reshape(corp, to = "sentences") %>%
  corpus_sample(replace = TRUE, by = docid(.))

# oversampling
corpus_sample(corp, size = 5, replace = TRUE)

# ------ end 2 method


# ------ 3 method

# get the count of tokens (total features) or types (unique tokens)
ntoken(corpus_subset(data_corpus_inaugural, Year < 1806), remove_punct = TRUE)
ntype(corpus_subset(data_corpus_inaugural, Year < 1806), remove_punct = TRUE)
ntoken(dfm(tokens(corpus_subset(data_corpus_inaugural, Year < 1800))))
ntype(dfm(tokens(corpus_subset(data_corpus_inaugural, Year < 1800))))

# ------ end 3 method


# ------------------------- END QUANTEDA

# ------------------------------------------------------------------- END TASK G