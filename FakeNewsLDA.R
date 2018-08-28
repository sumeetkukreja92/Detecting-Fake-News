#***********************************************************************************************************
# This code identifies fake and real news using the Latent Dirichlet allocation(LDA) method 
# Submitted By : Sumeet Kukreja
# emailid : skukre3@uic.edu
#***********************************************************************************************************

#****************************************************************************
# Setting  up the environment
#****************************************************************************
# Read the libraries
library(tidyverse) #for genenral purpose data manipulation
library(tidytext) #for NLP implementation
library(topicmodels) #for LDA topic modelling
library(tm) #for text mining
library(SnowballC) #for stemming

#read the data
texts <- read.csv(file.choose(), stringsAsFactors = F)

#subset the data to implement LDA faster
set.seed(1234)
row_indices <- sample(1:nrow(texts), 2000, replace = F)
text_sample <- texts[row_indices,]


#****************************************************************************
# Unsupervised learning with LDA
#****************************************************************************
# function to get most frequent words used for predefined number of topics

top_terms_by_topic_LDA <- function(input_text, plot = T, number_of_topics = 3)  #3 topics by default
{
  # create corpus and document term matrix
  Corp <- Corpus(VectorSource(input_text)) #collection of documents containing text
  DTM <- DocumentTermMatrix(Corp) #get the count of words/document
  
  #get unique terms from DTM
  unique_indices <- unique(DTM$i)
  DTM <- DTM[unique_indices,]
  
  #implement LDA and get words/topic
  lda <- LDA(DTM,k = number_of_topics, method = "VEM", control = list(seed = 1234))
  topics <- tidy(lda, matrix = 'beta') #beta is the per-topic-per-word probability
  
  #get top 10 words for each topic
  top_terms <- topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) #in descending order
  
  #If user asks for a plot
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
      ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
      geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
      labs(x = NULL, y = "Beta") + # no x label, change y label 
      coord_flip() # turn bars sideways
  }else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
  
}

# plot top ten terms in the text of the articles
top_terms_by_topic_LDA(text_sample$text, number_of_topics = 8)

#these are all just stop words which needs to be cleaned 
#create corpus and DTM for news articles
newsCorpus <- Corpus(VectorSource(text_sample$text))
newsDTM <- DocumentTermMatrix(newsCorpus)

#convert into tidy dataframe
newsDTM_tidy <- tidy(newsDTM)

#remove stopwords
newsDTM_tidy_cleansed <- newsDTM_tidy %>%
  anti_join(stop_words, by = c("term" = "word"))

#create clean documents
cleaned_documents <- newsDTM_tidy_cleansed %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()

head(cleaned_documents)

# now let's look at the new most informative terms
top_terms_by_topic_LDA(cleaned_documents$terms, number_of_topics = 4)


# stem the words
newsDTM_tidy_cleansed  <- newsDTM_tidy_cleansed %>%
  mutate(stem = wordStem(term))

# reconstruct our documents
cleaned_documents <- newsDTM_tidy_cleansed %>%
  group_by(document) %>% 
  mutate(terms = toString(rep(term, count))) %>%
  select(document, terms) %>%
  unique()

# now let's look at the new most informative terms
top_terms_by_topic_LDA(cleaned_documents$terms, number_of_topics = 4)


#****************************************************************************
# Supervised topic modeling with TF-IDF
#****************************************************************************
#function to streamline the analysis using TF-IDF
#It takes in a dataframe, the name of the column that has the texts in it 
#and the name of the column that has the topic labels in it.

top_terms_by_topic_tfidf <- function(text_df, text_column, group_column, plot = T){
  # name for the column we're going to unnest_tokens_ to
  # (you only need to worry about enquo stuff if you're
  # writing a function using using tidyverse packages)
  group_column <- enquo(group_column)
  text_column <- enquo(text_column)
  
  # get the count of each word in each review
  words <- text_df %>%
    unnest_tokens(word, !!text_column) %>%
    count(!!group_column, word) %>% 
    ungroup()
  
  # get the number of words per text
  total_words <- words %>% 
    group_by(!!group_column) %>% 
    summarize(total = sum(n))
  
  # combine the two dataframes we just made
  words <- left_join(words, total_words)
  
  # get the tf_idf & order the words by degree of relevence
  tf_idf <- words %>%
    bind_tf_idf(word, !!group_column, n) %>%
    select(-total) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))))
  
  if(plot == T){
    # convert "group" into a quote of a name
    # (this is due to funkiness with calling ggplot2
    # in functions)
    group_name <- quo_name(group_column)
    
    # plot the 10 most informative terms per topic
    tf_idf %>% 
      group_by(!!group_column) %>% 
      top_n(10) %>% 
      ungroup %>%
      ggplot(aes(word, tf_idf, fill = as.factor(group_name))) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(reformulate(group_name), scales = "free") +
      coord_flip()
  }else{
    # return the entire tf_idf dataframe
    return(tf_idf)
  }
}

# Plot the top terms for each label in the "type" column.
top_terms_by_topic_tfidf(text_df = texts, text_column = text, group_column = type, plot = T)



# plot the top terms by language, using the labels in the "language" column
top_terms_by_topic_tfidf(text_df = texts, text_column = text, group_column = language, plot = T)
#Doesnt really helps if we do it by language