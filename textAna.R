# Learn text analytics

# Load the librarys needed
require(magrittr)
require(sotu)
require(tidytext)

# Clear the workspace
rm(list = ls())

# Load the meta data
data("sotu_meta")

#Load the speech
data("sotu_text")

# Attach the text data to the main meta data
sotu_meta$text <- sotu_text

# We tokenize it so that each row is a word for all the observations. Data is in tidy format and ready for analysis
sotu_meta %>% tidytext::unnest_tokens(word,text) -> sotu_unnest

# Load the stop words
data("stop_words")

# Remove stop words
sotu_tidy <- sotu_unnest %>% dplyr::anti_join(stop_words,by = "word")

# Count
sotu_tidy %>% dplyr::count(word,sort = T)

# Pass this to ggplot
sotu_tidy %>%
  dplyr::count(word,sort = T) %>%
  dplyr::filter(n > 2500) %>%
  dplyr::mutate(word = reorder(word,n)) %>%
  ggplot2::ggplot(ggplot2::aes(word,n)) +
  ggplot2::geom_col() + 
  ggplot2::xlab("") + 
  ggplot2::ylab("Frequency") +
  ggplot2::coord_flip()

# Word frequency for 
sumYear <- sotu_tidy %>% 
  dplyr::group_by(year) %>% 
  dplyr::summarise(totalWords = length(word)) %>% 
  dplyr::arrange(desc(totalWords))
sumYear

# Create a word cloud for Lincoln

# Get Lincoln speeches
sotu_cloud <- sotu_tidy %>%
  dplyr::filter(year>1860 & year<1865)

# Do the word cloud
qdap::trans_cloud(text.var = sotu_cloud$word,stem = FALSE,min.freq = 20)

# Do a word cloud but group by year
#qdap::trans_cloud(text.var = sotu_cloud$word,stem = FALSE,min.freq = 7,grouping.var = sotu_cloud$year)

# Nest the data again and select only one year
nested_1862 <- sotu_tidy %>%
  dplyr::filter(year == 1862) %>%
  dplyr::select(year,word) %>%
  tidyr::nest(word) %>%
  dplyr::mutate(
    text = purrr::map(data,unlist),
    text = purrr::map_chr(text,paste,collapse = " ")
  )

# Put it in a corpus
myCorpus <- tm::Corpus(tm::VectorSource(nested_1862$text))

# Keywords in context (kwic)
quanteda::kwic(x = myCorpus$content, pattern = "emancipation")

################ Sentiment analysis ################

# Does Lincoln express anger in his 1862 attempt to mollify his political opponents

# Get the angry words from nrc sentiment dictionary
nrc_anger <- tidytext::get_sentiments("nrc") %>%
  dplyr::filter(sentiment == "anger")

# Apply to data
sotu_tidy %>%
  dplyr::filter(year == 1862) %>%
  dplyr::inner_join(nrc_anger) %>%
  dplyr::count(word,sort = T)
