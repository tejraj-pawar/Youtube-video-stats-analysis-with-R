if (!require('tidyverse')) install.packages('tidyverse', quiet = TRUE); library('tidyverse')
if (!require('topicmodels')) install.packages('topicmodels', quiet = TRUE); library('topicmodels')
if (!require('tm')) install.packages('tm', quiet = TRUE); library('tm')
if (!require('ggplot2')) install.packages('ggplot2', quiet = TRUE); library('ggplot2')
if (!require('dplyr')) install.packages('dplyr', quiet = TRUE); library('dplyr')

# Read RDS post processed file
text_data <- readRDS("processed_youtube_text.rds")
glimpse(text_data)

# Create a corpus
corpus <- Corpus(VectorSource(text_data$Tags))

# Preprocessing: Convert to lower case, remove punctuation, remove numbers, and remove stopwords
corpus <- tm_map(corpus, content_transformer(tolower))
#corpus <- tm_map(corpus, removePunctuation)
#corpus <- tm_map(corpus, removeNumbers)
#corpus <- tm_map(corpus, removeWords, stopwords("en"))

# Create a document-term matrix
minimumFrequency <- 5
dtm <- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))

# remove we have empty rows in our DTM
sel_idx <- slam::row_sums(dtm) > 0
dtm <- dtm[sel_idx, ]

# create models with different number of topics
result <- ldatuning::FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)
FindTopicsNumber_plot(result)

# set a seed so that the output of the model is predictable
if (!require('tidytext')) install.packages('tidytext', quiet = TRUE); library('tidytext')
if (!require('reshape2')) install.packages('reshape2', quiet = TRUE); library('reshape2')
topicModel <- LDA(dtm, k = 4, method = "Gibbs", control = list(iter = 500, verbose = 25)) |> tidy(matrix = "beta")
topicModel

# Save LDA model
saveRDS(topicModel, "4-topic-modeling.rds")

word_probs <- topicModel %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  mutate(term = fct_reorder(term, beta)) %>%
  head(50)

ggplot(
  word_probs,
  aes(term, beta, fill=as.factor(topic))
) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()