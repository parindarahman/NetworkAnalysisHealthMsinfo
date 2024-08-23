require(igraph)
require(tm)
require(tidytext)
require(dplyr)
require(igraph)
require(tidyr)
require(ggraph)
require(pdftools)

data <- read.csv("Valid Info Translations.csv",header=T)
document <- Corpus(VectorSource(data))
document <- tm_map(document, content_transformer(tolower))
document <- tm_map(document, removeNumbers)
document <- tm_map(document, removeWords, stopwords("english"))
document <- tm_map(document,removePunctuation)
DataFrame1 <- data.frame(text =sapply(document, as.character), stringAsFactors = FALSE)
View(DataFrame1)
#creating bigrams 
first_bigrams <- DataFrame1 %>%
  unnest_tokens(bigram, text, token="ngrams", n=2)
first_bigrams

#bigram frequency
first_bigrams%>%
  count(bigram, sort=TRUE)

#separting bigrams and stop word removal
bigrams_separated <- first_bigrams %>%
  separate(bigram, c("word1", "word2"), sep= " ")
bigrams_filter <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)

#counting after cleaning
bigram_count <- bigrams_filter%>%
  count(word1,word2, sort= TRUE)
bigram_count

#specific word filtering 
bigrams_filter %>%
  filter(word1=="cure")
count(word2, sort-TRUE)

#graph creation
bigram_graph <- bigram_count %>%
  filter(n>15) %>%
  graph_from_data_frame()
print(bigram_graph)

#set.seed(2017)
#ggraph(bigram_graph, layout = "fr")+
#geom_edge_link()+geom_node_point()+ geom_node_text(aes(label =name))
layout_on_grid(bigram_graph, width = 100, height = 100, dim = 2)
set.seed(222)
plot(bigram_graph, layout=layout_with_fr, vertex.size=4,
     rescale=TRUE,vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5)
#plot(bigram_graph, rescale = FALSE, ylim=c(2,5),xlim=c(5,5), asp = 0)
tkplot(bigram_graph)