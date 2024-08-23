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
  filter(n>19) %>%
  graph_from_data_frame(directed = TRUE)

bigram_countdf <-filter(bigram_count,n>19)
df <- data.frame(column1 = c(bigram_countdf$word1,bigram_countdf$word2))
df2 <-data.frame(vert=df[!duplicated(df$column1), ])
plot(bigram_graph,layout=layout.circle(bigram_graph),vertex.size= (bigram_countdf$n)/5,edge.arrow.size=0.2 )
print(bigram_graph)
vcount(bigram_graph)


tkplot(bigram_graph,canvas.width = 1400, canvas.height = 750,
       edge.width=(bigram_countdf$n)/5, vertex.label.degree=pi/2,
       vertex.label.dist=1,vertex.size=5,vertex.color="green", vertex.label.cex=1.5)
indeg <- degree(bigram_graph, mode="in")
outdeg <- degree(bigram_graph, mode="out")
indegv <- c(indeg)
outdegv <- c(outdeg)
hist(indeg,ylim=c(0,max(150)),breaks = 0:(max(indeg)+1),xlab= "In Degree", col="green",main= "Histogram of In degree")
hist(outdeg,ylim=c(0,max(150)),breaks = 0:(max(outdeg)+1),xlab= "Out Degree", col="green",main= "Histogram of Out degree")
install.packages("CINNA")
require(CINNA)
install.packages("centiserve")
require(centiserve)
df2[,2] <-indeg
df2[,3] <-outdeg
df2[,4] <-katzcent(bigram_graph)
df2[,5] <-betweenness(bigram_graph)
df2[,6] <-closeness(bigram_graph)
df2[,7] <-page_rank(bigram_graph, directed = TRUE)
pr_cent<-proper_centralities(bigram_graph)
colnames(df2)[2:4] <-c("indegree","outdegree","katz")
colnames(df2)[5:6] <-c("betweeness","closeness")
colnames(df2)[7:7] <- c("PageRank")
newdata <- df2[order(df2$indegree,decreasing=TRUE),]
newdata1 <- df2[order(df2$outdegree,decreasing=TRUE),]
newdata2 <- df2[order(df2$betweeness,decreasing=TRUE),]
newdata3 <- df2[order(df2$closeness,decreasing=TRUE),]
newdata4 <- df2[order(df2$katz,decreasing=TRUE),]
newdata5 <- df2[order(df2$PageRank,decreasing=TRUE),]

graph2 <- as.undirected(bigram_graph, mode="collapse")
x <-cluster_louvain(graph2, weights = NULL, resolution = 1)
membership(x)
communities(x)
plot(x, graph2,vertex.size=6, layout= layout_nicely(graph2), vertex.label.dist=1,vertex.size=2, vertex.label.cex=0.5)
edge_density(bigram_graph)
reciprocity(bigram_graph)
transitivity(graph2)
#set.seed(2017)
#ggraph(bigram_graph, layout = "fr")+
#geom_edge_link()+geom_node_point()+ geom_node_text(aes(label =name))
layout_on_grid(bigram_graph, width = 100, height = 100, dim = 2)
set.seed(222)
plot(bigram_graph, layout=layout_with_fr(bigram_graph), vertex.size=4,
     rescale=TRUE,vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.2)
#plot(bigram_graph, rescale = FALSE, ylim=c(2,5),xlim=c(5,5), asp = 0)
tkplot(bigram_graph)
x <-c(bigram_count$n)
plot(bigram_graph,layout=layout.random, main="random")
custom<- layout_(bigram_graph, with_fr()) 

custom <- norm_coords(custom, ymin=-1.1, ymax=1.1, xmin=-1.1, xmax=1.1) 

plot(bigram_graph,vertex.size=as.matrix(x),vertex.label.cex = 0.75, 
     
     edge.arrow.size=0.1,edge.size=1000,vertex.label.degree=-pi/2,vertex.label.dist=0.7)
plot(bigram_graph)


