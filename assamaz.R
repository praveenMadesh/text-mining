library(rvest)
library(XML)
library(magrittr)

# Amazon Reviews #############################
x <- "https://www.amazon.in/Apple-iPhone-11-128GB-Black/dp/B07XVLW7YK/ref=sr_1_1?crid=2ANTW4S4CZ37O&keywords=appleiphone11&qid=1581762816&sprefix=applei%2Caps%2C352&sr=8-1#customerReviews"
amazon_reviews <- NULL
for (i in 1:10){
  y <- read_html(as.character(paste(x,i,sep="=")))
  review <- y %>%
    html_nodes(".review-text") %>%
    html_text()
  amazon_reviews <- c(amazon_reviews,review)
}
write.table(amazon_reviews,"ABCiphonereview.txt",row.names = F)
getwd()
### Extracted review from amazon 

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
review <- readLines(file.choose())
View(review)
docs<- Corpus(VectorSource(review)) ##converting the text to corpus
length(docs)

#### doing the text some cleaning, converting text into lower case
###removing a numbers, punctuations,stopwords,white space
tospace <- function(x,pattern)gsub(pattern," ",x)
docs1 <- tm_map(docs,tospace,"/")
docs2 <- tm_map(docs1,tospace,"\n")
docs3 <- tm_map(docs2,tospace,"@")
docs4 <- tm_map(docs3,tospace,"\\|")
###converting text to lower case
docs5 <- tm_map(docs4,tolower)
##remove number
docs6 <- tm_map(docs5,removeNumbers)
docs7 <- tm_map(docs6,removePunctuation)
docs8 <- tm_map(docs7,removeWords,stopwords("english"))
docs9 <- tm_map(docs8,stripWhitespace)

##### converting documents into TDM 
tdm <- TermDocumentMatrix(docs9)
m <- as.matrix(tdm)
w <- sort(rowSums(m),decreasing = T)
set.seed(222)
#seeing wordcloud by using function wordcloud
wordcloud(words = names(w),
          freq =w,
          max.words = 150,
          colors = brewer.pal(6,'Dark2'),
          rot.per = 0.35)
findFreqTerms(tdm,lowfreq = 8)###finding frequently use terms
findAssocs(tdm,terms = "phone",corlimit = 0.3)
head(w,10)
w <- subset(w,w>25)##taking subset
barplot(w,las=2,col = rainbow(50))

###emotion 
library("syuzhet")
s_v <- get_sentences(review)

nrc_data <-  get_sentiment(s_v, method="nrc")

# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[,1:6]))),
        horiz = F, cex.names = 0.6,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:10)
