######
library(rvest)
library(XML)
library(magrittr)
 a<-10
 wonder_woman<-NULL
 url1<-"https://www.imdb.com/title/tt0451279/reviews?start"
 for(i in 0:22){
   url<-read_html(as.character(paste(url1,i*a,sep="")))
   wonder<-url %>%
     html_nodes("#tn15content div+ p") %>%
     html_text() 
wonder_woman<-c(wonder_woman,wonder)
}
write.table(wonder_woman,file="wonder_woman1.txt")
getwd()



########
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
movie <- readLines(file.choose())
View(movie)
docs<- Corpus(VectorSource(movie))
length(docs)
str(docs)
###cleaning the text
tospace <- function(x,pattern)gsub(pattern," ",x)
docs1 <- tm_map(docs,tospace,"/")
docs2 <- tm_map(docs1,tospace,"\n")
docs3 <- tm_map(docs2,tospace,"@")
docs4 <- tm_map(docs3,tospace,"\\|")
###converting text to lower case
docs5 <- tm_map(docs4,tolower)
##remove number,punctution,stopwords,whitespace
docs6 <- tm_map(docs5,removeNumbers)
docs7 <- tm_map(docs6,removePunctuation)
docs8 <- tm_map(docs7,removeWords,stopwords("english"))
docs9 <- tm_map(docs8,stripWhitespace)
#####converting docs into TDM
tdm <- TermDocumentMatrix(docs9)
m <- as.matrix(tdm)
w <- sort(rowSums(m),decreasing = T)
set.seed(222)
wordcloud(words = names(w),
          freq =w,
          max.words = 150,
          colors = brewer.pal(6,'Dark2'),
          rot.per = 0.35)
findFreqTerms(tdm,lowfreq = 8)
findAssocs(tdm,terms = "phone",corlimit = 0.3)
head(w,10)
w <- subset(w,w>20)
barplot(w,las=2,col = rainbow(50))

###Emotion mining
library("syuzhet")
s_v <- get_sentences(movie)

nrc_data <- get_nrc_sentiment(movie)

# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[, 1:10]))),
        horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:8)

