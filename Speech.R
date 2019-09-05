#read_excel
library(readxl)
post <- read_excel("~/speeches.xlsx", 
                   sheet = "Sheet1")
str(post)

#BUILD CORPUS
library(tm)
corpus <- iconv(post, to = "UTF-8")
corpus2 <- Corpus(VectorSource(corpus)) 
inspect(corpus2[1:5])

# Clean Text
corpus3 <- tm_map(corpus2, tolower) 
inspect(corpus3[1:5])

corpus4 <- tm_map(corpus3, removePunctuation) 
inspect(corpus4[1:5])

corpus5 <- tm_map(corpus4, removeNumbers) 
inspect(corpus5[1:5])

#install_github("quanteda/stopwords", force=TRUE)
#devtools::install_github("quanteda/stopwords")

cleanset <- tm_map(corpus5, removeWords,stopwords::stopwords("en", source = "stopwords-iso"))
inspect(cleanset[1:5])

removeURL <- function(x) gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset,content_transformer(removeURL))
inspect(cleanset[1:5])

cleanset  <- tm_map(cleanset,removeWords,c("applause", "laughter", "ang", "kung","mga","ninyo", "yan","nga","niya"
                                           ,"ako","rin","sino","yung","lang","may","tsk","sila","sabi","kaya","siya"
                                           ,"...","dito","pero","ganun","ito", "wala"))
cleanset  <- tm_map(cleanset, gsub, pattern ="philippine", replacement= "philippines")
#cleanset <- tm_map(cleanset, removeWords, c("\008d"))

cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#Term document matrix
tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm [1:20]


#Bar_plot
w <- rowSums(tdm)
w <- subset(w, w>=4)
barplot(w, las=2,col =rainbow(50))

#wordcloud
library(wordcloud)

w <- sort(rowSums(tdm), decreasing =TRUE)
set.seed(222)
wordcloud(words=names(w),freq =w, max.words = 200, random.order = FALSE, min.freq = 10)

library(wordcloud2)
w <- data.frame(names(w),w)
colnames(w) <- c('word','freq')
wordcloud2(w, size = 0.5 , shape ='triangle-forward', rotateRatio = 0.5, minSize=1,color = "orange",backgroundColor = "black" )






