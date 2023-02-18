#The code assumes two directories. One for before and one for after
#they were called doc.corpus and doc.corpusB
#all files were assumed to be pdfs
#allEssaysSentsAfter and allEssaysSentsBefore have the values/data from which the graphs are made and thus can be used to do quantitative analysis. But to any comparison between before and after you would need to make sure they are alignned properly (first student in before is same as first in after etc. )
#I could not get the word cloud graphs to be posted to the pdf using the ggarange function.

library("tm")
library(dplyr)
library("NLP")
library("ggplot2")
library("syuzhet")
library("SnowballC")
library("RColorBrewer")
library("wordcloud")
library(RSQLite)
library(quanteda)
library(readtext)         # To read .txt files
library(stm)              # For structural topic models
library(stminsights)      # For visual exploration of STM
library(gsl)              # Required for the topicmodels package
library(topicmodels)      # For topicmodels
library(caret)            # For machine learning
library(wordcloud)
library(textreg)
library(grid)
library(gridExtra)
library(ggpubr)
library(rlist)
library(ggwordcloud)
library(ggplot2)

#After files
setwd("C:/Users/willi/Documents/Chapter14/doc.corpus")
pdfNames <- list.files( pattern = "*.pdf")
docxNames <- list.files(path = "doc.corpus", pattern = "*.docx")


#directory <- "C:/Users/willi/Documents/Chapter14/doc.corpus"
directory <- getwd()
#claster added this for testing with pdf. Note I deleted directory as an argument to DirSource
my_corpus <- VCorpus(DirSource(directory, pattern = ".pdf"), readerControl = list(reader = readPDF))

beforeSentiment <- matrix(0,10,1)
afterSentiment <- matrix(0,10,1)
#emotionalDifferenceMatrix=matrix(0,10,50) #assuming there are at most 50 students.
emo_plot=list() 
sentiment_plot=list()
Essay_wc=list()
allEssaysSentsAfter <- as.data.frame(matrix(0 , nrow = 10))
allEssaysSentsBefore <- as.data.frame(matrix(0 , nrow = 10))

pdf("summaryaff.pdf",onefile=TRUE)

#for (cor in 1:1){
for (cor in 1:length(my_corpus)){
  #print(cor)
  
  Essay_Corpus <- Corpus(VectorSource(my_corpus[cor]))
  #Essay_Corpus <- Corpus(VectorSource(AttdAI))
  #browser()
  # Convert the text to lower case
  Essay_Corpus <- tm_map(Essay_Corpus, content_transformer(tolower))
  # Remove numbers
  Essay_Corpus <- tm_map(Essay_Corpus, removeNumbers)
  # Remove english common stopwords
  Essay_Corpus <- tm_map(Essay_Corpus, removeWords, stopwords("en"))
  # Remove punctuations
  Essay_Corpus <- tm_map(Essay_Corpus, removePunctuation)
  # Eliminate extra white spaces
  Essay_Corpus <- tm_map(Essay_Corpus, stripWhitespace)
  # Text stemming
  Essay_Corpus <- tm_map(Essay_Corpus, stemDocument)
  
  
  # Remove additional stopwords
  Essay_Corpus <- tm_map(Essay_Corpus, removeWords, c("also", "can", "may", "even", "will", "however", "like", "many","retrieved","like" ,"name","data","ghotbi"))
  
  #Create term document matrix
  dtm_essay <- DocumentTermMatrix(Essay_Corpus)
  
  tdm_essay <- TermDocumentMatrix(Essay_Corpus)
  m_essay <- as.matrix(tdm_essay)
  v_essay <- sort(rowSums(m_essay),decreasing=TRUE)
  d_essay <- data.frame(word = names(v_essay),freq=v_essay)
  head(d_essay, 10)
  
  
  findAssocs(tdm_essay, c("vaccine", "time"), c(0))
  findAssocs(tdm_essay, c("job", "human"), c(0))
  
  
  #Draw word cloud
  set.seed(1234)
  # Essay_wc[[cor]] <- wordcloud(d_essay$word, d_essay$freq, colors=brewer.pal(6, "Dark2"), min.freq=10, max.words=800, scale=c(3,.4), random.order=FALSE, rot.per=0.35)
  
  # Essay_wc[[cor]] <- ggplot(d_essay, aes(label = word, size = freq, color = factor(sample.int(10, nrow(d_essay), replace = TRUE)),angle = angle)) + geom_text_wordcloud() + scale_size_area(max_size = 14)
  
  Essay_wc[[cor]] <- ggplot(d_essay, aes(label = word, size = freq)) + geom_text_wordcloud()
  #Sentiment analysis
  
  AttdAI <- convert.tm.to.character(my_corpus[cor])
  
  d<-get_nrc_sentiment(AttdAI)
  
  td<-data.frame(t(d))
  
  td_new <- data.frame(rowSums(td[1:2]))
  
  
  #Transformation and cleaning
  names(td_new)[1] <- "count"
  td_new <- cbind("sentiment"= rownames(td_new), td_new)
  allEssaysSentsAfter <- cbind(allEssaysSentsAfter,td_new[,2])
  rownames(td_new) <- NULL
  td_emo <-td_new[1:8,]
  td_sentiment<-td_new[9:10,]
  
  #Calculate difference in Emotion
  afterSentiment <- cbind(afterSentiment,as.matrix(td_new[,2]))
  
  #plotting
  
  emo_plot[[cor]] <- qplot(sentiment, xlab="After Essay emotions", data=td_emo, weight=count, geom="bar",fill=sentiment)+ggtitle(pdfNames[cor])+theme(axis.title.x = element_text(size = 9, lineheight = .9,family = "Times", face = "bold.italic", colour = "red")) + theme(plot.title = element_text(size = 9, lineheight = .9,family = "Times", face = "bold.italic", colour = "red"))+ylim(0,22)
  
  
  
  sentiment_plot[[cor]] <- qplot(sentiment, xlab ="After Essay sentiments in binary terms", data=td_sentiment, weight=count, geom="bar",fill=sentiment)+ggtitle(pdfNames[cor])+theme(axis.title.x = element_text(size = 9, lineheight = .9,family = "Times", face = "bold.italic", colour = "red")) + theme(plot.title = element_text(size = 9, lineheight = .9,family = "Times", face = "bold.italic", colour = "red"))+ylim(0,55)
  # print(sentiment_plot[[cor]])
  # print(emo_plot[[cor]])
  
  
  coll2 <- textstat_collocations(AttdAI, size = 4:6)
  
  
  freqterms <- findFreqTerms(dtm_essay, lowfreq = 50)
  
  
  
}
dev.off() 




#####################################
#Repeat Code for Before but change names of graphs.


setwd("C:/Users/willi/Documents/Chapter14/doc.corpusB")
pdfNamesB <- list.files( pattern = "*.pdf")
docxNamesB <- list.files(path = "doc.corpusB", pattern = "*.docx")


#directory <- "C:/Users/willi/Documents/Chapter14/doc.corpus"
directory <- getwd()
#claster added this for testing with pdf. Note I deleted directory as an argument to DirSource
my_corpus <- VCorpus(DirSource(directory, pattern = ".pdf"), readerControl = list(reader = readPDF))



emo_plotB=list() 
sentiment_plotB=list()
Essay_wcB=list()

#pdf("summaryBefore.pdf",onefile=TRUE)

for (cor in 1:length(my_corpus)){
  #print(cor)
  
  Essay_Corpus <- Corpus(VectorSource(my_corpus[cor]))
  #Essay_Corpus <- Corpus(VectorSource(AttdAI))
  #browser()
  # Convert the text to lower case
  Essay_Corpus <- tm_map(Essay_Corpus, content_transformer(tolower))
  # Remove numbers
  Essay_Corpus <- tm_map(Essay_Corpus, removeNumbers)
  # Remove english common stopwords
  Essay_Corpus <- tm_map(Essay_Corpus, removeWords, stopwords("en"))
  # Remove punctuations
  Essay_Corpus <- tm_map(Essay_Corpus, removePunctuation)
  # Eliminate extra white spaces
  Essay_Corpus <- tm_map(Essay_Corpus, stripWhitespace)
  # Text stemming
  Essay_Corpus <- tm_map(Essay_Corpus, stemDocument)
  
  
  # Remove additional stopwords
  Essay_Corpus <- tm_map(Essay_Corpus, removeWords, c("also", "can", "may", "even", "will", "however", "like", "many","retrieved","like" ,"name","data","ghotbi"))
  
  #Create term document matrix
  dtm_essay <- DocumentTermMatrix(Essay_Corpus)
  
  tdm_essay <- TermDocumentMatrix(Essay_Corpus)
  m_essay <- as.matrix(tdm_essay)
  v_essay <- sort(rowSums(m_essay),decreasing=TRUE)
  d_essay <- data.frame(word = names(v_essay),freq=v_essay)
  head(d_essay, 10)
  
  
  findAssocs(tdm_essay, c("vaccine", "time"), c(0))
  findAssocs(tdm_essay, c("job", "human"), c(0))
  
  
  #Draw word cloud
  set.seed(1234)
  Essay_wcB[[cor]] <- wordcloud(d_essay$word, d_essay$freq, colors=brewer.pal(6, "Dark2"), min.freq=10, max.words=800, scale=c(3,.4), random.order=FALSE, rot.per=0.35)
  
  Essay_wcB[[cor]] <- ggplot(d_essay, aes(label = word, size = freq)) + geom_text_wordcloud()
  
  
  
  #Sentiment analysis
  
  AttdAI <- convert.tm.to.character(my_corpus[cor])
  
  d<-get_nrc_sentiment(AttdAI)
  
  td<-data.frame(t(d))
  
  td_new <- data.frame(rowSums(td[1:2]))
  
  
  #Transformation and cleaning
  names(td_new)[1] <- "count"
  td_new <- cbind("sentiment"= rownames(td_new), td_new)
  allEssaysSentsBefore <- cbind(allEssaysSentsBefore,td_new[,2])
  rownames(td_new) <- NULL
  td_emo <-td_new[1:8,]
  td_sentiment<-td_new[9:10,]
  #newdata <- mydata[ which(mydata$gender=='F' & mydata$age > 65), ]
  
  
  beforeSentiment <- cbind(beforeSentiment,as.matrix(td_new[,2]))
  #plotting
   
  
  emo_plotB[[cor]] <- qplot(sentiment, xlab="Before Essay emotions", data=td_emo, weight=count, geom="bar",fill=sentiment)+ggtitle(pdfNamesB[cor])+theme(axis.title.x = element_text(size = 9, lineheight = .9,family = "Times", face = "bold.italic", colour = "red")) + theme(plot.title = element_text(size = 9, lineheight = .9,family = "Times", face = "bold.italic", colour = "red"))+ylim(0,22)
  
  
  
  sentiment_plotB[[cor]] <- qplot(sentiment, xlab ="Before Essay sentiments in binary terms", data=td_sentiment, weight=count, geom="bar",fill=sentiment)+ggtitle(pdfNamesB[cor])+theme(axis.title.x = element_text(size = 9, lineheight = .9,family = "Times", face = "bold.italic", colour = "red")) + theme(plot.title = element_text(size = 9, lineheight = .9,family = "Times", face = "bold.italic", colour = "red"))+ylim(0,55)
  # print(sentiment_plotB[[cor]])
  # print(emo_plotB[[cor]])
  
  
  coll2 <- textstat_collocations(AttdAI, size = 4:6)
  
  
  freqterms <- findFreqTerms(dtm_essay, lowfreq = 50)
  
  
  
}
#dev.off()









#######################################
#interweave emo_plot and sentiment_plot
#graphsList=c(emo_plot[1],sentiment_plot[1])
graphsList=list()
for(i in 1:length(my_corpus)){  #length(my_corpus)
  graphsList=c(graphsList,emo_plotB[i])
  graphsList=c(graphsList,sentiment_plotB[i])
  #graphsList=c(graphsList,Essay_wcB[i])
  graphsList=c(graphsList,emo_plot[i])
  graphsList=c(graphsList,sentiment_plot[i])
  #graphsList=c(graphsList,Essay_wc[i])
}

pdf("..\\BeforeAndAfter.pdf",onefile=TRUE)
#do.call(ggarrange, c(graphsList, list(ncol = 2, nrow=3, rremove("x.text"), align="hv" ) ))
do.call(ggarrange, c(graphsList, list(ncol = 2, nrow=4, rremove("x.text") ) )) #align="hv"
changeInSentiment <- afterSentiment - beforeSentiment
changeInSentiment <- changeInSentiment[,-1]
row.names(changeInSentiment)=td_new[,1]

grid.table(changeInSentiment)
#print("Change in sentiment is")
#print(changeInSentiment[,1:5])
dev.off()

write.csv(as.table(beforeSentiment),file = "beforeSentiment.csv")
write.csv(as.table(afterSentiment),file = "afterSentiment.csv")

write.csv(as.table(changeInSentiment),file = "BeforeVsAfter_changeInSentiment.csv")
print("Change in sentiment is")
print(as.table(changeInSentiment))

