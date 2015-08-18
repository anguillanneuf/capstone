setwd("./R15/milestone")

rm(list=ls())

# library(stringi); library(tm); library(SnowballC); library(RWeka)
library(stringr); library(data.table); library(stylo); library(dplyr)

if(!file.exists("final")){
        temp <- tempfile()
        url <- "http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
        download.file(url, temp)
        unzip(temp)
        unlink(temp)
}

# Create a random sample

# Modifed from StackOverflow post: http://goo.gl/1pne3T
fsample <- function(fname, n, seed, header=FALSE, ..., reader = readLines, readmode){
        set.seed(seed)
        con <- file(fname, open=readmode)
        buf <- readLines(con, n, skipNul = TRUE)
        n_tot <- length(buf)
        
        repeat{
                txt <- readLines(con, n, skipNul = TRUE)
                if ((n_txt <- length(txt)) == 0L)
                        break
                
                n_tot <- n_tot + n_txt
                n_keep <- rbinom(1, n_txt, n_txt / n_tot)
                if (n_keep == 0L)
                        next
                
                keep <- sample(n_txt, n_keep)
                drop <- sample(n, n_keep)
                buf[drop] <- txt[keep]
        }
        close(con)
        reader(textConnection(buf, ...))
}

if(!file.exists("twitter_sample.txt")){
        twitter <- fsample("./final/en_US/en_US.twitter.txt", 20000, 2015, readmode = "r")
        writeLines(twitter, "twitter_sample.txt", sep = "\n")      
}
if(!file.exists("news_sample.txt")){
        news <- fsample("./final/en_US/en_US.news.txt", 20000, 2015, readmode = "rb")
        writeLines(news, "news_sample.txt", sep = "\n")        
}
if(!file.exists("blogs_sample.txt")){
        blogs <- fsample("./final/en_US/en_US.blogs.txt", 20000, 2015, readmode = "r")
        writeLines(blogs, "blogs_sample.txt", sep = "\n")        
}

twitter <- readLines("twitter_sample.txt")
twitter <- iconv(twitter, to="ANSI_X3.4-1968", sub="")

news <- readLines("news_sample.txt")
news <- iconv(news, to="ANSI_X3.4-1968", sub="")

blogs <- readLines("blogs_sample.txt")
blogs <- iconv(blogs, to="ANSI_X3.4-1968", sub="")

cleanTxt <- function(x){
        x <- tolower(x)
        x <- gsub("<3", " love ", x)
        x <- gsub("rt :", " ", x)
        x <- gsub(" u ", " you ", x)
        x <- gsub(" y ", " why ", x)
        x <- gsub(" n ", " and ", x)
        x <- gsub("[\x82\x91\x92]", "'", x)
        x <- gsub("dont", "don't", x)
        x <- gsub("cant", "can't", x)
        x <- gsub("isnt", "isn't", x)
        x <- gsub("hasnt", "hasn't", x)
        x <- gsub("havent", "haven't", x)
        x <- gsub(" im ", " i'm ", x)
        x <- gsub("[^ a-z'-]", " ", x)
        x <- gsub("(\\w['-]\\w)|[[:punct:]]", "\\1", x)
        x <- gsub("\\s+"," ",x)
        x <- gsub("^\\s+|\\s+$", "", x)
        return(x) 
}

twitter <- cleanTxt(twitter)
twitter <- VCorpus(VectorSource(twitter))
saveRDS(twitter, "twitter corpus.Rds")

news <- cleanTxt(news)
news <- VCorpus(VectorSource(news))
saveRDS(news, "news corpus.Rds")

blogs <- cleanTxt(blogs)
blogs <- VCorpus(VectorSource(blogs))
saveRDS(blogs, "blogs corpus.Rds")

if (!file.exists("profanity.txt")) download.file(url = "http://pattern-for-python.googlecode.com/svn-history/r20/trunk/pattern/vector/wordlists/profanity.txt", 
                                                 destfile = "profanity.txt")
profanityWords <- str_trim(names(fread("profanity.txt", sep = ",")))
profanityWords <- profanityWords[-which(profanityWords=="dick")]

clean <- function(my.corpus) {
        my.corpus <- tm_map(my.corpus, removeWords, profanityWords)
        my.corpus <- tm_map(my.corpus, stripWhitespace)
        my.corpus <- tm_map(my.corpus, content_transformer(str_trim))
        
        lastRound <- function(x){
                gsub("(\\w['-]\\w)|[[:punct:]]", "\\1", x)
        }
        my.corpus <- tm_map(my.corpus, content_transformer(lastRound))
        # to take care of cases like "cock-a-hoop" (no idea what that is!)
        
        return(my.corpus)
}

clean2 <- function(my.corpus) {
        my.corpus <- tm_map(my.corpus, removeWords, stopwords("english"))
        my.corpus <- tm_map(my.corpus, removeWords, profanityWords)
        my.corpus <- tm_map(my.corpus, stripWhitespace)
        my.corpus <- tm_map(my.corpus, content_transformer(str_trim))

        lastRound <- function(x){
                gsub("(\\w['-]\\w)|[[:punct:]]", "\\1", x)
        }
        my.corpus <- tm_map(my.corpus, content_transformer(lastRound))
        # to take care of cases like "cock-a-hoop" (no idea what that is!)
        
        return(my.corpus)
}

# TWITTER: 8 files in total!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

twitter.cleaned <- clean(twitter)
saveRDS(twitter.cleaned, "twitter cleaned.Rds")

twitter.cleaned2 <- clean2(twitter)
saveRDS(twitter.cleaned2, "twitter cleaned2.Rds")
twitter.cleaned[[1540]]$content
twitter.cleaned2[[1540]]$content

# ready batch: twitter.tdm, twitter.tdm2 (without stopwords)
twitter.tdm <- TermDocumentMatrix(twitter.cleaned)
saveRDS(twitter.tdm, "twitter tdm.Rds")
dim(twitter.tdm)
dimnames(twitter.tdm)$Terms[1:10]
show(twitter.tdm)

twitter.tdm2 <- TermDocumentMatrix(twitter.cleaned2)
saveRDS(twitter.tdm2, "twitter tdm2.Rds")
dim(twitter.tdm2)
dimnames(twitter.tdm2)$Terms[1:10]

# ready batch: twitter.ready, twitter.ready2 (without stopwords)
twitter.ready <- character(20000)
for(i in 1:20000) twitter.ready[i] <- twitter.cleaned[[i]]$content
saveRDS(twitter.ready, "twitter ready.Rds")
twitter.ready <- readRDS("twitter ready.Rds")

twitter.ready2 <- character(20000)
for(i in 1:20000) twitter.ready2[i] <- twitter.cleaned2[[i]]$content
saveRDS(twitter.ready2, "twitter ready2.Rds")
twitter.ready2 <- readRDS("twitter ready2.Rds")

# unigrams
twitter.n1 <- make.ngrams(txt.to.words(twitter.ready, splitting.rule = "[ \n\t]"), ngram.size = 1)
t.n1 <- unique(twitter.n1)
saveRDS(t.n1, "t.n1.Rds")

# bigrams: t.n2, t.n2.b (with stopwords)
twitter.n2 <- make.ngrams(txt.to.words(twitter.ready, splitting.rule = "[ \n\t]"), ngram.size = 2)
t.n2 <- table(twitter.n2)
t.n2 <- data.table(count=as.vector(t.n2), bigrams=names(t.n2))
t.n2 <- arrange(t.n2, desc(count))
saveRDS(t.n2, "t.n2.Rds")

twitter.n2.b <- make.ngrams(txt.to.words(twitter.ready2, splitting.rule = "[ \n\t]"), ngram.size = 2)
t.n2.b <- table(twitter.n2.b)
t.n2.b <- data.table(count=as.vector(t.n2.b), bigrams=names(t.n2.b))
t.n2.b <- arrange(t.n2.b, desc(count))
saveRDS(t.n2.b, "t.n2.b.Rds")

# trigrams: t.n3, t.n3.b (with stopwords)
twitter.n3 <- make.ngrams(txt.to.words(twitter.ready, splitting.rule = "[ \n\t]"), ngram.size = 3)
t.n3 <- table(twitter.n3)
t.n3 <- data.table(count=as.vector(t.n3), trigrams=names(t.n3))
t.n3 <- arrange(t.n3, desc(count))
saveRDS(t.n3, "t.n3.Rds")

twitter.n3.b <- make.ngrams(txt.to.words(twitter.ready2, splitting.rule = "[ \n\t]"), ngram.size = 3)
t.n3.b <- table(twitter.n3.b)
t.n3.b <- data.table(count=as.vector(t.n3.b), trigrams=names(t.n3.b))
t.n3.b <- arrange(t.n3.b, desc(count))
saveRDS(t.n3.b, "t.n3.b.Rds")

# fourgrams

twitter.n4 <- make.ngrams(txt.to.words(twitter.ready, splitting.rule = "[ \n\t]"), ngram.size = 4)
t.n4 <- table(twitter.n4)
t.n4 <- data.table(count=as.vector(t.n4), fourgrams=names(t.n4))
t.n4 <- arrange(t.n4, desc(count))
saveRDS(t.n4, "t.n4.Rds")

twitter.n4.b <- make.ngrams(txt.to.words(twitter.ready2, splitting.rule = "[ \n\t]"), ngram.size = 4)
t.n4.b <- table(twitter.n4.b)
t.n4.b <- data.table(count=as.vector(t.n4.b), fourgrams=names(t.n4.b))
t.n4.b <- arrange(t.n4.b, desc(count))
head(t.n4.b)
saveRDS(t.n4.b, "t.n4.b.Rds")

# fivegrams

twitter.n5 <- make.ngrams(txt.to.words(twitter.ready, splitting.rule = "[ \n\t]"), ngram.size = 5)
t.n5 <- table(twitter.n5)
t.n5 <- data.table(count=as.vector(t.n5), fivegrams=names(t.n5))
t.n5 <- arrange(t.n5, desc(count))
saveRDS(t.n5, "t.n5.Rds")

twitter.n5.b <- make.ngrams(txt.to.words(twitter.ready2, splitting.rule = "[ \n\t]"), ngram.size = 5)
t.n5.b <- table(twitter.n5.b)
t.n5.b <- data.table(count=as.vector(t.n5.b), fivegrams=names(t.n5.b))
t.n5.b <- arrange(t.n5.b, desc(count))
saveRDS(t.n5.b, "t.n5.b.Rds")

# NEWS: 8 files in total!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

news.cleaned <- clean(news)
saveRDS(news.cleaned, "news cleaned.Rds")
news.cleaned[[1540]]$content

news.cleaned2 <- clean2(news)
saveRDS(news.cleaned2, "news cleaned2.Rds")
news.cleaned2[[1540]]$content

# ready batch: news.tdm, news.tdm2 (without stopwords)
news.tdm <- TermDocumentMatrix(news.cleaned)
dimnames(news.tdm)$Terms[1:10]
saveRDS(news.tdm, "news tdm.Rds")

news.tdm2 <- TermDocumentMatrix(news.cleaned2)
dimnames(news.tdm2)$Terms[1:10]
saveRDS(news.tdm2, "news tdm2.Rds")

# ready batch: twitter.ready, twitter.ready2 (without stopwords)
news.ready <- character(20000)
for(i in 1:20000) news.ready[i] <- news.cleaned[[i]]$content
saveRDS(news.ready, "news ready.Rds")
news.ready <- readRDS("news ready.Rds")

news.ready2 <- character(20000)
for(i in 1:20000) news.ready2[i] <- news.cleaned2[[i]]$content
saveRDS(news.ready2, "news ready2.Rds")
news.ready2 <- readRDS("news ready2.Rds")

# unigrams
news.n1 <- make.ngrams(txt.to.words(news.ready, splitting.rule = "[ \n\t]"), ngram.size = 1)
n.n1 <- unique(news.n1)
saveRDS(n.n1, "n.n1.Rds")

# bigrams: n.n2, n.n2.b (with stopwords)
news.n2 <- make.ngrams(txt.to.words(news.ready, splitting.rule = "[ \n\t]"), ngram.size = 2)
n.n2 <- table(news.n2)
n.n2 <- data.table(count=as.vector(n.n2), bigrams=names(n.n2))
n.n2 <- arrange(n.n2, desc(count))
head(n.n2)
saveRDS(n.n2, "n.n2.Rds")

news.n2.b <- make.ngrams(txt.to.words(news.ready2, splitting.rule = "[ \n\t]"), ngram.size = 2)
n.n2.b <- table(news.n2.b)
n.n2.b <- data.table(count=as.vector(n.n2.b), bigrams=names(n.n2.b))
n.n2.b <- arrange(n.n2.b, desc(count))
head(n.n2.b)
saveRDS(n.n2.b, "n.n2.b.Rds")

# trigrams: n.n3, n.n3.b (with stopwords)
news.n3 <- make.ngrams(txt.to.words(news.ready, splitting.rule = "[ \n\t]"), ngram.size = 3)
n.n3 <- table(news.n3)
n.n3 <- data.table(count=as.vector(n.n3), trigrams=names(n.n3))
n.n3 <- arrange(n.n3, desc(count))
head(n.n3)
saveRDS(n.n3, "n.n3.Rds")

news.n3.b <- make.ngrams(txt.to.words(news.ready2, splitting.rule = "[ \n\t]"), ngram.size = 3)
n.n3.b <- table(news.n3.b)
n.n3.b <- data.table(count=as.vector(n.n3.b), trigrams=names(n.n3.b))
n.n3.b <- arrange(n.n3.b, desc(count))
head(n.n3.b)
saveRDS(n.n3.b, "n.n3.b.Rds")

# fourgrams

news.n4 <- make.ngrams(txt.to.words(news.ready, splitting.rule = "[ \n\t]"), ngram.size = 4)
n.n4 <- table(news.n4)
n.n4 <- data.table(count=as.vector(n.n4), fourgrams=names(n.n4))
n.n4 <- arrange(n.n4, desc(count))
head(n.n4)
saveRDS(n.n4, "n.n4.Rds")

news.n4.b <- make.ngrams(txt.to.words(news.ready2, splitting.rule = "[ \n\t]"), ngram.size = 4)
n.n4.b <- table(news.n4.b)
n.n4.b <- data.table(count=as.vector(n.n4.b), fourgrams=names(n.n4.b))
n.n4.b <- arrange(n.n4.b, desc(count))
head(n.n4.b)
saveRDS(n.n4.b, "n.n4.b.Rds")


# fivegrams

news.n5 <- make.ngrams(txt.to.words(news.ready, splitting.rule = "[ \n\t]"), ngram.size = 5)
n.n5 <- table(news.n5)
n.n5 <- data.table(count=as.vector(n.n5), fivegrams=names(n.n5))
n.n5 <- arrange(n.n5, desc(count))
saveRDS(n.n5, "n.n5.Rds")

news.n5.b <- make.ngrams(txt.to.words(news.ready2, splitting.rule = "[ \n\t]"), ngram.size = 5)
n.n5.b <- table(news.n5.b)
n.n5.b <- data.table(count=as.vector(n.n5.b), fivegrams=names(n.n5.b))
n.n5.b <- arrange(n.n5.b, desc(count))
saveRDS(n.n5.b, "n.n5.b.Rds")

# BLOGS: 8 files in total!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

blogs.cleaned <- clean(blogs)
saveRDS(blogs.cleaned, "blogs cleaned.Rds")
blogs.cleaned[[154]]$content

blogs.cleaned2 <- clean2(blogs)
saveRDS(blogs.cleaned2, "blogs cleaned2.Rds")
blogs.cleaned2[[154]]$content

# ready batch: blogs.tdm, blogs.tdm2 (without stopwords)
blogs.tdm <- TermDocumentMatrix(blogs.cleaned)
dimnames(blogs.tdm)$Terms[1:10]
saveRDS(blogs.tdm, "blogs tdm.Rds")

blogs.tdm2 <- TermDocumentMatrix(blogs.cleaned2)
dimnames(blogs.tdm2)$Terms[1:10]
saveRDS(blogs.tdm2, "blogs tdm2.Rds")

# ready batch: blogs.ready, blogs.ready2 (without stopwords)
blogs.ready <- character(20000)
for(i in 1:20000) blogs.ready[i] <- blogs.cleaned[[i]]$content
saveRDS(blogs.ready, "blogs ready.Rds")
blogs.ready <- readRDS("blogs ready.Rds")

blogs.ready2 <- character(20000)
for(i in 1:20000) blogs.ready2[i] <- blogs.cleaned2[[i]]$content
saveRDS(blogs.ready2, "blogs ready2.Rds")
blogs.ready2 <- readRDS("blogs ready2.Rds")

# unigrams
blogs.n1 <- make.ngrams(txt.to.words(blogs.ready, splitting.rule = "[ \n\t]"), ngram.size = 1)
b.n1 <- unique(blogs.n1)
saveRDS(b.n1, "b.n1.Rds")

# bigrams: b.n2, b.n2.b (with stopwords)
blogs.n2 <- make.ngrams(txt.to.words(blogs.ready, splitting.rule = "[ \n\t]"), ngram.size = 2)
b.n2 <- table(blogs.n2)
b.n2 <- data.table(count=as.vector(b.n2), bigrams=names(b.n2))
b.n2 <- arrange(b.n2, desc(count))
head(b.n2)
saveRDS(b.n2, "b.n2.Rds")

blogs.n2.b <- make.ngrams(txt.to.words(blogs.ready2, splitting.rule = "[ \n\t]"), ngram.size = 2)
b.n2.b <- table(blogs.n2.b)
b.n2.b <- data.table(count=as.vector(b.n2.b), bigrams=names(b.n2.b))
b.n2.b <- arrange(b.n2.b, desc(count))
head(b.n2.b)
saveRDS(b.n2.b, "b.n2.b.Rds")

# trigrams: b.n3, b.n3.b (with stopwords)
blogs.n3 <- make.ngrams(txt.to.words(blogs.ready, splitting.rule = "[ \n\t]"), ngram.size = 3)
b.n3 <- table(blogs.n3)
b.n3 <- data.table(count=as.vector(b.n3), trigrams=names(b.n3))
b.n3 <- arrange(b.n3, desc(count))
head(b.n3)
saveRDS(b.n3, "b.n3.Rds")

blogs.n3.b <- make.ngrams(txt.to.words(blogs.ready2, splitting.rule = "[ \n\t]"), ngram.size = 3)
b.n3.b <- table(blogs.n3.b)
b.n3.b <- data.table(count=as.vector(b.n3.b), trigrams=names(b.n3.b))
b.n3.b <- arrange(b.n3.b, desc(count))
head(b.n3.b)
saveRDS(b.n3.b, "b.n3.b.Rds")

# fourgrams

blogs.n4 <- make.ngrams(txt.to.words(blogs.ready, splitting.rule = "[ \n\t]"), ngram.size = 4)
b.n4 <- table(blogs.n4)
b.n4 <- data.table(count=as.vector(b.n4), fourgrams=names(b.n4))
b.n4 <- arrange(b.n4, desc(count))
head(b.n4)
saveRDS(b.n4, "b.n4.Rds")

blogs.n4.b <- make.ngrams(txt.to.words(blogs.ready2, splitting.rule = "[ \n\t]"), ngram.size = 4)
b.n4.b <- table(blogs.n4.b)
b.n4.b <- data.table(count=as.vector(b.n4.b), fourgrams=names(b.n4.b))
b.n4.b <- arrange(b.n4.b, desc(count))
head(b.n4.b)
saveRDS(b.n4.b, "b.n4.b.Rds")


# fivegrams 

blogs.n5 <- make.ngrams(txt.to.words(blogs.ready, splitting.rule = "[ \n\t]"), ngram.size = 5)
b.n5 <- table(blogs.n5)
b.n5 <- data.table(count=as.vector(b.n5), fivegrams=names(b.n5))
b.n5 <- arrange(b.n5, desc(count))
saveRDS(b.n5, "b.n5.Rds")

blogs.n5.b <- make.ngrams(txt.to.words(blogs.ready2, splitting.rule = "[ \n\t]"), ngram.size = 5)
b.n5.b <- table(blogs.n5.b)
b.n5.b <- data.table(count=as.vector(b.n5.b), fivegrams=names(b.n5.b))
b.n5.b <- arrange(b.n5.b, desc(count))
saveRDS(b.n5.b, "b.n5.b.Rds")

# AGGREGATE ALL THE NGRAMS

t.n5 <- readRDS("t.n5.Rds")
t.n5.b <- readRDS("t.n5.b.Rds")
n.n5 <- readRDS("n.n5.Rds")
n.n5.b <- readRDS("n.n5.b.Rds")
b.n5 <- readRDS("b.n5.Rds")
b.n5.b <- readRDS("b.n5.b.Rds")

fivegrams <- rbind(t.n5, t.n5.b, n.n5, n.n5.b, b.n5, b.n5.b)
fivegrams <- fivegrams %>%
        group_by(fivegrams) %>%
        summarize(count=max(count)) %>%
        arrange(desc(count))
saveRDS(fivegrams, "fivegrams.Rds")


t.n4 <- readRDS("t.n4.Rds")
t.n4.b <- readRDS("t.n4.b.Rds")
n.n4 <- readRDS("n.n4.Rds")
n.n4.b <- readRDS("n.n4.b.Rds")
b.n4 <- readRDS("b.n4.Rds")
b.n4.b <- readRDS("b.n4.b.Rds")

fourgrams <- rbind(t.n4, t.n4.b, n.n4, n.n4.b, b.n4, b.n4.b)
fourgrams <- fourgrams %>%
        group_by(fourgrams) %>%
        summarize(count=max(count)) %>%
        arrange(desc(count))
saveRDS(fourgrams, "fourgrams.Rds")

t.n3 <- readRDS("t.n3.Rds")
t.n3.b <- readRDS("t.n3.b.Rds")
n.n3 <- readRDS("n.n3.Rds")
n.n3.b <- readRDS("n.n3.b.Rds")
b.n3 <- readRDS("b.n3.Rds")
b.n3.b <- readRDS("b.n3.b.Rds")

trigrams <- rbind(t.n3, t.n3.b, n.n3, n.n3.b, b.n3, b.n3.b)
trigrams <- trigrams %>%
        group_by(trigrams) %>%
        summarize(count=max(count)) %>%
        arrange(desc(count))
saveRDS(trigrams, "trigrams.Rds")

t.n2 <- readRDS("t.n2.Rds")
t.n2.b <- readRDS("t.n2.b.Rds")
n.n2 <- readRDS("n.n2.Rds")
n.n2.b <- readRDS("n.n2.b.Rds")
b.n2 <- readRDS("b.n2.Rds")
b.n2.b <- readRDS("b.n2.b.Rds")

bigrams <- rbind(t.n2, t.n2.b, n.n2, n.n2.b, b.n2, b.n2.b)
bigrams <- bigrams %>%
        group_by(bigrams) %>%
        summarize(count=max(count)) %>%
        arrange(desc(count))
head(bigrams)
saveRDS(bigrams, "bigrams.Rds")

t.n1 <- readRDS("t.n1.Rds")
n.n1 <- readRDS("n.n1.Rds")
b.n1 <- readRDS("b.n1.Rds")
all <- sort(unique(c(t.n1, n.n1, b.n1)))
head(all)
saveRDS(all, "all.Rds")
