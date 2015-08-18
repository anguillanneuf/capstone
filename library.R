setwd("./R15/milestone")
library(data.table); library(tm); library(dplyr)

twitter.tdm <- readRDS("twitter tdm.Rds")
twitter.tdm2 <- readRDS("twitter tdm2.Rds")
news.tdm <- readRDS("news tdm.Rds")
news.tdm2 <- readRDS("news tdm2.Rds")
blogs.tdm <- readRDS("blogs tdm.Rds")
blogs.tdm2 <- readRDS("blogs tdm2.Rds")

twitter.once <- findFreqTerms(twitter.tdm, lowfreq = 0, highfreq = 1)
twitter.once2 <- findFreqTerms(twitter.tdm2, lowfreq = 0, highfreq = 1)
news.once <- findFreqTerms(news.tdm, lowfreq = 0, highfreq = 1)
news.once2 <- findFreqTerms(news.tdm2, lowfreq = 0, highfreq = 1)
blogs.once <- findFreqTerms(blogs.tdm, lowfreq = 0, highfreq = 1)
blogs.once2 <- findFreqTerms(blogs.tdm2, lowfreq = 0, highfreq = 1)

once <- unique(c(twitter.once, twitter.once2, 
                 news.once, news.once2, 
                 blogs.once, blogs.once2))
saveRDS(once, "once.Rds")
once <- readRDS("once.Rds")

all <- readRDS("all.Rds")
dictionary <- all[!all %in% once]
saveRDS(dictionary, "dictionary.Rds")
dictionary <- readRDS("dictionary.Rds")

once <- readRDS("once.Rds")
n2 <-  readRDS("bigrams.Rds")
n2[, term1:=unlist(strsplit(bigrams, " "))[[1]], by=bigrams]
n2[, term2:=unlist(strsplit(bigrams, " "))[[2]], by=bigrams]
n2$term1[which(n2$term1 %in% once)] <- "unk"
n2$term2[which(n2$term2 %in% once)] <- "unk"
n2[, term := paste(term1, term2, " ")]
unwanted <- which(n2$term2 == "unk")
n2.final <- n2[-unwanted, ] %>%
        group_by(term) %>%
        summarize(count=sum(count))
n2.final[, ans:=unlist(strsplit(term, " "))[[2]], by=term]
saveRDS(n2.final, "n2.Rds")

once <- readRDS("once.Rds")
n3 <-  readRDS("trigrams.Rds")
n3[, term1:=unlist(strsplit(trigrams, " "))[[1]], by=trigrams]
n3[, term2:=unlist(strsplit(trigrams, " "))[[2]], by=trigrams]
n3[, term3:=unlist(strsplit(trigrams, " "))[[3]], by=trigrams]
n3$term1[which(n3$term1 %in% once)] <- "unk"
n3$term2[which(n3$term2 %in% once)] <- "unk"
n3$term3[which(n3$term3 %in% once)] <- "unk"
n3[, term := paste(term1, term2, term3, " ")]
unwanted <- which(n3$term3=="unk")
n3.final <- n3[-unwanted, ] %>%
        group_by(term) %>%
        summarize(count=sum(count))
n3.final[, ans:=unlist(strsplit(term, " "))[[3]], by=term]
saveRDS(n3.final, "n3.Rds")

once <- readRDS("once.Rds")
n4 <-  readRDS("fourgrams.Rds")
n4[, term1:=unlist(strsplit(fourgrams, " "))[[1]], by=fourgrams]
n4[, term2:=unlist(strsplit(fourgrams, " "))[[2]], by=fourgrams]
n4[, term3:=unlist(strsplit(fourgrams, " "))[[3]], by=fourgrams]
n4[, term4:=unlist(strsplit(fourgrams, " "))[[4]], by=fourgrams]
n4$term1[which(n4$term1 %in% once)] <- "unk"
n4$term2[which(n4$term2 %in% once)] <- "unk"
n4$term3[which(n4$term3 %in% once)] <- "unk"
n4$term4[which(n4$term4 %in% once)] <- "unk"
n4[, term := paste(term1, term2, term3, term4, " ")]
n4.final <- n4 %>%
        filter(term4!="unk") %>%
        group_by(term) %>%
        summarize(count=sum(count))
n4.final[, ans:=unlist(strsplit(term, " "))[[4]], by=term]
saveRDS(n4.final, "n4.Rds")


once <- readRDS("once.Rds")
n5 <-  readRDS("fivegrams.Rds")
n5[, term1:=unlist(strsplit(fivegrams, " "))[[1]], by=fivegrams]
n5[, term2:=unlist(strsplit(fivegrams, " "))[[2]], by=fivegrams]
n5[, term3:=unlist(strsplit(fivegrams, " "))[[3]], by=fivegrams]
n5[, term4:=unlist(strsplit(fivegrams, " "))[[4]], by=fivegrams]
n5[, term5:=unlist(strsplit(fivegrams, " "))[[5]], by=fivegrams]
n5$term1[which(n5$term1 %in% once)] <- "unk"
n5$term2[which(n5$term2 %in% once)] <- "unk"
n5$term3[which(n5$term3 %in% once)] <- "unk"
n5$term4[which(n5$term4 %in% once)] <- "unk"
n5$term5[which(n5$term5 %in% once)] <- "unk"
n5[, term := paste(term1, term2, term3, term4, term5, " ")]
n5.final <- n5 %>%
        filter(term5!="unk") %>%
        group_by(term) %>%
        summarize(count=sum(count))
n5.final[, ans:=unlist(strsplit(term, " "))[[5]], by=term]
saveRDS(n5.final, "n5.Rds")
