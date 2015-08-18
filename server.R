library(shiny)
library(data.table); library(dplyr)
n1 <- readRDS("dictionary-a.Rds")
n2 <- readRDS("n2 mini-a.Rds")
n3 <- readRDS("n3 mini-a.Rds")
n4 <- readRDS("n4 mini-a.Rds")

cleanInput <- function(x){
        x <- tolower(x)
        x <- gsub("\\S*[0-9]+\\S*", " ", x)
        x <- gsub("[^[:alnum:][:space:]'-]", " ", x)
        x <- gsub("(\\w['-]\\w)|[[:punct:]]", "\\1", x)
        x <- gsub("\\s+"," ",x)
        x <- gsub("^\\s+|\\s+$", "", x)
        return(x) 
}

predictNextWord <- function(x){
        x <- cleanInput(x)
        x <- unlist(strsplit(x, " "))
        m <- length(x)
        x <- sapply(x, function(i){
                if(!(i %in% n1)) "unk"
                else i
        })
        
        if(m==1){
                ans.dt <- filter(n2, grepl(paste0("^", x, " "), term)) %>%
                        arrange(desc(count))
                nextword <- head(ans.dt$ans)
        }else if(m==2){
                ans2.dt <- filter(n3, grepl(paste0("^", paste(x[1], x[2]), " "), term)) %>%
                        arrange(desc(count)) %>%
                        mutate(score = count/sum(count)) %>%
                        select(ans, score)
                ans1.dt <- filter(n2, grepl(paste0("^", x[2], " "), term)) %>%
                        arrange(desc(count)) %>%
                        mutate(score = count/sum(count)*0.4) %>%
                        select(ans, score)
                ans.dt <- rbind(ans2.dt, ans1.dt) %>%
                        group_by(ans) %>%
                        summarize(p = sum(score)) %>%
                        arrange(desc(p))
                nextword <- head(ans.dt$ans)
        }else{
                ans3.dt <- filter(n4, grepl(paste0("^", paste(x[m-2], x[m-1], x[m]), " "), term)) %>%
                        arrange(desc(count)) %>%
                        mutate(score = count/sum(count)) %>%
                        select(ans, score)
                ans2.dt <- filter(n3, grepl(paste0("^", paste(x[m-1], x[m]), " "), term)) %>%
                        arrange(desc(count)) %>%
                        mutate(score = count/sum(count)*0.4) %>%
                        select(ans, score)
                ans1.dt <- filter(n2, grepl(paste0("^", x[m], " "), term)) %>%
                        arrange(desc(count)) %>%
                        mutate(score = count/sum(count)*0.4*0.4) %>%
                        select(ans, score)
                ans.dt <- rbind(ans3.dt, ans2.dt, ans1.dt) %>%
                        group_by(ans) %>%
                        summarize(p = sum(score)) %>%
                        arrange(desc(p))
                nextword <- head(ans.dt$ans)
        }
        return(nextword)
}

shinyServer(function(input, output) {
        
        phrase <- eventReactive(input$go, {input$phrase})
        result <- reactive({predictNextWord(phrase())})
        output$prediction <- renderText({
                result()[1]
        })
        output$suggestion <- renderText({
                c(paste0(result()[-1], sep=", "), "etc.")
        })
        
})
