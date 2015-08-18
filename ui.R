library(shiny)

shinyUI
(
        fluidPage
        (theme =  "united.css",
        navbarPage("Next Word Prediction",
                   tabPanel("App",
                            fluidRow(
                                    column(8, offset = 2,
                                           br(), br(),
                                           h1("Enter your phrase below..."), 
                                           br(), br(),
                                           p(textInput("phrase", label="", "Everything under the ... ")),
                                           actionButton("go", "Go!"),
                                           br(), br(),
                                           h4(span(textOutput("prediction", inline=TRUE),
                                            style="color: #d16527;")),
                                           
                                           p(textOutput("suggestion", inline=TRUE))
                                           )

                            )
                            ),
                   tabPanel("About",
                            h3("What is this?"),
                            p("This is a simple word prediction app that predicts
                              the next word after a user inputs a sentence or a phrase."),
                            h3("Who created it?"),
                            p("It was created by me. I'm Tianzi."),
                            h3("When?"),
                            p("In August 2015."),
                            h3("How was it done?"),
                            p("This is just the barest outline in layman's terms. A more
                              detailed description can be found ", 
                              a(href="http://rpubs.com/anguillanneuf", "here.")), 
                            p("I used texts mined from Twitter, news, and blogs to 
                              create my own library of words and phrases. 
                              When a user types a sentence or a phrase, my program
                              goes to my library to look up the most frequently 
                              occuring such phrases and predicts the answers accordingly."),
                            p("My library consists of 2-word, 3-word, and 4-word phrases,
                              trained repeatedly on the aforementioned bodies of texts. It 
                              also contains a dictionary. The program looks in the 4-word 
                              phrases for a 3-word match first, then in the 3-word phrases 
                              for a 2-word match, and finally in the 2-word phrases for a 
                              1-word match. It tabulates the probabilities of such occurrances. 
                              It then outputs words that have the highest probabilities."),
                            h3("Why?"),
                            p("It's fun! See how a limited library and a small dictionary
                              can surprise you! Start typing!")
                            )
                            )
                   )
        )

