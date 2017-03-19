#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
 # string <- "A"
  
  
  string <- reactive({tolower(input$your_text)})
  
  num_words_str_func <- reactive({
  
   length(unlist(strsplit(string(), " ")))
   })
  
  #next_sam_ple_func <- function(string) {
 #     sam_ple<-unlist(strsplit(string, " "))
  #    sam_ple<- sam_ple[2:length(sam_ple)]
 #     sam_ple<- paste(sam_ple, collapse = " ")
 #     sam_ple<- c("^", sam_ple)
 #     sam_ple<- paste(sam_ple, collapse = "")
 #  }
  
### Loading ngram datatables  
  load("C:\\Users\\akodirov\\Documents\\Capstone\\n2gram.Rda")
  load("C:\\Users\\akodirov\\Documents\\Capstone\\n3gram.Rda")
  load("C:\\Users\\akodirov\\Documents\\Capstone\\n4gram.Rda")
  load("C:\\Users\\akodirov\\Documents\\Capstone\\n5gram.Rda")
  
nlp_func<-reactive({
  #### function to keep the last 5 words of the user input string
  last_4_words<-function(long_string){
    
    sam_ple_out <- unlist(strsplit(long_string, " "))
    sam_ple_out <- sam_ple_out[(length(sam_ple_out)-3):length(sam_ple_out)]
    sam_ple_out <- paste(sam_ple_out, collapse = " ")
    sam_ple_out
  }
 
  #### Make sure you have up to the last 4 words in the string search sample
  sampl_e <- string()
  ifelse (num_words_str_func() > 4, sam_ple <- last_4_words(sampl_e), sam_ple <- sampl_e)
 
  sam_ple <- c("^", sam_ple)
  sam_ple <- paste(sam_ple, collapse = "")

  ### function to search for sam_ple in ngram table
  nxt_word_func <- function(ngram.df, search_smpl){
    
 
  nxt_words <- grep(search_smpl,ngram.df[,1], value = TRUE)
  ifelse (length(nxt_words)==0, nxt_word <- 0,{ nxt_word <- strsplit(nxt_words[1], " ");  nxt_word <- paste(unlist(nxt_word));  nxt_word <- nxt_word[length(nxt_word)]})
  nxt_word
  }
  
  ### Function to remove the first word in the sample for n-1 recursion
  next_sam_ple_func <- function(string) {
    sam_ple<-unlist(strsplit(string, " "))
    sam_ple<- sam_ple[2:length(sam_ple)]
    sam_ple<- paste(sam_ple, collapse = " ")
    sam_ple<- c("^", sam_ple)
    sam_ple<- paste(sam_ple, collapse = "")
  }
  
  #####
  next_word <- 0
  n<-sam_ple
  
  if (length(unlist(strsplit(sam_ple, " "))) == 4) { next_word <- nxt_word_func(df.5gram.cut, sam_ple); sam_ple <-next_sam_ple_func(sam_ple)}

  if (next_word == 0 & length(unlist(strsplit(sam_ple, " "))) == 3) { next_word <- nxt_word_func(df.4gram.cut, sam_ple); sam_ple <-next_sam_ple_func(sam_ple)}
 
  if (next_word == 0 & length(unlist(strsplit(sam_ple, " "))) == 2) { next_word <- nxt_word_func(df.3gram.cut, sam_ple); sam_ple <-next_sam_ple_func(sam_ple)}
  
  if (next_word == 0 & length(unlist(strsplit(sam_ple, " "))) == 1) { next_word <- nxt_word_func(df.2gram.cut, sam_ple)}

if (next_word == "i") { next_word <- toupper((next_word))}
  next_word

  #for (k in 1: num_words_str_func()) { 
 #   ifelse (k == 3, k , k<-2) }
  #num_words_str_func()
  #paste(k, string())
})

  
 # sam_ple <-next_sam_ple_func(string)

  #output$next_word <- (renderText(string2))
 # output$next_word <- renderText(input$your_text)
  #output$distPlot <- renderPlot({
  
  
 
  output$next_word <- renderText(nlp_func())
  
})
