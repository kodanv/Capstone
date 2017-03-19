#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
#library(tm)
#library(stylo)
#library(ggplot2)
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("NLP Capstone: Word Predictor"),
  
  # Sidebar with a user text input
  sidebarLayout(
    sidebarPanel(
      h3("Please input a text"),
       textInput("your_text", "your text", value = "I A"),
      submitButton("Submit")
    ),
    
    # Show the next word
    mainPanel(
      h3("Next Word?"),
       textOutput("next_word")
    )
  )
))
