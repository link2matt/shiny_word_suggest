##Shiny Lecture 2
##Script 3
##Submit Action Button

library(shiny)
#library(shinythemes)

shinyUI(pageWithSidebar(

  headerPanel("Word Suggestion App"),
  sidebarPanel(
    textInput(inputId = "text1", label = "Write Your Text Here"),
    actionButton("goButton", "Suggest")
  ),
  mainPanel(
    p('Here are some suggested words to follow your text.', align = "left"),

   # uiOutput('matrix', align = "right"),
    tableOutput('matrix1')

    
  )
))