library(shiny)

shinyUI(pageWithSidebar(  
  headerPanel("Word prediction"),  
  sidebarPanel(    
    textInput("phrase",label = h3("Input phrase here"), value="Hello"),
    helpText("Hello, my name is PredicTOR. I like to try and predict the next word from a phrase. Please be patient, I will tell you when I am ready to start predicting. I will only be a minute!")
  )
  , 
  mainPanel(    
    textOutput('greeting'),
    textOutput("next_words")
  )
))
