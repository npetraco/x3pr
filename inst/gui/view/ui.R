library(shinyRGL)
source(system.file("gui/view", "helpers.R", package="x3pr"))


shinyUI(fluidPage(
  
  titlePanel("View Surface"),
  #Sidebar controls:
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "surface", label = "Choose a surface", accept = "surface"),
      
      #Once the file is selected generate sliders:
      uiOutput("slider.zmag")
      
    ),
    
    #Main Panel:
    mainPanel(
    
      #h3(textOutput("header"))
      webGLOutput("plot", width="100%", height="600px")
          
    )
  )
  
))