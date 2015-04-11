library(shinyRGL)

shinyUI(fluidPage(
  
  titlePanel("View Surface"),
  #Load a network
  sidebarLayout(
    sidebarPanel(
      fileInput(inputId = "surface", label = "Choose a surface", accept = "surface"),
      
      sliderInput("slider.xpts", label = h3("x-resolution (pts)"), min = 1, max = 100, value = 50)
    ),
    
    mainPanel(
    
      #h3(textOutput("header"))
      webGLOutput("plot", width="100%", height="800px")
          
    )
  )
  
))