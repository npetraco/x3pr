library(x3pr)

options(shiny.maxRequestSize=60*1024^2) 

shinyServer(function(input, output) {
  
  #To load a surface:
  theSurface <- reactive({
    inFile <- input$surface  
    if (is.null(inFile))
      return(NULL)  
    #loadHuginNet(file=inFile$datapath)
    #surface.info <- read.digital.surf.file(inFile$datapath)
    surface.info <- read.x3p(inFile$datapath)
    return(surface.info)
  })  
  
#   output$header <- renderPrint({
#     #surf.all <- theSurface()    
#     #surf.all[[1]]
#     theSurface()[[1]]
#   })
  
  output$plot <- renderWebGL({
    plot.surface(theSurface(), 512, 80, aspect=c(1,3,0.4), plot.type="surface")
  })
  
})
