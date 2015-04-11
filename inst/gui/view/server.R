library(x3pr)

options(shiny.maxRequestSize=60*1024^2) 

shinyServer(function(input, output) {
  
  #To load a surface:
  theSurface <- reactive({
    inFile <- input$surface  
    if (is.null(inFile))
      return(NULL)  
    
    ftype.ext <- tolower(unlist(strsplit(inFile$name,"[.]"))[2])
    if(ftype.ext =="x3p"){
      surface.info <- read.x3p(inFile$datapath)
    } else if(ftype.ext =="sur") {
      surface.info <- read.digital.surf.file(inFile$datapath)
    } else if(ftype.ext =="lms") {
      surface.info <- read.zeiss.lms.file(inFile$datapath)
    } else {
      return(NULL)
    }
    
    return(surface.info)
  })  
  
#   output$header <- renderPrint({
#     #theSurface()[[1]]
#     inFile$datapath
#   })  
  
  output$plot <- renderWebGL({
    if(is.null(theSurface())){
      #return()#Don't do anything
      plot3d(NULL,NULL,NULL)
    } else {
      plot.surface(theSurface(), 512, 80, aspect=c(1,3,0.4), plot.type="surface")
    }
  })
  
})
