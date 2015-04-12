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
      plot3d(NULL,NULL,NULL)
    } else {
      si <- theSurface()
      ini.res <- resolution.initalizer(si[[2]])
      if(min(ini.res)/max(ini.res) < 0.5) { #surface is pretty retangular. Start with this aspect ratio:
        
        lng.side <- which(ini.res == max(ini.res))
        if(lng.side == 2) {
          ini.asp <- c(1,3,0.4)
        } else {
          ini.asp <- c(3,1,0.4)
        }
        
      } else { #surface is pretty square. Start with this aspect ratio:
        ini.asp <- c(1,1,0.4)
      }
      
      plot.surface(si, ini.res[2], ini.res[1], aspect=ini.asp, plot.type="surface")
      
    }
  })
  
})
