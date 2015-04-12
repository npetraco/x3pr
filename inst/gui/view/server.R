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
  
  #Render UI sliders if a surface has been loaded.
  #These sets the initial resolution. 
  output$slider.xpts <- renderUI({
    inFile <- input$surface  
    if (is.null(inFile))
      return(NULL)
    
    si <- theSurface()
    res <- dim(si[[2]])
    ini.res <- resolution.initalizer(si[[2]])
    
    sliderInput("slider.xpts", label = h3("x-resolution (%)"), min = 1, max = 100, value = round(ini.res[2]/res[2]*100))
  })
  
  output$slider.ypts <- renderUI({
    inFile <- input$surface  
    if (is.null(inFile))
      return(NULL)
    
    si <- theSurface()
    res <- dim(si[[2]])
    ini.res <- resolution.initalizer(si[[2]])
    
    sliderInput("slider.ypts", label = h3("y-resolution (%)"), min = 1, max = 100, value = round(ini.res[1]/res[1]*100))
  })
  
  
  #To render a 3D surface plot:
  output$plot <- renderWebGL({
    if(is.null(theSurface())){
      plot3d(NULL,NULL,NULL)
    } else {
      si <- theSurface()
      res <- dim(si[[2]])
      #ini.res <- resolution.initalizer(si[[2]])
      ini.res <- c( round((input$slider.ypts/100)*res[1]), round((input$slider.xpts/100)*res[2])) 
      #print(res)
      #print(ini.res2)
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
