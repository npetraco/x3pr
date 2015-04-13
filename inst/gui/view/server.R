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
  
  #Render UI slider for z magnification.
  output$slider.zmag <- renderUI({
    inFile <- input$surface  
    if (is.null(inFile))
      return(NULL)
        
    sliderInput("slider.zmag", label = h3("z-scale (%)"), min = 1, max = 100, value = 40)
  })

  output$slider.theta <- renderUI({
    inFile <- input$surface  
    if (is.null(inFile))
      return(NULL)
  
    sliderInput("slider.theta", label = h3("theta (deg)"), min = 0, max = 180, value = 0)
  })

  output$slider.phi <- renderUI({
    inFile <- input$surface  
    if (is.null(inFile))
      return(NULL)
  
    sliderInput("slider.phi", label = h3("phi (deg)"), min = 0, max = 360, value = 15)
  })

  #To render a 3D surface plot:
  output$plot <- renderWebGL({
    if(is.null(theSurface())){
      plot3d(NULL,NULL,NULL)
    } else {
      si <- theSurface()
      res <- dim(si[[2]])
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
      
      #Running really slow. Make reactive??
      plot.surface(si, ini.res[2], ini.res[1], aspect=ini.asp, plot.type="surface")
      
      #save <- par3d(skipRedraw=TRUE)
      #clear3d(type = "lights")
      
      #aspect3d(x = 1, y = 3, z = round(input$slider.zmag/100, 1) )
      light3d(theta = input$slider.theta, phi = input$slider.phi, diffuse = "gray75", specular = "gray75")      
      
      #light3d(diffuse="gray10", specular="gray25")
      #par3d(save)
      
    }
  })
  
})
