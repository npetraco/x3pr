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
  
#   plot.surf.react <- reactive({
#     si <- theSurface()
#     ini.res <- resolution.initalizer(si[[2]])
#     si <- theSurface()
#     res <- dim(si[[2]])
#     ini.res <- resolution.initalizer(si[[2]])
#     if(min(ini.res)/max(ini.res) < 0.5) { #surface is pretty retangular. Start with this aspect ratio:
#       
#       lng.side <- which(ini.res == max(ini.res))
#       if(lng.side == 2) {
#         ini.asp <- c(1,3,0.4)
#       } else {
#         ini.asp <- c(3,1,0.4)
#       }
#       
#     } else { #surface is pretty square. Start with this aspect ratio:
#       ini.asp <- c(1,1,0.4)
#     }
#     plot.surface(si[[2]], ini.res[2], ini.res[1], aspect=ini.asp, plot.type="surface")
#   })
  
  #Render UI slider for z magnification.
  output$slider.zmag <- renderUI({
    inFile <- input$surface  
    if (is.null(inFile))
      return(NULL)
        
    sliderInput("slider.zmag", label = h3("z-scale (%)"), min = 1, max = 100, value = 40)
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
      #plot.surf.react()
      aspect3d(x = 1, y = 3, z = input$slider.zmag/100)
      
    }
  })
  
})
