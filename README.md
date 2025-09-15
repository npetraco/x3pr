# x3pr 

Basic read/write utilities for the x3p surface metrology format in R. The package also supports the DigitalSurf .sur () format. X3pr is now failrly stable. Maybe one day I'll put it up on CRAN, but I'm leaving it here on GitHub for now. 
The best way to install x3pr is with the R package "remotes". 

* If you haven't already done so, first install R (http://www.r-project.org/). Optionally install RStudio (http://www.rstudio.com/) for your operating system. Note that both are open-source (and thus peer-reviewed, ...many times over if that is a concern) and free.

* Install remotes in your R distribution. In the R Console window type:
  * install.packages("remotes")

* Mac users: Make sure to install XQuartz (http://xquartz.macosforge.org/trac) which is required by rgl.

* Linux users: Being singularly DIY, you are probably ok. But, ... just incase ... make sure gcc is installed and running. Any toolchain should suffice however. Optionally for shinyRGL, try to install from git directly: http://trestletech.github.io/shinyRGL/
  * There is no shinyRGL for Linux up on CRAN

* Install the required support packages for x3pr. In the RStudio Console window, execute:
  * install.packages("XML")
  * install.packages("rgl")
  * install.packages("shiny")
  * install.packages("shinyRGL") (shinyRGL is optional. Also this line only works for Windows and Mac...)

* Finally, in the R Console window execute: remotes::install_github("npetraco/x3pr").
  * This may take a little while as the example 3D surface files that come with the package are large.

Examples are provided in the help pages. If you are new to R, below is an example R script which:

* Loads the x3pr package
* Reads in the example X3P file shipped with the package (glock.x3p)
* Prints out the header information in the file
* Plots the surface (a primer shear) in interactive 3D

`library(x3pr)`

`file.path <- system.file("extdata", "glock.x3p", package="x3pr")`

`glock.x3p.info <- read.x3p(file.path)`

`print.x3p.file.info(glock.x3p.info[[1]])`

`plot3D.x3p.file(glock.x3p.info, 1024, 80, aspect=c(1,3,0.4), plot.type="surface")`
