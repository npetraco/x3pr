# x3pr 

Basic read/write utilities for the x3p surface metrology format in R. The package also supports the DigitalSurf .sur () and Zeiss LMS format for the CSM-700 confocal microscope (Warning: still buggy). The package is not up on CRAN yet. 
The best way to install x3pr is with Hadley Wickham's devtools: https://github.com/hadley/devtools 
and http://cran.r-project.org/web/packages/devtools/index.html. 

* If you haven't already done so, first install R (http://www.r-project.org/) and then RStudio (http://www.rstudio.com/) for your operating system. Note that both are open-source (and thus peer-reviewed, ...many times over if that is a concern) and free. 

* Install devtools in your R distribution. In the RStudio Console window type:
  * install.packages("devtools")

* Windows users: Install Rtools 3.1 as recommended by the devtools package: http://cran.r-project.org/bin/windows/Rtools/ 

* Mac users: Make sure to install XQuartz (http://xquartz.macosforge.org/trac) which is required by rgl.

* Linux users: Being singularly DIY, you are probably ok. But, ... just incase ... make sure gcc is installed and running. Any toolchain should suffice however. For shinyRGL, try to install from git directly: http://trestletech.github.io/shinyRGL/
  * There is no shinyRGL for Linux up on CRAN

* Install the required support packages for x3pr. In the RStudio Console window, execute:
  * install.packages("XML")
  * install.packages("rgl")
  * install.packages("shiny")
  * install.packages("shinyRGL") (Windows and Mac only...)

* Finally, in the RStudio Console window execute: devtools()::install_github("x3pr", username="npetraco").

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
