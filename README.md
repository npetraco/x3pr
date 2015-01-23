# x3pr 

Basic read/write utilities for the x3p surface metrology format in R. The package is not up on CRAN yet. 
The best way to install x3pr is with Hadley Wickham's devtools: https://github.com/hadley/devtools 
and http://cran.r-project.org/web/packages/devtools/index.html. 

* If you haven't already done so, first install R (http://www.r-project.org/) and then RStudio (http://www.rstudio.com/) for your operating system. Note that both are open-source (and thus peer-reviewed, ...many times over...) and free. 

* Install devtools in your R distribution (devtools is up on CRAN now, so executing install.packages("devtools") in the RStudio Console window should suffice).

* Windows users: Install Rtools 3.1 as recommended by the devtools package: http://cran.r-project.org/bin/windows/Rtools/ 

* Install the required support packages for x3pr. In the RStudio Console window, execute:
  * install.packages("XML")
  * install.packages("tools")
  * install.packages("rgl")
  * install.packages("akima")

* Finally, in the RStudio Console window execute: install_github("npetraco/x3pr").

Examples are provided in the help pages. If you are new to R, below is an example R script which:

* Loads the x3pr package
* Reads in the example X3P file shipped with the package (glock.x3p)
* Prints out the header information in the file
* Plots the surface (a primer shear) in interactive 3D