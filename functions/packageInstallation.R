# Running this script to install old versions of RStem, maxent and RTextTools packages:

require(devtools)

# RStem version 0.4-1 
Rstem_packageurl <- "https://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz"
install.packages(Rstem_packageurl, repos=NULL, type="source")

# maxent version 1.3.3 
maxent_packageurl <- "https://cran.r-project.org/src/contrib/Archive/maxent/maxent_1.3.3.tar.gz"
install.packages(maxent_packageurl, repos=NULL, type="source")

# RTextTools version 1.4.2
RTextTools_packageurl <- "https://cran.r-project.org/src/contrib/Archive/RTextTools/RTextTools_1.4.2.tar.gz"
install.packages(RTextTools_packageurl, repos=NULL, type="source")