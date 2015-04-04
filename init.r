#
# Install R package for tadaridaC
# See http://cran.r-project.org/doc/manuals/R-admin.html#Installing-packages for details
#

###########################################################
# Update this line with the R packages to install:

my_packages = c("randomForest", "data.table")

###########################################################

local({r <- getOption("repos"); 
       r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)})

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  else {
    cat(paste("Skipping already installed package:", p, "\n"))
  }
}
invisible(sapply(my_packages, install_if_missing))
