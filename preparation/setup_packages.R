## Preparation for course ## 


# Check version of R
version

# Update all installed packages 

update.packages(ask = FALSE, checkBuilt = TRUE)

# packages to install
pkgs <- c("mgcv",  "brms", "qgam", "gamm4", "tidyverse", "readxl",
          "rstan", "mgcViz", "DHARMa")

# install those packages
install.packages(pkgs, Ncpus = 6) # set Ncpus to # of CPU cores you have

# Download and install gratia
install.packages('gratia', repos = c('https://gavinsimpson.r-universe.dev', 'https://cloud.r-project.org'))
