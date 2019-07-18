
# Requirements beyond the packages in rocker/shiny-verse

req_cran <- c('cowplot','rjson')
req_gh <- c('peder2911/armour_ever_testy')
sapply(req_cran, install.packages, dependencies = FALSE)
sapply(req_gh, install_github, dependencies = FALSE)
