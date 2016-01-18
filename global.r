library(shiny)
library(dplyr)
library(RJSONIO)
library(Hmisc)
library(tm)
library(VennDiagram)
library(stringr) 
library(colorspace)

# wheel <- function(col, radius = 1, ...)
#   pie(rep(1, length(col)), col = col, radius = radius, ...)
# 
# wheel(rev(rainbow_hcl(14)))
# # cat(paste(paste('"',heat_hcl(14, h = c(-100, 0), l = c(75, 40), c = c(40, 80), power = 1),'"',sep=""),collapse = ","))
# cat(paste(paste('"',rev(rainbow_hcl(14)),'"',sep=""),collapse = ","))
# 
# wheel(rev(rainbow_hcl(14)))
# wheel(terrain_hcl(12, c = c(65, 0), l = c(45, 95), power = c(1/3, 1.5)))
# wheel(diverge_hcl(14, c = 100, l = c(50, 90)))
# wheel(cm.colors(14))
# wheel(desaturate(heat.colors(12)))
# wheel(rev(heat_hcl(14)))
#       
# rev(rainbow_hcl(14))



source('helpers.R')
source('jsons.R')
