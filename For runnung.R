library(shinylive)
library(httpuv)
library(usethis)
library(shinyWidgets)

shinylive::export(appdir = "../First_Demo/", destdir = "docs")

httpuv::runStaticServer("docs/", port = 8008)

sliderTextInput(inputId = "kl1", 
                label = "Level of Preference:",
                choices = c("Low", "Medium", "High"),
                selected = "Medium"),