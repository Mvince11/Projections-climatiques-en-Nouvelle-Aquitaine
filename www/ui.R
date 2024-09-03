library(shiny) # permet de réaliser des applications web interactives en langage R (développée par la société RStudio)
library(shinyjs)
library(shinydashboard) # permet de créer des tableaux de bords (ou dashboard en anglais), à l’aide de la librairie shiny
#library(shinyWidgets) # permet de customiser vos applications Shiny en mettant à disposition de nouveaux inputs. 

# Traitement
library(fmsb)#permet de faire graphique en araignée
library(dplyr)#permet de faire graphique
library(ggplot2)
library(tidyverse) # permet de manipuler une base de données (est inclus ggplot2 entre autres)
library(DT) # tableau interactif en format HTML
library(fontawesome)# Librairies icônes
library(rsconnect)# Permet d'envoyer son projet sous shiny.apps.io
library(rmarkdown)
#library(knitr)
library(webshot)
library(htmlwidgets)
library(xlsx)
library(readxl)
#library(tinytex)


# Interface utilisateur
 ui <- bootstrapPage(
    includeCSS("www/style.css"),
    tags$head(tags$script(type = "text/javascript", src = "index.js"),
              tags$div(
                img(src = "friche_photo_test.jpg", style = "width:100%;height:600px;"),
                style = "text-align:center;")),   
              #)), # Utile pour la div fixed à relative
    
    fluidPage(
      navbarPage(
        id = "tabs",
        fluid = TRUE, 
        #position = "fixed-top",
        windowTitle = "Potentiel de mutabilité de la friche",
          title=tags$div(
            img(src = "LogoCerema.png", style = "height:101px;width:auto;margin-top:-17px;margin-right:10px"),
            "Potentiel de mutabilité de la friche", 
            style = "font-family: YACkoD1yZN0-0;font-size:24px;text-align:center;"
          
        ),
        collapsible = TRUE,

               fluidRow(column(6,
                              div("Le terme 'potentiel de mutabilité des friches' fait référence à la capacité des terrains en friche
                                  à se transformer ou à évoluer vers de nouveaux usages ou fonctions. Les friches,
                                  souvent des terrains abandonnés ou sous-utilisés, présentent un potentiel significatif pour des
                                  projets de réaménagement et de revitalisation.",style="text-align:center;font-family: YACkoD1yZN0-0;
                                    color: #464924;letter-spacing: 0.10em;font-size:1.3em;margin-left:10%;margin-right:10%") 
                              )),
      
      #tabPanel("Importation de données",
               fluidRow("Importer des données Excel",style="margin-top:100px;",
              # sidebarLayout(
                 #sidebarPanel(
                   fileInput("file1", "Choisir un fichier Excel", accept = c(".xlsx")),
                   tags$hr(),
                   uiOutput("sheet_ui")  # Générer dynamiquement les boutons radio pour les feuilles
                 ),
                 mainPanel(
                   tableOutput("contents")
                 )
               )
      )
    )
  #)
#)
