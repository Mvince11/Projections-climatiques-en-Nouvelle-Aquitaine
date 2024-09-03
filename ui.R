library(shiny) # permet de réaliser des applications web interactives en langage R (développée par la société RStudio)
library(shinyjs)
#library(shinydashboard) # permet de créer des tableaux de bords (ou dashboard en anglais), à l’aide de la librairie shiny
library(shinyWidgets) # permet de customiser vos applications Shiny en mettant à disposition de nouveaux inputs.

# Traitement
#library(dplyr)#permet de faire graphique
 #library(ggplot2)
 library(tidyverse) # permet de manipuler une base de données (est inclus ggplot2 entre autres)
 #library(DT) # tableau interactif en format HTML
 #library(fontawesome)# Librairies icônes
library(rsconnect)# Permet d'envoyer son projet sous shiny.apps.io
#library(rmarkdown)
#library(knitr)
library(htmlwidgets)
library(sf)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
#library(glue)

ui <- bootstrapPage(
  includeCSS("www/style.css"),
  #tags$head(tags$script(type = "text/javascript", src = "index.js"),
  #tags$head(tags$script(src = "https://cdn.rawgit.com/digidem/leaflet-side-by-side/gh-pages/leaflet-side-by-side.min.js")), # slider
            tags$div(
              img(src = "rechaufement-climatique-081139.jpg", style = "width:100%;height:500px;"),
              style = "text-align:center;"),
             # tags$script(HTML("
             #                     document.addEventListener('DOMContentLoaded', function() {
             #                       const summaries = document.querySelectorAll('.summary');
             #                       summaries.forEach(summary => {
             #                         summary.addEventListener('click', function() {
             #                           const details = this.nextElementSibling;
             #                           if (details.style.display === 'block') {
             #                             details.style.display = 'none';
             #                             this.classList.remove('open');
             #                           } else {
             #                             details.style.display = 'block';
             #                             this.classList.add('open');
             #                           }
             #                         });
             #                       });
             #                     });
             #                   ")
             #              # )
             # ),   
  
  
      navbarPage(
        id = "tabs",
        fluid = TRUE, 
        #position = "fixed-top",
        windowTitle = "Potentiel de mutabilité des friches",
        title=tags$div(tags$div(class = "semi-circle"),
                       img(src = "LogoCerema.png", style = "height:110px;width:auto;margin-top:-15px;margin-right:10px"),
                       "Projections Climatiques Nouvelle Aquitaine 2050", 
                       style = "font-family: YACkoD1yZN0-0;font-size:24px;text-align:center;margin-left:300px;"
                       
        ),
        collapsible = TRUE,
                  ),
  div(#class="outer",
      leafletOutput("map",
                    height = "800px"
                      ),
      div(id = "couche_slider", 
          textOutput("couche_slider_gauche"),
          textOutput("couche_slider_droite")
      ),
      # Combobox des communes
      div(id = "combobox",
          h4("Projections climatiques"),
          selectizeInput(
            "epci",
            label = "À l'EPCI",
            choices = sort(epci$NOM),
            options = list(
              placeholder = "Sélectionner un EPCI",
              onInitialize = I('function() { this.setValue(""); }')
            ),
          ),
          selectizeInput(
            "dpt",
            label = "Au Département",
            choices = sort(departement$NOM),
            options = list(
              placeholder = "Sélectionner un département",
              onInitialize = I('function() { this.setValue(""); }')
            ),
          )
      ))
              )
                      