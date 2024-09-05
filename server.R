#source("load_data.R")
server <- function(input, output, session){
  #Couleurs ------
   ## couleurs Température_été_haute --------------------------------
   dpt_ete_brks <- c(20.5, 21, 21.5, 22, 22.5, 23)
   departement_donnees$T_ete._haute_cat <- cut(departement_donnees$T_ete._haute, breaks= dpt_ete_brks, 
                                               labels=c("#d9d9d9", "#ffffff", "#ffaaaa", "#ff5555", "#ff0000"),include.lowest = TRUE)
   pal_dpt_ete <- colorFactor(palette =c("#d9d9d9", "#ffffff", "#ffaaaa", "#ff5555", "#ff0000"),domain = unique(departement_donnees$T_ete._haute_cat))
   
   
   ## couleurs été référence --------------
   dpt_ete_ref_brks <- c(18, 18.5, 19, 19.5, 20, 20.5)
   departement_donnees$T_ete_ref_cat <- cut(departement_donnees$T_ete_ref, breaks= dpt_ete_ref_brks, 
                                            labels=c("#dcded5", "#f6ea96", "#f4bc1c", "#e5a748", "#f38319"),include.lowest = TRUE)
   pal_dpt_ete_ref <- colorFactor(palette =c("#dcded5", "#f6ea96", "#f4bc1c", "#e5a748", "#f38319"),domain = unique(departement_donnees$T_ete_ref_cat))
  
  
  ## couleurs Température_automne_haute --------------------------------
  dpt_automne_brks <- c(13.5, 14.5, 15.5, 16.5, 17.5)
  departement_donnees$T_.Automne_haute_cat <- cut(departement_donnees$T_.Automne_haute, breaks= dpt_automne_brks, 
                                                  labels=c("#ffffff", "#eda166", "#cea42b", "#8c4309"),include.lowest = TRUE)
  pal_dpt_automne <- colorFactor(palette =c("#ffffff", "#eda166", "#cea42b", "#8c4309"),domain = unique(departement_donnees$T_.Automne_haute_cat))
  
  ## couleurs températures automne de référence -----------------
  dpt_automne_ref_brks <- c(10, 11, 12, 13, 14)
  departement_donnees$T_.Automne_ref_cat <- cut(departement_donnees$T_.Automne_ref, breaks= dpt_automne_ref_brks, 
                                                labels=c("#eee773", "#f5ea37", "#f7a424", "#f97311"),include.lowest = TRUE)
  pal_dpt_automne_ref <- colorFactor(palette =c("#eee773", "#f5ea37", "#f7a424", "#f97311"),domain = unique(departement_donnees$T_.Automne_ref_cat))
  
  
  ## couleurs température_printemps_haute ----------
  dpt_printemps_brks <- c(11.5,12, 12.5, 13, 13.5, 14, 14.5)
  departement_donnees$T_printemps_haute_cat <- cut(departement_donnees$T_printemps_haute, breaks = dpt_printemps_brks,
                                                   labels = c("#ffffff","#E6E6E6","#F6CED8","#F5A9F2","#D358F7","#5858FA"))
  pal_dpt_printemps <- colorFactor(palette=c("#ffffff","#E6E6E6","#F6CED8","#F5A9F2","#D358F7","#5858FA"),domain = unique(departement_donnees$T_printemps_haute_cat))
  
  
  ## couleurs température printemps de référence -----------------
  dpt_printemps_ref_brks <- c(9.2, 9.8, 10.4, 11.0, 11.6, 12.2, 12.8)
  departement_donnees$T_printemps_ref_cat <- cut(departement_donnees$T_printemps_ref, breaks = dpt_printemps_ref_brks,
                                                   labels = c("#8ef110","#c5f110","#eef110","#f1be10","#f18e10","#f15810"))
  pal_dpt_printemps_ref <- colorFactor(palette=c("#8ef110","#c5f110","#eef110","#f1be10","#f18e10","#f15810"),domain = unique(departement_donnees$T_printemps_ref_cat))
  
  ## couleurs température_hivers ----------
  dpt_hiver_brks <- c(6.0,6.5,7.0,7.5,8.0,8.5,9.0)
  departement_donnees$T_hiver_haute_cat <- cut(departement_donnees$T_hiver_haute, breaks = dpt_hiver_brks,
                                               labels = c("#ffffff","#CEE3F6","#81F7F3","#04B486","#088A4B","#688A08"))
  pal_dpt_hiver <- colorFactor(palette=c("#ffffff","#CEE3F6","#81F7F3","#04B486","#088A4B","#688A08"),domain = unique(departement_donnees$T_hiver_haute_cat))
  
  dpt_hiver_ref_brks <- c(3.4,4.0,4.5,5.0,5.5,6.0,6.5)
  departement_donnees$T_hiver_ref_cat <- cut(departement_donnees$T_hiver_ref, breaks = dpt_hiver_ref_brks,
                                               labels = c("#A9F5F2","#81F7F3","#81DAF5","#819FF7","#5882FA","#9F81F7"))
  pal_dpt_hiver_ref <- colorFactor(palette=c("#A9F5F2","#81F7F3","#81DAF5","#819FF7","#5882FA","#9F81F7"),domain = unique(departement_donnees$T_hiver_ref_cat))
  
  ## Cumul précipitations été ----------
  ### cumul été 2050 -------
  cumul_ete_brks <- c(140,160,180,200,220,240,260)
  departement_donnees$Cumul_precipitations_ete_cat <- cut(departement_donnees$Cumul_precipitations_ete, breaks = cumul_ete_brks,
                                                          labels = c("#E6E6E6","#E1F5A9","#ACFA58","#AEB404","#DBA901","#DF3A01"))
  pal_cumul_ete <- colorFactor(palette = c("#E6E6E6","#E1F5A9","#ACFA58","#AEB404","#DBA901","#DF3A01"),domain = unique(departement_donnees$Cumul_precipitations_ete_cat))
  
  ### cumul été référence -----
  cumul_ete_ref_brks <- c(130,150,170,190,210,230,250)
  departement_donnees$Cumul_precipitations_ete_ref_cat <- cut(departement_donnees$Cumul_precipitations_ete_ref, breaks = cumul_ete_ref_brks,
                                                          labels = c("#10c8f1","#10f13c","#8bf110","#f1e310","#f1b010","#f16510"))
  pal_cumul_ete_ref <- colorFactor(palette = c("#10c8f1","#10f13c","#8bf110","#f1e310","#f1b010","#f16510"),domain = unique(departement_donnees$Cumul_precipitations_ete_ref_cat))
  
  
  ## Cumul précipitations automne -------
  ### cumul automne 2050 --------
  cumul_automne_brks <- c(210,250,280,310,340,370)
  departement_donnees$Cumul_precipitations_automne_cat <- cut(departement_donnees$Cumul_precipitations_automne,breaks = cumul_automne_brks,
                                                              labels = c("#D8D8D8","#A9F5D0","#F4FA58","#FF8000","#FF0000"))
  pal_cumul_automne <- colorFactor(palette = c("#D8D8D8","#A9F5D0","#F4FA58","#FF8000","#FF0000"), domain = unique(departement_donnees$Cumul_precipitations_automne_cat))
  
  ### cumul automne référence -----
  cumul_automne_ref_brks <- c(190,230,270,310,350,390)
  departement_donnees$Cumul_precipitations_automne_ref_cat <- cut(departement_donnees$Cumul_precipitations_automne_ref,breaks = cumul_automne_ref_brks,
                                                              labels = c("#D8D8D8","#A9F5D0","#F4FA58","#FF8000","#FF0000"))
  pal_cumul_automne_ref <- colorFactor(palette = c("#D8D8D8","#A9F5D0","#F4FA58","#FF8000","#FF0000"),
                                   domain = unique(departement_donnees$Cumul_precipitations_automne_ref_cat))
  
  
  ## Cumul précipitations printemps ------
  ### cumul printemps 2050 -------
  cumul_printemps_brks <- c(220,270,320,370,420,470)
  departement_donnees$Cumul_precipitations_printemps_cat <- cut(departement_donnees$Cumul_precipitations_printemps,breaks = cumul_printemps_brks,
                                                                labels = c("#E6E6E6","#F6CED8","#F781F3","#9A2EFE","#642EFE"))
  pal_cumul_printemps <- colorFactor(palette = c("#E6E6E6","#F6CED8","#F781F3","#9A2EFE","#642EFE"),
                                     domain = unique(departement_donnees$Cumul_precipitations_printemps_cat))
  
  ### cumul printemps référence -------
  cumul_printemps_ref_brks <- c(170,210,250,290,330,370)
  departement_donnees$Cumul_precipitations_printemps_ref_cat <- cut(departement_donnees$Cumul_precipitations_printemps_ref,breaks = cumul_printemps_ref_brks,
                                                                labels = c("#E6E6E6","#F6CED8","#F781F3","#9A2EFE","#642EFE"))
  pal_cumul_printemps_ref <- colorFactor(palette = c("#E6E6E6","#F6CED8","#F781F3","#9A2EFE","#642EFE"),
                                         domain = unique(departement_donnees$Cumul_precipitations_printemps_ref_cat))
  
  
  ## Cumul précipitations hiver ------
  ### cumul hiver 2050 -------
  cumul_hiver_brks <- c(260,300,340,380,420,460)
  departement_donnees$Cumul_precipitations_hiver_cat <- cut(departement_donnees$Cumul_precipitations_hiver,breaks = cumul_hiver_brks,
                                                            labels = c("#E6E6E6","#A9F5A9","#BFFF00","#FE9A2E","#FA8258"))
  pal_cumul_hiver <- colorFactor(palette = c("#E6E6E6","#A9F5A9","#BFFF00","#FE9A2E","#FA8258"),
                                 domain = unique(departement_donnees$Cumul_precipitations_hiver_cat))
  
  
  ### cumul hiver référence -------
  cumul_hiver_ref_brks <- c(180,220,260,300,340,380)
  departement_donnees$Cumul_precipitations_hiver_ref_cat <- cut(departement_donnees$Cumul_precipitations_hiver_ref,
                                                                breaks = cumul_hiver_ref_brks,
                                                                labels = c("#E6E6E6","#A9F5A9","#BFFF00","#FE9A2E","#FA8258"))
  pal_cumul_hiver_ref <- colorFactor(palette = c("#E6E6E6","#A9F5A9","#BFFF00","#FE9A2E","#FA8258"),
                                 domain = unique(departement_donnees$Cumul_precipitations_hiver_ref_cat))
  
  ## Feu de végétation ----
  ### feu de végétation 2050 ----
  feu_brks <- c(4,8,12,16,20)
  departement_donnees$feu_de_vegetation_cat <- cut(departement_donnees$feu_de_vegetation,
                                                   breaks = feu_brks,
                                                   labels = c("#F7FE2E","#FE9A2E","#DF3A01","#B40404"))
  pal_feu <- colorFactor(palette = c("#F7FE2E","#FE9A2E","#DF3A01","#B40404"),
                         domain = unique(departement_donnees$feu_de_vegetation_cat))
  
  ### feu de végétation de référence ----
  feu_ref_brks <- c(0,0.8,1.6,2.4,3.2)
  departement_donnees$feu_de_vegetation_ref_cat <- cut(departement_donnees$feu_de_vegetation_ref,
                                                       breaks = feu_ref_brks,
                                                       labels = c("#F7FE2E","#FE9A2E","#DF3A01","#B40404"))
  pal_feu_ref <- colorFactor(palette = c("#F7FE2E","#FE9A2E","#DF3A01","#B40404"),
                             domain = unique(departement_donnees$feu_de_vegetation_ref_cat))
  
  
  ### Nuits chaudes 2050 ----
  nuits_chaudes_brks <- c(18,21,24,27,30,33)
  departement_donnees$nuits_chaudes_cat <- cut(departement_donnees$nuits_chaudes,
                                               breaks = nuits_chaudes_brks,
                                               labels = c("#F6CED8","#CECEF6","#81F7BE","#A5DF00","#B40404"))
  pal_nuits_chaudes <- colorFactor(palette = c("#F6CED8","#CECEF6","#81F7BE","#A5DF00","#B40404"),
                                   domain = unique(departement_donnees$nuits_chaudes_cat))
  
  ### Nuits chaudes de référence ----
  nuits_chaudes_ref_brks <- c(2,3,4,5,6,7)
  departement_donnees$nuits_chaudes_ref_cat <- cut(departement_donnees$nuits_chaudes_ref,
                                               breaks = nuits_chaudes_ref_brks,
                                               labels = c("#F6CED8","#CECEF6","#81F7BE","#A5DF00","#B40404"))
  pal_nuits_chaudes_ref <- colorFactor(palette = c("#F6CED8","#CECEF6","#81F7BE","#A5DF00","#B40404"),
                                   domain = unique(departement_donnees$nuits_chaudes_ref_cat))
  
  ### jours chauds 2050 ---- 
  jours_chauds_brks <- c(5,6,7,8,9,10)
  departement_donnees$jours_chauds_cat <- cut(departement_donnees$jours_chauds,breaks = jours_chauds_brks,
                                              labels = c("#F2F2F2","#8181F7","#AEB404","#DF7401","#DF3A01"))
  pal_jours_chauds <- colorFactor(palette = c("#F2F2F2","#8181F7","#AEB404","#DF7401","#DF3A01"),
                                  domain = unique(departement_donnees$jours_chauds_cat))
  
  
  ### jours chauds de référence ---- 
  jours_chauds_ref_brks <- c(0,0.3,0.6,0.9,1.2,1.5)
  departement_donnees$jours_chauds_ref_cat <- cut(departement_donnees$jours_chauds_ref,
                                                  breaks = jours_chauds_ref_brks,
                                                  labels = c("#F2F2F2","#8181F7","#AEB404","#DF7401","#DF3A01"))
  pal_jours_chauds_ref <- colorFactor(palette = c("#F2F2F2","#8181F7","#AEB404","#DF7401","#DF3A01"),
                                      domain = unique(departement_donnees$jours_chauds_ref_cat))
  
  ### jours gel 2050 -----
  jours_gel_brks <- c(14,18,22,26,30,34)
  departement_donnees$jours_gel_cat <- cut(departement_donnees$jours_gel,
                                           breaks = jours_gel_brks,
                                           labels = c("#E6E6E6","#8181F7","#2E64FE","#0431B4","#0B3861"))
  pal_jours_gel <- colorFactor(palette = c("#E6E6E6","#8181F7","#2E64FE","#0431B4","#0B3861"),
                               domain = unique(departement_donnees$jours_gel_cat))
  
  ### jours gel référence ----
  jours_gel_ref_brks <- c(20,27,34,41,48,55)
  departement_donnees$jours_gel_ref_cat <- cut(departement_donnees$jours_gel_ref,
                                           breaks = jours_gel_ref_brks,
                                           labels = c("#E6E6E6","#8181F7","#2E64FE","#0431B4","#0B3861"))
  pal_jours_gel_ref <- colorFactor(palette = c("#E6E6E6","#8181F7","#2E64FE","#0431B4","#0B3861"),
                               domain = unique(departement_donnees$jours_gel_ref_cat))
  
  ### jours estivaux 2050 ----
  jours_estivaux_brks <- c(60,70,80,90,100,110)
  departement_donnees$jours_estivaux_cat <- cut(departement_donnees$jours_estivaux,breaks = jours_estivaux_brks,
                                                c("#e5e7e9","#edbb99","#f8c471","#f4d03f","#e74c3c"))
  pal_jours_estivaux <- colorFactor(palette = c("#e5e7e9","#edbb99","#f8c471","#f4d03f","#e74c3c"),
                                    domain = unique(departement_donnees$jours_estivaux_cat))
 
  
  ### jours estivaux référence ----
  jours_estivaux_ref_brks <- c(30,38,46,54,62,70)
  departement_donnees$jours_estivaux_ref_cat <- cut(departement_donnees$jours_estivaux_ref,breaks = jours_estivaux_ref_brks,
                                                c("#e5e7e9","#edbb99","#f8c471","#f4d03f","#e74c3c"))
  pal_jours_estivaux_ref <- colorFactor(palette = c("#e5e7e9","#edbb99","#f8c471","#f4d03f","#e74c3c"),
                                    domain = unique(departement_donnees$jours_estivaux_ref_cat))
  
  
  
  #  Reactive Values  ####
  fdepartement <- reactive({
    departement_donnees %>% filter(NOM == input$dpt) %>% st_centroid()
    
  })
  
   fepci <- reactive({
     epci %>% filter(NOM == input$epci)
   })
  
   
   bounds <- region %>% 
     st_bbox() %>% 
     as.character()
   
   # Fonction générique pour créer les labels HTML ----
   create_labels_2050 <- function(departement_donnees, title, col_name, color = "red", unit = "") {
     sapply(seq_len(nrow(departement_donnees)), function(i) {
       paste0(
         '<div style="text-align:center;width:auto;">',
         '<strong style="font-size:1.2em;">', 'Département : ','<br>',
         '<span style="color: ', color, ';">', departement_donnees$NOM[i], '</span>', '</strong>','<br>',
         '<strong style="font-size:1.2em;">', title, ': ', '<span style="color: ', color, ';">',
         as.character(departement_donnees[[col_name]][i]), unit, '</span>', '</strong>',
         '</div>'
       )
     }) %>% lapply(htmltools::HTML)
   }
   
   # Fonction générique pour créer les labels HTML ----
   create_labels_ref <- function(departement_donnees, title, col_name, color = "blue", unit = "") {
     sapply(seq_len(nrow(departement_donnees)), function(i) {
       paste0(
         '<div style="text-align:center;width:auto;">',
         '<strong style="font-size:1.2em;">', 'Département : ','<br>',
         '<span style="color: ', color, ';">', departement_donnees$NOM[i], '</span>', '</strong>','<br>',
         '<strong style="font-size:1.2em;">', title, ': ', '<span style="color: ', color, ';">',
         as.character(departement_donnees[[col_name]][i]), unit, '</span>', '</strong>',
         '</div>'
       )
     }) %>% lapply(htmltools::HTML)
   }
   
   # Utilisation de la fonction pour créer différents labels ----
   labels_automne <- create_labels_2050(departement_donnees, "Température à l'horizon 2050", "T_.Automne_haute", "red", "°C")
   labels_automne_ref <- create_labels_ref(departement_donnees, "Température de référence", "T_.Automne_ref", "blue", "°C")
   labels_cumul_automne <- create_labels_2050(departement_donnees, "Cumul des précipitations à l'horizon 2050", "Cumul_precipitations_automne", "red", " mm")
   labels_cumul_automne_ref <- create_labels_ref(departement_donnees, "Cumul des précipitations de référence", "Cumul_precipitations_automne_ref", "blue", " mm")
   
   
   labels_ete <- create_labels_2050(departement_donnees, "Température à l'horizon 2050", "T_ete._haute", "red", "°C")
   labels_ete_ref <- create_labels_ref(departement_donnees, "Température de référence", "T_ete_ref", "blue", "°C")
   labels_cumul_ete <- create_labels_2050(departement_donnees, "Cumul des précipitations à l'horizon 2050", "Cumul_precipitations_ete", "red", " mm")
   labels_cumul_ete_ref <- create_labels_ref(departement_donnees, "Cumul des précipitations de référence", "Cumul_precipitations_ete_ref", "blue", " mm")
   
   
   
   labels_printemps <- create_labels_2050(departement_donnees, "Température à l'horizon 2050", "T_printemps_haute", "red", "°C")
   labels_printemps_ref <- create_labels_ref(departement_donnees, "Température de référence", "T_printemps_ref", "blue", "°C")
   labels_cumul_printemps <- create_labels_2050(departement_donnees, "Cumul des précipitations à l'horizon 2050", "Cumul_precipitations_printemps", "red", " mm")
   labels_cumul_printemps_ref <- create_labels_ref(departement_donnees, "Cumul des précipitations de référence", "Cumul_precipitations_printemps_ref", "blue", " mm")
   
   
   labels_hiver <- create_labels_2050(departement_donnees, "Température à l'horizon 2050", "T_hiver_haute", "red", "°C")
   labels_hiver_ref <- create_labels_ref(departement_donnees,"Température de référence", "T_hiver_ref", "blue", "°C")
   labels_cumul_hiver <- create_labels_2050(departement_donnees, "Cumul des précipitations à l'horizon 2050", "Cumul_precipitations_hiver", "red", " mm")
   labels_cumul_hiver_ref <- create_labels_ref(departement_donnees, "Cumul des précipitations de référence", "Cumul_precipitations_hiver_ref", "blue", " mm")
   
   
   labels_feu_vegetation <- create_labels_2050(departement_donnees,
                                               tagList("Nombre de jours à l'horizon 2050",tags$br(),
                                               "avec risque de feu de végétation"),
                                               "feu_de_vegetation",
                                               "red")
   labels_feu_vegetation_ref <- create_labels_ref(departement_donnees,
                                                  tagList("Nombre de jours de référence",tags$br(),
                                                  "avec risque de feu de végétation"),
                                                  "feu_de_vegetation_ref",
                                                  "blue")
   
   labels_nuits_chaudes <- create_labels_2050(departement_donnees,
                                              "Nombre annuel de nuits chaudes à l'horizon 2050",
                                              "nuits_chaudes",
                                              "red")
   
   labels_nuits_chaudes_ref <- create_labels_ref(departement_donnees,
                                                 "Nombre annuel de nuits chaudes de références",
                                                 "nuits_chaudes_ref",
                                                 "blue")
   
   labels_jours_chauds <- create_labels_2050(departement_donnees,
                                             tagList("Nombre annuel de jours très chauds",tags$br(),
                                                     "à l'horizon 2050"),
                                             "jours_chauds",
                                             "red")
   
   labels_jours_chauds_ref <- create_labels_ref(departement_donnees,
                                                "Nombre annuel de jours très chauds",
                                                "jours_chauds_ref",
                                                "blue")
   
   labels_jours_gel <- create_labels_2050(departement_donnees,
                                          tagList("Nombre annuel de jours de gel",tags$br(),
                                                  "à l'horizon 2050"),
                                          "jours_gel",
                                          "red")
   
   labels_jours_gel_ref <- create_labels_ref(departement_donnees,
                                             tagList("Nombre annuel de jours de gel",tags$br(),
                                                     "de référence"),
                                             "jours_gel_ref",
                                             "blue")
   
   labels_jours_estivaux <- create_labels_2050(departement_donnees,
                                               tagList("Nombre annuel de jours estivaux",tags$br(),
                                                       "à l'horizon 2050"),
                                               "jours_estivaux",
                                               "red")
   
   labels_jours_estivaux_ref <- create_labels_ref(departement_donnees,
                                                  tagList("Nombre annuel de jours estivaux",tags$br(),
                                                          "de référence"),
                                                  "jours_estivaux_ref",
                                                  "blue")
  
   
   # Fenêtre modal d'ouverture de session -----
   observe({
     showModal(modalDialog(
       title = 
         div(
          h2("Bienvenue",style="color: #464924; font-weight:bold; text-align:center;
                         font-family: YACkoD1yZN0-0;")
          ),
          br(),
            div("Veuillez choisir un département ou un epci.
                Il vous suffira ensuite de sélectionner une couche
                afin de comparer et de visulaiser les variations de plusieurs paramètres climatiques à partir
                des valeurs de références et les valeurs prévues
                à l'horizon 2050.",
                style="color: #464924;text-align:center;
                         font-family: YACkoD1yZN0-0;font-size:1.4em;"),
       easyClose = TRUE,
       footer = modalButton("Fermer")
     ))
   })
   
   
  observe({
    req(input$dpt)  # nécessite l'instanciation de input$dpt pour exécution du bloc
    showModal(modalDialog(
      class="modal-content",
      tags$h2("Fiche d'identité du département" ,style ="color: #464924; font-weight:bold; text-align:center;
                         font-family: YACkoD1yZN0-0;"),
      tags$hr(style="border-block-color:darkgrey;"),
             div(tags$img(src = fdepartement()$images,style="width:inherit;"), style="width:300px;display:contents;"
          ),
      tags$br(),
      tags$hr(style="border-block-color:darkgrey;"),
      div(tags$em(tags$strong("Nombre de communes dans le département : "),
                  tags$b(fdepartement()$nb_communes,style="font-family: YACkoD1yZN0-0;color: #464924;letter-spacing: 0.109em;font-size:1.3em;")
                  )
          ),
      tags$br(),
      div(tags$em(tags$strong("Population du département : "),
                  tags$b(format(fdepartement()$population, big.mark=" "),
                                style="font-family: YACkoD1yZN0-0;color: #464924;letter-spacing: 0.109em;font-size:1.3em;")
                         ,"hab")
                  ),
      tags$br(),
      div(tags$em(tags$strong("Nombre d'EPCI du département : "),
                  tags$b(fdepartement()$nb_epci,
                         style="font-family: YACkoD1yZN0-0;color: #464924;letter-spacing: 0.109em;font-size:1.3em;")
                  )
          ),
      tags$br(),
      div(tags$em(tags$strong("Chef-lieu du département : "),
                  tags$b(fdepartement()$chef_lieu,
                         style="font-family: YACkoD1yZN0-0;color: #464924;letter-spacing: 0.109em;font-size:1.3em;")
                  )
          ),
      tags$br(),
      div(tags$em(tags$strong("Sous-préfectures du département : "),
                  tags$b(fdepartement()$sous_prefecture,
                         style="font-family: YACkoD1yZN0-0;color: #464924;letter-spacing: 0.109em;font-size:1.3em;")
                  )
          ),
      tags$br(),
      div(tags$em(tags$strong("Supérficie du département : "),
                  tags$b(format(fdepartement()$superficie,big.mark = " "),
                         style="font-family: YACkoD1yZN0-0;color: #464924;letter-spacing: 0.109em;font-size:1.3em;")
                  ,"km²")
          ),
      tags$br(),
      easyClose = TRUE,
      footer = NULL
                          )
              )
          })
  
  
  
# carte leaflet -------------------------------------------                           
  output$map <-renderLeaflet({
    leaflet() %>%
      addMapPane("left", zIndex = 0) %>%
      addMapPane("right", zIndex = 0) %>%
      addTiles(group = "base",
               layerId = "baseid",
               options = pathOptions(pane = "right")) %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik,
                       group="carto",
                       layerId = "cartoid",
                       options = pathOptions(pane = "left")) %>%
      addSidebyside(layerId = "sidecontrols",
                    rightId = "baseid",
                    leftId = "cartoid")%>%
      addControl(footer,
                 position = "bottomleft", 
                 className = "map-footer")%>%
      addPolygons(data = region,
                  popup = region$NOM_M,
                  popupOptions =popupOptions(closeButton = FALSE),
                  weight = 2,
                  color = "grey",
                  fillOpacity = 0,
                  opacity = 4,
                  layerId = region$geometry,
                  group = "Région")%>%
      addPolygons(data = departement,
                  fillOpacity = 0,
                  opacity = 4,
                  color="grey",
                  weight = 2,
                  layerId = departement$geometry,
                  group = "Département")%>%
      addFullscreenControl(pseudoFullscreen = FALSE,
                           position = "topleft")%>%
      addLayersControl(
        baseGroups = c("Open Street Map"),
        overlayGroups = c("Région","Département","Température (en °C) en été à l'échelle Départementale","Température (en °C) automnale à l'échelle Départementale",
                          "Température (en °C) au printemps à l'échelle Départementale","Température (en °C) en hiver à l'échelle Départementale",
                          "Cumul des précipitations en été à l'échelle Départementale","Cumul des précipitations en automne à l'échelle Départementale",
                          "Cumul des précipitations au printemps à l'échelle Départementale","Cumul des précipitations en hiver à l'échelle Départementale",
                          "Nombre de jours avec risque significatif de feu de végétation","Nombre annuel de nuits chaudes","Nombre annuel de jours très chauds",
                          "Nombre annuel de jours de gel","Nombre de jours annuels estivaux"),
        options = layersControlOptions(collapsed = TRUE)) %>%
      
      # cacher le groupe SISAL à l'ouverture de la carte'
      hideGroup(c("Département","Température (en °C) en été à l'échelle Départementale","Température (en °C) automnale à l'échelle Départementale","Département","Température (en °C) au printemps à l'échelle Départementale",
                  "Température (en °C) en hiver à l'échelle Départementale","Cumul des précipitations en été à l'échelle Départementale",
                  "Cumul des précipitations en automne à l'échelle Départementale","Cumul des précipitations au printemps à l'échelle Départementale",
                  "Cumul des précipitations en hiver à l'échelle Départementale","Nombre de jours avec risque significatif de feu de végétation",
                  "Nombre annuel de nuits chaudes","Nombre annuel de jours très chauds","Nombre annuel de jours de gel","Nombre de jours annuels estivaux"))%>%
      htmlwidgets::onRender("
           function(el, x) {
               $('.leaflet-control-layers-list').prepend('<label style=\"text-align:left;display:block; color: #424242;font-size:1.3em;font-family:arial-black;\">Fond de carte</label>');
           }
       ")%>%
      htmlwidgets::onRender("
           function(el,x) {
               //$('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:left;display:block; color: #424242;font-size:1.4em;\">Données de base</label>');
               
               $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:left;display:block; color: #424242;font-size:1.3em;font-family:arial-black;\">Couches de base & Couches thématiques</label>');
           }
                         ")%>%
        addFullscreenControl(pseudoFullscreen = FALSE, position = "topleft")%>%
          onRender("
        function(el, x) {
          var fullscreenControl = document.querySelector('.leaflet-control-fullscreen-button');
          if (fullscreenControl) {
            fullscreenControl.title = 'Voir en plein écran';
          }
    
          // Listen to fullscreenchange event to update the button title
          document.addEventListener('fullscreenchange', function() {
            if (document.fullscreenElement) {
              fullscreenControl.title = 'Quitter le plein écran';
            } else {
              fullscreenControl.title = 'Voir en plein écran';
            }
          });
                      }
               ")
          })  
 
          # Les sliders ####
          output$couche_slider_gauche <- renderText({"Valeurs de références"})
          output$couche_slider_droite <- renderText({"Valeurs 2050"})
          
          
          # zoom départements ----
          observe({
            req(input$dpt)
            centroide <- fdepartement() %>% st_geometry() %>% st_centroid() %>% .[[1]]
            
            leafletProxy("map")%>%
              
              flyTo(lng = centroide[1], lat = centroide[2], zoom = 9) %>%
              addSidebyside(layerId = "sidecontrols",
                            rightId = "baseid",
                            leftId = "cartoid")%>%
              removeShape("Région")%>%
              removeShape("Département")
    
    
          })           
      
      
      
      # 
      # addPolygons(data = departement_donnees,
      #             fillColor = ~pal_cumul_printemps(departement_donnees$Cumul_precipitations_printemps_cat), #fill = F,
      #             weight = 2.5,
      #             fillOpacity = 0.65,
      #             label = labels_cumul_printemps,
      #             labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
      #             color = "black",#label = departement_donnees$NOM,
      #             group = "Cumul des précipitations au printemps à l'échelle Départementale") %>%
      # addPolygons(data = departement_donnees,
      #             fillColor = ~pal_cumul_hiver(departement_donnees$Cumul_precipitations_hiver_cat), #fill = F,
      #             weight = 2.5,
      #             fillOpacity = 0.65,
      #             label = labels_cumul_hiver,
      #             labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
      #             color = "black",#label = departement_donnees$NOM,
      #             group = "Cumul des précipitations en hiver à l'échelle Départementale") %>%
      # addPolygons(data = departement_donnees,
      #             fillColor = ~pal_feu(departement_donnees$feu_de_vegetation_cat), #fill = F,
      #             weight = 2.5,
      #             fillOpacity = 0.65,
      #             label = labels_feu_vegetation,
      #             labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
      #             color = "black",#label = departement_donnees$NOM,
      #             group = "Nombre de jours avec risque significatif de feu de végétation") %>%
      # addPolygons(data = departement_donnees,
      #             fillColor = ~pal_nuits_chaudes(departement_donnees$nuits_chaudes_cat), #fill = F,
      #             weight = 2.5,
      #             fillOpacity = 0.65,
      #             label = labels_nuits_chaudes,
      #             labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
      #             color = "black",#label = departement_donnees$NOM,
      #             group = "Nombre annuel de nuits chaudes") %>%
      # addPolygons(data = departement_donnees,
      #             fillColor = ~pal_jours_chauds(departement_donnees$jours_chauds_cat), #fill = F,
      #             weight = 2.5,
      #             fillOpacity = 0.65,
      #             label = labels_jous_chauds,
      #             labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
      #             color = "black",#label = departement_donnees$NOM,
      #             group = "Nombre annuel de jours très chauds") %>%
      # addPolygons(data = departement_donnees,
      #             fillColor = ~pal_jours_gel(departement_donnees$jours_gel_cat), #fill = F,
      #             weight = 2.5,
      #             fillOpacity = 0.65,
      #             label = labels_jous_gel,
      #             labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
      #             color = "black",#label = departement_donnees$NOM,
      #             group = "Nombre annuel de jours de gel") %>%
      # addPolygons(data = departement_donnees,
      #             fillColor = ~pal_jours_estivaux(departement_donnees$jours_estivaux_cat), #fill = F,
      #             weight = 2.5,
      #             fillOpacity = 0.65,
      #             label = labels_jous_estivaux,labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
      #             color = "black",#label = departement_donnees$NOM,
      #             group = "Nombre de jours annuels estivaux") %>%

  
 
    
    
          
          
          # Définition du waiter --------
          w <- Waiter$new(
            html = tagList(spin_3circles(),
                           h3("Chargement de la couche en cours...",
                              style="color:#464924;letter-spacing: 0.109em;font-size:1.5em;")
            ),
            color = transparent(.1),
            id="map",
            hide_on_render = TRUE
          )   
      
          
      # Chargement des couches selon le groupe ------    
        observe({
          ## Températures été ----------
          if ("Température (en °C) en été à l'échelle Départementale" %in% input$map_groups) {
           w$show()
            Sys.sleep(3)
            leafletProxy("map") %>%
              clearShapes()%>%
              clearControls()%>%
               removeShape("Région")%>%
               removeShape("Département")%>%
              addSidebyside(layerId = "sidecontrols", rightId = "baseid", leftId = "cartoid") %>%
              addControl(footer,
                         position = "bottomleft",
                         className = "map-footer")%>%
            addPolygons(data = departement_donnees,
                        fillColor = ~pal_dpt_ete(departement_donnees$T_ete._haute_cat),
                        weight = 2.5, fillOpacity = 0.65,
                        label = labels_ete,
                        labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                        color = "black",
                        group = "Température (en °C) en été à l'échelle Départementale",
                        options = pathOptions(pane = "right")) %>%
              addPolygons(data = departement_donnees,
                          fillColor = ~pal_dpt_ete_ref(departement_donnees$T_ete_ref_cat),
                          weight = 2.5, fillOpacity = 0.65,
                          label = labels_ete_ref,
                          labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                          color = "black",
                          group = "Température (en °C) en été à l'échelle Départementale",
                          options = pathOptions(pane = "left")) %>%
              # Ajout de la première légende
              addLegend("bottomright",
                        colors = c("#d9d9d9", "#ffffff", "#ffaaaa", "#ff5555", "#ff0000"),
                        labels = c("de 20.5 à 21 inclus", "de 21 à 21.5 inclus", "de 21.5 à 22 inclus", "de 22 à 22.5 inclus", "de 22.5 à 23 inclus"),
                        title = "Température estivale en °C",
                        group = "Température (en °C) en été à l'échelle Départementale",
                        opacity = 1,
                        layerId = "legend1") %>%
              
              # Ajout de la deuxième légende
              addLegend("bottomleft",
                        colors = c("#dcded5", "#f6ea96", "#f4bc1c", "#e5a748", "#f38319"),
                        labels = c("de 18.0 à 18.5 inclus", "de 18.5 à 19 inclus", "de 19 à 19.5 inclus", "de 19.5 à 20 inclus", "de 20 à 20.5 inclus"),
                        title = "Température estivale en °C",
                        group = "Température (en °C) en été à l'échelle Départementale",
                        opacity = 1,
                        layerId = "legend2")
            
            w$hide()
            
          }
          
          # Températures automne ----------
           else if ("Température (en °C) automnale à l'échelle Départementale" %in% input$map_groups) {
             w$show()
             Sys.sleep(3)
             leafletProxy("map") %>%
               clearShapes()%>%
               clearControls()%>%
               removeShape("Région")%>%
               removeShape("Département")%>%
               addSidebyside(layerId = "sidecontrols", rightId = "baseid", leftId = "cartoid") %>%
               addControl(footer,
                          position = "bottomleft",
                          className = "map-footer")%>%
             addPolygons(data = departement_donnees,
                         fillColor = ~pal_dpt_automne(departement_donnees$T_.Automne_haute_cat),
                         weight = 2.5, fillOpacity = 0.65,
                         label = labels_automne,
                         labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                         color = "black",
                         group = "Température (en °C) automnale à l'échelle Départementale",
                         options = pathOptions(pane = "right")) %>%
               addPolygons(data = departement_donnees,
                           fillColor = ~pal_dpt_automne_ref(departement_donnees$T_.Automne_ref_cat),
                           weight = 2.5, fillOpacity = 0.65,
                           label = labels_automne_ref,
                           labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                           color = "black",
                           group = "Température (en °C) automnale à l'échelle Départementale",
                           options = pathOptions(pane = "left"))%>%
               addLegend("bottomright",
                         colors = c("#ffffff", "#eda166", "#cea42b", "#8c4309"),
                         labels = c("de 13.5 à 14.5 inclus", "de 14.5 à 15.5 inclus", "de 15.5 à 16.5 inclus", "> 16.5"),
                         title = "Température automnale en °C",
                         group = "Température (en °C) automnale à l'échelle Départementale",
                         opacity = 1,
                         layerId = "legend3") %>%
               
               # Ajout de la quatrième légende
               addLegend("bottomleft",
                         colors = c("#eee773", "#f5ea37", "#f7a424", "#f97311"),
                         labels = c("de 10 à 11 inclus", "de 11 à 12 inclus", "de 12 à 13 inclus", "> 13"),
                         title = "Température automnale en °C",
                         group = "Température (en °C) automnale à l'échelle Départementale",
                         opacity = 1,
                         layerId = "legend4")
             
             w$hide()
           }
          
          # Températures printemps ----------
          else if("Température (en °C) au printemps à l'échelle Départementale" %in% input$map_groups){
            w$show()
            Sys.sleep(3)
            leafletProxy("map") %>%
              clearShapes()%>%
              clearControls()%>%
              removeShape("Région")%>%
              removeShape("Département")%>%
              addSidebyside(layerId = "sidecontrols", rightId = "baseid", leftId = "cartoid") %>%
              addControl(footer,
                         position = "bottomleft",
                         className = "map-footer")%>%
            addPolygons(data = departement_donnees,
                        fillColor = ~pal_dpt_printemps(departement_donnees$T_printemps_haute_cat), #fill = F,
                        weight = 2.5,
                        fillOpacity = 0.65,
                        label = labels_printemps,
                        labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                        color = "black",#label = departement_donnees$NOM,
                        group = "Température (en °C) au printemps à l'échelle Départementale",
                        options = pathOptions(pane = "right"))%>%
              addPolygons(data = departement_donnees,
                          fillColor = ~pal_dpt_printemps_ref(departement_donnees$T_printemps_ref_cat), #fill = F,
                          weight = 2.5,
                          fillOpacity = 0.65,
                          label = labels_printemps_ref,
                          labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                          color = "black",#label = departement_donnees$NOM,
                          group = "Température (en °C) au printemps à l'échelle Départementale",
                          options = pathOptions(pane = "left")) %>%
              addLegend("bottomright",
                        colors =c("#ffffff","#E6E6E6","#F6CED8","#F5A9F2","#D358F7","#5858FA"),
                        labels = c("de 11.5 à 12 inclus","de 12 à 12.5 inclus","de 12.5 à 13.0 inclus","de 13.0 à 13.5 inclus","de 13.5 à 14.0 inclus","de 14.0 à 14.5 inclus"), 
                        title = "Température printanière en °C ",
                        group = "Température (en °C) au printemps à l'échelle Départementale",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors =c("#8ef110","#c5f110","#eef110","#f1be10","#f18e10","#f15810"),
                        labels = c("de 9.2 à 9.8 inclus","de 9.8 à 10.4 inclus","de 10.4 à 11.0 inclus","de 11.0 à 11.6 inclus","de 11.6 à 12.2 inclus","> 12.2"), 
                        title = "Température printanière en °C ",
                        group = "Température (en °C) au printemps à l'échelle Départementale",
                        opacity = 1)
            
            w$hide()
          }
          
          # Températures hiver ----------
          else if("Température (en °C) en hiver à l'échelle Départementale" %in% input$map_groups){
            w$show()
            Sys.sleep(3)
            leafletProxy("map") %>%
              clearShapes()%>%
              clearControls()%>%
              removeShape("Région")%>%
              removeShape("Département")%>%
              addSidebyside(layerId = "sidecontrols", rightId = "baseid", leftId = "cartoid") %>%
              addControl(footer,
                         position = "bottomleft",
                         className = "map-footer")%>%
            addPolygons(data = departement_donnees,
                        fillColor = ~pal_dpt_hiver_ref(departement_donnees$T_hiver_ref_cat), #fill = F,
                        weight = 2.5,
                        fillOpacity = 0.65,
                        label = labels_hiver_ref,
                        labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                        color = "black",#label = departement_donnees$NOM,
                        group = "Température (en °C) en hiver à l'échelle Départementale",
                        options = pathOptions(pane = "left")) %>%
              addPolygons(data = departement_donnees,
                          fillColor = ~pal_dpt_hiver(departement_donnees$T_hiver_haute_cat), #fill = F,
                          weight = 2.5,
                          fillOpacity = 0.65,
                          label = labels_hiver,
                          labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                          color = "black",#label = departement_donnees$NOM,
                          group = "Température (en °C) en hiver à l'échelle Départementale",
                          options = pathOptions(pane = "right"))%>%
              addLegend("bottomright",
                        colors =c("#ffffff","#CEE3F6","#81F7F3","#04B486","#088A4B","#688A08"),
                        labels = c("de 6.0 à 6.5 inclus","de 6.5 à 7.0 inclus","de 7.0 à 7.5 inclus","de 7.5 à 8.0 inclus","de 8.0 à 8.5 inclus","de 8.5 à 9.0 inclus"), 
                        title = "Température hivernale en °C ",
                        group = "Température (en °C) en hiver à l'échelle Départementale",
                        opacity = 1) %>%
              addLegend("bottomleft",
                        colors =c("#A9F5F2","#81F7F3","#81DAF5","#819FF7","#5882FA","#9F81F7"),
                        labels = c("de 3.0 à 4.0 inclus","de 4.0 à 4.5 inclus","de 4.5 à 5.0 inclus","de 5.0 à 5.5 inclus","de 5.5 à 6.0 inclus","> 6.0"), 
                        title = "Température hivernale en °C ",
                        group = "Température (en °C) en hiver à l'échelle Départementale",
                        opacity = 1)
            
            w$hide()
          }
          
          # cumul été ----------
          else if("Cumul des précipitations en été à l'échelle Départementale" %in% input$map_groups){
            w$show()
            Sys.sleep(3)
            leafletProxy("map") %>%
              clearShapes()%>%
              clearControls()%>%
              removeShape("Région")%>%
              removeShape("Département")%>%
              addSidebyside(layerId = "sidecontrols", rightId = "baseid", leftId = "cartoid") %>%
              addControl(footer,
                         position = "bottomleft",
                         className = "map-footer")%>%
            addPolygons(data = departement_donnees,
                        fillColor = ~pal_cumul_ete(departement_donnees$Cumul_precipitations_ete_cat),
                        weight = 2.5,
                        fillOpacity = 0.65,
                        label = labels_cumul_ete,
                        labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                        color = "black",#label = departement_donnees$NOM,
                        group = "Cumul des précipitations en été à l'échelle Départementale",
                        options = pathOptions(pane = "right")) %>%
              
            addLegend("bottomright",
                      colors =c("#E6E6E6","#E1F5A9","#ACFA58","#AEB404","#DBA901","#DF3A01"),
                      labels = c("140 à 160 inclus","160 à 180 inclus","180 à 200 inclus","200 à 220 inclus","220 à 240 inclus","> 240"), 
                      title = "Cumul des précipitations en été (en mm)",
                      group = "Cumul des précipitations en été à l'échelle Départementale",
                      opacity = 1) %>%
              
            addPolygons(data = departement_donnees,
                        fillColor = ~pal_cumul_ete_ref(departement_donnees$Cumul_precipitations_ete_ref_cat),
                        weight = 2.5,
                        fillOpacity = 0.65,
                        label = labels_cumul_ete_ref,
                        labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                        color = "black",#label = departement_donnees$NOM,
                        group = "Cumul des précipitations en été à l'échelle Départementale",
                        options = pathOptions(pane = "left")) %>%
              
            addLegend("bottomleft",
                      colors =c("#10c8f1","#10f13c","#8bf110","#f1e310","#f1b010","#f16510"),
                      labels = c("130 à 150 inclus","150 à 170 inclus","170 à 190 inclus","190 à 210 inclus","210 à 230 inclus","> 230"), 
                      title = "Cumul des précipitations en été (en mm)",
                      group = "Cumul des précipitations en été à l'échelle Départementale",
                      opacity = 1)
            
            w$hide()
          }
          
          # cumul automne -----
          else if("Cumul des précipitations en automne à l'échelle Départementale" %in% input$map_groups){
            w$show()
            Sys.sleep(3)
            leafletProxy("map") %>%
              clearShapes()%>%
              clearControls()%>%
              removeShape("Région")%>%
              removeShape("Département")%>%
              addSidebyside(layerId = "sidecontrols", rightId = "baseid", leftId = "cartoid") %>%
              addControl(footer,
                         position = "bottomleft",
                         className = "map-footer")%>%
            addPolygons(data = departement_donnees,
                        fillColor = ~pal_cumul_automne(departement_donnees$Cumul_precipitations_automne_cat), #fill = F,
                        weight = 2.5,
                        fillOpacity = 0.65,
                        label = labels_cumul_automne,
                        labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                        color = "black",#label = departement_donnees$NOM,
                        group = "Cumul des précipitations en automne à l'échelle Départementale",
                        options = pathOptions(pane = "right")) %>%
              
            addLegend("bottomright",
                      colors =c("#D8D8D8","#A9F5D0","#F4FA58","#FF8000","#FF0000"),
                      labels = c("210-250","250-280","280-310","310-340","340-370"), 
                      title = "Cumul des précipitations en automne (en mm)",
                      group = "Cumul des précipitations en automne à l'échelle Départementale",
                      opacity = 1)%>%
              
              addPolygons(data = departement_donnees,
                          fillColor = ~pal_cumul_automne_ref(departement_donnees$Cumul_precipitations_automne_ref_cat), #fill = F,
                          weight = 2.5,
                          fillOpacity = 0.65,
                          label = labels_cumul_automne_ref,
                          labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                          color = "black",#label = departement_donnees$NOM,
                          group = "Cumul des précipitations en automne à l'échelle Départementale",
                          options = pathOptions(pane = "left")) %>%
              
              addLegend("bottomleft",
                        colors =c("#D8D8D8","#A9F5D0","#F4FA58","#FF8000","#FF0000"),
                        labels = c("190 à 230 inclus","230 à 270 inclus","270 à 310 inclus","310 à 350 inclus","> 350"), 
                        title = "Cumul des précipitations en automne (en mm)",
                        group = "Cumul des précipitations en automne à l'échelle Départementale",
                        opacity = 1)
            
            w$hide()
          }
          
          # cumul printemps -----
          else if("Cumul des précipitations au printemps à l'échelle Départementale" %in% input$map_groups){
            w$show()
            Sys.sleep(3)
            leafletProxy("map") %>%
              clearShapes()%>%
              clearControls()%>%
              removeShape("Région")%>%
              removeShape("Département")%>%
              addSidebyside(layerId = "sidecontrols", rightId = "baseid", leftId = "cartoid") %>%
              addControl(footer,
                         position = "bottomleft",
                         className = "map-footer")%>%
            addPolygons(data = departement_donnees,
                        fillColor = ~pal_cumul_printemps(departement_donnees$Cumul_precipitations_printemps_cat), #fill = F,
                        weight = 2.5,
                        fillOpacity = 0.65,
                        label = labels_cumul_printemps,
                        labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                        color = "black",#label = departement_donnees$NOM,
                        group = "Cumul des précipitations au printemps à l'échelle Départementale",
                        options = pathOptions(pane = "right")) %>%
              
            addLegend("bottomright",
                      colors =c("#E6E6E6","#F6CED8","#F781F3","#9A2EFE","#642EFE"),
                      labels = c("220-270","270-320","320-370","370-420","> 420"), 
                      title = "Cumul des précipitations au printemps (en mm)",
                      group = "Cumul des précipitations au printemps à l'échelle Départementale",
                      opacity = 1)%>%
              
              addPolygons(data = departement_donnees,
                          fillColor = ~pal_cumul_printemps_ref(departement_donnees$Cumul_precipitations_printemps_ref_cat), #fill = F,
                          weight = 2.5,
                          fillOpacity = 0.65,
                          label = labels_cumul_printemps_ref,
                          labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                          color = "black",#label = departement_donnees$NOM,
                          group = "Cumul des précipitations au printemps à l'échelle Départementale",
                          options = pathOptions(pane = "left")) %>%
              
              addLegend("bottomleft",
                        colors =c("#E6E6E6","#F6CED8","#F781F3","#9A2EFE","#642EFE"),
                        labels = c("170 à 210 inclus","210 à 250 inclus","250 à 290 inclus","290 à 330","> 330"), 
                        title = "Cumul des précipitations au printemps (en mm)",
                        group = "Cumul des précipitations au printemps à l'échelle Départementale",
                        opacity = 1)
              
              w$hide()
          }
          
          # cumul hiver -----
          else if("Cumul des précipitations en hiver à l'échelle Départementale" %in% input$map_groups){
            w$show()
            Sys.sleep(3)
            leafletProxy("map") %>%
              clearShapes()%>%
              clearControls()%>%
              removeShape("Région")%>%
              removeShape("Département")%>%
              addSidebyside(layerId = "sidecontrols", rightId = "baseid", leftId = "cartoid") %>%
              addControl(footer,
                         position = "bottomleft",
                         className = "map-footer")%>%
            addPolygons(data = departement_donnees,
                        fillColor = ~pal_cumul_hiver(departement_donnees$Cumul_precipitations_hiver_cat), #fill = F,
                        weight = 2.5,
                        fillOpacity = 0.65,
                        label = labels_cumul_hiver,
                        labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                        color = "black",
                        group = "Cumul des précipitations en hiver à l'échelle Départementale",
                        options = pathOptions(pane = "right")) %>%
              
              addLegend("bottomright",
                        colors =c("#E6E6E6","#A9F5A9","#BFFF00","#FE9A2E","#FA8258"),
                        labels = c("260-300","300-340","340-380","380-420","> 420"), 
                        title = "Cumul des précipitations en hiver (en mm)",
                        group = "Cumul des précipitations en hiver à l'échelle Départementale", opacity = 1)%>%
              
              addPolygons(data = departement_donnees,
                          fillColor = ~pal_cumul_hiver_ref(departement_donnees$Cumul_precipitations_hiver_ref_cat), #fill = F,
                          weight = 2.5,
                          fillOpacity = 0.65,
                          label = labels_cumul_hiver_ref,
                          labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                          color = "black",
                          group = "Cumul des précipitations en hiver à l'échelle Départementale",
                          options = pathOptions(pane = "left")) %>%
              
              addLegend("bottomleft",
                        colors =c("#E6E6E6","#A9F5A9","#BFFF00","#FE9A2E","#FA8258"),
                        labels = c("180 à 220 inclus","220 à 260 inclus","260 à 300 inclus","300 à 340 inclus","> 340"), 
                        title = "Cumul des précipitations en hiver (en mm)",
                        group = "Cumul des précipitations en hiver à l'échelle Départementale", opacity = 1)
            
            w$hide()
          }
          
          else if("Nombre de jours avec risque significatif de feu de végétation" %in% input$map_groups){
            w$show()
            Sys.sleep(3)
            leafletProxy("map") %>%
              clearShapes()%>%
              clearControls()%>%
              removeShape("Région")%>%
              removeShape("Département")%>%
              addSidebyside(layerId = "sidecontrols", rightId = "baseid", leftId = "cartoid") %>%
              addControl(footer,
                         position = "bottomleft",
                         className = "map-footer")%>%
            addPolygons(data = departement_donnees,
                        fillColor = ~pal_feu(departement_donnees$feu_de_vegetation_cat), 
                        weight = 2.5,
                        fillOpacity = 0.65,
                        label = labels_feu_vegetation,
                        labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                        color = "black",
                        group = "Nombre de jours avec risque significatif de feu de végétation",
                        options = pathOptions(pane = "right")) %>%
              
            addLegend("bottomright",
                      colors =c("#F7FE2E","#FE9A2E","#DF3A01","#B40404"),
                      labels = c("4-8 inclus","8-12 inclus","12-16 inclus",">16"), 
                      title = "Nombre de jours",
                      group = "Nombre de jours avec risque significatif de feu de végétation",
                      opacity = 1) %>%
              
              addPolygons(data = departement_donnees,
                          fillColor = ~pal_feu_ref(departement_donnees$feu_de_vegetation_ref_cat), 
                          weight = 2.5,
                          fillOpacity = 0.65,
                          label = labels_feu_vegetation_ref,
                          labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                          color = "black",
                          group = "Nombre de jours avec risque significatif de feu de végétation",
                          options = pathOptions(pane = "left")) %>%
              
              addLegend("bottomleft",
                        colors =c("#F7FE2E","#FE9A2E","#DF3A01","#B40404"),
                        labels = c("0 à 0.8 jours inclus","0.8 à 1.6 jours inclus","1.6 à 2.4 inclus","> 2.4"), 
                        title = "Nombre de jours",
                        group = "Nombre de jours avec risque significatif de feu de végétation",
                        opacity = 1)
            
            w$hide()
          }
          
          else if("Nombre annuel de nuits chaudes" %in% input$map_groups){
            w$show()
            Sys.sleep(3)
            leafletProxy("map") %>%
              clearShapes()%>%
              clearControls()%>%
              removeShape("Région")%>%
              removeShape("Département")%>%
              addSidebyside(layerId = "sidecontrols", rightId = "baseid", leftId = "cartoid") %>%
              addControl(footer,
                         position = "bottomleft",
                         className = "map-footer")%>%
              addPolygons(data = departement_donnees,
                          fillColor = ~pal_nuits_chaudes(departement_donnees$nuits_chaudes_cat), #fill = F,
                          weight = 2.5,
                          fillOpacity = 0.65,
                          label = labels_nuits_chaudes,
                          labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                          color = "black",#label = departement_donnees$NOM,
                          group = "Nombre annuel de nuits chaudes",
                          options = pathOptions(pane = "right")) %>%
              addLegend("bottomright",
                        colors =c("#F6CED8","#CECEF6","#81F7BE","#A5DF00","#B40404"),
                        labels = c("18 à 21 inclus","21 à 24 inclus","24 à 27 inclus","27 à 30 inclus","> 30"), 
                        title = "Nombre annuel de nuits chaudes",
                        group = "Nombre annuel de nuits chaudes",
                        opacity = 1) %>%
              addPolygons(data = departement_donnees,
                          fillColor = ~pal_nuits_chaudes_ref(departement_donnees$nuits_chaudes_ref_cat), #fill = F,
                          weight = 2.5,
                          fillOpacity = 0.65,
                          label = labels_nuits_chaudes_ref,
                          labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                          color = "black",#label = departement_donnees$NOM,
                          group = "Nombre annuel de nuits chaudes",
                          options = pathOptions(pane = "left")) %>%
              addLegend("bottomleft",
                        colors =c("#F6CED8","#CECEF6","#81F7BE","#A5DF00","#B40404"),
                        labels = c("2 à 3 inclus","3 à 4 inclus","4 à 5 inclus","5 à 6 inclus","> 6"), 
                        title = "Nombre annuel de nuits chaudes",
                        group = "Nombre annuel de nuits chaudes",
                        opacity = 1)
            
            w$hide()
          }
    
          else if("Nombre annuel de jours très chauds" %in% input$map_groups){
            w$show()
            Sys.sleep(3)
            leafletProxy("map") %>%
              clearShapes()%>%
              clearControls()%>%
              removeShape("Région")%>%
              removeShape("Département")%>%
              addSidebyside(layerId = "sidecontrols", rightId = "baseid", leftId = "cartoid") %>%
              addControl(footer,
                         position = "bottomleft",
                         className = "map-footer")%>%
              addPolygons(data = departement_donnees,
                          fillColor = ~pal_jours_chauds(departement_donnees$jours_chauds_cat), #fill = F,
                          weight = 2.5,
                          fillOpacity = 0.65,
                          label = labels_jours_chauds,
                          labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                          color = "black",
                          group = "Nombre annuel de jours très chauds",
                          options = pathOptions(pane = "right")) %>%
              
              addLegend("bottomright",
                        colors =c("#F2F2F2","#8181F7","#AEB404","#DF7401","#DF3A01"),
                        labels = c("5 à 6 inclus","6 à 7 inclus","7 à 8 inclus","8 à 9 inclus","> 9"), 
                        title = "Nombre annuel de jours très chauds",
                        group = "Nombre annuel de jours très chauds",
                        opacity = 1) %>% 
              
              addPolygons(data = departement_donnees,
                          fillColor = ~pal_jours_chauds_ref(departement_donnees$jours_chauds_ref_cat), #fill = F,
                          weight = 2.5,
                          fillOpacity = 0.65,
                          label = labels_jours_chauds_ref,
                          labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                          color = "black",
                          group = "Nombre annuel de jours très chauds",
                          options = pathOptions(pane = "left")) %>%
              
              addLegend("bottomleft",
                        colors =c("#F2F2F2","#8181F7","#AEB404","#DF7401","#DF3A01"),
                        labels = c("0 à 0.3 inclus","0.3 à 0.6 inclus","0.6 à 0.9 inclus","0.9 à 1.2 inclus","> 1.2"), 
                        title = "Nombre annuel de jours très chauds",
                        group = "Nombre annuel de jours très chauds",
                        opacity = 1)
            
            w$hide()
          }
          
          else if("Nombre annuel de jours de gel" %in% input$map_groups){
            w$show()
            Sys.sleep(3)
            leafletProxy("map") %>%
              clearShapes()%>%
              clearControls()%>%
              removeShape("Région")%>%
              removeShape("Département")%>%
              addSidebyside(layerId = "sidecontrols", rightId = "baseid", leftId = "cartoid") %>%
              addControl(footer,
                         position = "bottomleft",
                         className = "map-footer")%>%
            addPolygons(data = departement_donnees,
                        fillColor = ~pal_jours_gel(departement_donnees$jours_gel_cat), #fill = F,
                        weight = 2.5,
                        fillOpacity = 0.65,
                        label = labels_jours_gel,
                        labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                        color = "black",
                        group = "Nombre annuel de jours de gel",
                        options = pathOptions(pane = "right")) %>%
              
              addLegend("bottomright",
                        colors =c("#E6E6E6","#8181F7","#2E64FE","#0431B4","#0B3861"),
                        labels = c("14 à 18 inclus","18 à 22 inclus","22 à 26 inclus","26 à 30 inclus","> 30"), 
                        title = "Nombre annuel de jours de gel",
                        group = "Nombre annuel de jours de gel",
                        opacity = 1)%>%
              
              addPolygons(data = departement_donnees,
                          fillColor = ~pal_jours_gel_ref(departement_donnees$jours_gel_ref_cat), #fill = F,
                          weight = 2.5,
                          fillOpacity = 0.65,
                          label = labels_jours_gel_ref,
                          labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                          color = "black",
                          group = "Nombre annuel de jours de gel",
                          options = pathOptions(pane = "left")) %>%
              
              addLegend("bottomleft",
                        colors =c("#E6E6E6","#8181F7","#2E64FE","#0431B4","#0B3861"),
                        labels = c("20 à 27 inclus","27 à 34 inclus","34 à 41 inclus","41 à 48 inclus","> 48"), 
                        title = "Nombre annuel de jours de gel",
                        group = "Nombre annuel de jours de gel",
                        opacity = 1)
            
            w$hide()
          }
          
          else if("Nombre de jours annuels estivaux" %in% input$map_groups){
            w$show()
            Sys.sleep(3)
            leafletProxy("map") %>%
              clearShapes()%>%
              clearControls()%>%
              removeShape("Région")%>%
              removeShape("Département")%>%
              addSidebyside(layerId = "sidecontrols", rightId = "baseid", leftId = "cartoid") %>%
              addControl(footer,
                         position = "bottomleft",
                         className = "map-footer")%>%
            addPolygons(data = departement_donnees,
                        fillColor = ~pal_jours_estivaux(departement_donnees$jours_estivaux_cat), #fill = F,
                        weight = 2.5,
                        fillOpacity = 0.65,
                        label = labels_jours_estivaux,
                        labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                        color = "black",#label = departement_donnees$NOM,
                        group = "Nombre de jours annuels estivaux",
                        options = pathOptions(pane = "right")) %>%
              
            addLegend("bottomright",
                      colors =c("#e5e7e9","#edbb99","#f8c471","#f4d03f","#e74c3c"),
                      labels = c("60 à 70 inclus","70 à 80 inclus","80 à 90 inclus","90 à 100 inclus","> 100"), 
                      title = "Nombre de jours annuels estivaux",
                      group = "Nombre de jours annuels estivaux",
                      opacity = 1) %>%
              
              addPolygons(data = departement_donnees,
                          fillColor = ~pal_jours_estivaux_ref(departement_donnees$jours_estivaux_ref_cat), #fill = F,
                          weight = 2.5,
                          fillOpacity = 0.65,
                          label = labels_jours_estivaux_ref,
                          labelOptions = labelOptions(style=list("border"="1px solid black","border-radius"="20px")),
                          color = "black",#label = departement_donnees$NOM,
                          group = "Nombre de jours annuels estivaux",
                          options = pathOptions(pane = "left")) %>%
              
              addLegend("bottomleft",
                        colors =c("#e5e7e9","#edbb99","#f8c471","#f4d03f","#e74c3c"),
                        labels = c("30 à 38 inclus","38 à 46 inclus","46 à 54 inclus","54 à 62 inclus","> 62"), 
                        title = "Nombre de jours annuels estivaux",
                        group = "Nombre de jours annuels estivaux",
                        opacity = 1)
            
            w$hide()
          }
          
  })
  
  
  
   
}