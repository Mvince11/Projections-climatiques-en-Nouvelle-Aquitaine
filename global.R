

 #communes <- st_read("Data/communes_epci_nouvelle_aquitaine.shp",options = "ENCODING=UTF8")%>%st_transform(st_crs(4326))%>%
#   filter(INSEE_DEP %in% c("33","47","16","17","64","86","87","24","19","23","40","79"))
 
 #communes <- readRDS("Data/Communes.rds") 
# 
# # ## Calculer la somme de la population et le nombre de communes par département
#  communes <- communes %>%
#    group_by(INSEE_DEP) %>%
#    mutate(
#      somme_population_dep = sum(POPULATION, na.rm = TRUE),  # Remplacer "POPULATION" par le nom de la colonne population
#      nombre_communes_dep = n(),  # n() compte le nombre de lignes (communes) par département
#      nombre_epci_dep = n_distinct(epci_NOM)
#    )
#  
#  communes <-communes %>% 
#    st_set_geometry(NULL)
#  communes <- communes%>%select(INSEE_DEP,somme_population_dep,nombre_communes_dep,nombre_epci_dep)

#donnees_departement_region <- st_read(as.data.frame("Data/donnees_region_departements.csv", sep=";",options = "ENCODING=UTF8"))
donnees_departement_region <- readRDS("Data/donnees_region_departements.rds")

# # Création département_33 --------------------------------------------
# departement <- st_read("Data/DEPARTEMENT.shp",options = "ENCODING=UTF8") %>% st_transform(st_crs(4326))%>%
#   filter(INSEE_DEP %in% c("33","47","16","17","64","86","87","24","19","23","40","79"))
departement <- readRDS("Data/DEPARTEMENT.rds")



#    departement_donnees <- inner_join(departement, donnees_departement_region, by = "INSEE_DEP")
#    departement_donnees$population <- c(349684,661404,239784,115702,413730,1654970,422976,331229,692723,374587,438907,37169)
#    departement_donnees$nb_communes <- c(362,463,279,256,503,535,327,319,545,256,265,195)
#    departement_donnees$nb_epci <- c(9,13,10,10,22,28,18,12,10,8,7,13)
#    departement_donnees$superficie <- c(5956,6864,5857,5565,9060,9975,9243,5361,7645,5999,6990,5520)
#    departement_donnees$chef_lieu <- c("Angoulême","La Rochelle","Tulle","Guéret","Périgueux","Bordeaux","Mont-de-Marsan","Agen","Pau","Niort","Poitiers","Limoges")
#    departement_donnees$sous_prefecture <- c("Cognac","Jonzac/Rochefort/Saintes/Saint-Jean-d'Angély","Brive-la-Gaillarde/Ussel","Aubusson",
#                                             "Bergerac/Nontron/Sarlat-la-Canéda","Arcachon/Blaye/Langon/Lesparre-Médoc/Libourne","Dax",
#                                              "Marmande/Nérac/Villeneuve-sur-Lot","Bayonne/Oloron-Sainte-Marie",
#                                             "Bressuire/Parthenay","Châtellerault/Montmorillon","Bellac/Rochechouart")
#    departement_donnees$T_ete_ref <- as.numeric(departement_donnees$T_ete_ref)
#    departement_donnees$T_ete._haute<- as.numeric(departement_donnees$T_ete._haute)
#    departement_donnees$T_.Automne_ref <- as.numeric(departement_donnees$T_.Automne_ref)
#    departement_donnees$T_.Automne_haute<- as.numeric(departement_donnees$T_.Automne_haute)
#    departement_donnees$T_hiver_ref <- as.numeric(departement_donnees$T_hiver_ref)
#    departement_donnees$T_hiver_haute<- as.numeric(departement_donnees$T_hiver_haute)
#    departement_donnees$T_printemps_ref <- as.numeric(departement_donnees$T_printemps_ref)
#    departement_donnees$T_printemps_haute<- as.numeric(departement_donnees$T_printemps_haute)
#    departement_donnees$Cumul_precipitations_ete_ref <- as.numeric(departement_donnees$Cumul_precipitations_ete_ref)
#    departement_donnees$Cumul_precipitations_ete <- as.numeric(departement_donnees$Cumul_precipitations_ete)
#    departement_donnees$Cumul_precipitations_automne_ref <- as.numeric(departement_donnees$Cumul_precipitations_automne_ref)
#    departement_donnees$Cumul_precipitations_automne <- as.numeric(departement_donnees$Cumul_precipitations_automne)
#    departement_donnees$Cumul_precipitations_hiver_ref <- as.numeric(departement_donnees$Cumul_precipitations_hiver_ref)
#    departement_donnees$Cumul_precipitations_hiver <- as.numeric(departement_donnees$Cumul_precipitations_hiver)
#    departement_donnees$Cumul_precipitations_printemps_ref <- as.numeric(departement_donnees$Cumul_precipitations_printemps_ref)
#    departement_donnees$Cumul_precipitations_printemps <- as.numeric(departement_donnees$Cumul_precipitations_printemps)
#    departement_donnees$feu_de_vegetation_ref <- as.numeric(departement_donnees$feu_de_vegetation_ref)
#    departement_donnees$feu_de_vegetation <- as.numeric(departement_donnees$feu_de_vegetation)
#    departement_donnees$nuits_chaudes_ref <- as.numeric(departement_donnees$nuits_chaudes_ref)
#    departement_donnees$nuits_chaudes <- as.numeric(departement_donnees$nuits_chaudes)
#    departement_donnees$jours_chauds_ref <- as.numeric(departement_donnees$jours_chauds_ref)
#    departement_donnees$jours_chauds <- as.numeric(departement_donnees$jours_chauds)
#    departement_donnees$jours_gel_ref <- as.numeric(departement_donnees$jours_gel_ref)
#    departement_donnees$jours_gel <- as.numeric(departement_donnees$jours_gel)
#    departement_donnees$jours_estivaux_ref <- as.numeric(departement_donnees$jours_estivaux_ref)
#    departement_donnees$jours_estivaux <- as.numeric(departement_donnees$jours_estivaux)
# # # # 
# # # # 
#    departement_donnees$images <-c("logo_Charente-quadri-2022.jpg",
#                                   "Logo_Charente_Maritime.svg.png",
#                                   "correze.png",
#                                   "Creuse_(23)_logo_2015.svg.png",
#                                   "Logo_Departement_Dordogne_juin_2015.svg.png",
#                                   "csm_CG33_nouveau_logo_1238_2373e1968b.jpg",
#                                   "Logo_Departement_Landes_2015.svg.png",
#                                   "lot-garonne.png",
#                                   "logo-vectoriel-le-departement-des-pyrenees-atlantiques.jpg",
#                                   "LOGO79_BULLE_BLEU.png",
#                                   "vienne.png",
#                                   "logo_hv_2015_cmjn.jpg")

departement_donnees <- readRDS("Data/departement_donnees.rds")

# # Ajoutez une colonne avec les centroïdes des polygones
# departement$centroid <- st_centroid(departement$geometry)
# 
# # Extraire les coordonnées des centroïdes
# departement_centroids_coords <- st_coordinates(departement$centroid)


#region <- st_read("Data/REGION.shp",options = "ENCODING=UTF8")%>%st_transform(st_crs(4326))%>%filter(INSEE_REG %in% "75")
region <- readRDS("Data/Region.rds")
## Création epci ---------------------------------------------
#epci <- st_read("Data/EPCI_75.shp", options = "ENCODING=UTF8") %>% st_transform(st_crs(4326))
epci <- readRDS("Data/EPCI.rds")

titre_modal <- div("Bienvenue",
                   "Bienvenue dans l'application.",br(),
                   "Veuillez tout d'abord choisir un département ou un epci.
        Il vous suffira ensuite de sélectionner une couche
        afin de comparer et de visulaiser les variations de plusieurs paramètres climatiques à partir
        des valeurs de références et les valeurs prévues
        à l'horizon 2050.")



footer <- tags$div("Sources: ©IGN (Admin Express), ©Météo France (Climadiag) | Mise à jour: juin 2024", style="background-color: white; font-size: 14px; padding-left: 10px; padding-right: 10px")


