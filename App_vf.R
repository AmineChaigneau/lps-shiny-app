library(tidyverse)
library(sp)
library(sf)
library(shiny)
library(dplyr)
library(shinydashboard)
library(DT)

# Adapter le code dans la partie server aux lignes 365, 378 et 393 et 422
# (source du script futsal_court, angle de rotation et nom des colonnes, là dcp que la source du script)
# Modifier la fonction plot_frame_team du script futsal_court : 

# plot_frame_team <-function(data, frameplot, number = number, team = team){ # pour visualiser rapidement les données (couleur = équipe, numéro = numéro de chasuble)
# dataplot = subset(data, frame == frameplot)
# fig = futsal_court()+
  # geom_label(data = dataplot, mapping = aes(x = X, y = Y, label = {{number}}, color = {{team}}),size = 4)+
  # labs(color = "Team",
       # title = paste0("Frame : ", as.character(frameplot)), subtitle = "")+
  # theme(plot.title = element_text(hjust = .5, face = "bold", size = 14), legend.position = "bottom",
        # legend.text = element_text(size = 10))
# return(fig)
# }



options(shiny.maxRequestSize = 30*1024^2)

# Définir l'application Shiny Dashboard
ui <- dashboardPage(
  dashboardHeader(title = "GRAPH.IT"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(" ACCUEIL", tabName = "accueil"),
      menuItem(" CHARGEMENT DES DONNÉES", tabName = "data_loading"),
      menuItem(" GRAPHIQUES", tabName = "graphs"),
      menuItem(" TABLEAU", tabName = "tab")
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(
        rel = "stylesheet",
        media = "screen",
        href = "https://fontlibrary.org//face/glacial-indifference",
        type = "text/css"
      ),
      tags$link(
        href = "https://fonts.googleapis.com/css2?family=Open+Sans:wght@500&family=Space+Grotesk:wght@500&display=swap",
        rel = "stylesheet"
      ),
      tags$style(HTML("
        .content-wrapper {
          background-color: white;
        }
        .accueil-content {
          margin-top: 20px; /* Marge supérieure pour séparer de l'en-tête */
          margin-bottom: 100px;
        }
        .presentation-text {
          text-align: center;
          margin-left: 20px;
          margin-right: 20px;
          font-family: 'Space Grotesk', sans-serif;
          font-size: 25px;
        }
        .developer-presentation {
          width: 100%;
          display: inline-block;
          margin-top: 20px;
          margin-bottom:5px;
        }
        .portrait-box-gauche {
          width: 30%;
          float: left;
          margin-left: 20px;
        }
        .portrait-box-droite {
          width: 30%;
          float: right;
          margin-right: 20px;
        }
        .text-box-gauche {
          width: 60%; /* Largeur pour le texte à gauche */
          float: right; /* Aligner à droite si l'image est à gauche */
          margin-right: 20px; /* Marge pour espacer le texte de l'image */
          font-family: 'Space Grotesk', sans-serif; 
          font-size: 20px;
        }
        .text-box-droite {
          width: 60%; /* Largeur pour le texte à droite */
          float: left; /* Aligner à gauche si l'image est à droite */
          margin-left: 20px; /* Marge pour espacer le texte de l'image */
          font-family: 'Space Grotesk', sans-serif; 
          font-size: 20px;
        }
        .download-section {
          width: 100%;
          background-color: transparent;
          margin-bottom: 20px;
          padding: 20px;
        }
        .download-button {
          width: 30%;
          height: 100%;
          float: left;
          background-color: transparent;
        }
        .download-explication {
          width: 65%;
          height: 100%;
          float: right;
          margin-left: 20px;
          background-color: transparent;
          font-family: 'Space Grotesk', sans-serif; 
          font-size: 20px;
        }
      "))
    ),
    tags$script(HTML("$('body').addClass('fixed');")),
    tabItems(
      tabItem(
        tabName = "accueil",
        div(
          class = "accueil-content",
          div(
            img(src = "nom_app.png", width = "100%")
          ),
          div(
            class = "presentation-text",
            p(
              "CETTE APPLICATION A ÉTÉ DÉVELOPPÉE PAR LA PROMO 2022-2024 DU MASTER STAPS EOPS,",
              "PARCOURS ANALYSE DE JEU ET BIG DATA. ELLE VOUS PERMET D'EXPLORER ET D'ANALYSER",
              "DES DONNÉES GPS POUR DIVERSES APPLICATIONS, NOTAMMENT DANS LE DOMAINE DU SPORT.",
              br(), br(),
              "NOTRE ÉQUIPE D'ÉTUDIANTS PASSIONNÉS DE DONNÉES SPORTIVES A TRAVAILLÉ DUR POUR",
              "METTRE EN PLACE CETTE APPLICATION CONVIVIALE, OFFRANT DES FONCTIONNALITÉS",
              "DE TRAITEMENT ET DE VISUALISATION AVANCÉES. NOUS ESPÉRONS QU'ELLE SERA",
              "UTILE POUR VOS BESOINS EN MATIÈRE DE DONNÉES GPS.",
              br(), br(),
              "N'HÉSITEZ PAS À EXPLORER LES DIFFÉRENTES FONCTIONNALITÉS DE L'APPLICATION,",
              "ET NOUS SOMMES OUVERTS AUX COMMENTAIRES ET AUX SUGGESTIONS POUR L'AMÉLIORER.",
              "PROFITEZ DE L'EXPÉRIENCE !"
            ),
            style = "font-family: 'Space Grotesk', sans-serif;"
          )
        ),
        div(
          class = "accueil-second-part",
          div(
            class = "developer-presentation",
            div(
              class = "portrait-box-gauche",
              img(src = "portrait_luca.png", width = "100%")
            ),
            div(
              class = "text-box-gauche",
              h2("LUCA", style = "font-size: 40px;
                 font-family: 'GlacialIndifferenceBold';"),
              p("Director of Development Strategies",
                br(), br(),
                "Luca est un développeur passionné doté d'un talent pour créer des solutions logicielles innovantes.",
                "Il est responsable du développement de notre application et de la mise en place de fonctionnalités clés",
                "pour garantir une expérience utilisateur exceptionnelle."
              )
            )
          ),
          div(
            class = "developer-presentation",
            div(
              class = "portrait-box-droite",
              img(src = "portrait_lucas.png", width = "100%")
            ),
            div(
              class = "text-box-droite",
              h2("LUCAS", style = "font-size: 40px;
                 font-family: 'GlacialIndifferenceBold';"),
              p("Director of Creative Design",
                br(), br(),
                "Lucas est un designer talentueux avec une vision créative. Il joue un rôle essentiel",
                "dans la conception de l'interface utilisateur de notre application, veillant à ce qu'elle soit à la fois",
                "esthétiquement plaisante et conviviale pour nos utilisateurs."
              )
            )
          ),
          div(
            class = "developer-presentation",
            div(
              class = "portrait-box-gauche",
              img(src = "portrait_hugo.png", width = "100%")
            ),
            div(
              class = "text-box-gauche",
              h2("HUGO", style = "font-size: 40px;
                 font-family: 'GlacialIndifferenceBold';"),
              p("Senior Development Architect",
                br(), br(),
                "Hugo est un développeur dévoué passionné par la création de solutions logicielles innovantes.",
                "Il est chargé de superviser le développement de notre application et de mettre en œuvre des caractéristiques architecturales clés",
                "pour garantir une expérience utilisateur exceptionnelle."
              )
            )
          ),
          div(
            class = "developer-presentation",
            div(
              class = "portrait-box-droite",
              img(src = "portrait_kylian.png", width = "100%")
            ),
            div(
              class = "text-box-droite",
              h2("KYLIAN", style = "font-size: 40px;
                 font-family: 'GlacialIndifferenceBold';"),
              p("Head of Research and Innovation",
                br(), br(),
                "Kylian est un chercheur visionnaire à la tête de nos initiatives d'innovation.",
                "Il joue un rôle vital dans l'exploration de nouvelles possibilités et s'assure",
                "que notre application reste à la pointe de la technologie et de la recherche."
              )
            )
          ),
          div(
            class = "developer-presentation",
            div(
              class = "portrait-box-gauche",
              img(src = "portrait_samuel.png", width = "100%")
            ),
            div(
              class = "text-box-gauche",
              h2("SAMUEL", style = "font-size: 40px;
                 font-family: 'GlacialIndifferenceBold';"),
              p("Principal User Experience Designer",
                br(), br(),
                "Samuel est un virtuose de la conception, capable de créer des expériences utilisateur qui émerveillent.",
                "Son talent artistique et son sens aigu de l'ergonomie se rejoignent pour donner vie à une interface",
                "utilisateur inégalée. Il façonne notre application avec une précision d'orfèvre, créant une harmonie",
                "entre l'esthétique et la fonctionnalité pour offrir une expérience utilisateur véritablement exquise."
              )
            ),
            div(
              class = "developer-presentation",
              div(
                class = "portrait-box-droite",
                img(src = "portrait_rafael.png", width = "100%")
              ),
              div(
                class = "text-box-droite",
                h2("RAFAEL", style = "font-size: 40px;
                 font-family: 'GlacialIndifferenceBold';"),
                p("Stagiaire",
                  br(), br(),
                  "Rafael est un stagiaire enthousiaste avec une passion pour",
                  "l'apprentissage. Il est désireux d'acquérir de l'expérience",
                  "et de contribuer au développement de notre application sous",
                  "la direction de notre équipe expérimentée."
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "data_loading",
        div(
          class = "download-section",
          div(
            class = "download-button",
            fileInput("file1", "Étape 1",
                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"),
                      multiple = TRUE
            )
          ),
          div(
            class = "download-explication",
            h2("Étape 1", style = "font-size: 40px;
                 font-family: 'GlacialIndifferenceBold';"),
            p(
              "Dans un premier temps importez les fichiers des données LPS de chaque joueur.",
              "Ils doivent être en format csv et doivent contenir les colonnes longitude et latitude."
            )
          )
        ),
        div(
          class = "download-section",
          div(
            class = "download-button",
            fileInput("file2", "Étape 2",
                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
            )
          ),
          div(
            class = "download-explication",
            h2("Étape 2", style = "font-size: 40px;
                 font-family: 'GlacialIndifferenceBold';"),  # Modification de l'étape ici
            p(
              "Dans un second temps, sélectionnez le fichier contenant les  coordonnées",
              "de références du terrain.",
              "Il doit être en format csv."
            )
          )
        ),
        div(
          class = "download-section",
          div(
            class = "download-button",
            fileInput("file3", "Étape 3",
                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
            )
          ),
          div(
            class = "download-explication",
            h2("Étape 3", style = "font-size: 40px;
                 font-family: 'GlacialIndifferenceBold';"),  # Modification de l'étape ici
            p(
              "Enfin, sélectionnez le fichier contenant les informations joueurs,",
              "indiquant quel balise est utilisé par quel joueur, ainsi que d'autres",
              "informations relatives aux joueurs.",
              "Il doit être en format csv."
            )
          )
        )
      ),
      tabItem("graphs",
              plotOutput("plot"),
              br(),
              br(),
              sliderInput("frameplot", "Frame à afficher", min = 0, max = 1, value = 1, width = "100%", step = 1),
              actionButton("playButton", "Play"),
              actionButton("resetButton", "Reset")),
      
      tabItem("tab",
              DTOutput("tab"),
              br(),
              downloadButton("downloadCSV", "Exporter en CSV")
              
      )
    )
  )
)


# Définir le serveur
server <- function(input, output, session) {
  current_index <- reactiveVal(0)
  data <- reactive({
    req(input$file1, input$file2, input$file3)
    names <- c(input$file1$name)  
    files <- c(input$file1$datapath)  
    data <- data.frame()  
    for (i in 1:length(files)) {
      print(files[i])
      print(names[i])
      if (grepl(".*_fusion.csv$", names[i])) {
        filepath <- input$file1$name[i]  
        filedata <- read.csv(files[i])  
        filedata <- filedata %>% 
          mutate(Balise = substr(filepath, 1, nchar(filepath) - 11))
        data <- rbind(data, filedata)  
      } else {
        showNotification("Veuillez charger des fichiers se terminant par _fusion.csv", type = "error")
        return(NULL)
      }
    }
    ref_terrain <- read.csv2(input$file2$datapath)
    info_joueurs <- read.csv2(input$file3$datapath)
    source("/Users/samuel/Desktop/R/Traitement du signal/script/futsal_court.R")
    data <- data[, -c(3,4)]
    data <- na.omit(data)
    
    data = data %>% select(Balise, latitude_fusion, longitude_fusion, timestamp)
    
    data$timestamp <- data$timestamp/100
    data$timestamp <- (data$timestamp-min(data$timestamp))
    
    info_joueurs <- info_joueurs[,c(1:3)]
    data <- data %>%
      left_join(info_joueurs)
    
    df_sf <- st_as_sf(data, coords = c("longitude_fusion", "latitude_fusion"), crs = 4326) # Étape 1 : créer un objet sf avec les coordonnées géographiques (longitude et latitude)
    
    df_utm <- st_transform(df_sf, crs = 32631) # Étape 2 : convertir en UTM (UTM Zone 31)
    
    data <- data %>% # Étape 3 : ajouter les coordonnées cartésiennes au df principal
      mutate(X = st_coordinates(df_utm)[, "X"],
             Y = st_coordinates(df_utm)[, "Y"]) 
    
    data$xmean <- ref_terrain$xmean
    data$ymean <- ref_terrain$ymean
    
    data <- data %>%
      mutate(X = (X-xmean),
             Y = (Y-ymean))
    
    angle = pi
    
    data <- data %>% # Appliquer la rotation aux données
      mutate(X_rot = X*cos(angle)-Y*sin(angle),
             Y_rot = X*sin(angle)+Y*cos(angle))
    
    data <- data %>%
      mutate(X = X_rot,
             Y = Y_rot)
    
    columns_to_exclude <- c("xmean", "ymean", "X_rot", "Y_rot")
    data <- data[, !names(data) %in% columns_to_exclude]
    
    data$frame <- data$timestamp
    data <- na.omit(data)
    return(data)
    
  })
  
  observe({
    req(data())
    min_val <- min(data()$frame)
    max_val <- max(data()$frame)
    updateSliderInput(session, "frameplot", min = min_val, max = max_val, value = min_val)
  })
  
  output$plot <- renderPlot({
    req(data())
    futsal_court()
    plot_frame_team(data(), frameplot = input$frameplot, number = Numero, team = Equipe)
  })
  
  output$tab <- renderDT({
    datatable(data())
  })
  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("data_export", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  is_playing <- reactiveVal(FALSE)
  
  # Créer un reactiveTimer
  autoInvalidate <- reactiveTimer(100)
  
  # Observer réactif pour le bouton Play
  observeEvent(input$playButton, {
    is_playing(!is_playing())
  })
  
  observeEvent(input$resetButton, {
    updateSliderInput(session, "frameplot", value = 0)
  })
  
  observe({
    req(data())
    if (is_playing()) {
      autoInvalidate()
      current_value <- input$frameplot
      if (current_value < max(data()$frame)) {
        updateSliderInput(session, "frameplot", value = current_value + 1)
      } else {
        updateSliderInput(session, "frameplot", value = 1)
      }
    }
  })
}

# Exécutez l'application Shiny
shinyApp(ui = ui, server = server)
