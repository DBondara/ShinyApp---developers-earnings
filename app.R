
library(plyr)
library(dplyr)
library(shiny)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(shinyWidgets)
library(data.table)
library(DT)
library(formattable)
library(tidyr)
library(tidyverse)
library(plotly)

options(scipen = 999)

# ________________________________________________________________________________________________________

# wczytanie danych
load('data.RData')

ui <- 
  # pasek nawigacyjny
  navbarPage(title = "Developers", 
             
             theme = shinythemes::shinytheme("flatly"),
             
             # pierwsza zakładka Intro
             tabPanel(title = "Intro",
                      
                      # zmiana koloru paska
                      tags$style(type="text/css", '.navbar { background-color: #4BACC6}'),
                      # zmiana koloru suwaków 
                      chooseSliderSkin("Flat", color = "#31869B"),
                        
                        # panel główny
                        absolutePanel(
                          helpText(h1("Explore Developers Earnings!")),
                          helpText(h5("Salary converted to annual USD salaries using the exchange rate on 2018-01-18, assuming 12 working months and 50 working weeks.")),
                          tags$hr(),
                          helpText(h4("Developer Profile ")),
                          tags$i("get information about developers’ roles, experience, education or demographics in selected salary range"),
                          helpText(h4("Filters")),
                          tags$i("filter data to explore developers’ earnings in specific group e.g. display histogram of male data scientist from Poland with Master’s Degree. Additionally, create table to explore each record"),
                          helpText(h4("Statistics ")),
                          tags$i("display earnings statistics for developers selected in “Filters” tab e.g. display earnings statistics for male data scientist from Poland with Master’s Degree. You may also display earnings statistics in selected groups e.g. display statistics grouped by company size"),
                          tags$hr(),
                          helpText(h6("Data source: StackOverflow Developer Survey 2018"))
                        ) # koniec panelu głównego
             ), # koniec tabPanel "Intro"
             
             # druga zakładka Developer Profile
             
             tabPanel(title = "Developer Profile",
                      sidebarLayout(
                        # panel boczny
                        sidebarPanel(

                          # 1. Suwak - przedział płac
                          sliderInput("przedzial_plac", label = h3("Salary range (annual)"), min = 0,
                                      max = 1000000, value = c(0, 1000000), step = 1000,  pre = "$"),

                          # 2. Zmienne związane ze specjalizacją programistów
                          helpText(h4("Developer role")),

                          checkboxGroupButtons(inputId = "ProgramistaTyp",
                                      label = "Select variables:",
                                      choices = c("Developer type" = "DevType",
                                                  "Language" = "LanguageWorkedWith",
                                                  "Platform" = "PlatformWorkedWith"),
                                      checkIcon = list(yes = icon("chart-bar"),
                                                       no = icon("remove", lib = "glyphicon")),
                                      direction = "vertical",
                                      justified = FALSE,
                                      individual = TRUE,
                                      status = "primary"
                                      ),

                          # 3. Zmienne związane z doświadczeniem
                          helpText(h4("Experience")),

                          checkboxGroupButtons(inputId = "Doswiadczenie",
                                      label = "Select variables:",
                                      choices = c("Experience in coding" = "YearsCoding",
                                                  "Professional experience" = "YearsCodingProf"),
                                      checkIcon = list(yes = icon("chart-bar"),
                                                       no = icon("remove", lib = "glyphicon")),
                                      direction = "vertical",
                                      justified = FALSE,
                                      individual = TRUE,
                                      status = "primary"
                                      ),

                          # 4. Zmienne związane z edukacją
                          helpText(h4("Education")),

                          checkboxGroupButtons(inputId = "Edukacja",
                                      label = "Select variables:",
                                      choices = c("Formal education" = "FormalEducation",
                                                  "Other education types" = "EducationTypes",
                                                  "Undergraduate major" = "UndergradMajor"),
                                      checkIcon = list(yes = icon("chart-bar"),
                                                       no = icon("remove", lib = "glyphicon")),
                                      direction = "vertical",
                                      justified = FALSE,
                                      individual = TRUE,
                                      status = "primary"
                                      ),

                          # 5. Zmienne związane z deomgrafią
                          helpText(h4("Demographics")),

                          checkboxGroupButtons(inputId = "Demografia",
                                      label = "Select variables:",
                                      choices = c("Gender",
                                                  "Parents education" = "EducationParents",
                                                  "Age"),
                                      checkIcon = list(yes = icon("chart-bar"),
                                                       no = icon("remove", lib = "glyphicon")),
                                      direction = "vertical",
                                      justified = FALSE,
                                      individual = TRUE,
                                      status = "primary"
                                      ),

                          # actionButton - do uruchamiania akcji wczytania danych

                          helpText(h4("Press the GENERATE button to create graphs")),
                          actionButton(inputId = "przycisk_generuj",
                                       label = "GENERATE")


                        ), # koniec sidebarPanel

                        # panel główny
                        mainPanel(
                                     # Rodzaj programisty
                                     conditionalPanel(
                                       condition = "input.ProgramistaTyp.indexOf('DevType') > -1 ",
                                       plotOutput("wykres_DevType")
                                     ),
                                     conditionalPanel(
                                       condition = "input.ProgramistaTyp.indexOf('LanguageWorkedWith') > -1 ",
                                       plotOutput("wykres_LanguageWorkedWith")
                                     ),
                                     conditionalPanel(
                                       condition = "input.ProgramistaTyp.indexOf('PlatformWorkedWith') > -1 ",
                                       plotOutput("wykres_PlatformWorkedWith")
                                     ),
                                     # Doświadczenie
                                     conditionalPanel(
                                       condition = "input.Doswiadczenie.indexOf('YearsCoding') > -1 ",
                                       plotOutput("wykres_YearsCoding")
                                     ),
                                     conditionalPanel(
                                       condition = "input.Doswiadczenie.indexOf('YearsCodingProf') > -1 ",
                                       plotOutput("wykres_YearsCodingProf")
                                     ),
                                     # Edukacja
                                     conditionalPanel(
                                       condition = "input.Edukacja.indexOf('FormalEducation') > -1 ",
                                       plotOutput("wykres_FormalEducation")
                                     ),
                                     conditionalPanel(
                                       condition = "input.Edukacja.indexOf('EducationTypes') > -1 ",
                                       plotOutput("wykres_EducationTypes")
                                       ),
                                     conditionalPanel(
                                       condition = "input.Edukacja.indexOf('UndergradMajor') > -1 ",
                                       plotOutput("wykres_UndergradMajor")
                                       ),
                                     # Demografia
                                     conditionalPanel(
                                       condition = "input.Demografia.indexOf('Gender') > -1 ",
                                       plotOutput("wykres_Gender")
                                     ),
                                     conditionalPanel(
                                       condition = "input.Demografia.indexOf('EducationParents') > -1 ",
                                       plotOutput("wykres_EducationParents")
                                     ),
                                     conditionalPanel(
                                       condition = "input.Demografia.indexOf('Age') > -1 ",
                                       plotOutput("wykres_Age")
                                     )

                                )# koniec mainpanel
                        )
                      ), # koniec tabpanel
             
             # trzecia zakładka Filters
             
             tabPanel(title = "Filters",
                      sidebarLayout(
                        
                        sidebarPanel(
                          
                          # 1. Suwak - przedział płac
                          sliderInput("przedzial_plac_f", label = h3("Salary range (annual)"), min = 0, 
                                      max = 1000000, value = c(0, 1000000), step = 1000,  pre = "$"),
                          
                          
                          helpText(h3("Select fields you want to filter by")),
                          
                          # 2. Zmienne związane ze specjalizacją programistów
                          prettySwitch(
                            inputId = "Specjalizacja_f", label = "Developer role", slim = TRUE, status = 'info'
                          ),
                          
                          conditionalPanel(
                            condition = "input.Specjalizacja_f == 1 ",
                            selectInput(inputId = "Spec_zmienna_f", 
                                        label = "Display records when:",  
                                        choices = c("Back-end developer",
                                                    "Full-stack developer",
                                                    "Front-end developer",
                                                    "Mobile developer",
                                                    "Student",
                                                    "Database administrator",
                                                    "Data scientist or machine learning specialist"),
                                        selected = "Data scientist or machine learning specialist",
                                        multiple = FALSE)
                          ),
                          
                          # 3. Zmienne związane z edukacją
                        prettySwitch(
                            inputId = "Edukacja_f", label = "Education", slim = TRUE, status = 'info'
                          ),
                          
                          conditionalPanel(
                            condition = "input.Edukacja_f == 1 ",
                            selectInput(inputId = "Edukacja_zmienna_f", 
                                        label = "Display records when:",  
                                        choices = c("Bachelor’s degree",
                                                    "Master’s degree",
                                                    "Doctoral degree",
                                                    "Taught yourself",
                                                    "Online course",
                                                    "On-the-job training"),
                                        selected = "Bachelor’s degree",
                                        multiple = FALSE)
                          ),
                          
                          # 4. Zmienne związane z deomgrafią
                        prettySwitch(
                            inputId = "Demografia_f", label = "Demographics", slim = TRUE, status = 'info'
                          ),
                          
                          conditionalPanel(
                            condition = "input.Demografia_f == 1 ",
                            selectInput(inputId = "Demografia_zmienna_f", 
                                        label = "Display records when:",  
                                        choices = c("Male",
                                                    "Female"),
                                        selected = "Male",
                                        multiple = FALSE)
                          ),
                    
                          # 5. Zmienne związane z geografią
                        prettySwitch(
                            inputId = "Geografia_f", label = "Geography", slim = TRUE, status = 'info'
                          ),
                          
                          conditionalPanel(
                            condition = "input.Geografia_f == 1 ",
                            selectInput(inputId = "Geografia_zmienna_f", 
                                        label = "Display records when:",  
                                        choices = c("United States",
                                                    "United Kingdom",
                                                    "Australia",
                                                    "India",
                                                    "Poland",
                                                    "Russian Federation"),
                                        selected = "Poland",
                                        multiple = FALSE)
                          ),
                          
                          # Czy tabela?
                        prettyCheckbox(
                            inputId = "tabela_f", label = "Create table",
                             outline = FALSE, status = "info", icon = icon("table"), animation = "jelly"
                          )

                        ), # koniec sidebarPanel
                        
                        # panel główny
                        mainPanel(
                          
                          plotlyOutput("wykres_filtr"),
                            
                          conditionalPanel(
                            condition = "input.tabela_f == 1 ",
                            DT::dataTableOutput('TabelaDanych_f')
                          )
                          
                        ) # koniec mainPanel
                      ) # koniec sidebarLayout
             ), # koniec tabPanel "Filters"
             
             # Zakładka 4 Statistics
             
             tabPanel(title = "Statistics",
                      sidebarLayout(
                        # panel boczny
                        sidebarPanel(
                          helpText(h4('Earnings statistics for developers selected in the "Filters" tab')),
                          
                          switchInput(
                            inputId = "statystyki_f",
                            value = FALSE,
                            onStatus = "info"
                          ),

                          helpText(h4("Earnings statistics in groups")),
                          
                          switchInput(
                            inputId = "statystyki_g",
                            value = FALSE,
                            onStatus = "info"
                          ),
                          
                          conditionalPanel(
                            condition = "input.statystyki_g == 1 ",
                            selectInput(inputId = "Zmienna_g", 
                                        label = "Display earnings distribution for:",  
                                        choices = c("Gender",
                                                    "Developer type" = "DevType",
                                                    "Years coded professionally" = "YearsCodingProf",
                                                    "Formal Education" = "FormalEducation",
                                                    "Language" = "LanguageWorkedWith",
                                                    "Database" = "DatabaseWorkedWith",
                                                    "Company size" = "CompanySize"),
                                        selected = "Gender",
                                        multiple = FALSE),
                            selectInput(inputId = "Sortowanie_g", 
                                        label = "Sort by:",  
                                        choices = c("mean",
                                                    "median",
                                                    "Q1",
                                                    "Q2",
                                                    "count"),
                                        selected = "mean",
                                        multiple = FALSE)
                          ),
                          
                          conditionalPanel(
                            condition = "input.statystyki_g == 1 ",
                            prettyCheckbox(
                              inputId = "Boxplot", label = "Create box plot",
                              outline = TRUE, status = "info", icon = icon("chart-bar"), animation = "jelly"
                            )
                          )
                        ), # koniec sidebarPanel
                        # panel główny
                        mainPanel(
                          
                          conditionalPanel(
                            condition = "input.statystyki_f == 1 ",
                            helpText(h4('Earnings statistics for developers selected in the "Filters" tab')),
                            DT::dataTableOutput('TabelaDanych_stat_f')
                          ),
                          
                          conditionalPanel(
                            condition = "input.statystyki_g == 1 ",
                            helpText(h4("Earnings statistics in groups")),
                            formattableOutput('TabelaDanych_stat_g')
                          ),
                          
                          conditionalPanel(
                            condition = "input.Boxplot == 1 & input.statystyki_g == 1",
                            plotlyOutput('Boxplot_stat_g')
                          )
                          
                        ) # koniec mainPanel
                      ) # koniec sidebarLayout
             ) # koniec tabPanel Statistics
             
  ) # koniec navbarPage



server <- function(input, output) {

  # ZAKŁADKA 1 DEVELOPER PROFILE
  
    # Założenie filtra
    filtrowaneDane <- eventReactive(
    eventExpr = input$przycisk_generuj,
    valueExpr = {
      # Zakładanie filtra na dane - płace
      dane <- filter(data,
                     between(ConvertedSalary, input$przedzial_plac[[1]], input$przedzial_plac[[2]]))

      # eventReactive zwraca dane
      return(dane)
      
    })
    # Specjalizacja programistów
    wykresik_DevType <- eventReactive(
      eventExpr = input$przycisk_generuj, 
      valueExpr = { 
        
        wykres <- filtrowaneDane() %>%
          # rozdzielenie połączonych poziomów w kolumnach (odpowiedzi były wielowyborowe)
          separate_(.,
                    col = "DevType",
                    into = c("K1","K2","K3","K4","K5","K6","K7","K8","K9","K10","K11","K12","K13","K14","K15","K16","K17","K18","K19","K20"),
                    sep = ";") %>%
          # rozciągnięcie bazy w pionie
          gather(.,
                 key = long_variable, # zawiera nr kolumny
                 value = "kategoria", # zawiera kategorię
                 K1:K20, # weź wartości z utworzonych kolumn
                 na.rm = TRUE) %>% # pomiń braki danych
          # wykres
          ggplot(data = .,
                 aes_string(x = "kategoria")) +
          geom_bar(fill = "#005073",
                   show.legend = FALSE) +
          coord_flip() +
          labs(x = "Developer type",
               y = "Count",
               title = "Developer type") +
          theme_minimal(base_size = 15)
        
        # co ma zwrócić eventReactive?
        return(wykres)
        
      }) # koniec eventReactive  
    
    wykresik_LanguageWorkedWith <- eventReactive(
      eventExpr = input$przycisk_generuj, 
      valueExpr = { 
        
        wykres <- filtrowaneDane() %>%
          # rozdzielenie połączonych poziomów w kolumnach (odpowiedzi były wielowyborowe)
          separate_(.,
                    col = "LanguageWorkedWith",
                    into = c("K1","K2","K3","K4","K5","K6","K7","K8","K9","K10","K11","K12","K13","K14","K15","K16","K17","K18","K19","K20"),
                    sep = ";") %>%
          # rozciągnięcie bazy w pionie
          gather(.,
                 key = long_variable, # zawiera nr kolumny
                 value = "kategoria", # zawiera kategorię
                 K1:K20, # weź wartości z utworzonych kolumn
                 na.rm = TRUE) %>% # pomiń braki danych
          # wykres
          ggplot(data = .,
                 aes_string(x = "kategoria")) +
          geom_bar(fill = "#005073",
                   show.legend = FALSE) +
          coord_flip() +
          labs(x = "Language",
               y = "Count",
               title = "Language") +
          theme_minimal(base_size = 15)
        
        # co ma zwrócić eventReactive?
        return(wykres)
        
      }) # koniec eventReactive  
    
    wykresik_PlatformWorkedWith <- eventReactive(
      eventExpr = input$przycisk_generuj, 
      valueExpr = { 
        
        wykres <- filtrowaneDane() %>%
          # rozdzielenie połączonych poziomów w kolumnach (odpowiedzi były wielowyborowe)
          separate_(.,
                    col = "PlatformWorkedWith",
                    into = c("K1","K2","K3","K4","K5","K6","K7","K8","K9","K10","K11","K12","K13","K14","K15","K16","K17","K18","K19","K20"),
                    sep = ";") %>%
          # rozciągnięcie bazy w pionie
          gather(.,
                 key = long_variable, # zawiera nr kolumny
                 value = "kategoria", # zawiera kategorię
                 K1:K20, # weź wartości z utworzonych kolumn
                 na.rm = TRUE) %>% # pomiń braki danych
          # wykres
          ggplot(data = .,
                 aes_string(x = "kategoria")) +
          geom_bar(fill = "#005073",
                   show.legend = FALSE) +
          coord_flip() +
          labs(x = "Platform",
               y = "Count",
               title = "Platform") +
          theme_minimal(base_size = 15)
        
        # co ma zwrócić eventReactive?
        return(wykres)
        
      }) # koniec eventReactive  
  
  # Doświadczenie  
    
    wykresik_YearsCoding <- eventReactive(
      eventExpr = input$przycisk_generuj, 
      valueExpr = { 
        
        wykres <- ggplot(data = filtrowaneDane()[!is.na(filtrowaneDane()$YearsCoding),],
                         aes(x = YearsCoding))
        wykres <- wykres +
          geom_bar(fill = '#107dac',
                   show.legend = FALSE) +
          coord_flip() +
          labs(x = "Experience in coding",
               y = "Count",
               title = "Experience in coding (including any education)") +
          theme_minimal(base_size = 15)
        
        # co ma zwrócić eventReactive?
        return(wykres)
        
      }) # koniec eventReactive  
    
    wykresik_YearsCodingProf <- eventReactive(
      eventExpr = input$przycisk_generuj, 
      valueExpr = { 
        
        wykres <- ggplot(data = filtrowaneDane()[!is.na(filtrowaneDane()$YearsCodingProf),],
                         aes(x = YearsCodingProf))
        wykres <- wykres +
          geom_bar(fill = '#107dac',
                   show.legend = FALSE) +
          coord_flip() +
          labs(x = "Professional experience",
               y = "Count",
               title = "Professional experience (as a part of work)") +
          theme_minimal(base_size = 15)
        
        # co ma zwrócić eventReactive?
        return(wykres)
        
      }) # koniec eventReactive  
      
  # Edukacja   
  wykresik_EducationTypes <- eventReactive(
    eventExpr = input$przycisk_generuj, 
    valueExpr = { 

      wykres <- filtrowaneDane() %>%
        # rozdzielenie połączonych poziomów w kolumnach (odpowiedzi były wielowyborowe)
        separate_(.,
                  col = "EducationTypes",
                  into = c("K1","K2","K3","K4","K5","K6","K7","K8","K9","K10","K11","K12","K13","K14","K15","K16","K17","K18","K19","K20"),
                  sep = ";") %>%
        # rozciągnięcie bazy w pionie
        gather(.,
               key = long_variable, # zawiera nr kolumny
               value = "kategoria", # zawiera kategorię
               K1:K20, # weź wartości z utworzonych kolumn
               na.rm = TRUE) %>% # pomiń braki danych
        # wykres
        ggplot(data = .,
               aes_string(x = "kategoria")) +
        geom_bar(fill = "#189ad3",
                  show.legend = FALSE) +
        coord_flip() +
        labs(x = "Other education types",
             y = "Count",
             title = "Other education types") +
        theme_minimal(base_size = 15)

      # co ma zwrócić eventReactive?
      return(wykres)

    }) # koniec eventReactive
  
  wykresik_FormalEducation <- eventReactive(
    eventExpr = input$przycisk_generuj, 
    valueExpr = { 
      
      wykres <- ggplot(data = filtrowaneDane()[!is.na(filtrowaneDane()$FormalEducation),],
                       aes(x = FormalEducation))
        wykres <- wykres +
          geom_bar(fill = '#189ad3',
                    show.legend = FALSE) +
          coord_flip() +
          labs(x = "Formal education",
               y = "Count",
               title = "Formal education") +
          theme_minimal(base_size = 15)
      
      # co ma zwrócić eventReactive?
      return(wykres)
      
    }) # koniec eventReactive

  wykresik_UndergradMajor <- eventReactive(
    eventExpr = input$przycisk_generuj, 
    valueExpr = { 
      
      wykres <- ggplot(data = filtrowaneDane()[!is.na(filtrowaneDane()$UndergradMajor),],
                       aes(x = UndergradMajor))
      wykres <- wykres +
        geom_bar(fill = '#189ad3',
                 show.legend = FALSE) +
        coord_flip() +
        labs(x = "Undergraduate major",
             y = "Count",
             title = "Undergraduate major") +
        theme_minimal(base_size = 15)
      
      # co ma zwrócić eventReactive?
      return(wykres)    
      
    }) # koniec eventReactive
  
  # Demografia
  
  wykresik_Gender <- eventReactive(
    eventExpr = input$przycisk_generuj, 
    valueExpr = { 
      
      wykres <- filtrowaneDane() %>%
        # rozdzielenie połączonych poziomów w kolumnach (odpowiedzi były wielowyborowe)
        separate_(.,
                  col = "Gender",
                  into = c("K1","K2","K3","K4"),
                  sep = ";") %>%
        # rozciągnięcie bazy w pionie
        gather(.,
               key = long_variable, # zawiera nr kolumny
               value = "kategoria", # zawiera kategorię
               K1:K4, # weź wartości z utworzonych kolumn
               na.rm = TRUE) %>% # pomiń braki danych
        # wykres
        ggplot(data = .,
               aes_string(x = "kategoria")) +
        geom_bar(fill = "#1ebbd7",
                 show.legend = FALSE) +
        coord_flip() +
        labs(x = "Gender",
             y = "Count",
             title = "Gender") +
        theme_minimal(base_size = 15)
      
      # co ma zwrócić eventReactive?
      return(wykres)
      
    }) # koniec eventReactive
  
  wykresik_EducationParents <- eventReactive(
    eventExpr = input$przycisk_generuj, 
    valueExpr = { 
      
      wykres <- ggplot(data = filtrowaneDane()[!is.na(filtrowaneDane()$EducationParents),],
                       aes(x = EducationParents))
      wykres <- wykres +
        geom_bar(fill = '#1ebbd7',
                 show.legend = FALSE) +
        coord_flip() +
        labs(x = "Parents education",
             y = "Count",
             title = "Parents education") +
        theme_minimal(base_size = 15)
      
      # co ma zwrócić eventReactive?
      return(wykres)    
      
    }) # koniec eventReactive
  
  wykresik_Age <- eventReactive(
    eventExpr = input$przycisk_generuj, 
    valueExpr = { 
      
      wykres <- ggplot(data = filtrowaneDane()[!is.na(filtrowaneDane()$Age),],
                       aes(x = Age))
      wykres <- wykres +
        geom_bar(fill = '#1ebbd7',
                 show.legend = FALSE) +
        coord_flip() +
        labs(x = "Age",
             y = "Count",
             title = "Age") +
        theme_minimal(base_size = 15)
      
      # co ma zwrócić eventReactive?
      return(wykres)    
      
    }) # koniec eventReactive

  output$wykres_DevType <- renderPlot(wykresik_DevType())
  output$wykres_LanguageWorkedWith <- renderPlot(wykresik_LanguageWorkedWith())
  output$wykres_PlatformWorkedWith <- renderPlot(wykresik_PlatformWorkedWith())
  
  output$wykres_YearsCoding <- renderPlot(wykresik_YearsCoding())
  output$wykres_YearsCodingProf <- renderPlot(wykresik_YearsCodingProf())
  
  output$wykres_EducationTypes <- renderPlot(wykresik_EducationTypes())
  output$wykres_FormalEducation <- renderPlot(wykresik_FormalEducation())
  output$wykres_UndergradMajor <- renderPlot(wykresik_UndergradMajor())
  
  output$wykres_Gender <- renderPlot(wykresik_Gender())
  output$wykres_EducationParents <- renderPlot(wykresik_EducationParents())
  output$wykres_Age <- renderPlot(wykresik_Age())
  
  
  
  # ZAKŁADKA FILTERS
    # Filtrowanie
    
    filtrowaneDane_f <- eventReactive(
    eventExpr = c(input$przedzial_plac_f,
                  input$Specjalizacja_f,
                  input$Spec_zmienna_f,
                  input$Edukacja_f,
                  input$Edukacja_zmienna_f,
                  input$Demografia_f,
                  input$Demografia_zmienna_f,
                  input$Geografia_f,
                  input$Geografia_zmienna_f),
    valueExpr = {
      dane_filtr <- filter(data,
                        between(ConvertedSalary, input$przedzial_plac_f[[1]], input$przedzial_plac_f[[2]]))

      
      # Specjalizacja
      if(input$Spec_zmienna_f == 'Back-end developer' & input$Specjalizacja_f == 1){
        dane_filtr <- filter(dane_filtr,
                             DevType %like% "Back-end developer")
      }
      if(input$Spec_zmienna_f == 'Full-stack developer' & input$Specjalizacja_f == 1){
        dane_filtr <- filter(dane_filtr,
                             DevType %like% "Full-stack developer")
      }
      if(input$Spec_zmienna_f == 'Mobile developer' & input$Specjalizacja_f == 1){
        dane_filtr <- filter(dane_filtr,
                             DevType %like% "Mobile developer")
      }
      if(input$Spec_zmienna_f == 'Student' & input$Specjalizacja_f == 1){
        dane_filtr <- filter(dane_filtr,
                             DevType %like% "Student")
      }
      if(input$Spec_zmienna_f == 'Database administrator' & input$Specjalizacja_f == 1){
        dane_filtr <- filter(dane_filtr,
                             DevType %like% "Database administrator")
      }
      if(input$Spec_zmienna_f == 'Data scientist or machine learning specialist' & input$Specjalizacja_f == 1){
        dane_filtr <- filter(dane_filtr,
                             DevType %like% "Data scientist or machine learning specialist")
      }
      
      # Edukacja
      if(input$Edukacja_zmienna_f == 'Bachelor’s degree' & input$Edukacja_f == 1){
        dane_filtr <- filter(dane_filtr,
                             FormalEducation == "Bachelor’s degree (BA, BS, B.Eng., etc.)")
      }
      if(input$Edukacja_zmienna_f == 'Master’s degree' & input$Edukacja_f == 1){
        dane_filtr <- filter(dane_filtr,
                             FormalEducation == "Master’s degree (MA, MS, M.Eng., MBA, etc.)")
      }
      if(input$Edukacja_zmienna_f == 'Doctoral degree' & input$Edukacja_f == 1){
        dane_filtr <- filter(dane_filtr,
                             FormalEducation %like% "doctoral degree")
      }
      if(input$Edukacja_zmienna_f == 'Taught yourself' & input$Edukacja_f == 1){
        dane_filtr <- filter(dane_filtr,
                             EducationTypes %like% "Taught yourself")
      }
      if(input$Edukacja_zmienna_f == 'Online course' & input$Edukacja_f == 1){
        dane_filtr <- filter(dane_filtr,
                             EducationTypes %like% "online course")
      }
      if(input$Edukacja_zmienna_f == 'On-the-job training' & input$Edukacja_f == 1){
        dane_filtr <- filter(dane_filtr,
                             EducationTypes %like% "on-the-job training")
      }
      # Demografia
      if(input$Demografia_f == 1){
        dane_filtr <- filter(dane_filtr,
                             Gender == input$Demografia_zmienna_f)
      }
      # Geografia
      if(input$Geografia_f == 1){
        dane_filtr <- filter(dane_filtr,
                             Country == input$Geografia_zmienna_f)
      }
      
    # zwraca dane  
    return(dane_filtr)
    }  
    )

    output$wykres_filtr <- renderPlotly({
    # Wykres
    histogram <- ggplot(data = filtrowaneDane_f(),
                          aes(ConvertedSalary)) +
                  geom_histogram(aes(fill = ..count..),
                                     bins = 75) +
                  theme_minimal() +
                  scale_fill_gradient("Count", low = '#B7DEE8', high = '#31869B')
    
    # Interaktywny wykres  
    ggplotly(histogram)
  })
  
  # Tabela danych
  output$TabelaDanych_f <- renderDataTable({select(filtrowaneDane_f(),
                                                   Country, ConvertedSalary, YearsCodingProf, LanguageWorkedWith, DatabaseWorkedWith, CompanySize, JobSatisfaction)})
  
  
  
  # ZAKŁADKA STATISTICS
  
  # Tabela I
  output$TabelaDanych_stat_f <- renderDataTable({
    
    statystyki <- data.frame(minimum = sapply(select(filtrowaneDane_f(), ConvertedSalary), min, na.rm = T),
                             maximum = sapply(select(filtrowaneDane_f(), ConvertedSalary), max, na.rm = T),
                             mean = sapply(select(filtrowaneDane_f(), ConvertedSalary), mean, na.rm = T),
                             median =  sapply(select(filtrowaneDane_f(), ConvertedSalary), median, na.rm = T),
                             Q1 = sapply(select(filtrowaneDane_f(), ConvertedSalary), quantile, probs = 0.25, na.rm = T),
                             Q2 = sapply(select(filtrowaneDane_f(), ConvertedSalary), quantile, probs = 0.75, na.rm = T)
                             )
    
    formattable(statystyki)
    
    })
  
  # Tabela II
  output$TabelaDanych_stat_g <- renderFormattable({
    data %>%
      # rozdzielenie połączonych poziomów w kolumnach (odpowiedzi były wielowyborowe)
      separate_(.,
                col = input$Zmienna_g,
                into = c("K1","K2","K3","K4","K5","K6","K7","K8","K9","K10","K11","K12","K13","K14","K15","K16","K17","K18","K19","K20"),
                sep = ";") %>%
      # rozciągnięcie bazy w pionie
      gather(.,
             key = long_variable, # zawiera nr kolumny
             value = "group", # zawiera kategorię
             K1:K20, # weź wartości z utworzonych kolumn
             na.rm = TRUE) %>% # pomiń braki danych
      # pogrupowanie po wybranej zmiennej
      group_by_("group") %>%
      # statystyki dla płac w wybranej grupie
      summarise(minimum = min(na.omit(ConvertedSalary)),
                maximum = max(na.omit(ConvertedSalary)),
                mean = mean(na.omit(ConvertedSalary)),
                median = median(na.omit(ConvertedSalary)),
                Q1 = quantile(na.omit(ConvertedSalary), probs = 0.25),
                Q2 = quantile(na.omit(ConvertedSalary), probs = 0.75),
                count = n()) %>%
      # sortuję po średniej
      arrange_(., paste0("desc(",input$Sortowanie_g,")")) %>%
      # koloruję tabelkę
      formattable(., list(
                    mean = color_bar("#B7DEE8"),
                    median = color_bar("#B7DEE8"),
                    count = color_bar("#83d0c9")))
 
    })
  
  # Dodać wykres?
    output$Boxplot_stat_g <- renderPlotly({

      boxplot <- data %>%
      
        # rozdzielenie połączonych poziomów w kolumnach (odpowiedzi były wielowyborowe)
        separate_(.,
                  col = input$Zmienna_g,
                  into = c("K1","K2","K3","K4","K5","K6","K7","K8","K9","K10","K11","K12","K13","K14","K15","K16","K17","K18","K19","K20"),
                  sep = ";") %>%
        # rozciągnięcie bazy w pionie
        gather(.,
               key = long_variable, # zawiera nr kolumny
               value = "group", # zawiera kategorię
               K1:K20, # weź wartości z utworzonych kolumn
               na.rm = TRUE) %>% # pomiń braki danych
        # wykres
        ggplot(data = .,
               aes_string(x = "group",
                          y = "ConvertedSalary")) +
          geom_boxplot(fill = "#31869B",
                       outlier.colour = "#9BBB59") +
        coord_flip() +
        theme_minimal()
      
      # Interaktywny wykres  
      ggplotly(boxplot)
      })
    
}

shinyApp(ui = ui, server = server)
