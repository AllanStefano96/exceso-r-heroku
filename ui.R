library(shiny)


fallecidos=read.table('fallecidos2014-2020.csv',header = T,sep=',')
fallecidos$prov_fall=as.factor(fallecidos$prov_fall)
fallecidos$cant_fall=as.factor(fallecidos$cant_fall)
fallecidos$ï..fecha=as.Date(fallecidos$ï..fecha,'%m/%d/%Y')





fluidPage(#theme = shinytheme("yeti"),
  navbarPage(
    # theme = "cerulean",  # <--- To use a theme, uncomment this
    "Fallecidos Covid 19",
    tabPanel("Exceso de muertes",
             sidebarPanel(
               # Input: Slider for the number of bins ----
               selectInput(inputId="provincia",label="Seleccione provincia",choices = c("Guayas"="Guayas",
                                                                                        "Pichincha"="Pichincha",
                                                                                        "Manabi"="Manabi",
                                                                                        "Ecuador"="Ecuador"),
                           selected = "Guayas",multiple = F),
               
               selectInput(inputId="canton",label="Seleccione canton",choices = c("Guayaquil"="Guayaquil",
                                                                                  "Quito"="Quito",
                                                                                  "Cuenca"="Cuenca"),
                           selected = "Guayaquil",multiple = F),
               
             ), # sidebarPanel
             mainPanel(
               # Output:
               plotlyOutput(outputId = "distPlot"),
               plotlyOutput(outputId = "distPlot1")
               
             ) # mainPanel
             
    )
    
  ) # navbarPage
) # fluidPage