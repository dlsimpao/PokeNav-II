# interface tests for pokedex
library(tidyverse)
library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)
library(knitr)
library(ggfortify)
library(plotly)
library(FNN)
library(jsonlite)
library(lubridate)
library(httr)
library(rvest)
library(shinyjs)
library(gtrendsR)


allTypes <- c("Dark", "Dragon", "Electric", "Fire", "Grass", "Ice", "Psychic", "Water", "Bug",
              "Fighting", "Flying", "Ghost", "Ground", "Normal", "Poison", "Rock", "Steel", "Fairy")


### Sprites ###
h <- read_html("https://pokemondb.net/sprites")
sprites <- h %>% 
  html_nodes("a.infocard") %>% 
  html_nodes("span.img-fixed.icon-pkmn") %>% 
  html_attr("data-src")

name <- h %>% 
  html_nodes("a.infocard") %>% 
  html_text() %>% 
  gsub("^ ","",.)

sprite_df <- tibble(Name = name, Png = sprites)

# Pokemon from App
load("allMons.6.2020.RData")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "card.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "polaroid.css")
  ),
  navbarPage("Tabs",
             tabPanel("Tab1",
                      sidebarPanel(),
                      mainPanel()),
             tabPanel("Tab2",
                      sidebarPanel(
                        selectInput("mon2","Enter Pokemon",allMons %>% pull(Name),"-"),
                        helpText("Gen 8 Not Included"),
                        selectInput("gen2","Select generation", c("All","I","II","III","IV","V","VI"), "All"),
                        checkboxGroupInput("dex-filter", "Select Pokemon Type(s)", allTypes)
                      ),
                      mainPanel(
                        actionButton("learn", "See Learnset"),
                        actionButton("dex-info", "See Dex Entry"),
                        helpText("Note: Move Categories were specified further in Generation 4 and beyond."),
                        br(),
                        br(),
                        DT::dataTableOutput("tbl"),
                        div(
                          class = "card",
                          tags$img(uiOutput("sprite2"), width = "auto")
                        )
                      )),
             tabPanel("Tab3"))
)

server <- function(input,output){
  
  p2sprite <- reactive({
    req(input$mon2 != "-")
    #sprite_df %>% filter(Name == input$mon2) %>% pull(Png)
    NS_sprites %>% filter(Name == input$mon2, Class == "shiny") %>% pull(Url) 
  })
  
  output$sprite2 <- renderUI({
    tags$img(
      src = p2sprite(), width = "100", height = "100",
      style = "display: block; margin-left: auto; margin-right: auto"
    )
  })
  
  
}

shinyApp(ui = ui, server = server)
