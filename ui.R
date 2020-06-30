

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "card.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "polaroid.css")
  ),
  navbarPage("PokeNav II:",
             id = "pages",
             tabPanel(
               "Mon by Location",
                sidebarPanel(
                  selectInput("gen","Select generation", c("I","II","III","IV","V","VI","VII"), "III"),
                  checkboxGroupInput("loc-filter", "Select location filter", c("Cities/Towns","Routes","Other")),
                  selectInput("loc","Select location", "-", "-"),
                  selectInput("area","Select area", "-"),
                  selectInput("mon", "Select Pokemon","-"),
                  selectInput("mon1.2", "See Evolution Line","-")
                ),
                mainPanel(
                  actionButton("learn", "See Learnset"),
                  actionButton("dex-info", "See Dex Entry"),
                  helpText("Note: Move Categories were specified further in Generation 4 and beyond."),
                  br(),
                  br(),
                  DT::dataTableOutput("tbl")
                )
             ),
             tabPanel(
               "PokeDex",
               sidebarPanel(
                 selectInput("mon2","Enter Pokemon","-"),
                 helpText("Gen 8 Not Included"),
                 selectInput("gen2","Select generation", c("All","I","II","III","IV","V","VI"), "All"),
                 selectInput("learnset-filter2","Filter Learnset by Gen", c("-","I","II","III","IV","V","VI","VII"), "All"),
                 helpText("Defaults to the learnset in the generation the Pokemon is introduced. Gen VIII shown but unavailable."),
                 checkboxGroupInput("dex-filter", "Select Pokemon Type(s)", allTypes)
               ),
               mainPanel(
                 useShinyjs(),
                 actionButton("learn2", "See Learnset"),
                 actionButton("dex-info2","See Dex Entry"),
                 helpText("Note: Move Categories were specified further in Generation 4 and beyond."),
                 helpText("Only shows learnset in generation introduced."),
                 br(),
                 br(),
                 DT::dataTableOutput("tbl2"),
                 div(id = "dex2",
                   class = "card",
                   tags$img(uiOutput("sprite2"))
                 ) %>% shinyjs::hidden()
               )
             ),
             tabPanel(
               "ItemDex",
               navbarPage("Items",
                          tabPanel(
                            "General Items",
                             helpText("Item locations only available for Emerald"),
                             helpText("Note to self: make interface better"),
                             DT::dataTableOutput("tbl3")
                          ),
                          tabPanel(
                            "TM Information",
                            selectInput("genP3","Select generation", romGen$arabic, "III"),
                            helpText("TM information available for all generations"),
                            DT::dataTableOutput("tbl3.2")
                          )
               )
             ),
             tabPanel(
               "BreedDex"
             )
  )
)


