server <- function(input, output, session) {
  
  ### Location by Generation ###
  intGen <- reactive({
    romGen %>% filter(roman == input$gen) %>% pull(arabic)
  })
  
  # translates into generation version
  verGen <- reactive({
    romGen %>% filter(arabic == input$genP3) %>% pull(versions)
  })
  
  monP1 <- reactive({
    req(input$mon != "-")
    if(input$mon1.2 == "-"){
      input$mon
    } else {
      input$mon1.2
    }
  })
  
  location <- reactive({
    req(input$gen %in% romGen$roman)

    # if filter is used
    if (!is.null(input$`loc-filter`)) {
      f <- input$`loc-filter`
      
      f <- case_when(
        "Cities/Towns" == f ~ "(city|town)",
        "Routes" == f ~ "(route)",
        "Other" == f ~ "^((?!city|town|route).)*$",
        TRUE ~ ""
      )
      
      if(input$gen == "I"){
        kanto %>%
          str_subset(paste(f, collapse = "|"))
      }else if(input$gen == "II"){
        jhoto %>%
          str_subset(paste(f, collapse = "|"))
      }else if(input$gen == "III"){
        hoenn %>%
          str_subset(paste(f, collapse = "|"))
      }else if(input$gen == "IV"){
        sinnoh %>%
          str_subset(paste(f, collapse = "|"))
      }else if(input$gen == "V"){
        unova %>%
          str_subset(paste(f, collapse = "|"))
      }else if(input$gen == "VI"){
        kalos %>%
          str_subset(paste(f, collapse = "|"))
      }else if(input$gen == "VII"){
        alola %>% 
          str_subset(paste(f, collapse = "|"))
      }else if(input$gen == "VIII"){
        "sword-shield"
      }else{
        ""
      }
      
      
    } else {
      
      if(input$gen == "I"){
        kanto
      }else if(input$gen == "II"){
        jhoto
      }else if(input$gen == "III"){
        hoenn
      }else if(input$gen == "IV"){
        sinnoh
      }else if(input$gen == "V"){
        unova
      }else if(input$gen == "VI"){
        kalos
      }else if(input$gen == "VII"){
        alola
      }else if(input$gen == "VIII"){
        "sword-shield"
      }else{
        ""
      }
    }
    
  })
  
  ### Area by Location ###
  
  # formats for getLocationAreas
  loc_text <- reactive(input$loc %>% str_to_lower() %>% gsub(" ","-",.))

  area <- reactive({
    if(input$loc != "-"){
      if(intGen() >= 7){
        c("Unavailable")
      }else{
        getLocationAreas(loc_text())
      }
    }else{
      "-"
    }
  })
  
  ### Mon by Area ###
  
  # formats for getAreaMons
area_text <- reactive(input$area %>% str_to_lower() %>% gsub(" ", "-", .))

mons <- reactive({
  req(input$area != "-" | intGen() >= 7)

  if (input$loc != "-") {
    if(intGen() == 7){
      alola_byloc %>% 
        filter(src == loc_text()) %>% 
        pull(Name)
    }else{
      getAreaMons(area_text())  %>%
        select(Pokemon) %>% 
        pull()
    }
  } else {
    "-"
  }
  })
  
  ### Evolved Mons ###
  evolve <- reactive({
    req(input$mon != "-")
    
    if (input$loc != "-"){
      AM_evs <- allGenFams %>% filter(Basic == input$mon | Stage2 == input$mon | Stage3 == input$mon)
      
      #Finds and filter evolutions not introduced before the selected generation
      AM_evs  %>% 
        filter(!Stage2 %in% (allMons %>% filter(Gen > intGen()) %>% pull(Name))) %>% 
        filter(!Stage3 %in% (allMons %>% filter(Gen > intGen()) %>% pull(Name))) %>% 
        unlist() %>% 
        unique()
    } else {
      "-"
    }
  })
  
  
  ### Learnset by Mon ###
  learnset <- eventReactive(input$learn,{
    req(input$mon != "-")
    getLearnset_txt(monP1(), gen = input$gen)
  })
  
  
  ### Observe ###

  observe({
    updateSelectInput(session, "loc", choices = c("-", location() %>% 
                                                    gsub("-"," ",.) %>% 
                                                    str_to_title()))
  })

  observe({
    updateSelectInput(session, "area", choices = c("-", area() %>% 
                                                     gsub("-"," ",.) %>% 
                                                     str_to_title()))
  })
  
  observe({
    updateSelectInput(session, "mon", choices = c("-", mons()))
  })
  
  observe({
    updateSelectInput(session, "mon1.2", choices = c("-", evolve()))
  })
  
  
  observeEvent(input$learn,{
    if(monP1() != "-"){
      shinyjs::show("moveplot1")
      shinyjs::show("tbl")
    }
    if(input$`dex-info` > 0){
      hide("dex")
      hide("dex1.1")
    }
  })
  
  # Pokedex Info Page 1
  
  observeEvent(input$`dex-info`,{
    if(input$mon != "-"){
      toggle("dex")
      toggle("dex1.1")
    }
    hide("moveplot1")
    hide("tbl")
  })

  # Learnset Pie
  output$learnplot <- renderPlotly({
    req(input$mon != "-")
    plot_ly(learnset(), labels = ~ Type, type = "pie")
  })
  
  
  output$tbl <- DT::renderDataTable(learnset())
  
  #Sprites page 1
  sprite <- reactive(NS_sprites %>% filter(Name == monP1(), Class == "normal") %>% pull(Url))
  
  output$sprite <- renderUI({
    tags$img(
      src = sprite(), width = "200", height = "200",
      style = "display: block; margin-left: auto; margin-right: auto;"
    )
  })
  
  
  # Stats chart page 1
  stats_chart <- reactive({
    if(input$gen == "I"){
      allStatsbyGen %>% 
        filter(Pokemon == monP1(), Gen == input$gen, !str_detect(NatNo, "\\D")) %>% 
        select(HP, Atk, Def, Special, Spe) 
    }else{
      allStatsbyGen %>% 
        filter(Pokemon == monP1(), Gen == input$gen, !str_detect(NatNo, "\\D")) %>% 
        select(HP, Atk, Def, SpA, SpD, Spe)
    }
  })
  
  label2 <- reactive({
    if(input$gen == "I"){
      t = c("HP", "ATK", "DEF", "SPD", "Special")
    }else{
      t = c("HP", "ATK", "DEF", "SP.ATK", "SP.DEF", "SPD")
    }
  })
  
  output$dexplot <- renderPlotly({
    req(input$mon != "-")
    
    plot_ly(
      type = "scatterpolar",
      mode = "closest",
      fill = "toself"
    ) %>%
      add_trace(
        r = as.matrix(stats_chart()),
        theta = label2(),
        showlegend = TRUE,
        mode = "markers",
        name = monP1()
      )  %>%
      layout(
        title = list(
          text = "Base Stats",
          x = 0.46,
          font = list(
            size = 30
          )
        ),
        
        margin = list(t = 100),
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0, 255)
          )
        ),
        showlegend = TRUE
      )
  })

######################################## Page 2 ################################################
  
  intGen2 <- reactive({
    romGen %>% filter(numeral == input$gen2) %>% pull(arabic)
  })
  
  p2Mons <- reactive({
    #no learnset available for gen 8, limit to gen 6 for consistency
    df <- allMons %>% filter(Gen <= 7, Name != "MissingNo.")
    
    if(input$gen2 != "All"){
      df <- df %>% filter(GenX == input$gen2)
    }
    
    if(!is.null(input$`dex-filter`)){
      df %>% 
        filter(Type1 %in% input$`dex-filter` | Type2 %in% input$`dex-filter`) %>% 
          pull(Name)
    }else{
      df %>% pull(Name)
    }
    
  })
  
  #used in learnsetP2
  genIntro <- reactive({
    req(input$mon2 != "-")
    allMons %>% filter(Name == input$mon2) %>% pull(GenX)
  })
  
  genIntroNumber <- reactive(romGen %>% filter(roman == genIntro()) %>% pull(arabic))
  
  genLearnset2 <- reactive(romGen %>% filter(arabic >= genIntroNumber()) %>% pull(roman))
  
  learnsetP2 <- eventReactive(input$learn2,{
    req(input$mon2 != "-")
    df <- tryCatch({
      
      suppressWarnings(getLearnset_txt(input$mon2, input$`gen-filter2`))
      
    }, error = function(err){
      
      getLearnset_txt(input$mon2, genIntro())
      
    }, warning = function(warn){
      
      print("Error in learnsetP2")
      
    })
  })
  
  # Learnset Pie
  output$learnplot2 <- renderPlotly({
    req(input$mon2 != "-")
    plot_ly(learnsetP2(), labels = ~ Type, type = "pie")
  })
  
  spriteP2 <- reactive(NS_sprites %>% filter(Name == input$mon2, Class == "normal") %>% pull(Url))
  
  observeEvent(input$learn2,{
    if(input$mon2 != "-"){
      shinyjs::show("moveplot2")
      shinyjs::show("tbl2")
    }
    
    if(input$`dex-info2` > 0){
      hide("dex2")
      hide("dex2.1")
    }
  })
  
  observeEvent(input$`dex-info2`,{
    if(input$mon2 != "-"){
      toggle("dex2")
      toggle("dex2.1")
    }
    hide("tbl2")
    hide("moveplot2")
  })
  
  observe({
    updateSelectInput(session,"mon2", choices = c("-", p2Mons()))
  })
  
  observe({
    updateSelectInput(session,"gen-filter2", choices = genLearnset2(), selected = genIntro())
  })
  
  
  output$tbl2 <- DT::renderDataTable(learnsetP2())
  
  output$sprite2 <- renderUI({
    tags$img(
      src = spriteP2(), width = "200", height = "200",
      style = "display: block; margin-left: auto; margin-right: auto;"
    )
  })
  
  stats_chartP2 <- reactive({
    if(input$`gen-filter2` == "I"){
      allStatsbyGen %>% 
        filter(Pokemon == input$mon2, Gen == input$`gen-filter2`, !str_detect(NatNo, "\\D")) %>% 
        select(HP,Atk,Def,Special,Spe) 
    }else{
      allStatsbyGen %>% 
        filter(Pokemon == input$mon2, Gen == input$`gen-filter2`, !str_detect(NatNo, "\\D")) %>% 
        select(HP, Atk, Def, SpA, SpD, Spe)
    }
  })
  
  label <- reactive({
    if(input$`gen-filter2` == "I"){
      t = c("HP", "ATK", "DEF", "SPD", "Special")
    }else{
      t = c("HP", "ATK", "DEF", "SP.ATK", "SP.DEF", "SPD")
    }
  })
  
  output$dexplot2 <- renderPlotly({
    req(input$mon2 != "-")
    
    plot_ly(
      type = "scatterpolar",
      mode = "closest",
      fill = "toself"
    ) %>%
      add_trace(
        r = as.matrix(stats_chartP2()),
        theta = label(),
        showlegend = TRUE,
        mode = "markers",
        name = input$mon2
      )  %>%
      layout(
        title = list(
          text = "Base Stats",
          x = 0.46,
          font = list(
            size = 30
          )
        ),
        
        margin = list(t = 100),
        polar = list(
          radialaxis = list(
            visible = T,
            range = c(0, 255)
          )
        ),
        showlegend = TRUE
      )
  })
  
  ######### Page 3 ##########
  #Reactive for future applications
  item_info <- reactive({
    gen3_Findable_Items
  })
  
  output$tbl3 <- DT::renderDataTable(item_info())
  
  TM_info <- reactive({
    allTMs_byGen %>% 
      filter(Version == verGen())
  })
  
  output$tbl3.2 <- DT::renderDataTable(TM_info())
  
  session$onSessionEnded(function() {
    closeAllConnections()
    gc()
  })
}

# Notice convention

# gsub("-"," ",.) %>% str_to_title() for translating for end user
# str_to_lower() %>% gsub(" ","-",.) for translating for back end
