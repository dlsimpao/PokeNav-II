server <- function(input, output, session) {
  
  ### Location by Generation ###
  intGen <- reactive({
    romGen %>% filter(numerals == input$gen) %>% pull(arabic)
  })
  
  # translates into generation version
  verGen <- reactive({
    romGen %>% filter(numerals == input$genP3) %>% pull(versions)
  })
  
  
  location <- reactive({
    req(input$gen %in% romGen$numerals)

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
    if(input$mon1.2 == "-"){
      getLearnset(input$mon, gen = input$gen)
    }else{
      getLearnset(input$mon1.2, gen = input$gen)
    }
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

  output$tbl <- DT::renderDataTable(learnset())

########## Page 2 ############
  
  intGen2 <- reactive({
    romGen %>% filter(numeral == input$gen2) %>% pull(arabic)
  })
  
  p2Mons <- reactive({
    df <- allMons %>% filter(Gen <= 7, Name != "MissingNo.") #no learnset available for gen 8, limit to gen 6 for consistency
    
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
  
  genIntroNumber <- reactive(romGen %>% filter(numerals == genIntro()) %>% pull(arabic))
  
  genLearnset2 <- reactive(romGen %>% filter(arabic >= genIntroNumber()) %>% pull(numerals))
  
  learnsetP2 <- eventReactive(input$learn2,{
    req(input$mon2 != "-")
    
    
    tryCatch({
      getLearnset(input$mon2, input$`learnset-filter2`)
    }, error = function(err){
      getLearnset(input$mon2, genIntro())
      
    }, warning = function(warn){
      print("Try something else")
    })
    
  })
  
  observe({
    updateSelectInput(session,"mon2", choices = c("-", p2Mons()))
  })
  
  observe({
    updateSelectInput(session,"learnset-filter2", choices = genLearnset2(), selected = genIntro())
  })
  
  
  output$tbl2 <- DT::renderDataTable(learnsetP2())
  
  
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
}

# Notice convention

# gsub("-"," ",.) %>% str_to_title() for translating for end user
# str_to_lower() %>% gsub(" ","-",.) for translating for back end
