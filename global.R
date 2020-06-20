library(shiny)
library(shinythemes)
library(shinycssloaders)
library(shinyWidgets)

library(tidyverse)
library(ggfortify)
library(plotly)
library(jsonlite)
library(lubridate)
library(httr)
library(rvest)

library(parallel)

#Issues to tackle
#1. Item locations in Emerald [done]
#2. Generalize second page to all generations
#3. Generalize item locations to all versions
#4. 

specTypes <- c("Dark", "Dragon", "Electric", "Fire", "Grass", "Ice", "Psychic", "Water")
physTypes <- c("Bug", "Fighting", "Flying", "Ghost", "Ground", "Normal", "Poison", "Rock", "Steel")

allEmTypes <- c(specTypes,physTypes)

allTypes <- c(allEmTypes,"Fairy")

romGen <- c("I","II","III","IV","V","VI","VII","VIII")
##########################    Work from hoennDex   ############################

# Scrape info from gen III

getGenMons_info <- function(genNo){
  h <- read_html(str_glue("https://en.wikipedia.org/wiki/List_of_generation_{num}_Pok%C3%A9mon", num = genNo), encoding = "UTF-8")
  
  genDigit <- case_when(
    genNo == "I" ~ 1,
    genNo == "II" ~ 2,
    genNo == "III" ~ 3,
    genNo == "IV" ~ 4,
    genNo == "V" ~ 5,
    genNo == "VI" ~ 6,
    genNo == "VII" ~ 7,
    genNo == "VIII" ~ 8,
    TRUE ~ 0
  )
  
  if(genDigit != 7){
    genMons <- h %>%
      html_nodes("table") %>%
      .[[2]] %>%
      html_table(fill = TRUE) %>%
      as.data.frame() 
  }else{
    genMons <- h %>% 
      html_nodes("table") %>% 
      .[[3]] %>% 
      html_table(fill = TRUE) %>% 
      as.data.frame()
  }
  
  colnames(genMons) <- c("Name","Name2","NatNo","Type1","Type2","Evolves","Notes")
  
  
  
  (eMons <- genMons %>%
      slice(2:nrow(.)) %>% 
      select(NatNo, Name, Type1, Type2, Evolves) %>%
      mutate(
        Type2 = case_when(
          Type1 == Type2 ~ "",
          grepl("Fairy", Type2) ~ gsub(".*", "Fairy (Gen 6 and on)", Type2),
          TRUE ~ Type2
        ),
        Evolves = case_when(
          grepl("Gardevoir", Evolves) ~ gsub(".*", "Gardevoir,Gallade",Evolves),
          grepl("Ninjask", Evolves) ~ gsub(".*", "Ninjask,Shedinja", Evolves),
          grepl("Glalie", Evolves) ~ gsub(".*", "Glalie,Frosslass", Evolves),
          grepl("Huntail", Evolves) ~ gsub(".*", "Huntail,Gorebyss", Evolves),
          grepl("\\(", Evolves) ~ gsub(".\\(.*\\)","",Evolves) ,
          grepl("\\[", Evolves) ~ gsub("\\[.*","", Evolves),
          # grepl("(Mega|End|Obs|No|Primal)", Evolves) ~ gsub(".*","", Evolves),
          TRUE ~ Evolves
        ),
        Gen = genDigit,
        GenX = genNo
      ) %>%
      distinct(Name, .keep_all = TRUE)) 
}

#Helps find which evolutions exist in which generation

allMons <- lapply(romGen, function(NUM) getGenMons_info(NUM)) %>% bind_rows
# NatNo Name Type1 Type2 Evolves Gen GenX
#save(allMons, file = "allMons.6.2020.RData")


# Get Pokemon by Generation

getMon <- function(genNo) {
  genNo <- case_when(
    genNo == "I" ~ 1,
    genNo == "II" ~ 2,
    genNo == "III" ~ 3,
    genNo == "IV" ~ 4,
    genNo == "V" ~ 5,
    genNo == "VI" ~ 6,
    genNo == "VII" ~ 7,
    TRUE ~ 8
  )

  r <- GET(str_glue("https://pokeapi.co/api/v2/generation/{id}/", id = genNo))
  stop_for_status(r)

  jen3 <- content(r, type = "text", encoding = "UTF-8") %>% fromJSON(flatten = TRUE)
  gen3_names <- jen3$pokemon_species$name
}


# FOR PAGE 2 #


# Mon Names for display
disp_Mon_api <- getMon("III") %>% gsub("-"," ",.) %>% str_to_title()

disp_Mon_scrape <- getGenMons_info("III")

##############

# Scrape Pokemon Learnsets from Bulba

getLearnset <- function(Mon, gen = "III") {
  if (Mon %in% c("Melmetal", "Meltan")) {
    link <- "https://bulbapedia.bulbagarden.net/wiki/{mon}_(Pok%C3%A9mon)/Generation_{gen}_learnset#By_TM.2FHM"
  } else {
    link <- "https://bulbapedia.bulbagarden.net/wiki/{mon}_(Pok%C3%A9mon)/Generation_{gen}_learnset#By_leveling_up"
  }
  h <- read_html(str_glue(link,
                          mon = Mon, gen = gen
  ))
  
  gen <- case_when(
    gen == "I" ~ 1,
    gen == "II" ~ 2,
    gen == "III" ~ 3,
    gen == "IV" ~ 4,
    gen == "V" ~ 5,
    gen == "VI" ~ 6,
    gen == "VII" ~ 7,
    gen == "VIII" ~ 8,
    TRUE ~ 0
  )
  
  # By Level
  if (gen > 3) {
    byLevel <- h %>%
      html_node("table.sortable") %>%
      html_table() %>%
      as.data.frame() %>%
      select(1, Move, Type, `Cat.`, `Pwr.`, `Acc.`, PP)
    
    colnames(byLevel) <- c("Level", "Move", "Type", "Cat.", "Pwr.", "Acc.", "PP")
  } else {
    byLevel <- h %>%
      html_node("table.sortable") %>%
      html_table() %>%
      as.data.frame()
    
    byLevel <- tryCatch({
      byLevel <- byLevel %>%
        select(1, Move, Type, Power, Accuracy, PP) %>%
        mutate(`Cat.` = "Depends on Type")
    },error = function(err){
      byLevel <- byLevel %>%
        select(1, Move, Type, `Pwr.`, `Acc.`, PP) %>%
        mutate(`Cat.` = "Depends on Type")
    },warning = function(cond){
      message("Try something else")
    })
    
    colnames(byLevel) <- c("Level", "Move", "Type", "Pwr.", "Acc.", "PP", "Cat.")
    # if (gen < 3) {
    #   byLevel <- byLevel %>%
    #     select(1, Move, Type, Power, Accuracy, PP) %>%
    #     mutate(`Cat.` = "Depends on Type")
    # } else {
    #   byLevel <- byLevel %>%
    #     select(1, Move, Type, `Pwr.`, `Acc.`, PP) %>%
    #     mutate(`Cat.` = "Depends on Type")
    #}
    colnames(byLevel) <- c("Level", "Move", "Type", "Pwr.", "Acc.", "PP", "Cat.")
  }
  
  # Fix up values
  byLevel <- byLevel %>%
    mutate(
      Level = substring(Level, 0, nchar(as.character(Level)) / 2),
      `Pwr.` = as.character(`Pwr.`),
      PP = as.character(PP),
      Method = "Level"
    ) %>%
    as_tibble()
  
  
  # By Machine
  if (gen > 3) {
    byMachine <- h %>%
      html_nodes("table.roundy") %>%
      .[[3]] %>%
      html_table(fill = TRUE) %>%
      as_tibble() %>%
      slice(5:nrow(.)) %>%
      select(3:8)
    colnames(byMachine) <- c("Move", "Type", "Cat.", "Pwr.", "Acc.", "PP")
  } else {
    byMachine <- h %>%
      html_nodes("table.roundy") %>%
      .[[3]] %>%
      html_table(fill = TRUE) %>%
      as_tibble() %>%
      slice(5:nrow(.)) %>%
      select(3:7) %>%
      mutate(cat = "Depends on Type")
    colnames(byMachine) <- c("Move", "Type", "Pwr.", "Acc.", "PP", "Cat.")
  }
  
  
  
  byMachine <- byMachine %>%
    as_tibble() %>%
    mutate(
      Level = "",
      Method = "Machine"
    )
  
  if (Mon %in% c("Meltan", "Melmetal")){
    byBreed <- tibble(
      Move = "NA",
      Type = "NA",
      `Pwr.` = "NA",
      `Acc.` = "NA",
      PP = "NA",
      `Cat.` = "NA",
    )
  } else if (gen > 3) {
    byBreed <- h %>%
      html_nodes("table.roundy") %>%
      .[[5]] %>%
      html_table(fill = TRUE) %>%
      as_tibble() %>%
      slice(5:nrow(.)) %>%
      select(2:7)
    
    colnames(byBreed) <- c("Move", "Type", "Cat.", "Pwr.", "Acc.", "PP")
  } else if (gen > 1) {
    byBreed <- h %>%
      html_nodes("table.roundy") %>%
      .[[5]] %>%
      html_table(fill = TRUE) %>%
      as_tibble() %>%
      slice(5:nrow(.)) %>%
      select(2:6) %>%
      mutate(cat = "Depends on Type")
    
    colnames(byBreed) <- c("Move", "Type", "Pwr.", "Acc.", "PP", "Cat.")
  } else {
    byBreed <- tibble(
      Move = "NA",
      Type = "NA",
      `Pwr.` = "NA",
      `Acc.` = "NA",
      PP = "NA",
      `Cat.` = "NA",
    )
  }
  
  byBreed <- byBreed %>%
    as_tibble() %>%
    mutate(
      Level = "",
      Method = "Breed"
    )
  
  learnset <- bind_rows(byLevel, byMachine, byBreed)
  as_tibble(learnset) %>%
    filter(!str_detect(Type, "(Type|Bold|Move)")) %>%
    mutate(
      `Acc.` = case_when(
        grepl("—", `Acc.`) ~ gsub(".*", "—%", `Acc.`),
        grepl("\\d*}}", `Acc.`) ~ gsub("\\d*}}", "", `Acc.`),
        grepl("\\d{3}[^%]", `Acc.`) ~ gsub("^\\d{3}", "", `Acc.`),
        TRUE ~ `Acc.`
      ),
      `Pwr.` = case_when(
        grepl("—", `Pwr.`) ~ gsub(".*", "—", `Pwr.`),
        grepl("^0", `Pwr.`) ~ substring(`Pwr.`, 4, 5),
        grepl("\\d",`Pwr.`) ~ substring(`Pwr.`, 0, 3),
        TRUE ~ `Pwr.`
      )
    ) %>%
    select("Level", "Move", "Type", "Cat.", "Pwr.", "Acc.", "PP", "Method")
}



# Get Pokemon by Location

getLocations <- function(genNo) {
  genNo <- case_when(
    genNo == "I" ~ 1,
    genNo == "II" ~ 2,
    genNo == "III" ~ 3,
    genNo == "IV" ~ 4,
    genNo == "V" ~ 5,
    genNo == "VI" ~ 6,
    genNo == "VII" ~ 7,
    genNo == "VIII" ~ 8,
    TRUE ~ 0
  )
  rregions <- GET(str_glue("https://pokeapi.co/api/v2/region/{id}", id = genNo))
  stop_for_status(rregions)
  jregions <- content(rregions, type = "text", encoding = "UTF-8") %>% fromJSON(flatten = TRUE)

  # locations in generation 3
  locs <- jregions$locations$name
}



getLocationAreas <- function(loc) {
  rlocations <- GET(str_glue("https://pokeapi.co/api/v2/location/{name}/", name = loc))
  stop_for_status(rlocations)
  jlocations <- content(rlocations, type = "text", encoding = "UTF-8") %>% fromJSON(flatten = TRUE)
  location_areas <- jlocations$areas$name
}

#check for alola and galar, no areas available


getAreaMons <- function(area) {
  rlocation_area <- GET(str_glue("https://pokeapi.co/api/v2/location-area/{name}/", name = area))
  stop_for_status(rlocation_area)
  jlocation_area <- content(rlocation_area, type = "text", encoding = "UTF-8") %>% fromJSON(flatten = TRUE)
  pokemon_by_area <- jlocation_area$pokemon_encounters$pokemon.name %>%
    gsub("-", " ", .) %>%
    str_to_title()
  tibble(pokemon_by_area) %>%
    rename(Pokemon = pokemon_by_area)
}

# Get locations by generation
getLocations_withAreas <- function(gen){
  genLoc <- getLocations(gen)
  genLocAreas_withNull <- sapply(genLoc, function(loc) getLocationAreas(loc))
  genLocAreas <- compact(genLocAreas_withNull)
  genLoc[which(genLoc %in% names(genLocAreas))]
}

#kanto <- getLocations_withAreas("I")
#jhoto <- getLocations_withAreas("II")
#hoenn <- getLocations_withAreas("III")
#sinnoh <- getLocations_withAreas("IV")
#unova <- getLocations_withAreas("V")
#kalos <- getLocations_withAreas("VI")

# Not included in API yet
#alola <- getLocations_withAreas("VII")
#galar <- getLocations_withAreas("VIII")

# save(kanto, file = "area_Kanto.RData")
# save(jhoto, file = "area_Jhoto.RData")
# save(hoenn, file = "area_Hoenn.RData")
# save(sinnoh, file = "area_Sinnoh.RData")
# save(unova, file = "area_Unova.RData")
# save(kalos, file = "area_Kalos.RData")

#Not run
#save(alola, file = "alolaAreas.RData")

load("area_Kanto.RData")
load("area_Jhoto.RData")
load("area_Hoenn.RData")
load("area_Sinnoh.RData")
load("area_Unova.RData")
load("area_Kalos.RData")







getGenFams <- function(i) {
  genFam <- h %>%
    html_nodes("table.roundy") %>%
    .[[i]] %>%
    html_table(fill = TRUE) %>%
    as_tibble(.name_repair = "unique")

  genFam <- genFam %>%
    select(2, 5, 8) %>%
    rename(
      Basic = names(genFam)[2],
      Stage2 = names(genFam)[5],
      Stage3 = names(genFam)[8]
    ) %>%
    filter(!str_detect(Basic, "family"))
}

# allGenFams <- lapply(1:8, function(x) getGenFams(x)) %>% bind_rows()
load("allFams.RData")


### Page 3 ###

# Note each page has different structure
# Function only applies to gen 3

getItemsInfo <- function(psyGen = "rs"){
  hEmItems <- read_html(str_glue("http://www.psypokes.com/{name}/items.php", name = psyGen))
  
  Market <- hEmItems %>% 
    html_nodes("table.psypoke") %>% 
    .[[1]] %>% 
    html_table() %>% 
    select(2,4,5) %>% 
    mutate(Class = "Market")
  
  Held <- hEmItems %>% 
    html_nodes("table.psypoke") %>% 
    .[[2]] %>% 
    html_table() %>% 
    select(2:4) %>% 
    mutate(Class = "Held")
  
  Balls <- hEmItems %>% 
    html_nodes("table.psypoke") %>% 
    .[[3]] %>% 
    html_table() %>% 
    select(2:4) %>% 
    mutate(Class = "Ball")
  
  Mail <- hEmItems %>% 
    html_nodes("table.psypoke") %>% 
    .[[4]] %>% 
    html_table() %>% 
    select(2:4) %>% 
    mutate(Class = "Mail")
  
  Base <- hEmItems %>% 
    html_nodes("table.psypoke") %>% 
    .[[6]] %>% 
    html_table() %>% 
    select(2:4) %>% 
    mutate(Class = "Secret Base",
           X3 = "Decor")
  
  cNames <- c("Item","Desc","Location","Class")
  
  colnames(Market) <- cNames
  colnames(Held) <- cNames
  colnames(Balls) <- cNames
  colnames(Mail) <- cNames
  colnames(Base) <- cNames
  
  (genItems <- bind_rows(Market, Held, Balls, Mail, Base) %>% 
      filter(!str_detect(Item,"(Main Items|Name)")))
}

load("Gen3.Findable.Items.RData")

getTMInfo <- function(genName) {
  hTM <- read_html(str_glue("https://pokemondb.net/{name}/tms", name = genName))
  TMs <- hTM %>%
    html_nodes("table.data-table") %>%
    .[[1]] %>%
    html_table() %>%
    as_tibble()
  (TMs <- TMs %>%
      select(TM, Name, Type, Power, `Acc.`, PP, Location) %>%
      mutate(
        Location = case_when(
          grepl("\n", Location) ~ gsub("\n", ";", Location),
          TRUE ~ Location
        ),
        Version = genName,
        `Item Class` = "TM"
      ))
}
genNames <- c("red-blue","gold-silver","ruby-sapphire","diamond-pearl","black-white","x-y","sun-moon","sword-shield")
allTMs_byGen <- lapply(genNames, function(x) getTMInfo(x)) %>% bind_rows()



# Locations and Pokemon for Gen 7 (scraped)
h <- read_html("https://pokemondb.net/location#tab-alola")
alolaLinks <- h %>% 
  html_nodes("div#loc-alola") %>% 
  html_nodes("a") %>% 
  html_attr("href")

getG7Mon <- function(loc_link){
  h <- read_html(paste0("https://pokemondb.net/",loc_link))
  df <- h %>% 
    html_nodes("a.ent-name") %>% 
    html_text() %>% 
    as_tibble()
  df %>% mutate(src = loc_link %>% gsub("(/location/)","",.)) %>% 
    rename(Name = value)
}

alola_byloc <- lapply(alolaLinks,function(link) getG7Mon(link)) %>% bind_rows()
alola <- alola_byloc$src %>% unique()
#save(alola_byloc, file = "alolaMon.RData")




# cl <- makeCluster(4)
# clusterEvalQ(cl, {
#   library(tidyverse)
#   library(jsonlite)
#   library(lubridate)
#   library(httr)
#   library(rvest)
#   NULL
# })

# clusterExport(cl,c("getLocationAreas","getAreaMons","EmLoc"), envir = environment())

# EmLocAreas <- parSapply(cl,EmLoc, function(loc) getLocationAreas(loc))
# clusterExport(cl,c("EmLocAreas"), envir = environment())

# stopCluster(cl)
