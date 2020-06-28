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

romGen <- tibble(arabic = c(1,2,3,4,5,6,7,8),
                 roman = c("I","II","III","IV","V","VI","VII","VIII"),
                 versions = c("red-blue","gold-silver","ruby-sapphire","diamond-pearl","black-white","x-y","sun-moon","sword-shield"))
##########################    Work from pokeLocation   ############################

# Scrape info from all gens

getGenMons_info <- function(genNo){
  h <- read_html(str_glue("https://en.wikipedia.org/wiki/List_of_generation_{num}_Pok%C3%A9mon", num = genNo), encoding = "UTF-8")
  
  gen_arabic <- romGen %>% filter(roman == genNo) %>% pull(arabic)
  
  if(gen_arabic < 6){
    genMons <- h %>%
      html_nodes("table") %>%
      .[[2]] %>%
      html_table(fill = TRUE) %>%
      as.data.frame() 
  }else if (gen_arabic == 6){
    temp <- h %>% 
      html_nodes("table") %>% 
      .[[2]] %>% 
      html_table(fill = TRUE) %>% 
      as.data.frame()
    
    megaMons <- h %>% 
      html_nodes("table") %>% 
      .[[3]] %>% 
      html_table(fill = TRUE) %>% 
      as.data.frame()
    
    colnames(megaMons) <- names(temp)
    
    genMons <- bind_rows(temp,megaMons)
    
  }else if(gen_arabic == 7){
    temp <- h %>% 
      html_nodes("table") %>% 
      .[[3]] %>% 
      html_table(fill = TRUE) %>% 
      as.data.frame()
    
    alolaMons <- h %>% 
      html_nodes("table") %>% 
      .[[4]] %>% 
      html_table(fill = TRUE) %>% 
      as_tibble(.name_repair = "unique") %>% 
      mutate(`English name` = paste0(.[[1]]," Alola")) 
    
    colnames(alolaMons) <- names(temp)
    
    genMons <- bind_rows(temp,alolaMons)
    
  }else if(gen_arabic == 8){
    temp <- h %>% 
      html_nodes("table") %>% 
      .[[2]] %>% 
      html_table(fill = TRUE) %>% 
      as_tibble(.name_repair = "unique")
    
    galarMons <- h %>% 
      html_nodes("table") %>% 
      .[[3]] %>% 
      html_table(fill = TRUE) %>% 
      as_tibble(.name_repair = "unique") %>% 
      mutate(`English name` = paste0(.[[1]]," Galar"))
    
    gigantaMons <- h %>% 
      html_nodes("table") %>% 
      .[[4]] %>% 
      html_table(fill = TRUE) %>% 
      as_tibble(.name_repair = "unique") %>% 
      mutate(`English name` = paste0(.[[1]]," Gigantamax"))
    
    colnames(galarMons) <- names(temp)
    colnames(gigantaMons) <- names(temp)
    
    genMons <- bind_rows(temp, galarMons, gigantaMons)
    
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
        Gen = gen_arabic,
        GenX = genNo
      ) %>%
      distinct(Name, .keep_all = TRUE)) %>% 
    filter(!str_detect(Type1,"(Type(s)|Primary)"))
}


#Helps find which evolutions exist in which generation

allMons <- lapply(romGen$roman, function(NUM) suppressMessages(getGenMons_info(NUM))) %>% bind_rows
# NatNo Name Type1 Type2 Evolves Gen GenX
#save(allMons, file = "allMons.6.2020.RData")


# Get Pokemon by Generation

getMon <- function(genNo) {
  
  genNo <- romGen %>% filter(roman == genNo) %>% pull(arabic)

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
getLearnset_txt <- function(mon, gen) {
  h <- read_html(str_glue("https://bulbapedia.bulbagarden.net/w/index.php?title={Mon}_(Pok%C3%A9mon)/Generation_{gen}_learnset&action=edit", Mon = mon, gen = gen))
  
  gen_arabic <- romGen %>% filter(roman == gen) %>% pull(arabic)
  
  if(gen_arabic > 3){
    gen_colnames <- c("Level","Move", "Type", "Cat.", "Pwr.", "Acc.", "PP")
    tutor_colnames <- c("Move","Type","Cat.","Pwr.","Acc.","PP")
  }else{
    gen_colnames <- c("Level","Move", "Type", "Pwr.", "Acc.", "PP")
    tutor_colnames <- c("Move","Type","Pwr.","Acc.","PP")
  }
  
  learnset <- h %>%
    html_nodes("textarea") %>%
    html_text() %>%
    strsplit("\\n") %>%
    .[[1]]
  
  # Level
  byLevel <- learnset %>%
    str_subset("level") %>%
    str_extract("(?<=\\{\\{learnlist/level\\d\\|).*(?=\\}\\})") %>%
    str_subset("NA", negate = TRUE) %>%
    as_tibble() %>%
    separate(value, gen_colnames, sep = "\\|", extra = "drop") %>%
    mutate(Method = "Level")
  
  byMachine <- learnset %>%
    str_subset("tm") %>%
    str_extract("(?<=\\{\\{learnlist/tm\\d\\|).*(?=\\}\\})") %>%
    str_subset("NA", negate = TRUE) %>%
    as_tibble() %>%
    separate(value, gen_colnames, sep = "\\|", extra = "drop") %>%
    mutate(Level = "", Method = "Machine")
  
  byBreed <- learnset %>%
    str_subset("breed") %>%
    str_extract("(?<=\\{\\{learnlist/breed\\d\\|).*(?=\\}\\})") %>%
    str_subset("NA", negate = TRUE) %>% 
    gsub("\\{\\{.*\\}\\}","",.)%>%
    as_tibble() %>%
    separate(value, gen_colnames, sep = "\\|", extra = "drop") %>%
    mutate(Level = "", Method = "Breed")
  
  byTutor <- learnset %>%
    str_subset("tutor") %>%
    str_extract("(?<=\\{\\{learnlist/tutor\\d\\|).*(?=\\}\\})") %>%
    str_subset("NA", negate = TRUE) %>%
    as_tibble() %>%
    separate(value, tutor_colnames, sep = "\\|", extra = "drop") %>%
    mutate(Level = "", Method = "Tutor")
  
  byEvent <- learnset %>%
    str_subset("event") %>%
    str_extract("(?<=\\{\\{learnlist/event\\d\\|).*(?=\\}\\})") %>%
    gsub("\\{\\{.*\\}\\}","",.) %>% 
    gsub("\\[.*\\]","",.) %>% 
    str_subset("NA", negate = TRUE) %>%
    as_tibble() %>%
    separate(value, gen_colnames, sep = "\\|", extra = "drop") %>%
    mutate(Level = "", Method = "Tutor", 
           temp = NULL, temp2 = NULL, 'Event Mon' = NULL)
  
  df <- bind_rows(byLevel, byMachine, byBreed, byTutor, byEvent) %>% 
    mutate(
      Pwr. = case_when(
        Pwr. == "&mdash;" ~ "-",
        TRUE ~ Pwr.
      ),
      Acc. = case_when(
        Acc. == "&mdash;" ~ "-",
        TRUE ~ Acc.
      )
    ) %>% 
    filter(Type %in% allTypes)
}




# Get Pokemon by Location

getLocations <- function(genNo) {
  
  genNo <- romGen %>% filter(roman == genNo) %>% pull(arabic)
  
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
