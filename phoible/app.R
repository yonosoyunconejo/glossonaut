library("devtools")
devtools::install_github("hkauhanen/ritwals")

library("shiny")
library("bslib")
library("ritwals")
library("tidyverse")
library("magrittr")
library("DT")
library("leaflet")

phoible <- read_csv(url("https://github.com/phoible/dev/blob/master/data/phoible.csv?raw=true"),
                    col_types = cols(InventoryID = "i", Marginal = "l", .default = "c"))

glottolog <- read_csv(url("https://cdstar.shh.mpg.de/bitstreams/EAEA0-18EC-5079-0173-0/languages_and_dialects_geo.csv"),
                      col_types = cols(latitude = "d", longitude = "d", .default = "c")) %>%
  select(-isocodes, -level, -name) %>%
  mutate(latitude = case_when(glottocode == "gana1278" ~ -latitude, TRUE ~ latitude))

phoible <- left_join(phoible, glottolog, by = c("Glottocode" = "glottocode"))

wals <- WALS %>%
  select(glottocode, family, genus, countrycodes) %>%
  unique() %>%
  mutate(countrycodes = case_when(glottocode == "jola1262" ~ "GM SN", TRUE ~ countrycodes))

phoible <- left_join(phoible, wals, by = c("Glottocode" = "glottocode"), relationship = "many-to-many")

phoible %<>%
  mutate(
    LanguageName = str_to_title(LanguageName),
    Allophones = case_when(is.na(Allophones) ~ Phoneme, TRUE ~ Allophones),
    Marginal = case_when(is.na(Marginal) ~ "Unspecified", Marginal == TRUE ~ "Yes", Marginal == FALSE ~ "No"),
    SegmentClass = str_to_sentence(SegmentClass),
    Source = str_to_upper(Source)
  ) %>%
  rename_all(~ paste0(toupper(substr(.x, 1, 1)), substr(.x, 2, nchar(.x)))) %>%
  rename(
    `Inventory ID` = InventoryID, Language = LanguageName, Dialect = SpecificDialect, `Segment class` = SegmentClass, `Delayed release` = DelayedRelease,`Retracted tongue root` = RetractedTongueRoot, `Advanced tongue root` = AdvancedTongueRoot, `Periodic glottal source` = PeriodicGlottalSource, `Epilaryngeal Source` = EpilaryngealSource, `Spread glottis` = SpreadGlottis, `Constricted glottis` = ConstrictedGlottis, `Raised larynx ejective` = RaisedLarynxEjective, `Lowered larynx implosive` = LoweredLarynxImplosive, `Country codes` = Countrycodes)

phoible$Marginal <- factor(phoible$Marginal, levels = c("Yes", "No", "Unspecified"))
phoible$`Segment class` <- factor(phoible$`Segment class`, levels = c("Consonant", "Vowel", "Tone"))

special_pull <- function(data, column, split = FALSE) {
  if (split) {
    data <- data %>% separate_rows({{ column }}, sep = " ")
  }
  data %>%
    filter(!is.na({{ column }})) %>%
    pull({{ column }}) %>%
    as.character() %>%
    unique() %>%
    sort()
}

vfamily <- special_pull(phoible, Family)
vgenus <- special_pull(phoible, Genus)
vlanguage <- special_pull(phoible, Language)
vdialect <- special_pull(phoible, Dialect)
vsource <- special_pull(phoible, Source)

vmacroarea <- special_pull(phoible, Macroarea)
vcountrycodes <- special_pull(phoible, `Country codes`, split = TRUE)
vcountrycodes <- vcountrycodes[2:length(vcountrycodes)]

vphoneme <- special_pull(phoible, Phoneme)
vallophone <- special_pull(phoible, Allophones, split = TRUE)
vmarginal <- special_pull(phoible, Marginal)
vsegclass <- special_pull(phoible, `Segment class`)

vtone <- special_pull(phoible, Tone)
vstress <- special_pull(phoible, Stress)
vsyllabic <- special_pull(phoible, Syllabic)
vshort <- special_pull(phoible, Short)
vlong <- special_pull(phoible, Long)

vconsonantal <- special_pull(phoible, Consonantal)
vsonorant <- special_pull(phoible, Sonorant)
vcontinuant <- special_pull(phoible, Continuant)
vdelayedrelease <- special_pull(phoible, `Delayed release`)
vapproximant <- special_pull(phoible, Approximant)
vtap <- special_pull(phoible, Tap)
vtrill <- special_pull(phoible, Trill)
vnasal <- special_pull(phoible, Nasal)
vlateral <- special_pull(phoible, Lateral)
vlabial <- special_pull(phoible, Labial)
vround <- special_pull(phoible, Round)
vlabiodental <- special_pull(phoible, Labiodental)
vcoronal <- special_pull(phoible, Coronal)
vanterior <- special_pull(phoible, Anterior)
vdistributed <- special_pull(phoible, Distributed)
vstrident <- special_pull(phoible, Strident)
vdorsal <- special_pull(phoible, Dorsal)
vhigh <- special_pull(phoible, High)
vlow <- special_pull(phoible, Low)
vfront <- special_pull(phoible, Front)
vback <- special_pull(phoible, Back)
vtense <- special_pull(phoible, Tense)
vrtr <- special_pull(phoible, `Retracted tongue root`)
vatr <- special_pull(phoible, `Advanced tongue root`)
vperiodic <- special_pull(phoible, `Periodic glottal source`)
vepilaryngeal <- special_pull(phoible, `Epilaryngeal Source`)
vspreadglottis <- special_pull(phoible, `Spread glottis`)
vconstrictedglottis <- special_pull(phoible, `Constricted glottis`)
vfortis <- special_pull(phoible, Fortis)
vlenis <- special_pull(phoible, Lenis)
vejective <- special_pull(phoible, `Raised larynx ejective`)
vimplosive <- special_pull(phoible, `Lowered larynx implosive`)
vclick <- special_pull(phoible, Click)

leaflet_colours <- as.factor(c("red", "darkred", "lightred", "orange", "beige", "green", "darkgreen", "lightgreen", "blue", "darkblue", "lightblue", "purple", "darkpurple", "pink", "cadetblue", "gray", "lightgray", "black"))

leaflet_colours <- factor(leaflet_colours, levels = sample(levels(leaflet_colours)))

assign_colour <- function(phoible_col) {
  colour_index <- as.integer(factor(phoible_col)) %% length(leaflet_colours) + 1
  return(leaflet_colours[colour_index])
}

phoible %<>%
  mutate(Colour = assign_colour(Family)) %>%
  arrange(Language, Dialect, Source)

ui <- page_fillable(
  
  fillable = TRUE,
  
  title = "Glossonaut — PHOIBLE",
  
  theme = bs_theme(
    bg = "#ffffff",
    fg = "#000000",
    primary = "#aa0000",
    secondary = "#0000bb",
    success = "#00bb00",
    base_font = font_google("Noto Sans"),
    code_font = font_google("Noto Sans Mono")
  ),
  
  navset_card_tab(
    
    title = "Glossonaut: Online Interfaces For Exploring Linguistic Databases — PHOIBLE",
    
    sidebar = sidebar(
      
      markdown("**Language Filters**"),
      
      selectizeInput("family", "Family", choices = NULL, multiple = TRUE),
      selectizeInput("genus", "Genus", choices = NULL, multiple = TRUE),
      selectizeInput("language", "Language", choices = NULL, multiple = TRUE),
      selectizeInput("dialect", "Dialect", choices = NULL, multiple = TRUE),
      selectInput("source", "Source", vsource, selected = "UPSID", multiple = TRUE),
      
      markdown("**Geographic Filters**"),
      
      selectInput("macroarea", "Macroarea", vmacroarea, selected = vmacroarea, multiple = TRUE),
      selectizeInput("countrycodes", "Country codes", choices = NULL, multiple = TRUE),
      
      markdown("**S(upra)egment Filters**"),
      
      selectizeInput("phoneme", "Present phonemes", choices = NULL, multiple = TRUE),
      selectizeInput("allophone", "Present allophones", choices = NULL, multiple = TRUE),
      selectizeInput("negphoneme", "Absent phonemes", choices = NULL, multiple = TRUE),
      selectizeInput("negallophone", "Absent allophones", choices = NULL, multiple = TRUE),
      selectInput("marginal", "Marginal phonemes",
                  choices = c("Yes", "No", "Unspecified"),
                  selected = c("Yes", "No", "Unspecified"),
                  multiple = TRUE),
      selectInput("segclass", "S(upra)egment class",
                  choices = c("Consonant", "Vowel", "Tone"),
                  selected = c("Consonant", "Vowel", "Tone"),
                  multiple = TRUE),
      
      markdown("**Suprasegmental Feature Filters**"),
      
      selectInput("tone", "Tone", choices = vtone, selected = vtone, multiple = TRUE),
      selectInput("stress", "Stress", choices = vstress, selected = vstress, multiple = TRUE),
      selectInput("syllabic", "Syllabic", choices = vsyllabic, selected = vsyllabic, multiple = TRUE),
      selectInput("short", "Short", choices = vshort, selected = vshort, multiple = TRUE),
      selectInput("long", "Long", choices = vlong, selected = vlong, multiple = TRUE),
      
      markdown("**Segmental Feature Filters**"),
      
      selectInput("consonantal", "Consonantal", choices = vconsonantal, selected = vconsonantal, multiple = TRUE),
      selectInput("sonorant", "Sonorant", choices = vsonorant, selected = vsonorant, multiple = TRUE),
      selectInput("continuant", "Continuant", choices = vcontinuant, selected = vcontinuant, multiple = TRUE),
      selectInput("delayedrelease", "Delayed release", choices = vdelayedrelease, selected = vdelayedrelease, multiple = TRUE),
      selectInput("approximant", "Approximant", choices = vapproximant, selected = vapproximant, multiple = TRUE),
      selectInput("tap", "Tap", choices = vtap, selected = vtap, multiple = TRUE),
      selectInput("trill", "Trill", choices = vtrill, selected = vtrill, multiple = TRUE),
      selectInput("nasal", "Nasal", choices = vnasal, selected = vnasal, multiple = TRUE),
      selectInput("lateral", "Lateral", choices = vlateral, selected = vlateral, multiple = TRUE),
      selectInput("labial", "Labial", choices = vlabial, selected = vlabial, multiple = TRUE),
      selectInput("round", "Round", choices = vround, selected = vround, multiple = TRUE),
      selectInput("labiodental", "Labiodental", choices = vlabiodental, selected = vlabiodental, multiple = TRUE),
      selectInput("coronal", "Coronal", choices = vcoronal, selected = vcoronal, multiple = TRUE),
      selectInput("anterior", "Anterior", choices = vanterior, selected = vanterior, multiple = TRUE),
      selectInput("distributed", "Distributed", choices = vdistributed, selected = vdistributed, multiple = TRUE),
      selectInput("strident", "Strident", choices = vstrident, selected = vstrident, multiple = TRUE),
      selectInput("dorsal", "Dorsal", choices = vdorsal, selected = vdorsal, multiple = TRUE),
      selectInput("high", "High", choices = vhigh, selected = vhigh, multiple = TRUE),
      selectInput("low", "Low", choices = vlow, selected = vlow, multiple = TRUE),
      selectInput("front", "Front", choices = vfront, selected = vfront, multiple = TRUE),
      selectInput("back", "Back", choices = vback, selected = vback, multiple = TRUE),
      selectInput("tense", "Tense", choices = vtense, selected = vtense, multiple = TRUE),
      selectInput("rtr", "Retracted tongue root", choices = vrtr, selected = vrtr, multiple = TRUE),
      selectInput("atr", "Advanced tongue root", choices = vatr, selected = vatr, multiple = TRUE),
      selectInput("periodic", "Periodic glottal source", choices = vperiodic, selected = vperiodic, multiple = TRUE),
      selectInput("epilaryngeal", "Epilaryngeal Source", choices = vepilaryngeal, selected = vepilaryngeal, multiple = TRUE),
      selectInput("spreadglottis", "Spread glottis", choices = vspreadglottis, selected = vspreadglottis, multiple = TRUE),
      selectInput("constrictedglottis", "Constricted glottis", choices = vconstrictedglottis, selected = vconstrictedglottis, multiple = TRUE),
      selectInput("fortis", "Fortis", choices = vfortis, selected = vfortis, multiple = TRUE),
      selectInput("lenis", "Lenis", choices = vlenis, selected = vlenis, multiple = TRUE),
      selectInput("ejective", "Raised larynx ejective", choices = vejective, selected = vejective, multiple = TRUE),
      selectInput("implosive", "Lowered larynx implosive", choices = vimplosive, selected = vimplosive, multiple = TRUE),
      selectInput("click", "Click", choices = vclick, selected = vclick, multiple = TRUE)
      
    ),
    
    nav_panel(
      
      card_header("Languages Map"),
      
      card_body(
        
        leafletOutput("phoible_map")
        
      )
      
    ),
    
    nav_panel(
      
      card_header("Inventories Table"),
      
      card_body(
        
        DTOutput("inventories_table")
        
      )
      
    ),
    
    nav_panel(
      
      card_header("Phonemes Table"),
      
      card_body(
        
        checkboxInput(inputId = "show_features",
                      label = "Show features as additional columns?",
                      value = FALSE,
                      width = "100%"),
        
        DTOutput("phonemes_table")
        
      )
      
    ),
    
    nav_panel(
      
      card_header("About"),
      
      card_body(
        
        markdown("This 𝒮𝒽𝒾𝓃𝓎 app is the second in the **Glossonaut** collection of online interfaces for exploring linguistic databases (see also [Glossonaut WALS](https://stephen-nichols.shinyapps.io/glossonaut-wals/)). It provides an alternative way of examining the data underpinning the [Phonetics Information Base and Lexicon (PHOIBLE)](https://phoible.org/). The data are pulled directly from a [CSV](https://github.com/phoible/dev/blob/master/data/phoible.csv) of the PHOIBLE database hosted on GitHub. This is supplemented by location data pulled from [Glottolog](https://glottolog.org/) ([CSV sourced here](https://cdstar.shh.mpg.de/bitstreams/EAEA0-18EC-5079-0173-0/languages_and_dialects_geo.csv)) and genetic information from [WALS](https://wals.info/) imported with the R package [`ritwals`](https://hkauhanen.github.io/ritwals/) by [Henri Kauhanen](https://hkauhanen.fi/).
        
        You might also want to take a look at [Psmith](https://defseg.io/psmith/), an alternative to Glossonaut PHOIBLE, and the successor to [Pshrimp](https://defseg.io/pshrimp-client/) (both long predate Glossonaut). This has many of the same capabilities as this app but is lacking some, though it does have some this does not (yet).
        
        The **\"Languages Map\"** tab produces a map of languages according to the input in the sidebar. If you select mutually exclusive combinations, no languages will be found! For example, selecting the family \"Chiquito\" will display no languages if the data set is also filtered to the source \"UPSID\" only (as is the default). Filtering instead to the source \"SAPHON\" will show only Bésɨro, the only Chiquito language in the data set. This same logic applies to other combinations of filters. The filtered data can also be viewed in the **\"Inventories Table\"** and **\"Phonemes Table\"** tabs. The \"Inventories Table\" tab shows full phoneme inventories for every language returned by the filter settings. The \"Phonemes Table\" tab displays individial phonemes and its allophones within a language. By default, featural specifications, as encoded on PHOIBLE, are hidden but can optionally be revealed as additional columns in a wide-format table.
        
        On the map, language markers are currently colour-coded according to which family each language belongs, though, due to the limited number of colours, available in [`leaflet`](https://cran.r-project.org/web/packages/leaflet/), each colour is assigned to multiple families. Implementing colour-coding for other options is something on the table for future versions of Glossonaut PHOIBLE.
        
        For more information on the data viewable here, please consult [PHOIBLE](https://phoible.org/) itself.
        
        The code is available on [GitHub](https://github.com/yonosoyunconejo/glossonaut).
        
        By [Stephen Nichols](https://www.stephen-nichols.me/).")
        
      )
      
    )
    
  )
  
)

server <- function(input, output, session) {
  
  observe({
    updateSelectizeInput(session, "family", choices = c("All families", vfamily), selected = "All families", server = TRUE)
  })
  
  observe({
    updateSelectizeInput(session, "genus", choices = c("All genera", vgenus), selected = "All genera", server = TRUE)
  })
  
  observe({
    updateSelectizeInput(session, "language", choices = c("All languages", vlanguage), selected = "All languages", server = TRUE)
  })
  
  observe({
    updateSelectizeInput(session, "dialect", choices = c("All dialects", vdialect), selected = "All dialects", server = TRUE)
  })
  
  observe({
    updateSelectizeInput(session, "countrycodes", choices = c("All country codes", vcountrycodes), selected = "All country codes", server = TRUE)
  })
  
  observe({
    updateSelectizeInput(session, "phoneme", choices = c("Any phonemes", vphoneme), selected = "Any phonemes", server = TRUE)
  })
  
  observe({
    updateSelectizeInput(session, "allophone", choices = c("Any allophones", vallophone), selected = "Any allophones", server = TRUE)
  })
  
  observe({
    updateSelectizeInput(session, "negphoneme", choices = c("No phonemes", vphoneme), selected = "No phonemes", server = TRUE)
  })
  
  observe({
    updateSelectizeInput(session, "negallophone", choices = c("No allophones", vallophone), selected = "No allophones", server = TRUE)
  })
  
  filtered_phoible <- reactive({
    
    filtered_phoible <- phoible
    
    if (!("All families" %in% input$family)) {
      filtered_phoible %<>% filter(Family %in% input$family)
    }
    
    if (!("All genera" %in% input$genus)) {
      filtered_phoible %<>% filter(Genus %in% input$genera)
    }
    
    if (!("All languages" %in% input$language)) {
      filtered_phoible %<>% filter(Language %in% input$language)
    }
    
    if (!("All dialects" %in% input$dialect)) {
      filtered_phoible %<>% filter(Dialect %in% input$dialect)
    }
    
    filtered_phoible %<>% filter(Source %in% input$source)
    
    filtered_phoible %<>% filter(Macroarea %in% input$macroarea)
    
    if (!("All country codes" %in% input$countrycodes)) {
      filtered_phoible %<>% filter(`Country codes` %in% input$countrycodes)
    }
    
    if (!("No phonemes" %in% input$negphoneme)) {
      filtered_phoible %<>%
        group_by(`Inventory ID`) %>%
        filter(!all(Phoneme %in% input$negphoneme) & !any(input$negphoneme %in% Phoneme)) %>%
        ungroup()
    }
    
    if (!("No allophones" %in% input$negallophone)) {
      filtered_phoible %<>%
        group_by(`Inventory ID`) %>%
        filter(!all(Allophones %in% input$negallophone) & !any(input$negallophone %in% Phoneme)) %>%
        ungroup()
    }
    
    if (!("Any phonemes" %in% input$phoneme)) {
      filtered_phoible %<>%
        group_by(`Inventory ID`) %>%
        filter(Phoneme %in% input$phoneme & all(input$phoneme %in% Phoneme)) %>%
        ungroup()
    }
    
    if (!("Any allophones" %in% input$allophone)) {
      filtered_phoible %<>%
        group_by(`Inventory ID`) %>%
        filter(Phoneme %in% input$phoneme & all(input$phoneme %in% Phoneme)) %>%
        ungroup()
    }
    
    filtered_phoible %<>% filter(Marginal %in% input$marginal)
    
    filtered_phoible %<>% filter(`Segment class` %in% input$segclass)
    
    filtered_phoible %<>% filter(Tone %in% input$tone)
    
    filtered_phoible %<>% filter(Stress %in% input$stress)

    filtered_phoible %<>% filter(Syllabic %in% input$syllabic)

    filtered_phoible %<>% filter(Short %in% input$short)
    
    filtered_phoible %<>% filter(Long %in% input$long)
    
    filtered_phoible %<>% filter(Consonantal %in% input$consonantal)
    
    filtered_phoible %<>% filter(`Delayed release` %in% input$delayedrelease)
    
    filtered_phoible %<>% filter(Approximant %in% input$approximant)
    
    filtered_phoible %<>% filter(Tap %in% input$tap)
    
    filtered_phoible %<>% filter(Trill %in% input$trill)
    
    filtered_phoible %<>% filter(Nasal %in% input$nasal)
    
    filtered_phoible %<>% filter(Lateral %in% input$lateral)
    
    filtered_phoible %<>% filter(Labial %in% input$labial)
    
    filtered_phoible %<>% filter(Round %in% input$round)
    
    filtered_phoible %<>% filter(Labiodental %in% input$labiodental)
    
    filtered_phoible %<>% filter(Coronal %in% input$coronal)
    
    filtered_phoible %<>% filter(Anterior %in% input$anterior)
    
    filtered_phoible %<>% filter(Distributed %in% input$distributed)
    
    filtered_phoible %<>% filter(Strident %in% input$strident)
    
    filtered_phoible %<>% filter(Dorsal %in% input$dorsal)
    
    filtered_phoible %<>% filter(High %in% input$high)
    
    filtered_phoible %<>% filter(Low %in% input$low)
    
    filtered_phoible %<>% filter(Front %in% input$front)
    
    filtered_phoible %<>% filter(Back %in% input$back)
    
    filtered_phoible %<>% filter(`Retracted tongue root` %in% input$rtr)
    
    filtered_phoible %<>% filter(`Advanced tongue root` %in% input$atr)
    
    filtered_phoible %<>% filter(`Periodic glottal source` %in% input$periodic)
    
    filtered_phoible %<>% filter(`Epilaryngeal Source` %in% input$epilaryngeal)
    
    filtered_phoible %<>% filter(`Spread glottis` %in% input$spreadglottis)
    
    filtered_phoible %<>% filter(`Constricted glottis` %in% input$constrictedglottis)
    
    filtered_phoible %<>% filter(Fortis %in% input$fortis)
    
    filtered_phoible %<>% filter(Lenis %in% input$lenis)
    
    filtered_phoible %<>% filter(`Raised larynx ejective` %in% input$ejective)
    
    filtered_phoible %<>% filter(`Lowered larynx implosive` %in% input$implosive)
    
    filtered_phoible %<>% filter(Click %in% input$click)
    
    as.data.frame(filtered_phoible)
  })
  
  output$phoible_map <- renderLeaflet({
    
    filtered_phoible() %>%
      select(Family, Genus, Language, Dialect, Macroarea, `Country codes`, Source, Longitude, Latitude, Colour) %>%
      unique() %>%
      leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addAwesomeMarkers(
        lng = ~Longitude,
        lat = ~Latitude,
        popup = ~Language,
        icon = ~awesomeIcons(
          icon = "comment",
          library = "fa",
          iconColor = "#f2efe9",
          markerColor = Colour))
  })
  output$phonemes_table <- renderDataTable(server = TRUE, {
    
    if (input$show_features) {
      filtered_phoible() %>%
        select(-`Inventory ID`, -Glottocode, -ISO6393, -GlyphID, -Latitude, -Longitude, -Colour) %>%
        relocate(Genus, Family, Macroarea, `Country codes`, Source, .after = Dialect)
    } else {
      filtered_phoible() %>%
        select(Language, Dialect, Genus, Family, Macroarea, `Country codes`, Source, Phoneme, Allophones, Marginal, `Segment class`)
    }
    
  }, options = list(pageLength = 20), rownames = FALSE)
  
  output$inventories_table <- renderDataTable(server = TRUE, {
    
    inventories_to_keep <- filtered_phoible() %>%
      pull(`Inventory ID`)
    
    inventories <- phoible %>%
      group_by(`Inventory ID`, Language, Dialect, Genus, Family, Macroarea, `Country codes`, Source) %>%
      summarise(`Number of phonemes` = n(),
                Phonemes = str_c(Phoneme, collapse=" "),
                .groups = "drop") %>%
      filter(`Inventory ID` %in% inventories_to_keep) %>%
      select(-`Inventory ID`) %>%
      arrange(Language, Dialect, Source)
    
    as.data.frame(inventories)
    
  }, options = list(pageLength = 20), rownames = FALSE)
  
}

shinyApp(ui, server)
