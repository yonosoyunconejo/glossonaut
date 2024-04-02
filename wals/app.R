library("devtools")
devtools::install_github("hkauhanen/ritwals")

library("shiny")
library("bslib")
library("ritwals")
library("tidyverse")
library("magrittr")
library("DT")
library("leaflet")

wals_wide <- WALS %>%
  select(-combined_ID, -code_ID, -iso_code, -glottocode, -contributors, -source) %>%
  unite("ID_feature", feature_ID:feature, sep = " - ") %>%
  unite("ID_value", value_ID:value, sep = " - ") %>%
  pivot_wider(names_from = ID_feature, values_from = ID_value) %>%
  mutate(language = str_trim(language)) %>%
  unique() %>%
  mutate(countrycodes = case_when(language == "Diola-Kasa" ~ "GM SN", TRUE ~ countrycodes)) %>%
  mutate(macroarea = case_when(
    countrycodes %in% c("LB", "MA", "ML") ~ "Africa",
    countrycodes == "AU" ~ "Australia",
    countrycodes %in% c("IN", "MN CN", "RU", "TR") ~ "Eurasia",
    countrycodes == "MX" ~ "North America",
    countrycodes %in% c("ID", "PG") ~ "Papunesia",
    countrycodes %in% c("BR", "VR") ~ "South America",
    TRUE ~ macroarea)) %>%
  arrange(language_ID)

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

vfamily <- special_pull(wals_wide, family)
vgenus <- special_pull(wals_wide, genus)
vlanguage <- special_pull(wals_wide, language)
vmacroarea <- special_pull(wals_wide, macroarea)
vcountrycodes <- special_pull(wals_wide, countrycodes, split = TRUE)
vcountrycodes <- vcountrycodes[2:length(vcountrycodes)]

custom_sort <- function(x) {
  numbers <- gsub("\\D", "", x)
  non_numbers <- gsub("\\d", "", x)
  numbers_padded <- sprintf("%03d", as.numeric(numbers))
  combined <- paste0(numbers_padded, non_numbers)
  combined_sorted <- combined[order(combined)]
  combined_sorted <- gsub("^0+", "", combined_sorted)
  return(combined_sorted)
}

features <- colnames(wals_wide[,11:length(wals_wide)])
features <- custom_sort(features)

for (column in features) {
  var_name <- paste0("v", str_extract(column, "^[^\\s]+"))
  
  assign(var_name,
         wals_wide %>%
           filter(!is.na({{column}})) %>%
           pull({{column}}) %>%
           unique() %>%
           sort())
}

special_sort <- function(data) {
  data %>%
    as.character() %>%
    unique() %>%
    sort()
}

ui <- page_fillable(
  
  fillable = TRUE,
  
  title = "Glossonaut — WALS",
  
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
    
    title = "Glossonaut: Online Interfaces For Exploring Linguistic Databases — WALS",
    
    sidebar = sidebar(
      
      markdown("**Language Filters**"),
      
      selectInput("show_subset",
                  label = "Show all languages or only a subset?",
                  choices = list("WALS 100" = "w100", "WALS 200" = "w200", "All languages" = "all"),
                  selected = NULL),
      selectInput("family", "Family", c("All families", vfamily)),
      selectInput("genus", "Genus", c("All genera", vgenus)),
      selectizeInput("language", "Language", choices = c("All languages")),
      #selectInput("subvarieties", "Subvarieties", c("Include", "Exclude", "Only subvarieties")),
      selectInput("creoles_pidgins", "Creoles and pidgins", c("Include", "Exclude", "Only creoles and pidgins")),
      selectInput("sign_lgs", "Sign languages", c("Include", "Exclude", "Only sign languages")),
      
      markdown("**Geographic Filters**"),
      
      selectInput("macroarea", "Macroarea", c("All macroareas", vmacroarea)),
      selectInput("countrycodes", "Country codes", c("All country codes", vcountrycodes)),
      
      markdown("**Feature Filters**"),
      
      selectInput('f1A', '1A - Consonant Inventories', c('All values', v1A)),
      selectInput('f2A', '2A - Vowel Quality Inventories', c('All values', v2A)),
      selectInput('f3A', '3A - Consonant-Vowel Ratio', c('All values', v3A)),
      selectInput('f4A', '4A - Voicing in Plosives and Fricatives', c('All values', v4A)),
      selectInput('f5A', '5A - Voicing and Gaps in Plosive Systems', c('All values', v5A)),
      selectInput('f6A', '6A - Uvular Consonants', c('All values', v6A)),
      selectInput('f7A', '7A - Glottalized Consonants', c('All values', v7A)),
      selectInput('f8A', '8A - Lateral Consonants', c('All values', v8A)),
      selectInput('f9A', '9A - The Velar Nasal', c('All values', v9A)),
      selectInput('f10A', '10A - Vowel Nasalization', c('All values', v10A)),
      selectInput('f10B', '10B - Nasal Vowels in West Africa', c('All values', v10B)),
      selectInput('f11A', '11A - Front Rounded Vowels', c('All values', v11A)),
      selectInput('f12A', '12A - Syllable Structure', c('All values', v12A)),
      selectInput('f13A', '13A - Tone', c('All values', v13A)),
      selectInput('f14A', '14A - Fixed Stress Locations', c('All values', v14A)),
      selectInput('f15A', '15A - Weight-Sensitive Stress', c('All values', v15A)),
      selectInput('f16A', '16A - Weight Factors in Weight-Sensitive Stress Systems', c('All values', v16A)),
      selectInput('f17A', '17A - Rhythm Types', c('All values', v17A)),
      selectInput('f18A', '18A - Absence of Common Consonants', c('All values', v18A)),
      selectInput('f19A', '19A - Presence of Uncommon Consonants', c('All values', v19A)),
      selectInput('f20A', '20A - Fusion of Selected Inflectional Formatives', c('All values', v20A)),
      selectInput('f21A', '21A - Exponence of Selected Inflectional Formatives', c('All values', v21A)),
      selectInput('f21B', '21B - Exponence of Tense-Aspect-Mood Inflection', c('All values', v21B)),
      selectInput('f22A', '22A - Inflectional Synthesis of the Verb', c('All values', v22A)),
      selectInput('f23A', '23A - Locus of Marking in the Clause', c('All values', v23A)),
      selectInput('f24A', '24A - Locus of Marking in Possessive Noun Phrases', c('All values', v24A)),
      selectInput('f25A', '25A - Locus of Marking: Whole-language Typology', c('All values', v25A)),
      selectInput('f25B', '25B - Zero Marking of A and P Arguments', c('All values', v25B)),
      selectInput('f26A', '26A - Prefixing vs. Suffixing in Inflectional Morphology', c('All values', v26A)),
      selectInput('f27A', '27A - Reduplication', c('All values', v27A)),
      selectInput('f28A', '28A - Case Syncretism', c('All values', v28A)),
      selectInput('f29A', '29A - Syncretism in Verbal Person/Number Marking', c('All values', v29A)),
      selectInput('f30A', '30A - Number of Genders', c('All values', v30A)),
      selectInput('f31A', '31A - Sex-based and Non-sex-based Gender Systems', c('All values', v31A)),
      selectInput('f32A', '32A - Systems of Gender Assignment', c('All values', v32A)),
      selectInput('f33A', '33A - Coding of Nominal Plurality', c('All values', v33A)),
      selectInput('f34A', '34A - Occurrence of Nominal Plurality', c('All values', v34A)),
      selectInput('f35A', '35A - Plurality in Independent Personal Pronouns', c('All values', v35A)),
      selectInput('f36A', '36A - The Associative Plural', c('All values', v36A)),
      selectInput('f37A', '37A - Definite Articles', c('All values', v37A)),
      selectInput('f38A', '38A - Indefinite Articles', c('All values', v38A)),
      selectInput('f39A', '39A - Inclusive/Exclusive Distinction in Independent Pronouns', c('All values', v39A)),
      selectInput('f39B', '39B - Inclusive/Exclusive Forms in Pama-Nyungan', c('All values', v39B)),
      selectInput('f40A', '40A - Inclusive/Exclusive Distinction in Verbal Inflection', c('All values', v40A)),
      selectInput('f41A', '41A - Distance Contrasts in Demonstratives', c('All values', v41A)),
      selectInput('f42A', '42A - Pronominal and Adnominal Demonstratives', c('All values', v42A)),
      selectInput('f43A', '43A - Third Person Pronouns and Demonstratives', c('All values', v43A)),
      selectInput('f44A', '44A - Gender Distinctions in Independent Personal Pronouns', c('All values', v44A)),
      selectInput('f45A', '45A - Politeness Distinctions in Pronouns', c('All values', v45A)),
      selectInput('f46A', '46A - Indefinite Pronouns', c('All values', v46A)),
      selectInput('f47A', '47A - Intensifiers and Reflexive Pronouns', c('All values', v47A)),
      selectInput('f48A', '48A - Person Marking on Adpositions', c('All values', v48A)),
      selectInput('f49A', '49A - Number of Cases', c('All values', v49A)),
      selectInput('f50A', '50A - Asymmetrical Case-Marking', c('All values', v50A)),
      selectInput('f51A', '51A - Position of Case Affixes', c('All values', v51A)),
      selectInput('f52A', '52A - Comitatives and Instrumentals', c('All values', v52A)),
      selectInput('f53A', '53A - Ordinal Numerals', c('All values', v53A)),
      selectInput('f54A', '54A - Distributive Numerals', c('All values', v54A)),
      selectInput('f55A', '55A - Numeral Classifiers', c('All values', v55A)),
      selectInput('f56A', '56A - Conjunctions and Universal Quantifiers', c('All values', v56A)),
      selectInput('f57A', '57A - Position of Pronominal Possessive Affixes', c('All values', v57A)),
      selectInput('f58A', '58A - Obligatory Possessive Inflection', c('All values', v58A)),
      selectInput('f58B', '58B - Number of Possessive Nouns', c('All values', v58B)),
      selectInput('f59A', '59A - Possessive Classification', c('All values', v59A)),
      selectInput('f60A', '60A - Genitives, Adjectives and Relative Clauses', c('All values', v60A)),
      selectInput('f61A', '61A - Adjectives without Nouns', c('All values', v61A)),
      selectInput('f62A', '62A - Action Nominal Constructions', c('All values', v62A)),
      selectInput('f63A', '63A - Noun Phrase Conjunction', c('All values', v63A)),
      selectInput('f64A', '64A - Nominal and Verbal Conjunction', c('All values', v64A)),
      selectInput('f65A', '65A - Perfective/Imperfective Aspect', c('All values', v65A)),
      selectInput('f66A', '66A - The Past Tense', c('All values', v66A)),
      selectInput('f67A', '67A - The Future Tense', c('All values', v67A)),
      selectInput('f68A', '68A - The Perfect', c('All values', v68A)),
      selectInput('f69A', '69A - Position of Tense-Aspect Affixes', c('All values', v69A)),
      selectInput('f70A', '70A - The Morphological Imperative', c('All values', v70A)),
      selectInput('f71A', '71A - The Prohibitive', c('All values', v71A)),
      selectInput('f72A', '72A - Imperative-Hortative Systems', c('All values', v72A)),
      selectInput('f73A', '73A - The Optative', c('All values', v73A)),
      selectInput('f74A', '74A - Situational Possibility', c('All values', v74A)),
      selectInput('f75A', '75A - Epistemic Possibility', c('All values', v75A)),
      selectInput('f76A', '76A - Overlap between Situational and Epistemic Modal Marking', c('All values', v76A)),
      selectInput('f77A', '77A - Semantic Distinctions of Evidentiality', c('All values', v77A)),
      selectInput('f78A', '78A - Coding of Evidentiality', c('All values', v78A)),
      selectInput('f79A', '79A - Suppletion According to Tense and Aspect', c('All values', v79A)),
      selectInput('f79B', '79B - Suppletion in Imperatives and Hortatives', c('All values', v79B)),
      selectInput('f80A', '80A - Verbal Number and Suppletion', c('All values', v80A)),
      selectInput('f81A', '81A - Order of Subject, Object and Verb', c('All values', v81A)),
      selectInput('f81B', '81B - Languages with two Dominant Orders of Subject, Object, and Verb', c('All values', v81B)),
      selectInput('f82A', '82A - Order of Subject and Verb', c('All values', v82A)),
      selectInput('f83A', '83A - Order of Object and Verb', c('All values', v83A)),
      selectInput('f84A', '84A - Order of Object, Oblique, and Verb', c('All values', v84A)),
      selectInput('f85A', '85A - Order of Adposition and Noun Phrase', c('All values', v85A)),
      selectInput('f86A', '86A - Order of Genitive and Noun', c('All values', v86A)),
      selectInput('f87A', '87A - Order of Adjective and Noun', c('All values', v87A)),
      selectInput('f88A', '88A - Order of Demonstrative and Noun', c('All values', v88A)),
      selectInput('f89A', '89A - Order of Numeral and Noun', c('All values', v89A)),
      selectInput('f90A', '90A - Order of Relative Clause and Noun', c('All values', v90A)),
      selectInput('f90B', '90B - Prenominal relative clauses', c('All values', v90B)),
      selectInput('f90C', '90C - Postnominal relative clauses', c('All values', v90C)),
      selectInput('f90D', '90D - Internally-headed relative clauses', c('All values', v90D)),
      selectInput('f90E', '90E - Correlative relative clauses', c('All values', v90E)),
      selectInput('f90F', '90F - Adjoined relative clauses', c('All values', v90F)),
      selectInput('f90G', '90G - Double-headed relative clauses', c('All values', v90G)),
      selectInput('f91A', '91A - Order of Degree Word and Adjective', c('All values', v91A)),
      selectInput('f92A', '92A - Position of Polar Question Particles', c('All values', v92A)),
      selectInput('f93A', '93A - Position of Interrogative Phrases in Content Questions', c('All values', v93A)),
      selectInput('f94A', '94A - Order of Adverbial Subordinator and Clause', c('All values', v94A)),
      selectInput('f95A', '95A - Relationship between the Order of Object and Verb and the Order of Adposition and Noun Phrase', c('All values', v95A)),
      selectInput('f96A', '96A - Relationship between the Order of Object and Verb and the Order of Relative Clause and Noun', c('All values', v96A)),
      selectInput('f97A', '97A - Relationship between the Order of Object and Verb and the Order of Adjective and Noun', c('All values', v97A)),
      selectInput('f98A', '98A - Alignment of Case Marking of Full Noun Phrases', c('All values', v98A)),
      selectInput('f99A', '99A - Alignment of Case Marking of Pronouns', c('All values', v99A)),
      selectInput('f100A', '100A - Alignment of Verbal Person Marking', c('All values', v100A)),
      selectInput('f101A', '101A - Expression of Pronominal Subjects', c('All values', v101A)),
      selectInput('f102A', '102A - Verbal Person Marking', c('All values', v102A)),
      selectInput('f103A', '103A - Third Person Zero of Verbal Person Marking', c('All values', v103A)),
      selectInput('f104A', '104A - Order of Person Markers on the Verb', c('All values', v104A)),
      selectInput('f105A', '105A - Ditransitive Constructions: The Verb \'Give\'', c('All values', v105A)),
      selectInput('f106A', '106A - Reciprocal Constructions', c('All values', v106A)),
      selectInput('f107A', '107A - Passive Constructions', c('All values', v107A)),
      selectInput('f108A', '108A - Antipassive Constructions', c('All values', v108A)),
      selectInput('f108B', '108B - Productivity of the Antipassive Construction', c('All values', v108B)),
      selectInput('f109A', '109A - Applicative Constructions', c('All values', v109A)),
      selectInput('f109B', '109B - Other Roles of Applied Objects', c('All values', v109B)),
      selectInput('f110A', '110A - Periphrastic Causative Constructions', c('All values', v110A)),
      selectInput('f111A', '111A - Nonperiphrastic Causative Constructions', c('All values', v111A)),
      selectInput('f112A', '112A - Negative Morphemes', c('All values', v112A)),
      selectInput('f113A', '113A - Symmetric and Asymmetric Standard Negation', c('All values', v113A)),
      selectInput('f114A', '114A - Subtypes of Asymmetric Standard Negation', c('All values', v114A)),
      selectInput('f115A', '115A - Negative Indefinite Pronouns and Predicate Negation', c('All values', v115A)),
      selectInput('f116A', '116A - Polar Questions', c('All values', v116A)),
      selectInput('f117A', '117A - Predicative Possession', c('All values', v117A)),
      selectInput('f118A', '118A - Predicative Adjectives', c('All values', v118A)),
      selectInput('f119A', '119A - Nominal and Locational Predication', c('All values', v119A)),
      selectInput('f120A', '120A - Zero Copula for Predicate Nominals', c('All values', v120A)),
      selectInput('f121A', '121A - Comparative Constructions', c('All values', v121A)),
      selectInput('f122A', '122A - Relativization on Subjects', c('All values', v122A)),
      selectInput('f123A', '123A - Relativization on Obliques', c('All values', v123A)),
      selectInput('f124A', '124A - \'Want\' Complement Subjects', c('All values', v124A)),
      selectInput('f125A', '125A - Purpose Clauses', c('All values', v125A)),
      selectInput('f126A', '126A - \'When\' Clauses', c('All values', v126A)),
      selectInput('f127A', '127A - Reason Clauses', c('All values', v127A)),
      selectInput('f128A', '128A - Utterance Complement Clauses', c('All values', v128A)),
      selectInput('f129A', '129A - Hand and Arm', c('All values', v129A)),
      selectInput('f130A', '130A - Finger and Hand', c('All values', v130A)),
      selectInput('f130B', '130B - Cultural Categories of Languages with Identity of \'Finger\' and \'Hand\'', c('All values', v130B)),
      selectInput('f131A', '131A - Numeral Bases', c('All values', v131A)),
      selectInput('f132A', '132A - Number of Non-Derived Basic Colour Categories', c('All values', v132A)),
      selectInput('f133A', '133A - Number of Basic Colour Categories', c('All values', v133A)),
      selectInput('f134A', '134A - Green and Blue', c('All values', v134A)),
      selectInput('f135A', '135A - Red and Yellow', c('All values', v135A)),
      selectInput('f136A', '136A - M-T Pronouns', c('All values', v136A)),
      selectInput('f136B', '136B - M in First Person Singular', c('All values', v136B)),
      selectInput('f137A', '137A - N-M Pronouns', c('All values', v137A)),
      selectInput('f137B', '137B - M in Second Person Singular', c('All values', v137B)),
      selectInput('f138A', '138A - Tea', c('All values', v138A)),
      selectInput('f139A', '139A - Irregular Negatives in Sign Languages', c('All values', v139A)),
      selectInput('f140A', '140A - Question Particles in Sign Languages', c('All values', v140A)),
      selectInput('f141A', '141A - Writing Systems', c('All values', v141A)),
      selectInput('f142A', '142A - Para-Linguistic Usages of Clicks', c('All values', v142A)),
      selectInput('f143A', '143A - Order of Negative Morpheme and Verb', c('All values', v143A)),
      selectInput('f143B', '143B - Obligatory Double Negation', c('All values', v143B)),
      selectInput('f143C', '143C - Optional Double Negation', c('All values', v143C)),
      selectInput('f143D', '143D - Optional Triple Negation', c('All values', v143D)),
      selectInput('f143E', '143E - Preverbal Negative Morphemes', c('All values', v143E)),
      selectInput('f143F', '143F - Postverbal Negative Morphemes', c('All values', v143F)),
      selectInput('f143G', '143G - Minor morphological means of signaling negation', c('All values', v143G)),
      selectInput('f144A', '144A - Position of Negative Word With Respect to Subject, Object, and Verb', c('All values', v144A)),
      selectInput('f144B', '144B - Position of negative words relative to beginning and end of clause and with respect to adjacency to verb', c('All values', v144B)),
      selectInput('f144C', '144C - Languages with different word order in negative clauses', c('All values', v144C)),
      selectInput('f144D', '144D - The Position of Negative Morphemes in SVO Languages', c('All values', v144D)),
      selectInput('f144E', '144E - Multiple Negative Constructions in SVO Languages', c('All values', v144E)),
      selectInput('f144F', '144F - Obligatory Double Negation in SVO languages', c('All values', v144F)),
      selectInput('f144G', '144G - Optional Double Negation in SVO languages', c('All values', v144G)),
      selectInput('f144H', '144H - NegSVO Order', c('All values', v144H)),
      selectInput('f144I', '144I - SNegVO Order', c('All values', v144I)),
      selectInput('f144J', '144J - SVNegO Order', c('All values', v144J)),
      selectInput('f144K', '144K - SVONeg Order', c('All values', v144K)),
      selectInput('f144L', '144L - The Position of Negative Morphemes in SOV Languages', c('All values', v144L)),
      selectInput('f144M', '144M - Multiple Negative Constructions in SOV Languages', c('All values', v144M)),
      selectInput('f144N', '144N - Obligatory Double Negation in SOV languages', c('All values', v144N)),
      selectInput('f144O', '144O - Optional Double Negation in SOV languages', c('All values', v144O)),
      selectInput('f144P', '144P - NegSOV Order', c('All values', v144P)),
      selectInput('f144Q', '144Q - SNegOV Order', c('All values', v144Q)),
      selectInput('f144R', '144R - SONegV Order', c('All values', v144R)),
      selectInput('f144S', '144S - SOVNeg Order', c('All values', v144S)),
      selectInput('f144T', '144T - The Position of Negative Morphemes in Verb-Initial Languages', c('All values', v144T)),
      selectInput('f144U', '144U - Double negation in verb-initial languages', c('All values', v144U)),
      selectInput('f144V', '144V - Verb-Initial with Preverbal Negative', c('All values', v144V)),
      selectInput('f144W', '144W - Verb-Initial with Negative that is Immediately Postverbal or between Subject and Object', c('All values', v144W)),
      selectInput('f144X', '144X - Verb-Initial with Clause-Final Negative', c('All values', v144X)),
      selectInput('f144Y', '144Y - The Position of Negative Morphemes in Object-Initial Languages', c('All values', v144Y))
      
    ),
    
    nav_panel(
      
      card_header("Languages Map"),
      
      card_body(
        
        leafletOutput("wals_map")
        
      )
      
    ),
    
    nav_panel(
      
      card_header("Languages Table"),
      
      card_body(
        
        checkboxInput(inputId = "show_features",
                      label = "Show all features as additional columns?",
                      value = FALSE,
                      width = "100%"),
        
        DTOutput("wide_table")
        
      )
      
    ),
    
    nav_panel(
      
      card_header("Features Table"),
      
      card_body(
        
        textOutput("lg_selected"),
        
        checkboxInput(inputId = "show_missing_features",
                      label = "Show rows where feature values are missing?",
                      value = FALSE,
                      width = "100%"),
        
        DTOutput("long_table")
        
      )
      
    ),
    
    nav_panel(
      
      card_header("About"),
      
      card_body(
        markdown("This 𝒮𝒽𝒾𝓃𝓎 app is the first in what is intended to be a collection of online interfaces for exploring linguistic databases called **Glossonaut**. It provides an alternative way of examining the data underpinning the [World Atlas of Language Structures (WALS)](https://wals.info/). It uses the R package [`ritwals`](https://hkauhanen.github.io/ritwals/) by [Henri Kauhanen](https://hkauhanen.fi/) to import the WALS data.
        
        The **\"Languages Map\"** tab produces a map of languages with associated metadata filtered according to the input in the sidebar. If you select mutually exclusive combinations, no languages will be found! For example, selecting the family \"Panoan\" will display no languages if the data set is also filtered to \"WALS 100\" (as is the default). Filtering instead to \"WALS 200\" will show only Shipibo-Konibo, the only Panoan language in the WALS 200 list, and choosing \"All languages\" will output all eleven Panoan languages (unless other features are changed). This same logic applies to language, geographic and feature filters alike. This same filtered data can be viewed in the **\"Languages Table\"** and **\"Features Table\"** tabs. These two tabs are capable of show the same data just in different formats: the \"Languages Table\" uses a wide format whereas the  \"Features Table\" tab uses a long format. The \"Languages Table\" tab hides features and feature values by default but these can be shown as additional columns. The \"Features Table\" tab hides absent feature values by default but these can be show as additional rows.
        
        On the map, language markers are currently colour-coded according to which family each language belongs, though, due to the limited number of colours, available in [`leaflet`](https://cran.r-project.org/web/packages/leaflet/), each colour is assigned to multiple families.
        
        The code is available on [GitHub](https://github.com/yonosoyunconejo/glossonaut).
        
        By [Stephen Nichols](https://www.stephen-nichols.me/).")
        
      )
      
    )
    
  )
  
)

server <- function(input, output, session) {
  
  observe({
    updateSelectizeInput(session, "language", choices = c("All languages", vlanguage), server = TRUE)
  })
  
  filtered_wals <- reactive({
    
    filtered_wals <- wals_wide
    
    if (input$show_subset == "w100") {
      filtered_wals %<>% filter(in_WALS_100 == TRUE)
    } else if (input$show_subset == "w200") {
      filtered_wals %<>% filter(in_WALS_200 == TRUE)
    }
    
    if (input$creoles_pidgins == "Exclude") {
      filtered_wals %<>% filter(genus != "Creoles and Pidgins")
    } else if (input$creoles_pidgins == "Only creoles and pidgins") {
      filtered_wals %<>% filter(genus == "Creoles and Pidgins")
    }
    
    if (input$family != "All families") {
      filtered_wals %<>% filter(family == input$family)
    }
    
    if (input$genus != "All genera") {
      filtered_wals %<>% filter(genus == input$genus)
    }
    
    if (input$language != "All languages") {
      filtered_wals %<>% filter(language == input$language)
    }
    
    if (input$macroarea != "All macroareas") {
      filtered_wals %<>% filter(macroarea == input$macroarea)
    }
    
    if (input$countrycodes != "All country codes") {
      filtered_wals %<>% filter(str_detect(countrycodes, input$countrycodes))
    }
    
    if (input$sign_lgs == "Exclude") {
      filtered_wals %<>% filter(genus != "Sign Languages")
    } else if (input$sign_lgs == "Only sign languages") {
      filtered_wals %<>% filter(genus == "Sign Languages")
    }
    
    if (input$f1A != "All values") {
      filtered_wals %<>% filter(`1A - Consonant Inventories` == input$f1A)
    }
    
    if (input$f2A != "All values") {
      filtered_wals %<>% filter(`2A - Vowel Quality Inventories` == input$f2A)
    }
    
    if (input$f3A != "All values") {
      filtered_wals %<>% filter(`3A - Consonant-Vowel Ratio` == input$f3A)
    }
    
    if (input$f4A != "All values") {
      filtered_wals %<>% filter(`4A - Voicing in Plosives and Fricatives` == input$f4A)
    }
    
    if (input$f5A != "All values") {
      filtered_wals %<>% filter(`5A - Voicing and Gaps in Plosive Systems` == input$f5A)
    }
    
    if (input$f6A != "All values") {
      filtered_wals %<>% filter(`6A - Uvular Consonants` == input$f6A)
    }
    
    if (input$f7A != "All values") {
      filtered_wals %<>% filter(`7A - Glottalized Consonants` == input$f7A)
    }
    
    if (input$f8A != "All values") {
      filtered_wals %<>% filter(`8A - Lateral Consonants` == input$f8A)
    }
    
    if (input$f9A != "All values") {
      filtered_wals %<>% filter(`9A - The Velar Nasal` == input$f9A)
    }
    
    if (input$f10A != "All values") {
      filtered_wals %<>% filter(`10A - Vowel Nasalization` == input$f10A)
    }
    
    if (input$f10B != "All values") {
      filtered_wals %<>% filter(`10B - Nasal Vowels in West Africa` == input$f10B)
    }
    
    if (input$f11A != "All values") {
      filtered_wals %<>% filter(`11A - Front Rounded Vowels` == input$f11A)
    }
    
    if (input$f12A != "All values") {
      filtered_wals %<>% filter(`12A - Syllable Structure` == input$f12A)
    }
    
    if (input$f13A != "All values") {
      filtered_wals %<>% filter(`13A - Tone` == input$f13A)
    }
    
    if (input$f14A != "All values") {
      filtered_wals %<>% filter(`14A - Fixed Stress Locations` == input$f14A)
    }
    
    if (input$f15A != "All values") {
      filtered_wals %<>% filter(`15A - Weight-Sensitive Stress` == input$f15A)
    }
    
    if (input$f16A != "All values") {
      filtered_wals %<>% filter(`16A - Weight Factors in Weight-Sensitive Stress Systems` == input$f16A)
    }
    
    if (input$f17A != "All values") {
      filtered_wals %<>% filter(`17A - Rhythm Types` == input$f17A)
    }
    
    if (input$f18A != "All values") {
      filtered_wals %<>% filter(`18A - Absence of Common Consonants` == input$f18A)
    }
    
    if (input$f19A != "All values") {
      filtered_wals %<>% filter(`19A - Presence of Uncommon Consonants` == input$f19A)
    }
    
    if (input$f20A != "All values") {
      filtered_wals %<>% filter(`20A - Fusion of Selected Inflectional Formatives` == input$f20A)
    }
    
    if (input$f21A != "All values") {
      filtered_wals %<>% filter(`21A - Exponence of Selected Inflectional Formatives` == input$f21A)
    }
    
    if (input$f21B != "All values") {
      filtered_wals %<>% filter(`21B - Exponence of Tense-Aspect-Mood Inflection` == input$f21B)
    }
    
    if (input$f22A != "All values") {
      filtered_wals %<>% filter(`22A - Inflectional Synthesis of the Verb` == input$f22A)
    }
    
    if (input$f23A != "All values") {
      filtered_wals %<>% filter(`23A - Locus of Marking in the Clause` == input$f23A)
    }
    
    if (input$f24A != "All values") {
      filtered_wals %<>% filter(`24A - Locus of Marking in Possessive Noun Phrases` == input$f24A)
    }
    
    if (input$f25A != "All values") {
      filtered_wals %<>% filter(`25A - Locus of Marking: Whole-language Typology` == input$f25A)
    }
    
    if (input$f25B != "All values") {
      filtered_wals %<>% filter(`25B - Zero Marking of A and P Arguments` == input$f25B)
    }
    
    if (input$f26A != "All values") {
      filtered_wals %<>% filter(`26A - Prefixing vs. Suffixing in Inflectional Morphology` == input$f26A)
    }
    
    if (input$f27A != "All values") {
      filtered_wals %<>% filter(`27A - Reduplication` == input$f27A)
    }
    
    if (input$f28A != "All values") {
      filtered_wals %<>% filter(`28A - Case Syncretism` == input$f28A)
    }
    
    if (input$f29A != "All values") {
      filtered_wals %<>% filter(`29A - Syncretism in Verbal Person/Number Marking` == input$f29A)
    }
    
    if (input$f30A != "All values") {
      filtered_wals %<>% filter(`30A - Number of Genders` == input$f30A)
    }
    
    if (input$f31A != "All values") {
      filtered_wals %<>% filter(`31A - Sex-based and Non-sex-based Gender Systems` == input$f31A)
    }
    
    if (input$f32A != "All values") {
      filtered_wals %<>% filter(`32A - Systems of Gender Assignment` == input$f32A)
    }
    
    if (input$f33A != "All values") {
      filtered_wals %<>% filter(`33A - Coding of Nominal Plurality` == input$f33A)
    }
    
    if (input$f34A != "All values") {
      filtered_wals %<>% filter(`34A - Occurrence of Nominal Plurality` == input$f34A)
    }
    
    if (input$f35A != "All values") {
      filtered_wals %<>% filter(`35A - Plurality in Independent Personal Pronouns` == input$f35A)
    }
    
    if (input$f36A != "All values") {
      filtered_wals %<>% filter(`36A - The Associative Plural` == input$f36A)
    }
    
    if (input$f37A != "All values") {
      filtered_wals %<>% filter(`37A - Definite Articles` == input$f37A)
    }
    
    if (input$f38A != "All values") {
      filtered_wals %<>% filter(`38A - Indefinite Articles` == input$f38A)
    }
    
    if (input$f39A != "All values") {
      filtered_wals %<>% filter(`39A - Inclusive/Exclusive Distinction in Independent Pronouns` == input$f39A)
    }
    
    if (input$f39B != "All values") {
      filtered_wals %<>% filter(`39B - Inclusive/Exclusive Forms in Pama-Nyungan` == input$f39B)
    }
    
    if (input$f40A != "All values") {
      filtered_wals %<>% filter(`40A - Inclusive/Exclusive Distinction in Verbal Inflection` == input$f40A)
    }
    
    if (input$f41A != "All values") {
      filtered_wals %<>% filter(`41A - Distance Contrasts in Demonstratives` == input$f41A)
    }
    
    if (input$f42A != "All values") {
      filtered_wals %<>% filter(`42A - Pronominal and Adnominal Demonstratives` == input$f42A)
    }
    
    if (input$f43A != "All values") {
      filtered_wals %<>% filter(`43A - Third Person Pronouns and Demonstratives` == input$f43A)
    }
    
    if (input$f44A != "All values") {
      filtered_wals %<>% filter(`44A - Gender Distinctions in Independent Personal Pronouns` == input$f44A)
    }
    
    if (input$f45A != "All values") {
      filtered_wals %<>% filter(`45A - Politeness Distinctions in Pronouns` == input$f45A)
    }
    
    if (input$f46A != "All values") {
      filtered_wals %<>% filter(`46A - Indefinite Pronouns` == input$f46A)
    }
    
    if (input$f47A != "All values") {
      filtered_wals %<>% filter(`47A - Intensifiers and Reflexive Pronouns` == input$f47A)
    }
    
    if (input$f48A != "All values") {
      filtered_wals %<>% filter(`48A - Person Marking on Adpositions` == input$f48A)
    }
    
    if (input$f49A != "All values") {
      filtered_wals %<>% filter(`49A - Number of Cases` == input$f49A)
    }
    
    if (input$f50A != "All values") {
      filtered_wals %<>% filter(`50A - Asymmetrical Case-Marking` == input$f50A)
    }
    
    if (input$f51A != "All values") {
      filtered_wals %<>% filter(`51A - Position of Case Affixes` == input$f51A)
    }
    
    if (input$f52A != "All values") {
      filtered_wals %<>% filter(`52A - Comitatives and Instrumentals` == input$f52A)
    }
    
    if (input$f53A != "All values") {
      filtered_wals %<>% filter(`53A - Ordinal Numerals` == input$f53A)
    }
    
    if (input$f54A != "All values") {
      filtered_wals %<>% filter(`54A - Distributive Numerals` == input$f54A)
    }
    
    if (input$f55A != "All values") {
      filtered_wals %<>% filter(`55A - Numeral Classifiers` == input$f55A)
    }
    
    if (input$f56A != "All values") {
      filtered_wals %<>% filter(`56A - Conjunctions and Universal Quantifiers` == input$f56A)
    }
    
    if (input$f57A != "All values") {
      filtered_wals %<>% filter(`57A - Position of Pronominal Possessive Affixes` == input$f57A)
    }
    
    if (input$f58A != "All values") {
      filtered_wals %<>% filter(`58A - Obligatory Possessive Inflection` == input$f58A)
    }
    
    if (input$f58B != "All values") {
      filtered_wals %<>% filter(`58B - Number of Possessive Nouns` == input$f58B)
    }
    
    if (input$f59A != "All values") {
      filtered_wals %<>% filter(`59A - Possessive Classification` == input$f59A)
    }
    
    if (input$f60A != "All values") {
      filtered_wals %<>% filter(`60A - Genitives, Adjectives and Relative Clauses` == input$f60A)
    }
    
    if (input$f61A != "All values") {
      filtered_wals %<>% filter(`61A - Adjectives without Nouns` == input$f61A)
    }
    
    if (input$f62A != "All values") {
      filtered_wals %<>% filter(`62A - Action Nominal Constructions` == input$f62A)
    }
    
    if (input$f63A != "All values") {
      filtered_wals %<>% filter(`63A - Noun Phrase Conjunction` == input$f63A)
    }
    
    if (input$f64A != "All values") {
      filtered_wals %<>% filter(`64A - Nominal and Verbal Conjunction` == input$f64A)
    }
    
    if (input$f65A != "All values") {
      filtered_wals %<>% filter(`65A - Perfective/Imperfective Aspect` == input$f65A)
    }
    
    if (input$f66A != "All values") {
      filtered_wals %<>% filter(`66A - The Past Tense` == input$f66A)
    }
    
    if (input$f67A != "All values") {
      filtered_wals %<>% filter(`67A - The Future Tense` == input$f67A)
    }
    
    if (input$f68A != "All values") {
      filtered_wals %<>% filter(`68A - The Perfect` == input$f68A)
    }
    
    if (input$f69A != "All values") {
      filtered_wals %<>% filter(`69A - Position of Tense-Aspect Affixes` == input$f69A)
    }
    
    if (input$f70A != "All values") {
      filtered_wals %<>% filter(`70A - The Morphological Imperative` == input$f70A)
    }
    
    if (input$f71A != "All values") {
      filtered_wals %<>% filter(`71A - The Prohibitive` == input$f71A)
    }
    
    if (input$f72A != "All values") {
      filtered_wals %<>% filter(`72A - Imperative-Hortative Systems` == input$f72A)
    }
    
    if (input$f73A != "All values") {
      filtered_wals %<>% filter(`73A - The Optative` == input$f73A)
    }
    
    if (input$f74A != "All values") {
      filtered_wals %<>% filter(`74A - Situational Possibility` == input$f74A)
    }
    
    if (input$f75A != "All values") {
      filtered_wals %<>% filter(`75A - Epistemic Possibility` == input$f75A)
    }
    
    if (input$f76A != "All values") {
      filtered_wals %<>% filter(`76A - Overlap between Situational and Epistemic Modal Marking` == input$f76A)
    }
    
    if (input$f77A != "All values") {
      filtered_wals %<>% filter(`77A - Semantic Distinctions of Evidentiality` == input$f77A)
    }
    
    if (input$f78A != "All values") {
      filtered_wals %<>% filter(`78A - Coding of Evidentiality` == input$f78A)
    }
    
    if (input$f79A != "All values") {
      filtered_wals %<>% filter(`79A - Suppletion According to Tense and Aspect` == input$f79A)
    }
    
    if (input$f79B != "All values") {
      filtered_wals %<>% filter(`79B - Suppletion in Imperatives and Hortatives` == input$f79B)
    }
    
    if (input$f80A != "All values") {
      filtered_wals %<>% filter(`80A - Verbal Number and Suppletion` == input$f80A)
    }
    
    if (input$f81A != "All values") {
      filtered_wals %<>% filter(`81A - Order of Subject, Object and Verb` == input$f81A)
    }
    
    if (input$f81B != "All values") {
      filtered_wals %<>% filter(`81B - Languages with two Dominant Orders of Subject, Object, and Verb` == input$f81B)
    }
    
    if (input$f82A != "All values") {
      filtered_wals %<>% filter(`82A - Order of Subject and Verb` == input$f82A)
    }
    
    if (input$f83A != "All values") {
      filtered_wals %<>% filter(`83A - Order of Object and Verb` == input$f83A)
    }
    
    if (input$f84A != "All values") {
      filtered_wals %<>% filter(`84A - Order of Object, Oblique, and Verb` == input$f84A)
    }
    
    if (input$f85A != "All values") {
      filtered_wals %<>% filter(`85A - Order of Adposition and Noun Phrase` == input$f85A)
    }
    
    if (input$f86A != "All values") {
      filtered_wals %<>% filter(`86A - Order of Genitive and Noun` == input$f86A)
    }
    
    if (input$f87A != "All values") {
      filtered_wals %<>% filter(`87A - Order of Adjective and Noun` == input$f87A)
    }
    
    if (input$f88A != "All values") {
      filtered_wals %<>% filter(`88A - Order of Demonstrative and Noun` == input$f88A)
    }
    
    if (input$f89A != "All values") {
      filtered_wals %<>% filter(`89A - Order of Numeral and Noun` == input$f89A)
    }
    
    if (input$f90A != "All values") {
      filtered_wals %<>% filter(`90A - Order of Relative Clause and Noun` == input$f90A)
    }
    
    if (input$f90B != "All values") {
      filtered_wals %<>% filter(`90B - Prenominal relative clauses` == input$f90B)
    }
    
    if (input$f90C != "All values") {
      filtered_wals %<>% filter(`90C - Postnominal relative clauses` == input$f90C)
    }
    
    if (input$f90D != "All values") {
      filtered_wals %<>% filter(`90D - Internally-headed relative clauses` == input$f90D)
    }
    
    if (input$f90E != "All values") {
      filtered_wals %<>% filter(`90E - Correlative relative clauses` == input$f90E)
    }
    
    if (input$f90F != "All values") {
      filtered_wals %<>% filter(`90F - Adjoined relative clauses` == input$f90F)
    }
    
    if (input$f90G != "All values") {
      filtered_wals %<>% filter(`90G - Double-headed relative clauses` == input$f90G)
    }
    
    if (input$f91A != "All values") {
      filtered_wals %<>% filter(`91A - Order of Degree Word and Adjective` == input$f91A)
    }
    
    if (input$f92A != "All values") {
      filtered_wals %<>% filter(`92A - Position of Polar Question Particles` == input$f92A)
    }
    
    if (input$f93A != "All values") {
      filtered_wals %<>% filter(`93A - Position of Interrogative Phrases in Content Questions` == input$f93A)
    }
    
    if (input$f94A != "All values") {
      filtered_wals %<>% filter(`94A - Order of Adverbial Subordinator and Clause` == input$f94A)
    }
    
    if (input$f95A != "All values") {
      filtered_wals %<>% filter(`95A - Relationship between the Order of Object and Verb and the Order of Adposition and Noun Phrase` == input$f95A)
    }
    
    if (input$f96A != "All values") {
      filtered_wals %<>% filter(`96A - Relationship between the Order of Object and Verb and the Order of Relative Clause and Noun` == input$f96A)
    }
    
    if (input$f97A != "All values") {
      filtered_wals %<>% filter(`97A - Relationship between the Order of Object and Verb and the Order of Adjective and Noun` == input$f97A)
    }
    
    if (input$f98A != "All values") {
      filtered_wals %<>% filter(`98A - Alignment of Case Marking of Full Noun Phrases` == input$f98A)
    }
    
    if (input$f99A != "All values") {
      filtered_wals %<>% filter(`99A - Alignment of Case Marking of Pronouns` == input$f99A)
    }
    
    if (input$f100A != "All values") {
      filtered_wals %<>% filter(`100A - Alignment of Verbal Person Marking` == input$f100A)
    }
    
    if (input$f101A != "All values") {
      filtered_wals %<>% filter(`101A - Expression of Pronominal Subjects` == input$f101A)
    }
    
    if (input$f102A != "All values") {
      filtered_wals %<>% filter(`102A - Verbal Person Marking` == input$f102A)
    }
    
    if (input$f103A != "All values") {
      filtered_wals %<>% filter(`103A - Third Person Zero of Verbal Person Marking` == input$f103A)
    }
    
    if (input$f104A != "All values") {
      filtered_wals %<>% filter(`104A - Order of Person Markers on the Verb` == input$f104A)
    }
    
    if (input$f105A != "All values") {
      filtered_wals %<>% filter(`105A - Ditransitive Constructions: The Verb 'Give'` == input$f105A)
    }
    
    if (input$f106A != "All values") {
      filtered_wals %<>% filter(`106A - Reciprocal Constructions` == input$f106A)
    }
    
    if (input$f107A != "All values") {
      filtered_wals %<>% filter(`107A - Passive Constructions` == input$f107A)
    }
    
    if (input$f108A != "All values") {
      filtered_wals %<>% filter(`108A - Antipassive Constructions` == input$f108A)
    }
    
    if (input$f108B != "All values") {
      filtered_wals %<>% filter(`108B - Productivity of the Antipassive Construction` == input$f108B)
    }
    
    if (input$f109A != "All values") {
      filtered_wals %<>% filter(`190A - Applicative Constructions` == input$f109A)
    }
    
    if (input$f109B != "All values") {
      filtered_wals %<>% filter(`109B - Other Roles of Applied Objects` == input$f109B)
    }
    
    if (input$f110A != "All values") {
      filtered_wals %<>% filter(`110A - Periphrastic Causative Constructions` == input$f110A)
    }
    
    if (input$f111A != "All values") {
      filtered_wals %<>% filter(`111A - Nonperiphrastic Causative Constructions` == input$f111A)
    }
    
    if (input$f112A != "All values") {
      filtered_wals %<>% filter(`112A - Negative Morphemes` == input$f112A)
    }
    
    if (input$f113A != "All values") {
      filtered_wals %<>% filter(`113A - Symmetric and Asymmetric Standard Negation` == input$f113A)
    }
    
    if (input$f114A != "All values") {
      filtered_wals %<>% filter(`114A - Subtypes of Asymmetric Standard Negation` == input$f114A)
    }
    
    if (input$f115A != "All values") {
      filtered_wals %<>% filter(`115A - Negative Indefinite Pronouns and Predicate Negation` == input$f115A)
    }
    
    if (input$f116A != "All values") {
      filtered_wals %<>% filter(`116A - Polar Questions` == input$f116A)
    }
    
    if (input$f117A != "All values") {
      filtered_wals %<>% filter(`117A - Predicative Possession` == input$f117A)
    }
    
    if (input$f118A != "All values") {
      filtered_wals %<>% filter(`118A - Predicative Adjectives` == input$f118A)
    }
    
    if (input$f119A != "All values") {
      filtered_wals %<>% filter(`119A - Nominal and Locational Predication` == input$f119A)
    }
    
    if (input$f120A != "All values") {
      filtered_wals %<>% filter(`120A - Zero Copula for Predicate Nominals` == input$f120A)
    }
    
    if (input$f121A != "All values") {
      filtered_wals %<>% filter(`121A - Comparative Constructions` == input$f121A)
    }
    
    if (input$f122A != "All values") {
      filtered_wals %<>% filter(`122A - Relativization on Subjects` == input$f122A)
    }
    
    if (input$f123A != "All values") {
      filtered_wals %<>% filter(`123A - Relativization on Obliques` == input$f123A)
    }
    
    if (input$f124A != "All values") {
      filtered_wals %<>% filter(`124A - 'Want' Complement Subjects` == input$f124A)
    }
    
    if (input$f125A != "All values") {
      filtered_wals %<>% filter(`125A - Purpose Clauses` == input$f125A)
    }
    
    if (input$f126A != "All values") {
      filtered_wals %<>% filter(`126A - 'When' Clauses` == input$f126A)
    }
    
    if (input$f127A != "All values") {
      filtered_wals %<>% filter(`127A - Reason Clauses` == input$f127A)
    }
    
    if (input$f128A != "All values") {
      filtered_wals %<>% filter(`128A - Utterance Complement Clauses` == input$f128A)
    }
    
    if (input$f129A != "All values") {
      filtered_wals %<>% filter(`129A - Hand and Arm` == input$f129A)
    }
    
    if (input$f130A != "All values") {
      filtered_wals %<>% filter(`130A - Finger and Hand` == input$f130A)
    }
    
    if (input$f130B != "All values") {
      filtered_wals %<>% filter(`130B - Cultural Categories of Languages with Identity of 'Finger' and 'Hand'` == input$f130B)
    }
    
    if (input$f131A != "All values") {
      filtered_wals %<>% filter(`131A - Numeral Bases` == input$f131A)
    }
    
    if (input$f132A != "All values") {
      filtered_wals %<>% filter(`132A - Number of Non-Derived Basic Colour Categories` == input$f132A)
    }
    
    if (input$f133A != "All values") {
      filtered_wals %<>% filter(`133A - Number of Basic Colour Categories` == input$f133A)
    }
    
    if (input$f134A != "All values") {
      filtered_wals %<>% filter(`134A - Green and Blue` == input$f134A)
    }
    
    if (input$f135A != "All values") {
      filtered_wals %<>% filter(`135A - Red and Yellow` == input$f135A)
    }
    
    if (input$f136A != "All values") {
      filtered_wals %<>% filter(`136A - M-T Pronouns` == input$f136A)
    }
    
    if (input$f136B != "All values") {
      filtered_wals %<>% filter(`136B - M in First Person Singular` == input$f136B)
    }
    
    if (input$f137A != "All values") {
      filtered_wals %<>% filter(`137A - N-M Pronouns` == input$f137A)
    }
    
    if (input$f137B != "All values") {
      filtered_wals %<>% filter(`137B - M in Second Person Singular` == input$f137B)
    }
    
    if (input$f138A != "All values") {
      filtered_wals %<>% filter(`138A - Tea` == input$f138A)
    }
    
    if (input$f139A != "All values") {
      filtered_wals %<>% filter(`139A - Irregular Negatives in Sign Languages` == input$f139A)
    }
    
    if (input$f140A != "All values") {
      filtered_wals %<>% filter(`140A - Question Particles in Sign Languages` == input$f140A)
    }
    
    if (input$f141A != "All values") {
      filtered_wals %<>% filter(`141A - Writing Systems` == input$f141A)
    }
    
    if (input$f142A != "All values") {
      filtered_wals %<>% filter(`142A - Para-Linguistic Usages of Clicks` == input$f142A)
    }
    
    if (input$f143A != "All values") {
      filtered_wals %<>% filter(`143A - Order of Negative Morpheme and Verb` == input$f143A)
    }
    
    if (input$f143B != "All values") {
      filtered_wals %<>% filter(`143B - Obligatory Double Negation` == input$f143B)
    }
    
    if (input$f143C != "All values") {
      filtered_wals %<>% filter(`143C - Optional Double Negation` == input$f143C)
    }
    
    if (input$f143D != "All values") {
      filtered_wals %<>% filter(`143D - Optional Triple Negation` == input$f143D)
    }
    
    if (input$f143E != "All values") {
      filtered_wals %<>% filter(`143E - Preverbal Negative Morphemes` == input$f143E)
    }
    
    if (input$f143F != "All values") {
      filtered_wals %<>% filter(`143F - Postverbal Negative Morphemes` == input$f143F)
    }
    
    if (input$f143G != "All values") {
      filtered_wals %<>% filter(`143G - Minor morphological means of signaling negation` == input$f143G)
    }
    
    if (input$f144A != "All values") {
      filtered_wals %<>% filter(`144A - Position of Negative Word With Respect to Subject, Object, and Verb` == input$f144A)
    }
    
    if (input$f144B != "All values") {
      filtered_wals %<>% filter(`144B - Position of negative words relative to beginning and end of clause and with respect to adjacency to verb` == input$f144B)
    }
    
    if (input$f144C != "All values") {
      filtered_wals %<>% filter(` 144C - Languages with different word order in negative clauses` == input$f144C)
    }
    
    if (input$f144D != "All values") {
      filtered_wals %<>% filter(`144D - The Position of Negative Morphemes in SVO Languages` == input$f144D)
    }
    
    if (input$f144E != "All values") {
      filtered_wals %<>% filter(`144E - Multiple Negative Constructions in SVO Languages` == input$f144E)
    }
    
    if (input$f144F != "All values") {
      filtered_wals %<>% filter(`144F - Obligatory Double Negation in SVO languages` == input$f144F)
    }
    
    if (input$f144G != "All values") {
      filtered_wals %<>% filter(`144G - Optional Double Negation in SVO languages` == input$f144G)
    }
    
    if (input$f144H != "All values") {
      filtered_wals %<>% filter(`144H - NegSVO Order` == input$f144H)
    }
    
    if (input$f144I != "All values") {
      filtered_wals %<>% filter(`144I - SNegVO Order` == input$f144I)
    }
    
    if (input$f144J != "All values") {
      filtered_wals %<>% filter(`144J - SVNegO Order` == input$f144J)
    }
    
    if (input$f144K != "All values") {
      filtered_wals %<>% filter(`144K - SVONeg Order` == input$f144K)
    }
    
    if (input$f144L != "All values") {
      filtered_wals %<>% filter(`144L - The Position of Negative Morphemes in SOV Languages` == input$f144L)
    }
    
    if (input$f144M != "All values") {
      filtered_wals %<>% filter(`144M - Multiple Negative Constructions in SOV Languages` == input$f144M)
    }
    
    if (input$f144N != "All values") {
      filtered_wals %<>% filter(`144N - Obligatory Double Negation in SOV languages` == input$f144N)
    }
    
    if (input$f144O != "All values") {
      filtered_wals %<>% filter(`144O - Optional Double Negation in SOV languages` == input$f144O)
    }
    
    if (input$f144P != "All values") {
      filtered_wals %<>% filter(`144P - NegSOV Order` == input$f144P)
    }
    
    if (input$f144Q != "All values") {
      filtered_wals %<>% filter(`144Q - SNegOV Order` == input$f144Q)
    }
    
    if (input$f144R != "All values") {
      filtered_wals %<>% filter(`144R - SONegV Order` == input$f144R)
    }
    
    if (input$f144S != "All values") {
      filtered_wals %<>% filter(`144S - SOVNeg Order` == input$f144S)
    }
    
    if (input$f144T != "All values") {
      filtered_wals %<>% filter(`144T - The Position of Negative Morphemes in Verb-Initial Languages` == input$f144T)
    }
    
    if (input$f144U != "All values") {
      filtered_wals %<>% filter(`144U - Double negation in verb-initial languages` == input$f144U)
    }
    
    if (input$f144V != "All values") {
      filtered_wals %<>% filter(`144V - Verb-Initial with Preverbal Negative` == input$f144V)
    }
    
    if (input$f144W != "All values") {
      filtered_wals %<>% filter(`144W - Verb-Initial with Negative that is Immediately Postverbal or between Subject and Object` == input$f144W)
    }
    
    if (input$f144X != "All values") {
      filtered_wals %<>% filter(`144X - Verb-Initial with Clause-Final Negative` == input$f144X)
    }
    
    if (input$f144Y != "All values") {
      filtered_wals %<>% filter(`144Y - The Position of Negative Morphemes in Object-Initial Languages` == input$f144Y)
    }
    
    leaflet_colours <- as.factor(c("red", "darkred", "lightred", "orange", "beige", "green", "darkgreen", "lightgreen", "blue", "darkblue", "lightblue", "purple", "darkpurple", "pink", "cadetblue", "gray", "lightgray", "black"))
    
    assign_colour <- function(wals_col) {
      colour_index <- as.integer(factor(wals_col)) %% length(leaflet_colours) + 1
      return(leaflet_colours[colour_index])
    }
    
    filtered_wals %<>%
      mutate(colour = assign_colour(family))
    
    as.data.frame(filtered_wals)    
  })
  
  output$wals_map <- renderLeaflet({
    
    filtered_wals() %>%
      leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addAwesomeMarkers(
        popup = ~language,
        icon = ~awesomeIcons(
          icon = "comment",
          library = "fa",
          iconColor = "#f2efe9",
          markerColor = colour))
  })
  
  output$wide_table <- renderDataTable(server = TRUE, {
    
    if (input$show_features) {
      filtered_wals() %>%
        select(language_ID, language, genus, family, macroarea, countrycodes, features) %>%
        rename(ID = language_ID, Language = language, Genus = genus, Family = family, Macroarea = macroarea, `Country codes` = countrycodes)
    } else {
      filtered_wals() %>%
        select(language_ID, language, genus, family, macroarea, countrycodes) %>%
        rename(ID = language_ID, Language = language, Genus = genus, Family = family, Macroarea = macroarea, `Country codes` = countrycodes)
    }
  }, options = list(pageLength = 20), rownames = FALSE
  )
  
  output$long_table <- renderDataTable(server = TRUE, {
    
    wals_long <- filtered_wals() %>%
      pivot_longer(cols = 11:ncol(wals_wide), names_to = "feature", values_to = "value") %>%
      select(language_ID, language, genus, family, macroarea, countrycodes, feature, value) %>%
      rename(ID = language_ID, Language = language, Genus = genus, Family = family, Macroarea = macroarea, `Country codes` = countrycodes, Feature = feature, Value = value)
    
    values <- special_pull(wals_long, Value)
    values <- custom_sort(values)
    
    wals_long$Feature <- factor(wals_long$Feature, levels = features)
    wals_long$Value <- factor(wals_long$Value, levels = values)
    
    if (input$show_missing_features) {
      wals_long
    } else {
      wals_long %>%
        drop_na(Value)
    }
  }, options = list(pageLength = 20, order = list(6, 'asc')), rownames = FALSE
  )
  
}

shinyApp(ui, server)
