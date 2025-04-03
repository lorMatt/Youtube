if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, tidytext)

# Import data ------------------------------------------------------------------
PGData <- read_csv('Perugia/Perugia YT.csv') |> 
  mutate(city = 'PG',
         transcription = str_remove_all(transcription,# remove numbers, symbols, artifacts
                                        "[^[:alnum:][:alpha:]\\s'.\\.]")) |> 
  filter(keyword != 'biciclette')

  
TRData <- read_csv("Terni/TerniYT_clean.csv") |> 
  filter(`drop-flag` == 0) |> # dropping flagged transcriptions
  select(!`drop-flag`) |> 
  mutate(city = 'TR') |> 
  filter(keyword != 'biciclette')

## Merge 
YTData <- PGData |> 
  bind_rows(TRData) |> 
  distinct(video_id, .keep_all = T)

# Wrangling --------------------------------------------------------------------
## Recode ----
YTData <- YTData |> 
  mutate(cat = case_match(keyword,
                          c('AST', 'acciaieria', 'industria', 'nocività') ~ 'Industry',
                          c('ambiente', 'ecosistema', 'qualità dell\'aria', 'qualità dell\'acqua', 'spazi verdi', 'verde urbano', 'inquinamento', 'emissioni') ~ 'Environment, nature',
                          c('rifiuti', 'smaltimento', 'inceneritore', 'raccolta differenziata') ~ 'Waste management',
                          c('autobus', 'biciclette', 'BRT', 'ciclabile', 'mobilità sostenibile') ~ 'Transportation'
  ))

# Preprocessing - tidytext -----------------------------------------------------
## Tokenisation ----
YTtok <- as_tibble(YTData) %>% 
  unnest_tokens(output = token, input = transcription)

## Stopwords ----
### stopwords dataframe ----
it_stopwords <- data.frame(token = stopwords::stopwords("italian"),
                           lexicon = "custom")
### deleting stopwords ----
YTtok <- YTtok |>
  anti_join(it_stopwords)

# Dictionary sentiment analysis ------------------------------------------------
## import lexicon ----
lexPos <- read_delim('Lexicon/readable_pos_words_list.txt', delim = '\t', col_names = 'token') |> 
  separate_wider_delim(token, ' ', names = c('token', 'lang')) |> 
  filter(lang == 'it' & !token %in% c('chiaro', 'chiaramente', 'chiarire', 'chiarezza',
                                      'anzitutto', 'tutto', 'lavorare', 'lavorato', 'modo',
                                      'molto', 'come', 'di')) |> 
  select(!lang) |> 
  mutate(polarity = 'pos')


lexNeg <- read_delim('Lexicon/readable_neg_words_list.txt', delim = '\t', col_names = 'token') |> 
  separate_wider_delim(token, ' ', names = c('token', 'lang')) |> 
  filter(lang == 'it' & !token %in% c('fatto', 'trovata', 'trovarsi', 'in', 'sin',
                                      'coinvolto', 'comune', 'pari')) |> 
  select(!lang) |> 
  mutate(polarity = 'neg')

lex <- bind_rows(lexPos, lexNeg)

# Merge dictionary -------------------------------------------------------------
### Analysis ----
YTtokSent <- YTtok |> 
  inner_join(lex, join_by(token), relationship = 'many-to-many') # join dictionary

YTvidSent <- YTtokSent|> 
  group_by(video_id) |> 
  count(polarity) |> # count positive, negative words
  pivot_wider(names_from = polarity,
              values_from = n) |> 
  mutate(ratio = (pos - neg)/(pos + neg)) |> # compute pos/neg ratio
  inner_join(YTtok |> select(video_id, title, upload_date, keyword, cat, city), # retrieve all other info from original DF
             multiple = 'first') |> 
  relocate(neg:ratio, .after = keyword)

# Save results -----------------------------------------------------------------
write_rds(YTtokSent, 'Results/YTtokSent.RDS')
write_rds(YTvidSent, 'Results/YTvidSent.RDS')
write_rds(YTData, 'YTData.RDS')
