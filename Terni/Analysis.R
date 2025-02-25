if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, tidytext, quanteda, SnowballC, ggplot2, patchwork)

# Import transcripts -----------------------------------------------------------
TerniText <- read_csv("Terni/YTData_clean.csv") |> 
  filter(`drop-flag` == 0) |> 
  select(!`drop-flag`)


TerniText <- TerniText %>% 
  mutate(transcription = str_replace_all(transcription, "[\'’](?!\\s)", "' "))

# Data wrangling ---------------------------------------------------------------

## Recode ----

TerniText <- TerniText |> 
  mutate(cat = case_match(keyword,
    c('AST', 'acciaieria', 'industria') ~ 'Industry',
    c('ambiente', 'ecosistema', 'qualità dell\'aria', 'qualità dell\'acqua', 'spazi verdi', 'verde urbano', 'inquinamento') ~ 'Environment, nature',
    c('rifiuti', 'smaltimento', 'inceneritore') ~ 'Waste management'
  ))

# Preprocessing - tidytext -----------------------------------------------------
## Tokenisation ----
TRtok <- as_tibble(TerniText) %>% 
  unnest_tokens(output = word, input = transcription)

## Stopwords ----
### stopwords dataframe ----
it_stopwords <- data.frame(word = stopwords::stopwords("italian"),
                           lexicon = "custom")


### deleting stopwords ----
TRtok <- TRtok |>
  anti_join(it_stopwords) |> 
  filter(word != 'rifiuti')

## Stemming ----
TRtok <- TRtok |> 
  mutate(stemmed = wordStem(word, language = 'it'))

# Sentiment analysis -----------------------------------------------------------

## Dictionary ----

### Importing dictionary ----

# sentix <- read_delim('Terni/sentix', delim = '\t',
#                      trim_ws = T,
#                      col_names = c('lemma', 'POS', 'Wordnet synset ID', 'positive score', 'negative score', 'polarity', 'intensity'))

lexPos <- read_delim('Lexicon/readable_pos_words_list.txt', delim = '\t', col_names = 'word') |> 
  separate_wider_delim(word, ' ', names = c('word', 'lang')) |> 
  filter(lang == 'it' & word != 'di' & word != 'come' & word != 'molto' & word != 'modo') |> 
  select(!lang) |> 
  mutate(polarity = 'pos', stemmed = wordStem(word, language = 'it')) |> 
  filter(stemmed != 'lavor' & stemmed != 'tutt' & stemmed != 'chiar' & stemmed != 'qui' & stemmed != 'rispett')
  

lexNeg <- read_delim('Lexicon/readable_neg_words_list.txt', delim = '\t', col_names = 'word') |> 
  separate_wider_delim(word, ' ', names = c('word', 'lang')) |> 
  filter(word != 'fatto' & word != 'trovata' & word != 'trovarsi') |> 
  mutate(polarity = 'neg', stemmed = wordStem(word, language = 'it')) |> 
  filter(lang == 'it' & stemmed != 'rifiut' & stemmed != 'rif' & stemmed != 'in') |> 
  select(!lang) |> 
  filter(stemmed != 'comun' & stemmed != 'tutt' & stemmed != 'chiar' & stemmed != 'par')

lex <- bind_rows(lexPos, lexNeg)

### Analysis ----
TRtokSent <- TRtok |> 
  inner_join(lex, join_by(stemmed), relationship = 'many-to-many') 

TRdocSent <- TRtokSent|> # join dictionary
  group_by(video_id) |> 
  count(polarity) |> # count positive, negative words
  pivot_wider(names_from = polarity,
              values_from = n) |> 
  mutate(ratio = (pos - neg)/(pos + neg)) |> # compute pos/neg ratio
  inner_join(TRtok |> select(video_id, title, upload_date, keyword, cat), # retrieve all other info from original DF
            multiple = 'first') |> 
  relocate(neg:ratio, .after = keyword)
  
## Visualisation ----
### Pos-neg bar chart ----
graphs <- list()
for (x in as_vector(TRdocSent |> group_by(cat) |> count(cat) |> select(cat))) {
  graph <- TRdocSent |>
    filter(cat == x) |>
  ggplot(aes(reorder(title, ratio), ratio, group = keyword)) +
    geom_col() +
    geom_hline(yintercept = 0, col = 'gray85') +
    scale_y_continuous(limits = c(-1,1), expand = c(0,0)) +
    scale_x_discrete(expand = c(0,0)) +
    facet_grid(cols = vars(keyword), scales = 'free')
  
  graphs[[length(graphs)+1]] <- graph
}

#### Patchwork ----
gg1 <- graphs[[1]]
gg2 <- graphs[[2]]
gg3 <- graphs[[3]]

design <- 'AAAAAAA
           BBB####
           CCC####'

gg1 / gg2 / gg3 +
  plot_layout(design = design,
              axis_titles = 'collect',
              guides = 'collect') +
  plot_annotation(title = 'Document sentiment values by category and keyword',
                  ) &
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        panel.background = element_blank(),
        axis.line.y = element_line(colour = 'gray90'),
        panel.grid.major.y = element_line(colour = 'gray95'))

### Pos/neg ratio per category ----
TRdocSent |>
  drop_na() |>
  group_by(cat) |>
  reframe(ratio = mean(ratio)) |> 
ggplot(aes(ratio, cat)) +
  geom_point() +
  labs(title = 'Pos/neg ratio per category') +
  theme_minimal() +
  scale_x_continuous(limits = c(-1,1)) +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))


### Most relevant stems in pos and neg ----
TRtokSent |>
  group_by(polarity) |> 
  count(stemmed, sort = T) |> 
  slice(1:20) |> 
  mutate(polarity = factor(polarity, levels = c('pos', 'neg'))) |> 
ggplot(aes(reorder(stemmed, n, decreasing = T), n, fill = polarity)) +
  geom_col() +
  scale_y_continuous(limits = c(0, 850)) +
  facet_wrap(~polarity, scales = 'free') +
  labs(title = 'Most relevant stems in pos and neg') +
  ylab('Frequency') +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .1),
        legend.position = 'null')
  
