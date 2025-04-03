if (!require("pacman")) install.packages("pacman")
library(pacman)
p_load(tidyverse, ggh4x, patchwork, ggiraph, ggbump)

# Import data ------------------------------------------------------------------
YTvidSent <- read_rds('Results/YTvidSent.RDS')
YTData <- read_rds('YTData.RDS')

# Visualisation ---------------------------------------------------------------
## palette ----
pal <- c(
  "#FDA638",
  "#459395",
  "#EB7C69",
  '#972F5A',
  '#2BE19E',
  '#121333'
)
na_col <- "gray75"

### theming ----
theme_set(theme(panel.background = element_blank(),
                axis.ticks = element_blank(),
                legend.title = element_blank(),
                panel.grid.major = element_line(linetype = 'solid',
                                                colour = 'gray97',
                                                linewidth = .3),
                panel.grid.minor = element_blank(),
                axis.line.x = element_line(colour = 'gray25'),
                axis.line.y = element_line(colour = 'gray25'),
                strip.background = element_blank()
))

# Corpus composition
## wrangling
YTData_vis <- YTData |> 
  mutate(upload_date = case_when(
    str_detect(upload_date, "years ago") ~ today() - years(as.numeric(str_extract(upload_date, "\\d+"))),
    str_detect(upload_date, "days ago") ~ today() - days(as.numeric(str_extract(upload_date, "\\d+"))),
    TRUE ~ as.Date(NA) # Gestisce altri formati o errori
  )) |> 
  mutate(upload_date = floor_date(upload_date, unit = 'halfyears'))

write_rds(YTData_vis, 'YTData_vis.RDS')

# Sentiment distribution by thematic area over cities ----

YTgg <- YTvidSent |> 
  drop_na() |> 
  filter(ratio != 0) |> 
  ggplot(aes(reorder(video_id, ratio), ratio, fill = cat)) +
  geom_col(width = .9) +
  geom_vline(aes(xintercept = 0), linetype = 'dashed') +
  facet_grid2(city~cat, scales = 'free', independent = 'x') +
  scale_fill_manual(values = pal) +
  scale_y_continuous(limits = c(-1,1)) +
  theme(panel.grid.major.x = element_blank(),
        legend.position = 'none',
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        strip.text = element_text(face = 'bold'))

ggsave('Plots/YTgg.pdf', YTgg, width = 8)
# Keywords ---------------------------------------------------------------------
## Terni ----
TRgg <- YTvidSent |> 
  drop_na() |> 
  filter(ratio != 0, city == 'TR') |> 
  ggplot(aes(reorder(video_id, ratio), ratio, fill = cat)) +
  geom_col() +
  facet_nested(.~ cat + keyword,
               scales = 'free',
               independent = 'x') +
  scale_fill_manual(values = pal) +
  scale_y_continuous(limits = c(-1,1)) +
  labs(title = 'Terni') +
  theme(panel.grid.major.x = element_blank(),
        legend.position = 'none',
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        strip.text = element_text())

ggsave('Plots/TRgg.pdf', TRgg, width = 12)
## Perugia ----
PGgg <- YTvidSent |> 
  drop_na() |> 
  filter(ratio != 0, city == 'PG') |> 
  ggplot(aes(reorder(video_id, ratio), ratio, fill = cat)) +
  geom_col() +
  facet_nested(.~ cat + keyword,
               scales = 'free',
               independent = 'x') +
  scale_fill_manual(values = pal) +
  scale_y_continuous(limits = c(-1,1)) +
  labs(title = 'Perugia') +
  theme(panel.grid.major.x = element_blank(),
        legend.position = 'none',
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.line.x = element_blank(),
        strip.text = element_text())

ggsave('Plots/PGgg.pdf', PGgg, width = 12)
# Patchwork ----
patch <- YTgg / (TRgg / PGgg) &
  plot_annotation(title = 'Sentiment distribution by city and thematic area',
       subtitle = 'Positive/negative ratio per video')

ggsave('Plots/patch.pdf', patch, width = 22, height = 12)

# Sentiment over time by cat ----
YT_sal_gg <- YTvidSent |>
  mutate(upload_date = case_when(
    str_detect(upload_date, "year ago") ~ today() - years(as.numeric(str_extract(upload_date, "\\d+"))),
    str_detect(upload_date, "years ago") ~ today() - years(as.numeric(str_extract(upload_date, "\\d+"))),
    str_detect(upload_date, "days ago") ~ today() - days(as.numeric(str_extract(upload_date, "\\d+"))),
    str_detect(upload_date, "weeks ago") ~ today() - weeks(as.numeric(str_extract(upload_date, "\\d+"))),
    TRUE ~ as.Date(NA) # Gestisce altri formati o errori
  )) |>
  count(upload_date, cat, city) |>
  complete(upload_date, cat, city, fill = list(n = 0)) |>
  group_by(cat, city) |>
  mutate(n = n/sum(n)) |> 
  ungroup() |> 
  ggplot(aes(upload_date, n, fill = city, data_id = city, tooltip = round(n, digits = 2))) +
  geom_col_interactive(position = position_dodge(width = 300), width = 300) +
  theme(axis.line.y = element_blank()) +
  scale_x_date(limits = c(as_date('1 January 2011', format = "%d %B %Y") - days(360),
                          as_date('1 June 2025', format = "%d %B %Y")),
               date_breaks = '1 year', date_labels = '%Y',
               expand = c(0,0)) +
  facet_wrap(~cat, nrow = 1) +
  guides(x = guide_axis(angle = 45)) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(expand = c(0,0)) +
  theme(legend.position = 'bottom',
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

### interactive
girafe(ggobj = YT_sal_gg, width_svg = 14, height_svg = 5,
       options = list(
         opts_hover(css = ''), ## CSS code of line we're hovering over
         opts_hover_inv(css = "opacity:0.3;"), ## CSS code of all other lines
         opts_tooltip(css = "background-color:white;
                      color:black;
                      font-family:Helvetica;
                      font-style:empty;
                      padding:8px;
                      border-radius:10px;",
                      use_cursor_pos = T),
         opts_toolbar(position = 'bottomright')))


## Query environment-related words in other query categories ----
crossNum_gg <- YTData |>
  mutate(Industry = ifelse(str_detect(transcription, regex(Industry, ignore_case = T)), 1, 0),
         Transportation = ifelse(str_detect(transcription, regex(Transportation, ignore_case = T)), 1, 0),
         Environment = ifelse(str_detect(transcription, regex(Environment, ignore_case = T)), 1, 0)) |> 
  filter(Environment == 1, cat != 'Environment, nature') |> 
  count(cat, city) |> 
  ggplot(aes(n, cat, fill = cat)) +
  geom_col(position = 'dodge') +
  geom_text(aes(x = .5, label = cat), hjust = 0, colour = 'white') +
  geom_text(data = tibble(city = 'TR', cat = 'Industry', n = '0'),
            aes(x = .5, label = cat), hjust = 0, colour = 'black') +
  facet_grid2(city~., scales = 'free', independent = 'x') +
  scale_fill_manual(values = pal) +
  scale_x_continuous(expand = c(0,0), limits = c(0,30), breaks = seq(0, 30, 5)) +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.line.x = element_blank())

crossEnv_gg <- YTData |>
  mutate(Industry = ifelse(str_detect(transcription, regex(Industry, ignore_case = T)), 1, 0),
         Transportation = ifelse(str_detect(transcription, regex(Transportation, ignore_case = T)), 1, 0),
         Environment = ifelse(str_detect(transcription, regex(Environment, ignore_case = T)), 1, 0)) |> 
  filter(cat == 'Environment, nature', Transportation == 1 | Industry == 1) |> 
  drop_na() |> 
  count(Transportation, Industry, city) |> 
  mutate(var = case_when(Transportation == 0 & Industry == 1 ~ 'Industry',
                         Transportation == 1 & Industry == 0 ~ 'Transportation',
                         Transportation == 1 & Industry == 1 ~ 'Both')) |> 
  ggplot(aes(n, reorder(var, n), fill = var)) +
  geom_col() +
  geom_text(aes(x = .5, label = var), hjust = 0, colour = 'white') +
  geom_text(data = ~. |> filter(city == 'TR', var == 'Both' | var == 'Transportation'),
            aes(x = .5, label = var), hjust = 0, colour = 'black') +
  facet_grid2(city~.) +
  scale_fill_manual(values = pal) +
  scale_x_continuous(expand = c(0,0), limits = c(0,30), breaks = seq(0, 30, 5)) +
  theme(legend.position = 'none',
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.line.x = element_blank())

