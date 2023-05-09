# libraries -----
#if (length(dev.list()) > 0) {dev.off()}
rm(list = ls())

library(tidyverse)
library(ggthemes)
library(ggtext)
library(ragg)
library(systemfonts)
library(textshaping)

# load and tidy garden interest data ----

# csv file ("how to garden" interest vs. time, 2015-2022)
garden_csv <- file.path('data','google_garden_2015_2022.csv')

# make tibble 
garden_data <- read_csv(garden_csv, skip = 2) |>
  rename(date = Month, interest = 'how to garden: (United Kingdom)')

# add variables for month, year, and months from start, as doubles
yr_start <- 2015
garden_data <- garden_data |>
  separate_wider_delim(cols = date, delim = '-', names = c('year', 'month')) |>
  mutate(month = sub('^0+','',month)) |>
  type_convert() |>
  mutate(months_elapsed = (year - yr_start)*12 + month) |>
  mutate(covid = 0 + 1*(year == 2020)) # also mark 2020 data as "covid"

show(garden_data)

# plot interest vs. time (w/ and w/o grouping by year) ----
# (in progress visualisations)

clr_covid <- '#437C11'
  
# Plot interest vs months elapsed from start of 2015
ggplot(garden_data, aes(x = months_elapsed, y = interest)) +
  geom_line(color = clr_covid, linewidth = 1.5)

# Plot interest vs. month to emphasise seasonal trends

# rearrange so 2020 is last 
garden_data_reorder <- garden_data |>
  arrange(covid, 'desc')

pal = c(rep('#b59602',5),'#89a76c','#b5c4a6',clr_covid)
ggplot(garden_data, 
        aes(x = month, 
            y = interest, 
            color = fct_reorder(factor(year), covid, .fun = first, .desc = FALSE)
            )
        ) + 
   geom_line(linewidth = sort(garden_data$covid+1)) + 
   scale_colour_manual(values = pal) + 
   theme_bw() +
   theme(legend.position = 'none')

# pivot wider for datawrapper version ----

month_names = c('Jan', 'Feb', 'March',
                'April', 'May', 'June',
                'July', 'Aug', 'Sept',
                'Oct', 'Nov', 'Dec')
garden_data_dw <- garden_data |>
  select(c(year, month, interest)) |>
  pivot_wider(names_from = year, values_from = interest) |>
  mutate(month = month_names)

show(garden_data_dw)

write_csv(garden_data_dw,file.path('data','garden_data_dw.csv'))

# ------ Grow vegetables/flowers/food trends ----

# load and tidy data

# path to file
grow_csv <- file.path('data', 'google_grow_2015_2022.csv')

# make tibble
grow_data <- read_csv(grow_csv, skip = 2) |>
  rename_with(function(x) gsub(': (United Kingdom)', '', x, fixed = TRUE)) |>
  rename_with(function(x) gsub('grow ', '', x)) |>
  rename(date = Month)

# separate dates into year and month; add months elapsed since start
grow_data <- grow_data |>
  separate_wider_delim(cols = date, delim = '-', names = c('year', 'month')) |>
  mutate(month = sub('^0+','',month)) |>
  type_convert() |>
  mutate(months_elapsed = (year - yr_start)*12 + month) |>
  mutate(covid = 0 + 1*(year == 2020)) # also mark 2020 data as "covid"

show(grow_data)

# tidy format - also keep original for converting to datawrapper format
grow_data_tidy <- grow_data |>
  pivot_longer(
    cols = c('vegetables', 'food', 'flowers'),
    names_to = 'search',
    values_to = 'interest'
  ) |>
  mutate(search = factor(search, levels = c('vegetables', 'flowers', 'food')))

show(grow_data_tidy)

# plot interest (grow vegetables,  grow flowers, and grow food) vs time ---
# (rough plots, initial exploration) 

# search terms and corresponding colours
search_terms <- sort(unique(grow_data_tidy$search))
search_pal <- c('#A3C429', '#9C4AA3', '#2690A6')
n_search = length(search_terms)

# lineplot 
ggplot(grow_data_tidy, 
       aes(x = months_elapsed, y = interest, group = search, colour = search)
    ) +
  geom_line(linewidth = 0.75) +
  facet_grid(rows = vars(search)) +
  scale_colour_manual(values = search_pal) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    panel.background = element_blank()
    ) + 
  annotate("segment", x = -Inf, xend = Inf, y = 0, yend = 0,
           color = 'grey80', linewidth = 0.5, linetype = 3) +
  scale_x_continuous(
    limits = c(0, 100), 
    breaks = seq(6, max(grow_data_tidy$months_elapsed), by = 12),
    labels = unique(grow_data_tidy$year)
    ) + 
  theme(axis.ticks.length = unit(0, 'cm'), 
        plot.title = element_markdown(),
        legend.position = 'none',
        strip.text = element_blank()
        ) +
  labs(
    title = "Interest in growing 
    <b style='color:#A3C429;'> vegetables</b>, 
    <b style='color:#9C4AA3;'>flowers</b>, and 
    <b style='color:#2690A6;'>food</b>",
    subtitle = 'Number of Google searches for "how to grow ____" from 2015 to 2022'
    ) +
  xlab('')

# main visualisation!  ----
# plot interest (grow vegetables,  grow flowers, and grow food) vs time
# finalised labelled and annotated plot

# manually offset time series to make it easier to add annotations and lines that cross the subplots
y_step <- 105
grow_data_tidy <- grow_data_tidy |>
  mutate(y = interest)
for (i in 1:length(search_terms)) {
  grow_data_tidy <- grow_data_tidy |>
    mutate(y = ifelse(search == search_terms[i], y + y_step*(i-3)*-1, y))
}

# set some recurring settings/values as variables
max_months = max(grow_data_tidy$months_elapsed)
segm_start <- -3  
segm_end <- max_months + 25
segm_y <- seq(0, (n_search-1)*y_step, by = y_step)
segm_y <- c(segm_y, segm_y - 2)
x_tick <- seq(0.5, max_months + 0.5, by = 12)
month_lockdown <- which(
  (grow_data_tidy$month[seq(1, max_months * n_search, by = n_search)] == 3) &
  (grow_data_tidy$year[seq(1, max_months * n_search, by = n_search)] == 2020)
)
step_lockdown <- 0.15
x_lockdown <- seq(month_lockdown - step_lockdown, month_lockdown + step_lockdown, by = step_lockdown)
y_start <- -35

# fonts

# registered fonts with numbers = lining

serif_font <- 'aleg_lining'
register_font(
  name = serif_font,
  plain = '/Users/gmschroe/Library/Fonts/Alegreya-Medium.ttf',
  bold = '/Users/gmschroe/Library/Fonts/Alegreya-Bold.ttf',
  italic = '/Users/gmschroe/Library/Fonts/Alegreya-Italic.ttf',
  features = font_feature(numbers = "lining")
)

sans_font <- 'aleg_sans_lining'
register_font(
  name = sans_font,
  plain = '/Users/gmschroe/Library/Fonts/AlegreyaSans-Medium.ttf',
  bold = '/Users/gmschroe/Library/Fonts/AlegreyaSans-Bold.ttf',
  italic = '/Users/gmschroe/Library/Fonts/AlegreyaSans-MediumItalic.ttf',
  features = font_feature(numbers = "lining")
)

# alternative, use non-registered fonts - can use with dev.new
# these differ from the above in that the numbers will have different vertical positions

#serif_font <- 'Alegreya'
#sans_font <- 'Alegreya Sans'

# set colours (not all used)
clr_grey1 <- '#1b160f'
clr_grey2 <- '#404336'
clr_grey3 <- '#5e5d5b'
  
clr_taupe2 <- "#636059"
clr_taupe3 <- '#7A766D'
clr_taupe3b <- '#8C877D'
clr_taupe4 <- '#a9a397'
  
clr_pink <- '#b5395d'
clr_orange <-'#b65c15'
clr_green1 <- '#133017'
clr_green2 <- '#4a674a'
clr_green3 <- '#41671f'
clr_green4 <- '#597c3a'
clr_background <- '#FCF8EB' ##f7f2e3' # '#FFFCF5'
  
search_pal <- c(clr_green4, clr_pink, clr_orange) # search term colours

# lineplot 

# uncomment dev to see figure with correct proportions in the R Studio session
# however, does not work with registered fonts (which need AGG graphs device)

#if (length(dev.list()) > 0) {dev.off()}
#dev.new(width = 7, height = 5, unit = 'in', noRStudioGD = TRUE)

ggplot(grow_data_tidy, 
       aes(x = months_elapsed, y = y, group = search, colour = search)
) +
  # add lines and colour
  geom_line(linewidth = 0.75) + 
  scale_colour_manual(values = search_pal) + 
  
  # theme changes - remove grid lines, border, axis lines, background
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.line = element_blank(),
    panel.background = element_blank()
    ) +
  
  # x axis/baseline for each segment
  annotate("segment", 
           x = segm_start, xend = segm_end, 
           y = segm_y, yend = segm_y,
           color = clr_taupe4, linewidth = 0.25, linetype = 1) +
  annotate("segment",
           x = rep(x_tick, 3), xend = rep(x_tick, 3),
           y = rep(seq(-4, (n_search - 1) * y_step, by = y_step), each = length(x_tick)),
           yend = rep(seq(2, (n_search - 1) * y_step + 2, by = y_step), each = length(x_tick)),
           color = clr_taupe4, linewidth = 0.25, linetype = 1) + 
  
  # remove x and y axis ticks; set axis limits
  scale_x_continuous(
    limits = c(segm_start, segm_end),
    breaks = c(),
    labels = ''
  ) +
  scale_y_continuous(limits = c(y_start, (n_search + 0.45) * y_step + abs(y_start)),
                     breaks = c(), labels = '') +
  theme(axis.ticks.length = unit(0, 'inch'),
        legend.position = 'none', # removes legend
        axis.title.y = element_blank(),
        plot.background = element_rect(fill = clr_background, colour = 'white'),
        plot.margin = unit(c(0, 0, 0, 0), 'inch')
        ) +

  # labels
  # annotation: first lockdown
  annotate("segment",
         x = rep(x_lockdown, n_search),
         xend = rep(x_lockdown, n_search),
         y = rep(seq(2, n_search * y_step, y_step), each = length(x_lockdown)), 
         yend = rep(seq(y_step - 4, n_search * y_step, y_step), each = length(x_lockdown)),
         color = clr_grey1, linewidth = 0.2, linetype = 5, alpha = 0.5) + 
  geom_textbox(
    data = tibble(
      x = month_lockdown - 1, 
      y = (n_search) * y_step - 40,
      label = paste0('<span style = "color:', clr_grey2, '; font-size:9.1pt;">',
                     '<b>March 2020</b><br>First UK lockdown sees',
                     ' spike in gardening searches</span>'),
    ), 
    mapping = aes(x = x, y = y, label = label),
    family = sans_font, 
    inherit.aes = FALSE,
    box.colour = NA, fill = NA,     
    width = unit(1, 'inch'),
    box.padding = unit(rep(0, 4), 'pt'),
    hjust = 1, lineheight = 0.95) + 
  
  # title
  geom_textbox(
    data = tibble(
      x = segm_start,
      y = (n_search + 0.25) * y_step,
      label = paste0(
        '<span style="font-size:18pt; color:', clr_grey1, ' ; font-family:', serif_font, ';">',
        'Gardening interest surged during the COVID-19 pandemic',
        '</span>', '<span style="font-size:45pt;"><br></span>',
        '<span style="font-size:9.5pt;">',
        'Google searches for how to grow ',
        '<b style="color:', search_pal[1],';">vegetables</b>, ',
        '<b style="color:', search_pal[2], ';">flowers</b>, and ',
        '<b style="color:', search_pal[3], ';">food</b> from 2015 to 2022 in the United Kingdom',
        '</span>'
      )
    ),
    mapping = aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    family = sans_font,
    colour = clr_taupe2,
    hjust = 0,
    vjust = 0,
    fill = NA,
    box.colour = NA,
    width = unit(7, 'inch'),
    box.padding = unit(rep(0, 4), 'pt')
  ) +
  xlab('') +

  # caption
  geom_textbox(
    data = tibble(
      x = segm_start,
      y = y_start,
      label = paste0(
        '<span style="font-size:8pt; color:', clr_taupe3, ';">',
        'Visualisation by <b>Gabrielle M. Schroeder</b> | Data from Google Trends',
        '</span>'
      )
    ),
    mapping = aes(x = x, y = y, label = label),
    family = sans_font,
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,
    fill = NA,
    box.colour = NA,
    width = unit(7, 'inch'),
    box.padding = unit(rep(0, 4), 'pt')#,
   # fontface = 'italic'
  ) +

  # additional annotations

  # seasonal changes
  geom_textbox(
    data = tibble(
      x = month_lockdown/2 + 4,
      y = (n_search - 1) * y_step - 40,
      label = paste0('<span style = "color:', clr_taupe3, '; font-size:9.1pt;">',
                     'Both ',
                     '<span style="color:', search_pal[1],';">vegetable</span> and ',
                     '<span style="color:', search_pal[2], ';">flower</span>',
                     ' searches increase in spring </span>'),
    ),
    mapping = aes(x = x, y = y, label = label),
    family = sans_font,
    inherit.aes = FALSE,
    box.colour = NA, fill = NA,
    width = unit(1.6, 'inch'),
    box.padding = unit(rep(0, 4), 'pt'),
    hjust = 0.5, lineheight = 0.8
    ) +
  geom_curve( # arrow for vegetables
    data = tibble(
      x = 19,
      xend = 16,
      y = y_step + 73,
      yend = y_step + 120
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    arrow = arrow(length = unit(5, 'pt'), type = 'open'),
    inherit.aes = FALSE,
    curvature = -0.5,
    colour = clr_taupe3,
    linewidth = 0.4
  ) +
  geom_curve( # arrow for flowers
    data = tibble(
      x = 46,
      xend = 53,
      y = y_step + 73,
      yend = y_step + 53
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    arrow = arrow(length = unit(5, 'pt'), type = 'open'),
    inherit.aes = FALSE,
    curvature = -0.35,
    colour = clr_taupe3,
    linewidth = 0.4
  ) +
  # food patterns
  geom_textbox(
    data = tibble(
      x = month_lockdown*1.07,
      y = (n_search - 2) * y_step - 40,
      label = paste0('<span style = "color:', clr_taupe3, '; font-size:9.1pt;">',
                     ' "How to grow <span style="color:', search_pal[3],';">',
                     'food</span>" ',
                     'started varying seasonally after the first lockdown',
                     '</span>'),
      ),
    mapping = aes(x = x, y = y, label = label),
    family = sans_font,
    inherit.aes = FALSE,
    box.colour = NA, fill = NA,
    width = unit(1.75, 'inch'),
    box.padding = unit(rep(0, 4), 'pt'),
    hjust = 0, lineheight = 0.8
    ) +

  # years
  annotate("text", x = seq(6.5, max_months, by = 12),
           y = -8, label = 2015:2022,
           color = clr_taupe3, size = 3.5, vjust = 1, family = sans_font) +

  # mark y axis
  annotate("text", x = segm_start, y = c(y_step * (n_search) - 7, y_step * (n_search - 1) + 7),
           label = c('high interest', 'low interest'),
           hjust = 0, fontface = 'italic', size = 2.75,
           color = clr_taupe3, family = sans_font) +

  # searches
  annotate("richtext", x = max_months + 1,
           y = seq(y_step * (n_search - 1) -2, -2, by = -y_step),
           label = paste0('<span style="font-size:11pt;"> "</span>how to grow',
                          '<span style="font-size:20pt;"><br></span>',
                          '<span style="font-size:11pt;color:', clr_background, ';"> "</span>', # hidden quotes for spacing
                          '<b style="font-size:17pt; font-family:', serif_font, ';">',
                          search_terms,
                          '</b><span style="font-size:11pt;">"</span>'),
           hjust = 0, vjust = 0, color = search_pal,
           fill = NA, label.color = NA, size = 3, family = sans_font) +

  # set aspect ratio
  coord_fixed(ratio = 0.2)

# save 
fig_file <- file.path('plots','R_gardening_COVID.png')

ggsave(fig_file, width = 7, height = 5, units = "in", dpi = 600)
################################################################################
# y-axis = month, with each search a different subplot ----

#if (length(dev.list()) > 0) {dev.off()}

# re-order and add a different colours for pre-COVID years
search_pal <- c('#9C4AA3', '#2690A6', '#A3C429')
search_pal_light <- c('#d3a7d7', '#97dae7', '#d7e996')
n_year <- length(2015:2020)
facet_pal <- rep(search_pal_light, each = n_year)
for (i in 1:length(search_terms)) {
  facet_pal[n_year * i] <- search_pal[i]
}

# plot
ggplot(
  grow_data_tidy |>
    filter(year <= 2020) |> # simplify by only plotting up to 2020
    mutate(
      clr = paste(search, year, sep = '.') # color depends on search term and year
      ),
  aes(x = month, y = interest, group = factor(year), colour = clr)
  ) +
  geom_line(aes(linewidth = covid)) + 
  scale_colour_manual(values = facet_pal) +
  scale_linewidth(range = c(0.75, 1.5)) + 
  theme_bw() +
  facet_grid(cols = vars(search)) +
  theme(legend.position = 'none')

# compare peak interest each year vs time
# compare range in interest each year vs time
