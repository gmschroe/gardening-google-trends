# libraries -----

rm(list = ls())

library(tidyverse)
library(ggthemes)

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

# pivot wider for datawrapper version -----

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