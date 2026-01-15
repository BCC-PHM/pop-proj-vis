library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

data <- read.csv("data/2022 SNPP Population persons.csv") %>%
  filter(
    AREA_NAME %in% c(
      "Birmingham",
      "Solihull",
      "Dudley",
      "Sandwell",
      "Walsall",
      "Wolverhampton"),
    AGE_GROUP %in% c(60, 65, 70) 
  ) %>%
  mutate(
    ICB = case_when(
      AREA_NAME %in% c(
        "Birmingham",
        "Solihull"
      ) ~ "BSol ICB",
      TRUE ~ "Black Country ICB"
    )
  ) %>%
  select(
    AREA_NAME, AGE_GROUP, ICB, contains("X20")
  ) %>%
  pivot_longer(
    cols = contains("X"),
    names_to = "YEAR",
    values_to = "Population"
  ) %>%
  mutate(
    YEAR = as.numeric(
      stringr::str_extract(YEAR, "\\d{4}")
    ),
    AGE_LABEL = paste("Age =", AGE_GROUP, "years")
  ) %>%
  filter(
    YEAR <= 2036,
    YEAR >= 2026
  ) 

for (area_i in unique(data$AREA_NAME)) {
  
  data_i <- data %>%
    filter(
      AREA_NAME == area_i
    ) 
  
  highlight_i <- data_i %>%
    filter(
      YEAR %in% c(2026, 2031, 2036)
    )
  
  plt <- ggplot(
    data_i,
    aes(x = YEAR, y = Population)
  ) +
    geom_vline(
      xintercept = c(2026, 2031, 2036),
      color = "gray",
      linetype  = "dashed"
    ) +
    geom_line(
      linewidth = 1.05,
      color = "darkblue"
    ) +
    theme_bw() +
    facet_wrap(
      ~ AGE_LABEL,
      ncol = 1,
      scales = "free_y"
    ) +
    labs(
      x = "Year",
      y = "Projected Population",
      title = paste(area_i,
                    "ONS Population Projection (Based on 2022)") 
    ) +
    scale_y_continuous(
      expand = expansion(mult = c(0.4, 0.4)),
      label=comma
    ) +
    scale_x_continuous(
      breaks = seq(2026, 2036, 2)
    ) +
    geom_text(
      data = highlight_i,
      aes(x = YEAR,
          y = Population - 300,
          label = format(round(Population), big.mark = ",")),
      size = 3.5
    ) +
    geom_point(
      data = highlight_i,
      aes(x = YEAR, y = Population),
      size = 3,
      color = "black"
    ) + 
    geom_point(
      data = highlight_i,
      aes(x = YEAR, y = Population),
      size = 2.5,
      color = "#f88509"
    ) + 
    theme(strip.background = element_rect(fill="white"))
  
  file_i <- paste0(
    "output/pop-proj-",
    tolower(area_i),
    ".png"
  )

  ggsave(
    file_i,
    plt,
    width = 6,
    height = 5,
    dpi = 400
  )
  
}
