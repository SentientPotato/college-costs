library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
options(readr.show_col_types = FALSE)
dat = read_csv("median-income.csv") %>%
    left_join(read_csv("college-costs.csv"), by = "Year") %>%
    arrange(Year) %>%
    mutate(
        IncomeIncrease  = 100 * (Income - Income[1]) / Income[1],
        PublicIncrease  = 100 * (Public - Public[1]) / Public[1],
        PrivateIncrease = 100 * (Private - Private[1]) / Private[1]
    ) %>%
    pivot_longer(
        cols = ends_with("Increase"),
        names_to = "Metric",
        values_to = "Increase"
    ) %>%
    mutate(Metric = gsub("Increase", "", Metric)) %>%
    mutate(Metric = ifelse(grepl("Inc", Metric), Metric, paste(Metric, "U")))

ttl = "% Rise in Median Household Income & Mean College Costs"
cap = "Figure by Sentient Potato (Twitter: @SentientPotato6)"
URL = "github.com/SentientPotato/college-costs"
cap = paste(cap, paste0("Replication code at ", URL), sep = "\n")
map = aes(x = Year, y = Increase, color = Metric)
pal = c(Income = "#e69f00", "Public U" = "#56b4e9", "Private U" = "#009e73")
plt = ggplot(data = dat, mapping = map) +
    geom_line(size = 1, na.rm = TRUE) +
    labs(title = ttl, caption = cap) +
    scale_color_manual(values = pal, name = "") +
    theme_bw() +
    theme(
        axis.title = element_blank(),
        plot.title = element_text(size = 8.5),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8),
        plot.caption = element_text(color = "#5f5f5f", hjust = 0, size = 8)
    )
ggsave(
    filename = "college-costs.png",
    plot = plt,
    device = "png",
    width = 1200 / 300,
    height = 675 / 300
)
