library("ggplot2")
library("dplyr")
library("tidyverse")

book_10_df <- read.csv("~/info201/2017-2023-10-Checkouts-SPL-Data.csv", stringsAsFactors = FALSE)

asoiaf_year_trend <- book_10_df %>%
  group_by(CheckoutYear, MaterialType) %>%
  filter(CheckoutYear > 2017) %>%
  filter(str_detect(MaterialType, "EBOOK")) %>%
  filter(str_detect(Title, "A Song of Ice and Fire Series")) %>%
  summarize(Total = sum(Checkouts))

show_year_trend <- book_10_df %>%
  group_by(CheckoutYear, MaterialType) %>%
  filter(CheckoutYear > 2017) %>%
  filter(str_detect(MaterialType, "VIDEODISC")) %>%
  filter(str_detect(Title, "Game of thrones. The complete")) %>%
  summarize(TotalShow = sum(Checkouts))


ggplot() +
  geom_line(data = asoiaf_year_trend,
           aes(x = CheckoutYear, y = Total, color = "ASOIAF(Books)")) +
  geom_line(data = show_year_trend,
            aes(x = CheckoutYear, y = TotalShow, color = "GOT(Video)")) +
  scale_x_continuous(breaks = seq(2018, 2023)) +
  scale_y_continuous(breaks = seq(500, 5000, 500)) +
  labs(
    title = "A Song of Ice and Fire (ASOIAF) vs Game of Thrones(GOT) TV Series Checkouts",
    subtitle = "From 2018 to 2023",
    x = "Years",
    y = "Number of Checkouts",
    color = "Media Form"
  )
