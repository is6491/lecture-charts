
library(tidyverse)

## Lecture 3
diamond_clarity <- diamonds |>
    count(clarity)

ggplot(diamond_clarity) +
    geom_col(aes(x=clarity,y=n)) +
    theme_grey(base_size=15) # 1200 x 600

ggplot(diamond_clarity) +
    geom_col(aes(x=clarity,y=n)) +
    theme_bw(base_size=15)

ggplot(diamond_clarity) +
    geom_col(aes(x=clarity,y=n)) +
    theme_classic(base_size=15)

ggplot(diamond_clarity) +
    geom_col(aes(x=clarity,y=n)) +
    theme_grey(base_size=15) + # 1200 x 600
    theme(
        axis.text.x=element_text(angle=45)
    )

ggplot(diamond_clarity) +
    geom_col(aes(y=clarity,x=n)) +
    theme_grey(base_size=15) # 1200 x 600
