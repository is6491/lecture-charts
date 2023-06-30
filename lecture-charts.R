
library(tidyverse)
library(ggrepel)

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

## Lecture 5
flights <- nycflights13::flights
delay_by_month <- flights |> mutate(delay=if_else(dep_delay > 0,1,0)) |> 
    group_by(origin,month) |> summarise(n=n(),delay=sum(delay,na.rm=T),.groups='drop')

ggplot(delay_by_month) +
    geom_col(aes(x=month,y=delay,fill=origin),position='dodge') +
    scale_x_continuous(breaks=c(1:12)) +
    labs(x='Month',y='# of Flights with a Delay',fill='Origin',
         title='LGA Often Has the Fewest Delays') +
    theme_classic(base_size=15)

ggplot(delay_by_month) +
    geom_bar(aes(x=month,y=delay,fill=origin),position='dodge',stat='identity',color='#8ca2a6') +
    scale_x_continuous(breaks=c(1:12)) +
    scale_fill_manual(values=alpha(c('#bcc3c4','#bcc3c4','#fbbf5e'),0.7)) +
    labs(x='Month',y='# of Flights with a Delay',fill='Origin',
         title='LGA Often Has the Fewest Delays') +
    theme_classic(base_size=15)

month_list <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
ggplot(delay_by_month) + 
    geom_line(aes(x=month,y=delay,color=origin),size=1) +
    scale_color_manual(values=alpha(c('#bcc3c4','#bcc3c4','#fbbf5e'),0.7)) +
    scale_x_continuous(breaks=c(1:12),labels=month_list) +
    labs(x='Month',y='# of Flights with a Delay',color='Origin',
         title='LGA Often Has the Fewest Delays') +
    theme_classic(base_size=15) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank())

delay_by_carrier <- flights |> mutate(delay=if_else(dep_delay > 0,1,0)) |> 
    group_by(carrier) |> summarise(n=n(),delay=sum(delay,na.rm=T)) |> ungroup() |>
    mutate(p=delay/n,mean_n=mean(n),mean_p=mean(p)) |>
    mutate(quadrants=case_when(
        n < mean_n ~ 'left pane',
        n >= mean_n & p >= mean_p ~ 'upper right',
        n >= mean_n & p < mean_p ~ 'lower right',
        TRUE ~ 'OTHER'
    ))

quadrant_labels <- tibble(label=c('High Flights, Low Delay Pct','High Flights, High Delay Pct'),
                          x=c(45000,45000),y=c(0.21,0.54),color=c('#292929','#fa6d29'))

ggplot(delay_by_carrier,aes(x=n,y=p,color=quadrants)) +
    geom_point(size=3,show.legend=FALSE) +
    geom_text_repel(aes(label=carrier),size=5,show.legend=FALSE) +
    
    geom_hline(yintercept=mean(delay_by_carrier$p)) +
    geom_vline(xintercept=mean(delay_by_carrier$n)) +
    
    scale_x_continuous(labels=scales::comma) +
    scale_y_continuous(labels=scales::percent) +
    labs(x='Total Flights',y='Percent Delayed',title='Percentage Delay vs Total Flights') +
    
    geom_text(data=quadrant_labels,aes(x=x,y=y,label=label,color=label),size=6) +
    scale_color_manual(values=c('#292929','#fa6d29','#bcc3c4','#fa6d29','#292929')) +
    
    theme_bw(base_size=15) +
    theme(legend.position='none',
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())
