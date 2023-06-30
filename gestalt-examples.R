
library(tidyverse)

dta <- read_csv('gestalt-examples.csv') |> 
    mutate(rn=row_number()) |>
    mutate(cat=case_when(
        rn <= 4 ~ 'Actuals',
        rn  > 4 ~ 'Forecast'
    ))

fa_text <- tribble(
    ~rn, ~y, ~lbl,
    6, 0.5, "Forecast",
    2, 0.5, "Actuals"
)

#### Enclosure ####
enclosure_ex <- ggplot(dta,aes(x=rn,y=y)) +
    geom_ribbon(aes(ymin=0,ymax=8),data=filter(dta,rn > 3),alpha=0.2) +
    geom_line() +
    geom_point(size=4) +
    geom_text(aes(label=lbl),size=12,data=fa_text) +
    labs(x='',y='') +
    theme_classic(base_size=15)
enclosure_ex
ggsave(enclosure_ex,filename='enclosure.png',height=5,width=6)

enclosure_comp <- ggplot(dta,aes(x=rn,y=y)) +
    geom_line() +
    geom_point(aes(color=cat),size=4) +
    labs(x='',y='',color='') +
    scale_color_manual(values=c('#083D77','#F95738')) +
    scale_y_continuous(limits=c(0,8)) +
    theme_classic(base_size=15)
enclosure_comp
ggsave(enclosure_comp,filename='enclosure_comp.png',height=5,width=6)

#### Closure ####
gestalt_bar <- read_csv('gestalt-bar.csv') |>
    mutate(llf=factor(ll,levels=toupper(letters)[5:1]))

bar1 <- ggplot(gestalt_bar,aes(x=values,y=llf)) +
    geom_col(fill='#083D77') +
    labs(x='',y='') +
    theme_classic(base_size=15) +
    theme(axis.line.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
bar1

bar2 <- bar1 +
    theme(axis.line.y=element_blank(),axis.ticks.y=element_blank())
bar2

ggsave(bar1,filename='bar1.png',height=5,width=6)
ggsave(bar2,filename='bar2.png',height=5,width=6)

#### Connection ####
connection_points <- ggplot(dta,aes(x=rn,y=y)) +
    geom_point(size=4) +
    labs(x='',y='',color='') +
    scale_y_continuous(limits=c(0,8)) +
    theme_classic(base_size=15)
connection_points

connection_lines <- connection_points +
    geom_line()
connection_lines

ggsave(connection_points,filename='connection_points.png',height=5,width=6)
ggsave(connection_lines,filename='connection_lines.png',height=5,width=6)


#### Visual Order ####

vo <- read_csv('visual-order-1.csv') |>
    mutate(prop_fac=reorder(property,pct_response),rn=row_number()) |>
    mutate(color_cat=case_when(rn %in% c(7,2,3) ~ 'color',.default='no-color'))

vo1 <- ggplot(vo,aes(x=pct_response,y=prop_fac)) +
    geom_col(fill='#A2A7A0') +
    scale_x_continuous(labels=scales::percent) +
    labs(x='% selecting given attribute',y='') +
    theme_classic(base_size=15) +
    theme(axis.text.x=element_text(angle=45))
vo1
ggsave(vo1,filename='vo1.png',height=5,width=9)

vo2 <- ggplot(vo,aes(x=pct_response,y=prop_fac)) +
    geom_col(aes(fill=color_cat),show.legend=FALSE) +
    scale_x_continuous(labels=scales::percent) +
    scale_fill_manual(values=c('#3A3238','#A2A7A0')) +
    labs(x='% selecting given attribute',y='') +
    theme_classic(base_size=15)
vo2
ggsave(vo2,filename='vo2.png',height=5,width=9)


#### Non-strategic use of contrast ####
non_strategic <- read_csv('non-strategic-contrast.csv')

original_long <- non_strategic |>
    filter(sample == 'Original') |>
    pivot_longer(cols=-c(sample,rated_item),names_to='business',values_to='rating')

non_strat_1 <- ggplot(original_long,aes(x=rated_item,y=rating,color=business,shape=business)) +
    geom_point() +
    geom_hline(yintercept=0) +
    labs(x='',y='',color='',shape='',title='Weighted Performance Index') +
    theme_classic(base_size=12) +
    theme(legend.position='bottom')
non_strat_1
ggsave(non_strat_1,filename='non_strat_1.png',height=5,width=9)

rescaled_long <- non_strategic |>
    filter(sample == 'Rescaled') |>
    pivot_longer(cols=-c(sample,rated_item),names_to='business',values_to='rating') |>
    mutate(rank=rank(-rating),.by=rated_item) |>
    mutate(rk_text=case_when(
        business == 'Our Business' ~ paste0(' ',rank,' of 6'),
        .default=''
    )) |>
    mutate(business_fac=factor(business,levels=rev(c('Our Business','Competitor A','Competitor B','Competitor C','Competitor D','Competitor E')))) |>
    mutate(rated_fac=factor(rated_item,levels=rev(c('Price','Convenience','Relationship','Service','Selection'))))

non_strat_2 <- ggplot(rescaled_long,aes(x=rating,y=rated_fac,fill=business_fac)) +
    geom_col(position='dodge') +
    geom_text(aes(label=rk_text),position=position_dodge2(width = 1, preserve = "single"),hjust=0) +
    labs(x='',y='',fill='') +
    scale_fill_manual(values=c('#A2A7A0','#A2A7A0','#A2A7A0','#A2A7A0','#A2A7A0','#0C6291'),
                      guide = guide_legend(reverse = TRUE,title.theme=element_text(size=2))) +
    theme_classic(base_size=20) +
    theme(legend.position='bottom',#legend.justification='top',
          axis.line=element_blank(),axis.ticks=element_blank(),axis.text.x=element_blank())
non_strat_2
ggsave(non_strat_2,filename='non_strat_2.png',height=5,width=9)
