
# Bubble Graph - BoE DataSource: IHME Eurpean countries-------

library(tidyverse)

# download the table from Google sharing location--------------------------------
tb <- read.csv("graphs/Table1_v1.xlsx - Folha1.csv")

tb <- tibble(tb)

# vector countries of interest -----------
my_countries <-tb$X[2:30]

####################################

# download data from IHME for DALY, YLL, YLD and Deaths - EU countries diff 2010-2019
IHME_eu <- read.csv("IHME-GBD_2019_DATA-61891ecf-1/IHME-GBD_2019_DATA-61891ecf-1.csv")

bubble_df <- tibble(IHME_eu)

bubble_df$measure <-case_when(
  bubble_df$measure =="DALYs (Disability-Adjusted Life Years)"~ "DALY",
  bubble_df$measure =="YLDs (Years Lived with Disability)"~ "YLD",
  bubble_df$measure =="YLLs (Years of Life Lost)"~ "YLL",
  bubble_df$measure =="Deaths"~ "Deaths")

# pivot_wider
bubble_df <- bubble_df%>%
  filter(location %in% my_countries)%>%
  select(location,measure,sex_name=sex,cause, value=val)%>%
  pivot_wider(names_from = measure,values_from = value)


###################### PLOTTING #####################


library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)

# FIRST scatter PLOT two choises
with(bubble_df,plot(DALY~YLD+YLL,col=cause))


# SECOND bubble plot ###############################################
bubble_plot <- bubble_df %>%
  arrange(desc(DALY)) %>%
  group_by(cause)%>%summarise_each(funs(mean))%>%
  
  ggplot(aes(x=YLL, y=YLD, size=DALY, fill=cause)) +
  ggrepel::geom_text_repel(aes(label=cause), 
                           max.overlaps = Inf,size=2)+ #, stat="identity",position="dodge"
  geom_point(alpha=0.5, shape=21, color="black") +
  labs(title="",
       subtitle="",
       caption="") +
  scale_size(range = c(.1, 10), name="DALYs (rate)") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") + 
  theme_ipsum() + 
  theme(legend.position="bottom") +
  ylab("YLD") +
  xlab("YLL") 


# saving ###########


ragg::agg_png(here::here("graphs", "bubble_plot_BoE.png"),
              res = 320, width = 14, height = 8, units = "in")
bubble_plot

dev.off()













