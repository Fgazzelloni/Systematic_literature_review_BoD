

# BoE bubble graph IHME datasource----------


library(tidyverse)
library(plotly)
library(hrbrthemes)
library(viridis)


library(glue)
library(patchwork)
library(sf)



library(gridExtra)
library(grid)


# ihme_backup <- read.csv("graphs/IHME-BoE.csv")

# download the table from the sharing location--------------------------------
tb <- read.csv("graphs/Table1_v1.xlsx - Folha1.csv")
tb <- tibble(tb)

# select countries of interest -----------
my_countries <-tb$X[2:30]


# download further raw data from IHME ---------------------------
data_all_countries <- read.csv("graphs/data_all_countries.csv")
my_df <- tibble(data_all_countries)


# select my_countries from the raw data -----------------------
my_df <- my_df%>%filter(location_name %in% my_countries)%>%
  select(location_name,measure_name,cause_name,val)

plyr::count(my_df$location_name)
names(my_df)

####################################

IHME_eu <- read.csv("graphs/IHME-GBD_2019_DATA-61891ecf-1/IHME-GBD_2019_DATA-61891ecf-1.csv")

bubble_df <- tibble(IHME_eu)

bubble_df$measure <-case_when(
  bubble_df$measure =="DALYs (Disability-Adjusted Life Years)"~ "DALY",
  bubble_df$measure =="YLDs (Years Lived with Disability)"~ "YLD",
  bubble_df$measure =="YLLs (Years of Life Lost)"~ "YLL",
  bubble_df$measure =="Deaths"~ "Deaths")

table(bubble_df$measure)
  
bubble_df <- bubble_df%>%
  filter(location %in% my_countries)%>%
  select(location,measure,sex_name=sex,cause, value=val)%>%
  pivot_wider(names_from = measure,values_from = value)



bubble_df%>%arrange(DALY)%>%group_by(cause)%>%summarise_each(funs(mean))%>%

  
ggplot(aes(x=rnorm(YLL),y=rnorm(YLD),size=DALY)) + 
  geom_point(aes(fill=cause),alpha=0.5, shape=21) + 
  geom_smooth(method="loess",color="purple",size=0.8,linetype="dashed") + 
  #ggrepel::geom_text_repel(aes(label=cause), max.overlaps = Inf,size=2)+
  #coord_flip() +  
  scale_size(range = c(.1, 10), name="DALYs (rate)") +
  #scale_fill_viridis(discrete=TRUE, guide=FALSE, option="B") + 
  scale_x_reverse() +
  labs(fill="") +
  theme_bw()+
  theme(legend.position = "none")
  
ragg::agg_png(here::here("graphs", "bubble_plot2_BoE.png"),
              res = 320, width = 14, height = 8, units = "in")
bubble_plot

dev.off()


############ SECOND PLOT #################################
# download numbers ###################
numbers <- read.csv("graphs/IHME-GBD_2019_DATA-52a1b1bf-1/IHME-GBD_2019_DATA-52a1b1bf-1.csv")
numbers<-tibble(numbers)
head(numbers)
numbers$measure_name <-case_when(
  numbers$measure_name =="DALYs (Disability-Adjusted Life Years)"~ "DALY",
  numbers$measure_name =="YLDs (Years Lived with Disability)"~ "YLD",
  numbers$measure_name =="YLLs (Years of Life Lost)"~ "YLL",
  numbers$measure_name =="Deaths"~ "Deaths")
names(numbers)
my_numbers <- numbers%>%select(1,2,5,6,9,10,11,12,15)
head(my_numbers)

range(my_numbers$val)

# setting the legend for causes ############
causes_legend <- my_numbers%>%select(cause_id,cause_name)
causes_legend<-unique(causes_legend)
causes_legend<-as.data.frame(causes_legend)
names(causes_legend)<-c("id","Level 2 Cause")



# plotting by cause #############
# https://r-graphics.org/recipe-bar-graph-labels

bar_plot_BoE <-my_numbers %>%filter(sex_name=="Both")%>%
  arrange(desc(val),measure_id) %>%
  group_by(cause_id,measure_id,measure_name,cause_name)%>%summarise(avg_value=mean(val))%>%ungroup()%>%
   
  ggplot(aes(x=interaction(measure_id,avg_value),y=avg_value,
             fill=factor(measure_name))) +
  geom_col(alpha=0.5) +
  geom_text(aes(label = cause_id), vjust = 1.5, colour = "black",size=1) + 
  
  labs(title="European Union metrics by Level 2 Causes",
       subtitle="2010-2019",
       caption="DataSource: IHME",
       fill="Metrics",
       color="Level 2 Causes") + 
   
  theme_minimal() + 
  theme(plot.title = element_text(face="bold"),
        legend.position="top",
        legend.title = element_text(face="bold"),
        axis.text.x = element_blank(),
        axis.line.y = element_line(size=0.8,colour = "grey58"),
        axis.text.y = element_text(),
        panel.grid = element_blank()) +
  ylab("Metrics average value") +xlab("")

library(ggpubr)
legend2_plot<-ggtexttable(causes_legend, rows = NULL,theme=ttheme("minimal",
                                                                  base_size=6,
                                                                  padding = unit(c(2, 2), "mm")))

bar <- bar_plot_BoE+annotation_custom(ggplotGrob(legend2_plot),
                               xmin = 70, ymin = -1,
                               xmax = 80)
             

ragg::agg_png(here::here("graphs", "bar_plot_BoE.png"),
              res = 320, width = 14, height = 8, units = "in")
bar

dev.off()


#############################################################
             
library(patchwork)
bubble_plot1/bubble_plot2

#####################################################

# lm ############

with(bubble_df, plot(Deaths))
with(bubble_df, plot(YLL))
with(bubble_df, plot(YLD))
with(bubble_df, plot(DALY))

###################################
fit1 <- lm(DALY~0+YLD,bubble_df,method = "qr")
fit2 <- lm(DALY~YLL+YLD,bubble_df,method = "qr")

summary(fit1)

par(mfcol = c(2,4))
plot(fit1,main="modello additivo")
plot(fit2, main="modello con interazione")



# http://www.learnbymarketing.com/tutorials/linear-regression-in-r/
layout(matrix(c(1,1,2,3),2,2,byrow=T))
#Spend x Residuals Plot
plot(fit2$resid~bubble_df$DALY[order(bubble_df$YLD)],
     main="YLD Residuals\nfor Simple Regression",
     xlab="YLD", ylab="Residuals")
abline(h=0,lty=2)
#Histogram of Residuals
hist(fit2$resid, main="Histogram of Residuals",
     ylab="Residuals")
#Q-Q Plot
qqnorm(fit2$resid)
qqline(fit2$resid)


# http://www.insular.it/tag/lm/
library(car)
# qq-plot for studentized resid
par(mfcol = c(1,2))
qqPlot(fit1, main="QQ Plot – modello additivo")
qqPlot(fit2, main="QQ Plot – modello con interazione")


library(car)
# non-constant error variance test
ncvTest(fit1)
ncvTest(fit2)
# plot studentized residuals vs. fitted values
par(mfcol = c(1,2))
spreadLevelPlot(fit1, main=paste("Spread-Level - modello additivo"))
spreadLevelPlot(fit2, main=paste("Spread-Level - modello con interazione")) # this is the most similar



library(gvlma)

# modello additivo
gvmodel1 <- gvlma(fit1)
summary(gvmodel1)










yll<-bubble_df$value[bubble_df$measure=="YLL"]
yld<-bubble_df$value[bubble_df$measure=="YLD"]
daly<-bubble_df$value[bubble_df$measure=="DALY"]

table(bubble_df$measure)

my_cause <- unique(bubble_df$cause)
length(my_cause)

bubble_df<-bubble_df%>%filter(cause %in%my_cause)


ggplot(bubble_df)+
  geom_point(aes(x=cause,y=DALY))

################# plotting #################

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)

with(bubble_df,plot(DALY~YLD+YLL,col=cause))



bubble_df %>%
  arrange(desc(DALY)) %>%
  group_by(cause)%>%summarise_each(funs(mean))%>%
  
  ggplot(aes(x=YLL, y=YLD, size=DALY, fill=cause)) +
  ggrepel::geom_text_repel(aes(label=cause), 
                           max.overlaps = Inf,size=2)+ #, stat="identity",position="dodge"
  geom_point(alpha=0.5, shape=21, color="black") +
  labs(title="",
       subtitle="",
       caption="",
       fill="Level 2 causes")+
  scale_size(range = c(.1, 10), name="DALYs (rate)") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") + 
  theme_ipsum() + 
  theme(legend.position="bottom"
        ) +
  ylab("YLD") +
  xlab("YLL") +
  theme(legend.position = "none")





YLD <- YLD%>%
  mutate(YLD=measure,YLD_value=val)%>%
  select(YLD_value,sex)

DALY <-DALY%>%
  mutate(DALY=measure,DALY_value=val)%>%
  select(DALY_value,sex)

my_data <- YLL%>%
  mutate(YLL=measure,YLL_value=val)%>%
  select(YLL_value,sex)%>%inner_join(YLD,by="sex")%>%
  inner_join(DALY,by="sex")



####################population##################
population2010 <- read.csv("IHME_GBD_2019_POP_2010_Y2020M10D15.csv")%>%
  filter(age_group_name=="All Ages")%>%
  filter(location_name %in% my_countries)%>%
  filter(!sex_name=="both")%>%
  mutate(population2010=val)%>%
  select(location_name,sex_name,population2010)
population2011 <- read.csv("IHME_GBD_2019_POP_2011_Y2020M10D15.csv")%>%filter(age_group_name=="All Ages")%>%
  filter(location_name %in% my_countries)%>%
  filter(!sex_name=="both")%>%
  mutate(population2011=val)%>%
  select(location_name,sex_name,population2011)
population2012 <- read.csv("IHME_GBD_2019_POP_2012_Y2020M10D15.csv")%>%filter(age_group_name=="All Ages")%>%
  filter(location_name %in% my_countries)%>%
  filter(!sex_name=="both")%>%
  mutate(population2012=val)%>%
  select(location_name,sex_name,population2012)
population2013 <- read.csv("IHME_GBD_2019_POP_2013_Y2020M10D15.csv")%>%filter(age_group_name=="All Ages")%>%
  filter(location_name %in% my_countries)%>%
  filter(!sex_name=="both")%>%
  mutate(population2013=val)%>%
  select(location_name,sex_name,population2013)
population2014 <- read.csv("IHME_GBD_2019_POP_2014_Y2020M10D15.csv")%>%filter(age_group_name=="All Ages")%>%
  filter(location_name %in% my_countries)%>%
  filter(!sex_name=="both")%>%
  mutate(population2014=val)%>%
  select(location_name,sex_name,population2014)
population2015 <- read.csv("IHME_GBD_2019_POP_2015_Y2020M10D15.csv")%>%filter(age_group_name=="All Ages")%>%
  filter(location_name %in% my_countries)%>%
  filter(!sex_name=="both")%>%
  mutate(population2015=val)%>%
  select(location_name,sex_name,population2015)
population2016 <- read.csv("IHME_GBD_2019_POP_2016_Y2020M10D15.csv")%>%filter(age_group_name=="All Ages")%>%
  filter(location_name %in% my_countries)%>%
  filter(!sex_name=="both")%>%
  mutate(population2016=val)%>%
  select(location_name,sex_name,population2016)
population2017 <- read.csv("IHME_GBD_2019_POP_2017_Y2020M10D15.csv")%>%filter(age_group_name=="All Ages")%>%
  filter(location_name %in% my_countries)%>%
  filter(!sex_name=="both")%>%
  mutate(population2017=val)%>%
  select(location_name,sex_name,population2017)
population2018 <- read.csv("IHME_GBD_2019_POP_2018_Y2020M10D15.csv")%>%filter(age_group_name=="All Ages")%>%
  filter(location_name %in% my_countries)%>%
  filter(!sex_name=="both")%>%
  mutate(population2018=val)%>%
  select(location_name,sex_name,population2018)
population2019 <- read.csv("IHME_GBD_2019_POP_2019_Y2020M10D15.csv")%>%filter(age_group_name=="All Ages")%>%
  filter(location_name %in% my_countries)%>%
  filter(!sex_name=="both")%>%
  mutate(population2019=val)%>%
  select(location_name,sex_name,population2019)

population_2010_2019 <- population2010%>%
  inner_join(population2011,by=c("location_name","sex_name"))%>%
  inner_join(population2012,by=c("location_name","sex_name"))%>%
  inner_join(population2013,by=c("location_name","sex_name"))%>%
  inner_join(population2014,by=c("location_name","sex_name"))%>%
  inner_join(population2015,by=c("location_name","sex_name"))%>%
  inner_join(population2016,by=c("location_name","sex_name"))%>%
  inner_join(population2017,by=c("location_name","sex_name"))%>%
  inner_join(population2018,by=c("location_name","sex_name"))%>%
  inner_join(population2019,by=c("location_name","sex_name"))

population_2010_2019_long <- population_2010_2019%>%
  pivot_longer(cols=3:12,names_to="population_10_19",values_to="pop_value")%>%
  rename(location=location_name)%>%
  group_by(location)%>%summarize(avg_pop=mean(pop_value))





ggplot(data=population_2010_2019_long,
         aes(x=sex_name, y=location_name,  size= pop_value, color=factor(sex_name))) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 18), name="YLLs")



  
df_cause_and_pop <- bubble_df%>%
  inner_join(population_2010_2019_long,by="location")%>%
  group_by(location,cause)%>%summarise_if(is.numeric,mean,na.rm=T)

df_cause_and_pop[(is.na(df_cause_and_pop))]<-0
sum(is.na(df_cause_and_pop))

pop<-data.frame(df_cause_and_pop)
class(pop)
names(pop)
dim(pop)


write.rda("pop","pop_2010_2019.rda")

save(pop, file = "mydata.rda")
load(file = "mydata.rda")

data(pop)




library(ggfx)
library(gridExtra)
library(ggrepel)
library(extrafont)
options(scipen = 999)

ggplot(data=df_cause_and_pop,aes(x=log10((YLL+YLD)/DALY),y=log10(DALY),size=pop_value)) +
  geom_jitter(aes(size=pop_value,alpha=0.4),width = 0.15,height = 1) +
  geom_text(aes(label=cause),check_overlap = TRUE)+
  coord_polar() +
  scale_colour_identity() +
  labs(title="European Countries DAlys by Cause",
       subtitle="years 2010-2019",
       caption="Viz Federica Gazzelloni ! DataSource: IHME | Monochromo Day 24",
       x="proportion of DALY components",
       y="DALYs")+
  theme_grey() + 
  theme(plot.background = element_rect(fill = "black"),
        plot.title=element_text(face="bold",size= 25,family="Arial Narrow"),
        plot.subtitle=element_text(face="bold",size= 15),
        plot.caption=element_text(face="bold",size= 8))
                                       
 











