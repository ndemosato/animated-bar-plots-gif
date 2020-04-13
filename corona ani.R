library(tidyverse)
library(ggplot2)
library(gganimate)
library(scales)
countries_aggregated_csv <- read_csv("data/countries-aggregated_csv.csv")

countries_formated<-countries_aggregated_csv%>% group_by(Date)%>%mutate(rank=rank(-Deaths),value_rel=Deaths/Deaths[rank==1],value_lbl=paste0("", round(Deaths/1)))%>% group_by(Country)%>%filter(rank<=10)%>%ungroup()
view(countries_formated)

static_plot<-ggplot(countries_formated,aes(rank,group=Country,fill=as.factor(Country),color=as.factor(Country)))+
  geom_tile(aes(y=Deaths/2,
                height=Deaths,
                width=0.9),alpha=0.8,color=NA)+
  geom_text(aes(y=0,label=paste(Country,"")),vjust=0.2,hjust=1,size=8)+
  geom_text(aes(y=Deaths,label=value_lbl,hjust=0,size=30))+
  coord_flip(clip = "off",expand=FALSE)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_reverse()+
  guides(color=FALSE,fill=FALSE)+
  theme(axis.line = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major.x = element_line(size = .1,color="grey"),
        panel.grid.minor.x = element_line(size = .1,color="grey"),
        plot.title = element_text(size = 25,hjust=0.5,face="bold",color="red",vjust = -1),
        plot.subtitle = element_text(size=18,hjust = 0.5,face="italic",color = "red"),
        plot.caption = element_text(size=20,hjust = 0.5,face = "italic",color = "blue"),
        plot.background  =element_blank(),
        plot.margin =margin(2,2, 2,4, "cm"))
anim=static_plot+transition_states(Date,transition_length = 4,state_length = 1)+
  view_follow(fixed_x = TRUE)+
  labs(title='Covid19 Deaths per Date :{closest_state}',
       subtitle = "Top 10 Countries",
       caption = "Data source:John Hopkins @satoMartin")
animate(anim,200,fps=3,width=1200,height=1000,renderer = gifski_renderer("deaths.gif"))
#topconfirmed 15 cases 
countries_aggregated_csv <- read_csv("data/countries-aggregated_csv.csv")
countries_formated<-countries_aggregated_csv%>% group_by(Date)%>%mutate(rank=rank(-Confirmed),value_rel=Confirmed/Confirmed[rank==1],value_lbl=paste0("", round(Confirmed/1)))%>% group_by(Country)%>%filter(rank<=15)%>%ungroup()
attach(countries_formated)
static_plot<-ggplot(countries_formated,aes(rank,group=Country,fill=as.factor(Country),color=as.factor(Country)))+
  geom_tile(aes(y=Confirmed/2,
                height=Confirmed,
                width=0.9),alpha=0.8,color=NA)+
  geom_text(aes(y=0,label=paste(Country,"")),vjust=0.2,hjust=1,size=8)+
  geom_text(aes(y=Confirmed,label=value_lbl,hjust=0,size=30))+
  coord_flip(clip = "off",expand=FALSE)+
  scale_y_continuous(labels = scales::comma)+
  scale_x_reverse()+
  guides(color=FALSE,fill=FALSE)+
  theme(axis.line = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.major.x = element_line(size = .1,color="grey"),
        panel.grid.minor.x = element_line(size = .1,color="grey"),
        plot.title = element_text(size = 25,hjust=0.5,face="bold",color="red",vjust = -1),
        plot.subtitle = element_text(size=18,hjust = 0.5,face="italic",color = "red"),
        plot.caption = element_text(size=20,hjust = 0.5,face = "italic",color = "blue"),
        plot.background  =element_blank(),
        plot.margin =margin(2,2, 2,4, "cm"))
anim=static_plot+transition_states(Date,transition_length = 4,state_length = 1)+
  view_follow(fixed_x = TRUE)+
  labs(title='Covid19 confirmed cases per Date :{closest_state}',
       subtitle = "Top 10 Countries",
       caption = "Data source:John Hopkins @satoMartin")
animate(anim,200,fps=3,width=1200,height=1000,renderer = gifski_renderer("confirmed.gif"))
