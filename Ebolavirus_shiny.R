
# shinyDashboard 
install.packages("remotes")
remotes::install_github("nik01010/dashboardthemes")


list.of.packages <- c("tidyverse","shiny","plotly","readxl","DT","lubridate","leaflet","shinyWidgets","psycho","crosstalk","shinydashboard","shinythemes",
                      "magrittr","countrycode","ggmap","geonames","tidygeocoder","rworldmap","devtools","ggpubr","zoo","plyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] 


library(tidyverse)
library(magrittr)
library(shiny)
library(readxl)
library(DT)
library(plotly)
library(lubridate) # for convert datetime
library(leaflet) # Create maps 
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes) 
library(ggplot2)
library(RColorBrewer)
library(countrycode)
library(maps)
library(ggmap)
library(dplyr)
library(tidygeocoder)
library(rworldmap)
library(devtools)
library(ggpubr)
library(zoo)
library(plyr)
library(dashboardthemes)
library(scales)
library(sp)


ebola<- read.csv("ebola_2014_2016.csv")
ebola1<-ebola
summary(ebola1)
str(ebola1)
ebola1$Date <- as.POSIXct(ebola1$Date,format="%m/%d/%Y", tz="UTC")

# by date 
ebola_agg= aggregate(ebola1[,3:10], by=list(ebola1$Date), FUN=sum, na.rm=TRUE)
colnames(ebola_agg)[colnames(ebola_agg) == "Group.1"] <- "Date"


# by country 
ebola_aggregated_country = aggregate(ebola1[,3:10], by=list(ebola1$Date,ebola1$Country), FUN=sum, na.rm=TRUE)

colnames(ebola_aggregated_country)[colnames(ebola_aggregated_country) == "Group.1"] <- "Date"
colnames(ebola_aggregated_country)[colnames(ebola_aggregated_country) == "Group.2"] <- "Country"


g<- ebola1[ebola1$Country=="Guinea",c(1,2,6,10)]
n<-ebola1[ebola1$Country=="Nigeria",c(1,2,6,10)]
s<-ebola1[ebola1$Country=="Sierra Leone",c(1,2,6,10)]
l<-ebola1[ebola1$Country=="Liberia",c(1,2,6,10)]
s2<-ebola1[ebola1$Country=="Senegal",c(1,2,6,10)]
us<-ebola1[ebola1$Country=="United States of America",c(1,2,6,10)]
s3<-ebola1[ebola1$Country=="Spain",c(1,2,6,10)]
m<-ebola1[ebola1$Country=="Mali",c(1,2,6,10)]
uk<-ebola1[ebola1$Country=="United Kingdom",c(1,2,6,10)]
i<-ebola1[ebola1$Country=="Italy",c(1,2,6,10)]

mer<-Reduce(function(x, y) merge(x, y, by='Date',all=TRUE), list(g,n,s,l,s2,us,s3,m,uk,i))
mer1<-na.locf(mer, fromLast = TRUE)

a1<-mer1[,c(1,2,3,4)]
a2<-mer1[,c(1,5,6,7)]
a3<-mer1[,c(1,8,9,10)]
a4<-mer1[,c(1,11,12,13)]
a5<-mer1[,c(1,14,15,16)]
a6<-mer1[,c(1,17,18,19)]
a7<-mer1[,c(1,20,21,22)]
a8<-mer1[,c(1,23,24,25)]
a9<-mer1[,c(1,26,27,28)]
a10<-mer1[,c(1,29,30,31)]


names(a1)<-c('Date','Country','Confirmed_case','Death_case')

a1<- ddply(a1,.(Date),summarise,Confirmed_case=max(Confirmed_case), Death_case=max(Death_case))
a1[,c(4)]<-'Guinea'
colnames(a1)[colnames(a1) == "V4"] <- "Country"

a1<-a1 %>%
  complete(Date = seq(min(a1$Date), max(a1$Date), by="day")) %>% 
  fill(c('Confirmed_case','Death_case','Country'))

names(a2)<-c('Date','Country','Confirmed_case','Death_case')
a2<- ddply(a2,.(Date),summarise,Confirmed_case=max(Confirmed_case), Death_case=max(Death_case))
a2[,c(4)]<-"Nigeria"
colnames(a2)[colnames(a2) == "V4"] <- "Country"

a2<-a2 %>%
  complete(Date = seq(min(a1$Date), max(a1$Date), by="day")) %>% 
  fill(c('Confirmed_case','Death_case','Country'))


names(a3)<-c('Date','Country','Confirmed_case','Death_case')
a3<- ddply(a3,.(Date),summarise,Confirmed_case=max(Confirmed_case), Death_case=max(Death_case))
a3[,c(4)]<- "Sierra Leone"
colnames(a3)[colnames(a3) == "V4"] <- "Country"
a3<-a3 %>%
  complete(Date = seq(min(a1$Date), max(a1$Date), by="day")) %>% 
  fill(c('Confirmed_case','Death_case','Country'))

names(a4)<-c('Date','Country','Confirmed_case','Death_case')
a4<- ddply(a4,.(Date),summarise,Confirmed_case=max(Confirmed_case), Death_case=max(Death_case))
a4[,c(4)]<-"Liberia"
colnames(a4)[colnames(a4) == "V4"] <- "Country"
a4<-a4 %>%
  complete(Date = seq(min(a1$Date), max(a1$Date), by="day")) %>% 
  fill(c('Confirmed_case','Death_case','Country'))

names(a5)<-c('Date','Country','Confirmed_case','Death_case')
a5<- ddply(a5,.(Date),summarise,Confirmed_case=max(Confirmed_case), Death_case=max(Death_case))
a5[,c(4)]<-"Senegal"
colnames(a5)[colnames(a5) == "V4"] <- "Country"
a5<-a5 %>%
  complete(Date = seq(min(a1$Date), max(a1$Date), by="day")) %>% 
  fill(c('Confirmed_case','Death_case','Country'))

names(a6)<-c('Date','Country','Confirmed_case','Death_case')
a6<- ddply(a6,.(Date),summarise,Confirmed_case=max(Confirmed_case), Death_case=max(Death_case))
a6[,c(4)]<-"United States of America"
colnames(a6)[colnames(a6) == "V4"] <- "Country"
a6<-a6 %>%
  complete(Date = seq(min(a1$Date), max(a1$Date), by="day")) %>% 
  fill(c('Confirmed_case','Death_case','Country'))

names(a7)<-c('Date','Country','Confirmed_case','Death_case')
a7<- ddply(a7,.(Date),summarise,Confirmed_case=max(Confirmed_case), Death_case=max(Death_case))
a7[,c(4)]<-"Spain"
colnames(a7)[colnames(a7) == "V4"] <- "Country"
a7<-a7 %>%
  complete(Date = seq(min(a1$Date), max(a1$Date), by="day")) %>% 
  fill(c('Confirmed_case','Death_case','Country'))


names(a8)<-c('Date','Country','Confirmed_case','Death_case')
a8<- ddply(a8,.(Date),summarise,Confirmed_case=max(Confirmed_case), Death_case=max(Death_case))
a8[,c(4)]<-"Mali"
colnames(a8)[colnames(a8) == "V4"] <- "Country"
a8<-a8 %>%
  complete(Date = seq(min(a1$Date), max(a1$Date), by="day")) %>% 
  fill(c('Confirmed_case','Death_case','Country'))

names(a9)<-c('Date','Country','Confirmed_case','Death_case')
a9<- ddply(a9,.(Date),summarise,Confirmed_case=max(Confirmed_case), Death_case=max(Death_case))
a9[,c(4)]<-"United Kingdom"
colnames(a9)[colnames(a9) == "V4"] <- "Country"
a9<-a9 %>%
  complete(Date = seq(min(a1$Date), max(a1$Date), by="day")) %>% 
  fill(c('Confirmed_case','Death_case','Country'))

names(a10)<-c('Date','Country','Confirmed_case','Death_case')
a10<- ddply(a10,.(Date),summarise,Confirmed_case=max(Confirmed_case), Death_case=max(Death_case))
a10[,c(4)]<-"Senegal"
colnames(a10)[colnames(a10) == "V4"] <- "Country"
a10<-a10 %>%
  complete(Date = seq(min(a1$Date), max(a1$Date), by="day")) %>% 
  fill(c('Confirmed_case','Death_case','Country'))

agg_country<-rbind(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10)

#assign country name to long lat
unique(ebola1$Country)

#ebola1$iso_code<-countrycode(ebola1$Country, origin = 'country.name', destination = 'iso3c')
world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)
which(world_data$region=="Guinea")

agg_country$long<-0
agg_country$lat<-0
agg_country[agg_country$Country=="Guinea",c(5,6)]<-world_data[41000,c(1,2)]
agg_country[agg_country$Country=="Nigeria",c(5,6)]<-world_data[65479,c(1,2)]
agg_country[agg_country$Country=="Sierra Leone",c(5,6)]<-world_data[84093,c(1,2)]
agg_country[agg_country$Country=="Liberia",c(5,6)]<-world_data[41000,c(1,2)]
agg_country[agg_country$Country=="Senegal",c(5,6)]<-world_data[83450,c(1,2)]
agg_country[agg_country$Country=="United States of America",c(5,6)]<-c(39.381266,-97.922211)
agg_country[agg_country$Country=="Spain",c(5,6)]<-world_data[36655,c(1,2)]
agg_country[agg_country$Country=="Mali",c(5,6)]<-world_data[61155,c(1,2)]
agg_country[agg_country$Country=="United Kingdom",c(5,6)]<-c(51.50853, -0.12574)
agg_country[agg_country$Country=="Italy",c(5,6)]<-world_data[53508,c(1,2)]



#Mortality/recovery rate


is.na(ebola1$No..of.confirmed..probable.and.suspected.cases) %>% sum()
is.na(ebola1$No..of.confirmed..probable.and.suspected.deaths) %>% sum()

ebola_agg1<-ebola_agg%>%
  complete(Date = seq(min(ebola_agg$Date), max(ebola_agg$Date), by="day")) %>% 
  fill(c('Date','No..of.suspected.cases','No..of.probable.cases','No..of.confirmed.cases','No..of.confirmed..probable.and.suspected.cases',
         'No..of.suspected.deaths','No..of.probable.deaths','No..of.confirmed.deaths','No..of.confirmed..probable.and.suspected.deaths'))

ebola_agg1$Mortality_rate<- ebola_agg1$No..of.confirmed..probable.and.suspected.deaths/ebola_agg1$No..of.confirmed..probable.and.suspected.cases
ebola_agg1$Recovery_rate<- (ebola_agg1$No..of.confirmed..probable.and.suspected.cases-ebola_agg1$No..of.confirmed..probable.and.suspected.deaths)/ebola_agg1$No..of.confirmed..probable.and.suspected.cases




#Shiny


ui <- dashboardPage(
  
  dashboardHeader(title="Ebola Virus Global Cases"),
  
  dashboardSidebar(
    
    menuItem("Ebola Virus",tabName ="Ebola Virus", icon = icon("dashboard")),br(), br(),
    sidebarMenu(img(src = "https://directorsblog.nih.gov/wp-content/uploads/2017/05/137998.jpg", height = 240, width = 230)),
    sliderInput("date",
                "Dates:",
                min = as.Date("2014-08-29","%Y-%m-%d"),
                max = as.Date("2016-03-23","%Y-%m-%d"),
                value=as.Date("2015-06-15"),
                timeFormat="%Y-%m-%d")),
  
  dashboardBody(shinyDashboardThemes(theme = "grey_dark"),
                tags$head(
                  tags$style(
                    "body{
                     min-height: 611px;
                     height: auto;
                      max-width: 1600px;
                      margin: auto;
                        }"
                  )
                ),
                
                fluidPage(
                  fluidRow(
                    
                    valueBoxOutput("Total_case"), valueBoxOutput("Death_case"),valueBoxOutput("Mortality_rate"),
                    box(width=12,
                        column(4,title="Total Cases by date",plotOutput("Plot1",width='400px')),
                        
                        column(4,title="Total Death by date",plotOutput("Plot4",width='400px')),
                        
                        column(4,title = "Confirmed ebolar virus number by country",
                               status = "info",
                               tableOutput("virus_number"))),
                    
                    
                    column(width=8,
                           leafletOutput(outputId = "map",height='800px'),
                           DT::dataTableOutput(outputId = "report")),
                    
                    column(width=4,
                           tabBox(width="800px",
                                  tabPanel("Mortality rate",
                                           plotOutput("Plot2",height="350px",width="400px")),
                                  tabPanel("Recovery rate",
                                           plotOutput("Plot3",height="350px",width="400px")))),
                    column(width=4,
                           box(width='400px',
                               plotOutput("top5_country",height='360px',width="400px")))
                    
                    
                    
                    
                    
                  )
                )
  )
)







server<-function(input,output){
  
  
  
  output$Plot1<-renderPlot({
    a = ebola_agg1 %>% filter(ebola_agg1$Date <= as.Date(input$date))
    ggplot(data = a) +
      ggtitle("Total Cases by date")+
      geom_line(aes(x=Date,y=No..of.confirmed..probable.and.suspected.cases,color="Total cases"),linetype="solid")+
      geom_line(aes(x=Date,y=No..of.confirmed.cases,color="Confirmed cases"),linetype="dashed")+
      geom_line(aes(x=Date,y=No..of.probable.cases,color="Probalbe Cases"),linetype="dotted")+
      geom_line(aes(x=Date,y=No..of.suspected.cases,color="Suspected cases"),linetype="twodash")+
      scale_y_continuous("Ebola cases") +
      scale_color_manual(name="Ebola Cases",values=c("Total cases"="coral","Confirmed cases"="#00AFBB","Suspected cases"="#E1B378","Probalbe Cases"="chartreuse"))+
      theme(legend.position="bottom", legend.direction="horizontal")+
      theme(axis.ticks=element_blank(),
            axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
            legend.background = element_rect(fill="#2C3E4F"),
            legend.key = element_rect(fill = "#2C3E4F", color = NA),
            panel.border=element_blank(),panel.grid.major=element_blank(),
            plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
            plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5,color='white',size=12),
            axis.text.x = element_text(face="bold",color='white')
            ,axis.text.y = element_text(face="bold",color='white'))
    
  })
  
  output$Plot4<-renderPlot({
    a = ebola_agg1 %>% filter(ebola_agg1$Date <= as.Date(input$date))
    
    ggplot(data = a) +
      ggtitle("Total Death by date")+
      geom_line(aes(x=Date,y=No..of.confirmed..probable.and.suspected.deaths,color="Total deaths"),linetype="solid")+
      geom_line(aes(x=Date,y=No..of.confirmed.deaths,color="Confirmed deaths"),linetype="dashed")+
      geom_line(aes(x=Date,y=No..of.probable.deaths,color="Probalbe deaths"),linetype="dotted")+
      geom_line(aes(x=Date,y=No..of.suspected.deaths,color="Suspected deaths"),linetype="twodash")+
      scale_y_continuous("Ebola deaths") +
      scale_color_manual(name="Ebola deaths",values=c("Total deaths"="coral","Confirmed deaths"="#00AFBB","Suspected deaths"="#E1B378","Probalbe deaths"="chartreuse"))+
      theme(legend.position="bottom", legend.direction="horizontal", legend.title = element_blank())+
      theme(axis.ticks=element_blank(),
            axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
            legend.background = element_rect(fill="#2C3E4F"),
            legend.key = element_rect(fill = "#2C3E4F", color = NA),
            panel.border=element_blank(),panel.grid.major=element_blank(),
            plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
            plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5,color='white',size=12),
            axis.text.x = element_text(face="bold",color='white'),axis.text.y = element_text(face="bold",color='white'))
    
  })
  
  
  output$map<- renderLeaflet({
    a = agg_country %>% filter(agg_country$Date == input$date)
    leaflet(a)%>%
      setView(0,0,2)  %>% 
      addProviderTiles(providers$Stamen.TonerLite, group = "World Imagery")%>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite")%>%
      addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>%
      addMarkers(label=a$Confirmed_case,
                 popup = paste0( "</strong>",a$Country,"<br>","<br>",
                                 "<strong>Case number:</strong>"
                                 ,"<br>",
                                 a$Confirmed_case,
                                 "<br>",
                                 "<strong>death number:</strong>"
                                 ,"<br>",
                                 a$Death_case)) %>%
      addCircles( lng=~long,lat=~lat, weight = 10, radius = ~(Death_case*100), 
                  fillOpacity = 0.5, color = "#ff0000") 
  })
  
  
  
  
  output$Plot2 <- renderPlot({
    a = ebola_agg1 %>% filter(ebola_agg1$Date <= as.Date(input$date))
    ggplot(data = a,title="Ebola case rate") +
      geom_area(aes(x=Date,y=Mortality_rate,color='Mortality rate'),fill="#FC4E07",alpha=0.4)+
      scale_color_manual(name="Ebola rate",values=c("Mortality rate"="red"))+
      scale_y_continuous("Ebola mortality rate") +
      theme(axis.ticks=element_blank(),
            axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            legend.position="none",
            panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
            panel.border=element_blank(),panel.grid.major=element_blank(),
            plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
            axis.text.x = element_text(face="bold",color='white'),axis.text.y = element_text(face="bold",color='white'))
    
    
  })  
  
  output$Plot3 <- renderPlot({
    a = ebola_agg1 %>% filter(ebola_agg1$Date <= as.Date(input$date))
    ggplot(data = a,title="Ebola case rate") +
      geom_area(aes(x=Date,y=Recovery_rate,color='Recovery rate'),fill="#69b3a2",alpha=0.4)+
      scale_color_manual(name="Ebola rate",values=c("Recovery rate"="#69b3a2"))+
      scale_y_continuous("Ebola recovery rate") +
      theme(axis.ticks=element_blank(),
            axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            legend.position="none",
            panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
            panel.border=element_blank(),panel.grid.major=element_blank(),
            plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
            axis.text.x = element_text(face="bold",color='white'),axis.text.y = element_text(face="bold",color='white'))
    
  })
  
  output$virus_number<-renderTable({
    
    agg<-agg_country[,c(4,2,3)] %>% filter(agg_country$Date == input$date)
    agg$Mortality_rate <- agg$Death_case / agg$Confirmed_case
    agg[order(-agg$Confirmed_case),]
    
  },digits = 1)
  
  output$Total_case<-renderValueBox({
    b<-ebola_agg1[,c(1,5)]%>% filter(ebola_agg1$Date == input$date)
    
    valueBox(value=b[,2],
             subtitle=strong("Total number of cases"),
             color="yellow",
             icon=icon("star"))
    
  })
  output$Death_case<-renderValueBox({
    b<-ebola_agg1[,c(1,9)] %>% filter(ebola_agg1$Date == input$date)
    
    valueBox(value=b[,2],
             subtitle=strong("Total number of death"),
             color="red",
             icon=icon("bolt"))
    
  })
  output$Mortality_rate<-renderValueBox({
    b<-ebola_agg1[,c(1,10)] %>% filter(ebola_agg1$Date == input$date)
    
    valueBox(value=round(b[,2],2),
             subtitle=strong("Mortality rate"),
             color="blue",
             icon=icon("exclamation-circle"))
    
  })
  
  output$top5_country<-renderPlot({
    a = agg_country %>% filter(agg_country$Date == input$date)
    
    top_5<- a %>% arrange(desc(Confirmed_case))%>% head(5) 
    top_5 <- within(top_5, 
                    Position <- factor(Confirmed_case, 
                                       levels=names(sort(table(Confirmed_case), 
                                                         decreasing=TRUE))))
    
    ggplot(top_5, aes(x=reorder(Country,-Confirmed_case),y=Confirmed_case))+ 
      geom_bar(stat='identity', fill="#DD8888", width=.8) +
      ggtitle("Top 5 countries confirmed case")+
      coord_flip()+
      geom_text(aes(label=Confirmed_case),color="white")+
      theme(axis.ticks=element_blank(),
            axis.title.y=element_blank(),
            axis.title.x=element_blank(),
            legend.position="none",
            panel.background=element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
            panel.border=element_blank(),panel.grid.major=element_blank(),
            plot.background= element_rect(fill = "#2C3E4F", colour = "#2C3E4F"),
            axis.text.x = element_text(face="bold",color='white'),axis.text.y = element_text(face="bold",color='white'),
            plot.title = element_text(lineheight=.8, face="bold",hjust = 0.5,color='white',size=14))
    
  })
  
}

shinyApp(ui,server)


