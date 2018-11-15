
library(shiny)
library(DBI)
library(RSQLite)
library(tidyverse)
library(scales)
library(datetime)
library(lubridate)
library(ggplot2)
library(reshape)
library(zoo)
library(leaflet)
library(sp)
library(magrittr)
library(maps)
library(htmltools)
library(rgdal)
library(data.table)
library(tidyverse)
library(plotly)
library(shinydashboard)
########
temp<-dbConnect(SQLite(),dbname="Shire.db")
data1_bob<-dbGetQuery(temp,'SELECT count(patient_id)as num, product_name, BOB,dispensed_date
                      FROM New_Patient
                      where dispensed_date between "2016-07-01" and "2018-03-31" 
                      group by BOB,product_name,dispensed_date
                      order by dispensed_date')
data1_bob$yearmonth <- as.Date(as.yearmon(data1_bob$dispensed_date))
#########
sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("New Patient Plot", tabName="plot", icon=icon("line-chart"), selected=TRUE),
              menuSubItem("Date Plot", tabName = "date", icon = icon("angle-right")),
              menuSubItem("Geographic Plot", tabName = "geo", icon = icon("angle-right")),
              menuItem("Switch Patient Plot", tabName="plot1", icon=icon("line-chart"), selected=TRUE),
              menuSubItem("Date Plot", tabName = "date1", icon = icon("angle-right")),
              menuSubItem("Geographic Plot", tabName = "geo1", icon = icon("angle-right")),
              menuItem("Table", tabName = "table", icon=icon("table")),
              menuItem("Cluster", tabName = "cluster", icon=icon("line-chart"))
  )
)

#########
body <- dashboardBody(
  tabItems(
    ############
    tabItem(tabName = "date",
            fluidRow(width = 20,
                     
                     box(width = NULL, status = "primary", solidHeader = TRUE,
                         
                         selectizeInput("group", label="Group:",
                                        choices=c("Commercial","Medicare", "Cash", "Others"),
                                        multiple=F,options = list(create = TRUE)),
                         selectizeInput("bob",label = "Top 10 BOB:",
                                        choices = c("CVS Health - Commercial","UnitedHealthcare - Medicare","Express Scripts - Commercial",
                                                    "CVS Health - Medicare", "Humana - Medicare", "Cash - Cash", "All Other Third Party - Mix",
                                                    "OptumRx - Commercial", "Aetna - Commercial", "UnitedHealthcare - Commercial"),
                                        multiple=F, options = list(create = TRUE)),
                         radioButtons("type", "Data type",
                                      c("Date"), inline = TRUE)
                     ),
                     
                     box(width = NULL, 
                         plotOutput("date",height="500px"), 
                         side = "right", collapsible = TRUE,
                         title = "Plot", status = "primary", solidHeader = TRUE)
            )),
    
    ############
    tabItem(tabName = "geo",
            fluidRow(width = 20, 
                     box(width = NULL,status = "primary", solidHeader = TRUE,
                         tabPanel(h5("New Patient"),
                                  selectizeInput("group", label="Group:",
                                                 choices=c("Commercial","Medicare", "Cash", "Others"),
                                                 multiple=F,options = list(create = TRUE)),
                                  selectizeInput("BOB",label = "Top 10 BOB:",
                                                 choices = c("CVS Health - Commercial","UnitedHealthcare - Medicare","Express Scripts - Commercial",
                                                             "CVS Health - Medicare", "Humana - Medicare", "Cash - Cash", "All Other Third Party - Mix",
                                                             "OptumRx - Commercial", "Aetna - Commercial", "UnitedHealthcare - Commercial"),
                                                 multiple=F, options = list(create = TRUE)),
                                  radioButtons("type", "Data type",
                                               c("Geography"), inline = TRUE)
                         ),
                     box(width = NULL, plotlyOutput("geo",height="500px"), collapsible = TRUE,
                         title = "Plot", status = "primary", solidHeader = TRUE))
    )),
    tabItem(tabName = "table"),
    ############
    tabItem(tabName = "cluster"),
    ###########3
    tabItem(tabName = "date1",
            fluidRow(width = 20,
                     
                     box(width = NULL, status = "primary", solidHeader = TRUE,
                         
                         selectizeInput("group1", label="Group:",
                                        choices=c("Commercial","Medicare", "Cash", "Others"),
                                        multiple=F,options = list(create = TRUE)),
                         selectizeInput("bob1",label = "Top 10 BOB:",
                                        choices = c("CVS Health - Commercial","UnitedHealthcare - Medicare","Express Scripts - Commercial",
                                                    "CVS Health - Medicare", "Humana - Medicare", "Cash - Cash", "All Other Third Party - Mix",
                                                    "OptumRx - Commercial", "Aetna - Commercial", "UnitedHealthcare - Commercial"),
                                        multiple=F, options = list(create = TRUE)),
                         radioButtons("type", "Data type",
                                      c("Date"), inline = TRUE)
                     ),
                     
                     box(width = NULL, 
                         plotOutput("date1",height="500px"), 
                         side = "right", collapsible = TRUE,
                         title = "Plot", status = "primary", solidHeader = TRUE)
            )),
    ############
    tabItem(tabName = "geo1",
            fluidRow(width = 20,
                     
                     box(width = NULL, status = "primary", solidHeader = TRUE,
                         
                         selectizeInput("group1", label="Group:",
                                        choices=c("Commercial","Medicare", "Cash", "Others"),
                                        multiple=F,options = list(create = TRUE)),
                         selectizeInput("BOB1",label = "Top 10 BOB:",
                                        choices = c("CVS Health - Commercial","UnitedHealthcare - Medicare","Express Scripts - Commercial",
                                                    "CVS Health - Medicare", "Humana - Medicare", "Cash - Cash", "All Other Third Party - Mix",
                                                    "OptumRx - Commercial", "Aetna - Commercial", "UnitedHealthcare - Commercial"),
                                        multiple=F, options = list(create = TRUE)),
                         radioButtons("type", "Data type",
                                      c("Geographic"), inline = TRUE)
                     ),
                     
                     box(width = NULL, 
                         plotOutput("geo1",height="500px"), 
                         side = "right", collapsible = TRUE,
                         title = "Plot", status = "primary", solidHeader = TRUE)
            ) 
            
            )
    ##########
    ))
   
 

#########
dashboardPage(
  dashboardHeader(title = "Shire EDA"),
  sidebar,
  body
)

##########
library(shinydashboard)
library(mlxR)
data5<-dbGetQuery(temp,'Select BOB,count(patient_id)as num,ADDRESS_AT_CALL
                  from New_Patient N, (Select Shire_ID,ADDRESS_AT_CALL
                  from All_Call  
                  group by Shire_ID) as A
                  where N.Shire_ID=A.SHIRE_ID AND N.product_name=="XIIDRA"
                  group by A.ADDRESS_AT_CALL
                  order by num desc') 
data6<-dbGetQuery(temp,'Select BOB,count(patient_id)as num,ADDRESS_AT_CALL
                  from Switch_Patient N, (Select Shire_ID,ADDRESS_AT_CALL
                  from All_Call  
                  group by Shire_ID) as A
                  where N.Shire_ID=A.SHIRE_ID AND N.product_name=="XIIDRA"
                  group by A.ADDRESS_AT_CALL
                  order by num desc') 
data2_bob <- read.csv("Switch__Patient.csv")
##########
shinyApp(
  ui = dashboardPage(
    dashboardHeader(title = "Shire EDA"),
    sidebar,
    body),
  server = shinyServer(function(input, output) { 
    
    #################
    result1 <- reactive({
      data1_bob %>% filter(BOB==input$bob) %>% group_by(yearmonth, product_name) %>% 
        summarise(total_num =sum(num)) %>% 
        spread(product_name, total_num) %>% 
        ungroup() %>%
        transmute(yearmonth= yearmonth,  xiidra = XIIDRA, restasis = RESTASIS)
    })
    
    output$date <- renderPlot({
      
      ggplot() + 
        geom_line(data = result1(), aes(x = yearmonth, y = xiidra, color = "xiidra"), size=1.5) + 
        geom_line(data = result1(), aes(x = yearmonth, y = restasis, color = "restasis"),size=1.5) + 
        theme_classic()+
        scale_x_datetime(date_breaks = "3 months")+
        ggtitle(paste("Trend of Amount of Prescription among ", input$bob) )+
        xlab("Date")+ylab("Amount of Product Prescription")+scale_fill_discrete(breaks=c("xiidra","restasis"))+ 
        scale_color_brewer(palette="Dark2")+theme(axis.title.x = element_text(face="bold",  size=13), axis.title.y = element_text(face="bold",  size=13),plot.title = element_text(size=15, face="bold"),  axis.text.x  = element_text(angle=45,vjust=0.5, size=10),legend.text = element_text(face="bold",size=15),legend.title=element_blank())+
        scale_x_date(breaks = "1 month",labels = date_format("%m-%Y"))
    })
    ################
    result11 <- reactive({
      data2_bob %>% filter(BOB==input$bob1) %>% group_by(yearmonth, product_name) %>% 
        summarise(total_num =sum(num)) %>% 
        spread(product_name, total_num) %>% 
        ungroup() %>%
        transmute(yearmonth= yearmonth,  xiidra = XIIDRA, restasis = RESTASIS)
    })
    
    output$date1 <- renderPlot({
      
      ggplot() + 
        geom_line(data = result11(), aes(x = yearmonth, y = xiidra, color = "xiidra",group=1), size=1.5) + 
        geom_line(data = result11(), aes(x = yearmonth, y = restasis, color = "restasis",group=1),size=1.5) + 
        theme_classic()+
       #scale_x_datetime(date_breaks = "3 months")+
        ggtitle(paste("Trend of Amount of Prescription among ", input$bob1) )+
        xlab("Date")+ylab("Amount of Product Prescription")+scale_fill_discrete(breaks=c("xiidra","restasis"))+ 
        scale_color_brewer(palette="Dark2")+theme(axis.title.x = element_text(face="bold",  size=13), axis.title.y = element_text(face="bold",  size=13),plot.title = element_text(size=15, face="bold"),  axis.text.x  = element_text(angle=45,vjust=0.5, size=10),legend.text = element_text(face="bold",size=15),legend.title=element_blank())
      #+scale_x_date(breaks = "1 month",labels = date_format("%m-%Y"))
    })
    #################   
    result2 <- reactive({
      data5 %>% filter(BOB==input$BOB)%>%
        group_by(ADDRESS_AT_CALL) %>% 
        summarise(n_c=n()) %>% 
        mutate(State=str_trim(str_extract(ADDRESS_AT_CALL,"[A-Z][A-Z][^A-Z]")))%>%group_by(State)%>%summarise(total=n())
    })
    output$geo <- renderPlotly({
      l <- list(color = toRGB("white"), width = 2)
      # specify some map projection/options
      g <- list(
        scope = "usa",
        projection = list(type = "albers usa"),
        showlakes = TRUE,
        lakecolor = toRGB("white")
      )
      
      plot_geo(result2(), locationmode = "USA-states") %>%
        add_trace(
          z = ~total, locations = ~State,
          color = ~total, colors = "Reds"
        ) %>%
        colorbar(title = "Total Presription") %>%
        layout(
          title = paste("Total presription in", input$bob),
          geo = g
        )
    })
    ##########
    
    ##########
    
  })
)
