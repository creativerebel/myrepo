library(dplyr)
library(reshape2)
library(readxl)
library(tidyr)

#==================================================Import Raw Files===============================================

Dist_Comp_item_raw = read.csv("/home/fractaluser/Documents/SOI/Data Extraction/SOI_Always_Competitor_Data_item.csv")
Dist_Comp_raw = read.csv("/home/fractaluser/Documents/SOI/Data Extraction/SOI_Always_Competitor_Data (1).csv")
SFT_raw = read.csv("/home/fractaluser/Documents/SOI/Data Extraction/SFT.csv")
Dist_Always_item_raw = read.csv("/home/fractaluser/Documents/SOI/Data Extraction/SOI_Always_Data_Sample_item.csv")
Dist_Always_raw = read.csv("/home/fractaluser/Documents/SOI/Data Extraction/SOI_Always_Data_Sample1 (2).csv")

GRP_raw = read_excel("/home/fractaluser/Documents/SOI/Data Extraction/Explicit_Data.xlsx",sheet = "GRP")
Macro_raw = read_excel("/home/fractaluser/Documents/SOI/Data Extraction/Explicit_Data.xlsx",sheet = "Macro-economic data")
TV_reach_raw = read_excel("/home/fractaluser/Documents/SOI/Data Extraction/Explicit_Data.xlsx",sheet = "TV Spends & Reach+1")

#======================================================Harmonise===================================================

Dist_TDP_Always = Dist_Always_item_raw %>% 
  select("Time.Name","Area","Brand","Item","Volume.Sales..MSU.","Value.Sales..MLC.","Numerical.Distribution","Weighted.Distribution","Price.Per.SU..LC.")%>% rename(ND = Numerical.Distribution,WD = Weighted.Distribution,Month = Time.Name,Channel = Area,Vol = Volume.Sales..MSU.,Val = Value.Sales..MLC.,PPSU = Price.Per.SU..LC.)

Dist_TDP_Always$Month = 
  as.Date(paste0("01-",substr(Dist_TDP_Always$Month,start=1,stop=3),"-20",substr(Dist_TDP_Always$Month,start=4,stop=5)),format = "%d-%b-%Y")

TDP_Always = Dist_TDP_Always %>% 
  group_by(Month,Channel,Brand) %>% 
  summarise(TDP = sum(WD))

Dist_Always = Dist_Always_raw %>% 
  select("Time.Name","Area","Brand","Volume.Sales..MSU.","Value.Sales..MLC.","Numerical.Distribution","Weighted.Distribution","Price.Per.SU..LC.")%>% rename(ND = Numerical.Distribution,WD = Weighted.Distribution,Month = Time.Name,Channel = Area,Vol = Volume.Sales..MSU.,Val = Value.Sales..MLC.,PPSU = Price.Per.SU..LC.)
Dist_Always$Month = as.Date(paste0("01-",substr(Dist_Always$Month,start=1,stop=3),"-20",substr(Dist_Always$Month,start=4,stop=5)),format = "%d-%b-%Y")

All_Always = inner_join(TDP_Always,Dist_Always)

Dist_TDP_Comp = Dist_Comp_item_raw %>% 
  select("Time.Name","Area","Brand","Item","Volume.Sales..MSU.","Value.Sales..MLC.","Numerical.Distribution","Weighted.Distribution","Price.Per.SU..LC.")%>% rename(ND = Numerical.Distribution,WD = Weighted.Distribution,Month = Time.Name,Channel = Area,Vol = Volume.Sales..MSU.,Val = Value.Sales..MLC.,PPSU = Price.Per.SU..LC.)

Dist_TDP_Comp$Month = as.Date(paste0("01-",substr(Dist_TDP_Comp$Month,start=1,stop=3),"-20",substr(Dist_TDP_Comp$Month,start=4,stop=5)),format = "%d-%b-%Y")

TDP_Comp = Dist_TDP_Comp %>% 
  group_by(Month,Channel,Brand) %>% 
  summarise(TDP = sum(WD))

Dist_Comp = Dist_Comp_raw %>% 
  select("Time.Name","Area","Brand","Volume.Sales..MSU.","Value.Sales..MLC.","Numerical.Distribution","Weighted.Distribution","Price.Per.SU..LC.")%>% rename(ND = Numerical.Distribution,WD = Weighted.Distribution,Month = Time.Name,Channel = Area,Vol = Volume.Sales..MSU.,Val = Value.Sales..MLC.,PPSU = Price.Per.SU..LC.)

Dist_Comp$Month = 
  as.Date(paste0("01-",substr(Dist_Comp$Month,start=1,stop=3),"-20",substr(Dist_Comp$Month,start=4,stop=5)),format = "%d-%b-%Y")

All_Comp = inner_join(TDP_Comp,Dist_Comp)

All_Comp_pivot = All_Comp %>% gather("MeasureName","MeasureValue",4:9)

All_Comp_pivot$Brand = paste(All_Comp_pivot$Brand,All_Comp_pivot$MeasureName)

All_Comp_pivot = All_Comp_pivot %>% select(-MeasureName) %>% spread(Brand,MeasureValue)

GRP_raw$Month = as.Date(GRP_raw$Month)

All_Always <- left_join(All_Always,GRP_raw)

SFT <- SFT_raw %>% select("Time.Name","Brand","Display","Feature","Share.of.Shelf","Share.of.Features","Share.of.Display") %>% rename(Month = Time.Name)

SFT$Month = as.Date(paste0("01-",substr(SFT$Month,start=1,stop=3),"-20",substr(SFT$Month,start=4,stop=5)),format = "%d-%b-%Y")

SFT_pivot <- SFT %>% gather("MeasureName","MeasureValue",3:7) 
SFT_pivot$Brand = paste(SFT_pivot$Brand,SFT_pivot$MeasureName)

SFT_pivot <- SFT_pivot %>% select(-MeasureName) %>% spread(Brand,MeasureValue)

All_Always <- left_join(All_Always,SFT_pivot)  

Macro_raw$Date = as.Date(Macro_raw$Date)

Macro <- Macro_raw %>% select(-Month,-Country,-Year,-Month_No) %>% rename(Month = Date)

All_Always <- left_join(All_Always,Macro)

All_All <- left_join(All_Always,All_Comp_pivot)

TV_reach_raw$Date <- as.Date(TV_reach_raw$Date)
TV_Spend <- TV_reach_raw %>% rename(Month = Date)

All_All <- left_join(All_All,TV_Spend)

All_Data <- All_All %>% ungroup() %>% rename(ALWAYS.VOL = Vol,SOFY.VOL = `SOFY Vol`,PRIVATE.VOL = `PRIVATE Vol`,ALWAYS.VAL = Val,SOFY.VAL = `SOFY Val`,PRIVATE.VAL = `PRIVATE Val`)

competitive_perf <- All_Data


Category_Trend <- All_Data %>% 
  group_by(Month) %>% 
  summarise(Always_Vol = sum(ALWAYS.VOL),Always_Price = mean(PPSU),
            Sofy_Vol = sum(SOFY.VOL),Sofy_Price = mean(`SOFY PPSU`),
            Private_Vol = sum(PRIVATE.VOL),Private_Price = mean(`PRIVATE PPSU`)) %>%
  mutate(All_Vol = Always_Vol + Sofy_Vol+Private_Vol, All_Price = (Always_Price+Sofy_Price+Private_Price)/3)

Single_Bar <- All_Data %>% group_by(Channel) %>%
  summarise(Always_Vol = sum(ALWAYS.VOL),
            Sofy_Vol = sum(SOFY.VOL),
            Private_Vol = sum(PRIVATE.VOL)) %>%
  mutate(All_Vol = Always_Vol + Sofy_Vol+Private_Vol,Always_pct = Always_Vol /sum(Always_Vol),
         Sofy_pct = Sofy_Vol /sum(Sofy_Vol),
         Private_pct = Private_Vol/sum(Private_Vol),
         All_pct = All_Vol/sum(All_Vol))



#a <- Single_bar #%>%
#Pharmacies_All <- All_All %>% filter(Channel == 'PHARMACIES') %>% ungroup() %>% select(-Brand,-Channel)
#PHARMACIES <- Pharmacies_All %>% select(Month,Vol,`PRIVATE Vol`,`SOFY Vol`) %>% rename(ALWAYS = Vol,SOFY = `SOFY Vol`,PRIVATE = `PRIVATE Vol`) #%>% gather("Brand",Volume,2:4)
#Pharmacies_All_Vol <- Pharmacies_All_Vol[!is.na(Pharmacies_All_Vol$Volume),]

#Supermarkets_All <- All_All %>% filter(Channel == 'SUPERMARKETS') %>% ungroup() %>% select(-Brand,-Channel)
#SUPERMARKETS <- Supermarkets_All %>% select(Month,Vol,`PRIVATE Vol`,`SOFY Vol`) %>% rename(ALWAYS = Vol,SOFY = `SOFY Vol`,PRIVATE = `PRIVATE Vol` )# %>% gather("Brand",Volume,2:4)
#Supermarkets_All_Vol <- Supermarkets_All_Vol[!is.na(Supermarkets_All_Vol$Volume),]

#Total_Groceries_All <- All_All %>% filter(Channel == 'TOTAL GROCERIES') %>% ungroup() %>% select(-Brand,-Channel)
#Total_Groceries <- Total_Groceries_All %>% select(Month,Vol,`PRIVATE Vol`,`SOFY Vol`) %>% rename(ALWAYS = Vol,SOFY = `SOFY Vol`,PRIVATE = `PRIVATE Vol`)# %>% gather("Brand",Volume,2:4)
#Total_Groceries_All_Vol <- Total_Groceries_All_Vol[!is.na(Total_Groceries_All_Vol$Volume),]

#ggplot(data = Pharmacies_All_Vol,aes(x=Month,y=Volume)) +
#  geom_bar(aes(fill=Brand),position="fill",stat="identity") +
#  scale_y_continuous(labels = percent_format())
#ggplot(data = All_Data[All_Data$Channel == "TOTAL GROCERIES",], aes(Month)) + geom_line(aes(y = ALWAYS, colour = "ALWAYS")) + geom_line(aes(y = SOFY, colour = "SOFY")) + geom_line(aes(y = PRIVATE, colour = "PRIVATE"))

#=====================================Shiny Starts Here============================================================================

library(ggplot2)
library(scales)
library(plotly)
library(shiny)
library(shinythemes)

ui <- (navbarPage(title = "Title", 
                  #fluid = TRUE,
                  tabPanel("Overall Category Trends",
                           sidebarLayout(
                             sidebarPanel(  
                               # Select which Channel to plot
                               radioButtons(inputId = "brand", 
                                            label = "Select Brand:",
                                            choices = c("ALWAYS", "SOFY","PRIVATE","TOTAL CATEGORY"), 
                                            selected = "TOTAL CATEGORY")
                             ),
                             mainPanel(
                               fluidRow(
                                 plotOutput(outputId = "lineplot_vol", height = "160px",width = "100%")),
                               fluidRow(
                                 plotOutput(outputId = "lineplot_price", height = "160px",width = "100%")),
                               fluidRow(
                                 splitLayout(cellWidths = c("60%", "30%"), 
                                             plotOutput(outputId = "lineplot_channel", height = "195px"), 
                                             plotOutput(outputId = "single_bar", height = "195px"))
                               )
                             )
                           )
                           
                  ),
                  tabPanel("Competitive Performance",
                           sidebarLayout(
                             sidebarPanel(  
                               # Select which Channel to plot
                               checkboxGroupInput(inputId = "Channel2", 
                                            label = "Select Channel:",
                                            choices = c("PHARMACIES", "TOTAL GROCERIES", "SUPERMARKETS"), 
                                            selected = c("PHARMACIES", "TOTAL GROCERIES", "SUPERMARKETS"))
                              # selectInput(inputId = "measure2", 
                               #            label = "Select measure:",
                                #           choices = c("VAL", "VOL"), 
                                 #          selected = "VOL")
                             ),
                             mainPanel(
                               
                               plotOutput(outputId = "lineplot2")
                              # verbatimTextOutput("value")
                             )
                           )
                  )
)
)

server <- function(input, output, session) {
  
  # for display of histogram in the "Widget & Sidepar page"
  
  brand_vol <- reactive({
    req(input$brand)
    if("ALWAYS" %in% input$brand) return (Category_Trend$Always_Vol)
    if("SOFY" %in% input$brand) return (Category_Trend$Sofy_Vol)
    if("PRIVATE" %in% input$brand) return (Category_Trend$Private_Vol)
    if("TOTAL CATEGORY" %in% input$brand) return (Category_Trend$All_Vol)
  })
  
  brand_channel_vol <- reactive({
    req(input$brand)
    if("ALWAYS" %in% input$brand) return (All_Data$ALWAYS.VOL)
    if("SOFY" %in% input$brand) return (All_Data$SOFY.VOL)
    if("PRIVATE" %in% input$brand) return (All_Data$PRIVATE.VOL)
    if("TOTAL CATEGORY" %in% input$brand) return (All_Data$ALWAYS.VOL + All_Data$PRIVATE.VOL + All_Data$SOFY.VOL)
  })
  
  single_bar_vol <- reactive({
    req(input$brand)
    if("ALWAYS" %in% input$brand) return (Single_Bar$Always_pct)
    if("SOFY" %in% input$brand) return (Single_Bar$Sofy_pct)
    if("PRIVATE" %in% input$brand) return (Single_Bar$Private_pct)
    if("TOTAL CATEGORY" %in% input$brand) return (Single_Bar$All_pct)
  })
  
  brand_price <- reactive({
    req(input$brand)
    if("ALWAYS" %in% input$brand) return (Category_Trend$Always_Price)
    if("SOFY" %in% input$brand) return (Category_Trend$Sofy_Price)
    if("PRIVATE" %in% input$brand) return (Category_Trend$Private_Price)
    if("TOTAL CATEGORY" %in% input$brand) return (Category_Trend$All_Price)
  })
  
  channel_subset2 <- reactive({
    req(input$Channel2)
    All_Data %>% filter(Channel %in% input$Channel2) %>% group_by(Month) %>% 
      summarise(ALWAYS.VOL = sum(ALWAYS.VOL), SOFY.VOL = sum(SOFY.VOL), PRIVATE.VOL = sum(PRIVATE.VOL))
  })
  
#  ALWAYS2 <- reactive({
#    if ( "VAL" %in% input$measure2) return(channel_subset2()$ALWAYS.VAL)
#    if ( "VOL" %in% input$measure2) return(channel_subset2()$ALWAYS.VOL)
#  })
#  
#  SOFY2 <- reactive({
#    if ( "VAL" %in% input$measure2) return(channel_subset2()$SOFY.VAL)
#    if ( "VOL" %in% input$measure2) return(channel_subset2()$SOFY.VOL)
#  })
  
#  PRIVATE2 <- reactive({
#    if ( "VAL" %in% input$measure2) return(channel_subset2()$PRIVATE.VAL)
#    if ( "VOL" %in% input$measure2) return(channel_subset2()$PRIVATE.VOL)
#  })
  
  # Create lineplot object the plotlyOutput function is expecting
  output$lineplot_vol <- renderPlot({
    ggplot(data = Category_Trend, aes(Month)) + 
      geom_line(aes(y = brand_vol(), colour = "VOL")) + ylab(input$brand)  +
      xlab(NULL)+
      theme_linedraw() + scale_x_date(breaks = "3 months",labels = date_format("%b-%Y"))+
      theme(axis.text.x = element_text(colour="black",size=8,angle=90,hjust=.5,vjust=.5,face="plain"))
    
  })
  
  output$lineplot_price <- renderPlot({
    
    ggplot(data = Category_Trend, aes(Month)) + 
      geom_line(aes(y = brand_price(), colour = "PRICE")) + ylab(input$brand)  + 
      xlab(NULL)+
      theme_linedraw() + scale_x_date(breaks = "3 months",labels = date_format("%b-%Y"))+
      theme(axis.text.x = element_text(colour="black",size=8,angle=90,hjust=.5,vjust=.5,face="plain"))
    
  })
  
  output$lineplot_channel <- renderPlot({
    ggplot(data = All_Data, aes(x = Month, y = brand_channel_vol(), color = Channel)) + 
      geom_line() + 
      ylab(input$brand)  + 
      xlab(NULL)+
      theme_linedraw() + scale_x_date(breaks = "3 months",labels = date_format("%b-%Y"))+
      theme(axis.text.x = element_text(colour="black",size=8,angle=90,hjust=.5,vjust=.5,face="plain"),
            legend.position="none")
    
  })
  
  output$single_bar <- renderPlot({
    ggplot(data = Single_Bar,aes(x = 1,y=single_bar_vol(),fill = Channel)) +
      geom_col() +
      geom_text(aes(label=paste(round((single_bar_vol())*100), "%")),position = position_stack(vjust = .5),
                colour="white") +
      #geom_bar(aes(fill=Channel),position="fill",stat="identity",label = percent_format()) +
      #geom_text(aes(label = paste(round((single_bar_vol())*100), "%")),
      #          position=position_stack(vjust = 0.5), colour="white")+
      ylab(input$brand)  + 
      xlab(NULL) +
      theme_linedraw() +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
            axis.ticks.x=element_blank(), axis.ticks.y=element_blank(),
            axis.text.y=element_blank(), legend.position="left")
    #scale_y_continuous(labels = percent_format())
  })
  
  #   Create lineplot object the plotlyOutput function is expecting
  output$lineplot2 <- renderPlot({
    
    ggplot(data = channel_subset2(), aes(Month)) + 
               geom_line(aes(y = channel_subset2()$ALWAYS.VOL/(channel_subset2()$ALWAYS.VOL + 
                                                                 channel_subset2()$SOFY.VOL +
                                                                 channel_subset2()$PRIVATE.VOL
                                                               ), colour = "ALWAYS")) + 
               geom_line(aes(y = channel_subset2()$SOFY.VOL/(channel_subset2()$ALWAYS.VOL + 
                                                               channel_subset2()$SOFY.VOL +
                                                               channel_subset2()$PRIVATE.VOL
               ), colour = "SOFY")) + 
               geom_line(aes(y = channel_subset2()$PRIVATE.VOL/(channel_subset2()$ALWAYS.VOL + 
                                                                  channel_subset2()$SOFY.VOL +
                                                                  channel_subset2()$PRIVATE.VOL
               ), colour = "PRIVATE")) + 
               ylab("Vol Share") +
               scale_y_continuous(labels = percent_format())+
               theme_linedraw() + scale_x_date(breaks = "3 months",labels = date_format("%b-%Y"))+
               theme(axis.text.x=element_text(angle=90,hjust=1))
  })
 # output$value <- renderPrint({ input$Channel2 })
}

shinyApp(ui = ui, server = server)