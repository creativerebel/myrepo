#install.packages("shinythemes")

require(dplyr)
require(reshape2)
require(readxl)
require(tidyr)
require(lattice)
require(ggplot2)
require(scales)
require(plotly)
require(shiny)
require(shinythemes)
#==================================================Import Raw Files===============================================

Dist_Comp_item_raw = read.csv("/home/fractaluser/Documents/SOI/Data Extraction/SOI_Always_Competitor_Data_item.csv")
Dist_Comp_raw = read.csv("/home/fractaluser/Documents/SOI/Data Extraction/SOI_Always_Competitor_Data (1).csv")
SFT_raw = read.csv("/home/fractaluser/Documents/SOI/Data Extraction/SFT.csv")
Dist_Always_item_raw = read.csv("/home/fractaluser/Documents/SOI/Data Extraction/SOI_Always_Data_Sample_item.csv")
Dist_Always_raw = read.csv("/home/fractaluser/Documents/SOI/Data Extraction/SOI_Always_Data_Sample1 (2).csv")
GRP_raw = read_excel("/home/fractaluser/Documents/SOI/Data Extraction/Explicit_Data.xlsx",sheet = "GRP")

#colnames(GRP_raw) = c("Month","Actual_GRPs_Brand","Actual_GRPs_Category")
#GRP_raw$adstock[[1]]<- GRP_raw$Actual_GRPs_Brand[[1]]

#GRP_raw$adstock<-as.numeric(GRP_raw$adstock)
#for(i in 2:nrow(GRP_raw)){
#  GRP_raw$adstock[i]= GRP_raw$Actual_GRPs_Brand[i]+(0.45*(GRP_raw$adstock[i-1]))
#}

#GRP_raw <- GRP_raw %>% select("Month", "adstock")

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

All_Data <- All_All %>% ungroup() %>% rename(ALWAYS.VOL = Vol,SOFY.VOL = `SOFY Vol`,
                                             PRIVATE.VOL = `PRIVATE Vol`,ALWAYS.VAL = Val,
                                             SOFY.VAL = `SOFY Val`,PRIVATE.VAL = `PRIVATE Val`)

competitive_perf <- All_Data

#a <- c(unique(as.character(All_Data$Channel)))

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

National_Data <- All_Data %>% group_by(Month) %>% summarise(Always_Vol = sum(ALWAYS.VOL),
                                                            Sofy_Vol = sum(SOFY.VOL),
                                                            Private_Vol = sum(PRIVATE.VOL))

All_Data <- All_Data %>% rename(SOFY.WD = `SOFY WD`, SOFY.TDP = `SOFY TDP`, SOFY.PPSU = `SOFY PPSU`,SOFY.ND = `SOFY ND`,
                                PRIVATE.WD = `PRIVATE WD`, PRIVATE.TDP = `PRIVATE TDP`, PRIVATE.PPSU = `PRIVATE PPSU`,PRIVATE.ND = `PRIVATE ND`,
                                GDP = `Real GDP`, GDP.Growth.Rate = `GDP Growth Rate`, ALWAYS.DISPLAY = `ALWAYS Display`,
                                ALWAYS.FEATURE = `ALWAYS Feature`,ALWAYS.SHARE.OF.DISPLAY = `ALWAYS Share.of.Display`,
                                ALWAYS.SHARE.OF.FEATURES = `ALWAYS Share.of.Features`,ALWAYS.SHARE.OF.SHELF = `ALWAYS Share.of.Shelf`,
                                GRP.BRAND = `Actual GRPs (Brand)`,ALWAYS.ND = ND, ALWAYS.WD = WD, ALWAYS.PPSU = PPSU) %>%
  select(Channel, ALWAYS.ND, ALWAYS.WD,ALWAYS.PPSU,Brand, Month, TDP, ALWAYS.VAL,SOFY.VOL,PRIVATE.VOL,ALWAYS.VOL, SOFY.WD,SOFY.TDP,SOFY.PPSU,SOFY.ND,PRIVATE.WD,PRIVATE.TDP,PRIVATE.PPSU,PRIVATE.ND,GDP,GDP.Growth.Rate,ALWAYS.DISPLAY,
         ALWAYS.FEATURE,ALWAYS.SHARE.OF.DISPLAY,ALWAYS.SHARE.OF.FEATURES,ALWAYS.SHARE.OF.SHELF,GRP.BRAND,Population, TVSpends_NonWoven, TVSpends_Mainline, TVSpends_Total,
         Reach1_Brand)

#Adding Dummy Customer column

All_Data$Customer[[1]]<- "Dummy-Customer"

Column_Names = as.data.frame(colnames(All_Data))
library(shinydashboard)
#=====================================Shiny Starts Here============================================================================

submenuUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow( splitLayout(cellWidths = c("33%", "33%","33%"),
  selectInput(ns("Select1"),"Area Hierarchy:",c("Channel","Customer")),
              selectInput(ns("Select2"),"Area Selector:",choices = NULL),
              selectInput(ns("target"),"Target Variable:",choices = c("Value","Volume")))))
}

submenuServ <- function(input, output, session){
  observeEvent(input$Select1, {
    column_levels <- as.character(sort(unique(
      All_Data[[input$Select1]]
    )))
    updateSelectInput(session, "Select2", choices = column_levels)
  })
}
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      shinyjs::useShinyjs(),
      id = "tabs",
      menuItem("Landscape Assessment", icon = icon("bar-chart-o"),
               shinyjs::hidden(menuSubItem("dummy", tabName = "dummy")),
               menuSubItem("Category Level", tabName = "category_level")
      ))),
  dashboardBody(
    tabItems(tabItem("dummy"),
             tabItem("category_level", submenuUI('submenu1'))
    )
  ))
server <- function(input, output,session) {
  callModule(submenuServ,"submenu1")
}
shinyApp(ui = ui, server = server)