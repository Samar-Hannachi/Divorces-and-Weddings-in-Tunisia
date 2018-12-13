library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)

ui <- dashboardPage(
  dashboardHeader(title = "Weddings & Divorces in Tunisia (1998 ~ 2015)",titleWidth = 500
                 ),
  dashboardSidebar( sidebarMenu(
 
    menuItem("Weddings in Tunisia", tabName = "dashboard", icon = icon("bell")),
    menuItem("Divorces in Tunisia", tabName = "hh", icon = icon("balance-scale")),
    menuItem("Bivariate Analysis", tabName = "MA", icon = icon("check-double"))
  )),
  
  dashboardBody(tabItems(
tabItem(tabName = "dashboard",
            fluidRow(
              valueBoxOutput("value1")
              ,valueBoxOutput("value2")
              ,valueBoxOutput("value3")
            ),
            fluidRow( 
              box(
                title = "Number of Weddings per Region"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("weddingperRegion", height = "300px")
              ),
              box(
                title = "Number of Weddings per Year"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("weddingperYear", height = "300px")
              ),
              box(
                title = "BoxPlot per Region"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("weddingperRegionBP", height = "300px")
              )
            )),
    tabItem(tabName = "hh",
            
            
            fluidRow(
              valueBoxOutput("value4")
              ,valueBoxOutput("value5")
              ,valueBoxOutput("value6")
            ),
            fluidRow( 
              box(
                title = "Number of Divorces per Region"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("divorceperRegion", height = "300px")
              ),
              box(
                title = "Number of Divorces per Year"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("divorceperYear", height = "300px")
              ),
              box(
                title = "BoxPlot per Region"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("divorceperRegionBP", height = "300px")
              )
            )),
    tabItem(tabName = "MA"  ,
            fluidRow(
              box(
                title = "BiPlot - Weddings ~ Divorces"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("biplot", height = "300px")
              ),
              box(
                title = "BiDensityPlot - Weddings ~ Divorces"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("bidensityplot", height = "300px")
              ),
              box(
                title = "Scatter Plot - Weddings ~ Divorces"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("scatterplot", height = "300px")
              ),
              box(
                title = "Scatter Plot per Region - Weddings ~ Divorces"
                ,status = "primary"
                ,solidHeader = TRUE 
                ,collapsible = TRUE 
                ,plotOutput("relationplot", height = "300px")
              )
              
            )
    )
    
    
    
    
                                ))
  
  
  
  
  
  
  
  
)
# create the server functions for the dashboard  
# create the server functions for the dashboard  
server <- function(input, output) { 

  #TS
  output$TSplot <- renderPlot({
    ggplot(df, aes(x = Year, y = value)) +
      geom_area(aes(color = variable, fill = variable),alpha = 0.5, position = position_dodge(0.8)) +
      scale_color_manual(values = c("#00AFBB", "#E7B800")) +
      scale_fill_manual(values = c("#00AFBB", "#E7B800"))
    
  }) 
  
  
  
  
  
  
  #some data manipulation to derive the values of KPI boxes
  total.weddings <- sum(WD_in_TUnisia$Weddings)
  total.divorces <- sum(WD_in_TUnisia$Divorces)
  weddings.regions <- WD_in_TUnisia %>% group_by(Regions) %>% summarise(value = sum(Weddings)) %>% filter(value==max(value))
  weddings.year <- WD_in_TUnisia %>% group_by(Calendar) %>% summarise(value = sum(Weddings)) %>% filter(value==max(value))
  divorces.regions <- WD_in_TUnisia %>% group_by(Regions) %>% summarise(value = sum(Divorces)) %>% filter(value==max(value))
  divorces.year <- WD_in_TUnisia %>% group_by(Calendar) %>% summarise(value = sum(Divorces)) %>% filter(value==max(value))
  #creating the valueBoxOutput content
  
  output$value1 <- renderValueBox({ 
    valueBox(
      formatC(total.weddings, format="d", big.mark=',')
      ,'Total Number of Weddings in Tunisia (1997 -2015)'
      ,icon = icon("star",lib='glyphicon')
      ,color = "navy")  
  })
  output$value2 <- renderValueBox({
    valueBox(
      formatC(weddings.regions$value, format="d", big.mark=',')
      ,paste('Top Region:',weddings.regions$Regions)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  
  output$value3 <- renderValueBox({
    valueBox(
      formatC(weddings.year$value, format="d", big.mark=',')
      ,paste('Top Year:',weddings.year$Calendar)
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")   
  })  
  
  #creating the plotOutput content
  output$weddingperRegion <- renderPlot({
    ggplot(data = WD_in_TUnisia, 
           aes(x=Regions, y=Weddings, fill=factor(Regions))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Number of Weddings (per Region)") + 
      xlab("Region") + theme(legend.position="bottom" 
                             ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Number of Weddings per Region ")  + labs(fill = "Region")
  })
  
  output$weddingperYear <- renderPlot({
    ggplot(data = WD_in_TUnisia, 
           aes(x=Calendar, y=Weddings, fill=factor(Calendar))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Number of Weddings (per Year)") + 
      xlab("Year") + theme(legend.position="bottom" 
                           ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Number of Weddings per Year ")  + labs(fill = "Year")
  })
  
  output$weddingperRegionBP <- renderPlot({
    ggplot(data = WD_in_TUnisia,aes(Regions,Weddings)) + 
      geom_boxplot(aes(colour = Regions)) 
  })
  
  #For Divorces
  
  
  output$value4 <- renderValueBox({ 
    valueBox(
      formatC(total.divorces, format="d", big.mark=',')
      ,'Total Number of Divorces in Tunisia (1997 -2015)'
      ,icon = icon("star",lib='glyphicon')
      ,color = "navy")  
  })
  output$value5 <- renderValueBox({
    valueBox(
      formatC(divorces.regions$value, format="d", big.mark=',')
      ,paste('Top Region:',divorces.regions$Regions)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")  
  })
  
  output$value6 <- renderValueBox({
    valueBox(
      formatC(divorces.year$value, format="d", big.mark=',')
      ,paste('Top Year:',divorces.year$Calendar)
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")   
  })  
  
  #creating the plotOutput content
  output$divorceperRegion <- renderPlot({
    ggplot(data = WD_in_TUnisia, 
           aes(x=Regions, y=Divorces, fill=factor(Regions))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Number of Divorces (per Region)") + 
      xlab("Region") + theme(legend.position="bottom" 
                             ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Number of Divorces per Region ")  + labs(fill = "Region")
  })
  
  output$divorceperYear <- renderPlot({
    ggplot(data = WD_in_TUnisia, 
           aes(x=Calendar, y=Divorces, fill=factor(Calendar))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Number of Divorces (per Year)") + 
      xlab("Year") + theme(legend.position="bottom" 
                           ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Number of Divorces per Year ")  + labs(fill = "Year")
  })
  
  output$divorceperRegionBP <- renderPlot({
    ggplot(data = WD_in_TUnisia,aes(Regions,Divorces)) + 
      geom_boxplot(aes(colour = Regions)) 
  })
  
  output$biplot <- renderPlot({
    p <- ggplot(WD_in_TUnisia, mapping = aes(x = Divorces, y = Weddings)) 
    p + 
      geom_jitter() +  # using geom_jitter to avoid overplotting of points
      geom_smooth() 
  }) 
  
  
  output$bidensityplot <- renderPlot({
    p <- ggplot(WD_in_TUnisia, mapping = aes(x = Divorces, y = Weddings)) 
    p + 
      geom_density_2d() + 
      geom_jitter(alpha=0.35)
  }) 
  output$scatterplot <- renderPlot({
    p <- ggplot(WD_in_TUnisia, mapping = aes(x = Divorces, y = Weddings)) 
    p + 
      geom_point(aes(color = Regions, shape = Regions), size = 2, alpha = 0.6)
  }) 
  
  output$relationplot <- renderPlot({
    p <- ggplot(WD_in_TUnisia, mapping = aes(x = Divorces, y = Weddings)) 
    p + 
      geom_density_2d(aes(color = Regions), alpha = 0.5) + 
      geom_point(aes(color = Regions), alpha=0.5, size=1) + 
      facet_wrap(~ Regions)
  }) 
  
  
  
  
  
  
  
  
  
  
}
shinyApp(ui, server)
