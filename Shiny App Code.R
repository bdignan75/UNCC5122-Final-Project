library(dplyr)
library(rworldmap)
library(plotly)
library(ggplot2)
library(tidyverse)
library(rsconnect)
library(shinydashboard)



ui <- dashboardPage(skin = "green", 
                    dashboardHeader(title = "Our National Parks"),
                    dashboardSidebar(dashboardSidebar(
                      sidebarMenu(
                        menuItem("Purpose", tabName = "purpose", icon = icon("donate")),
                        menuItem("Map of Park Locations", tabName = "map", icon = icon("map")),
                        menuItem("Think", tabName = "think", icon = icon("exclamation-triangle")),
                        menuItem("Table of Visitation Data", tabName = "table", icon = icon("table")),
                        menuItem("Take Action", tabName = "action", icon = icon("exclamation-triangle")),
                        menuItem("Chart of Visitation Trends", tabName = "chart", icon = icon("chart-line")),
                        menuItem("About", tabName = "about", icon = icon("seedling"))
                      )
                    )),
                    dashboardBody(
                      tabItems(
                        # Purpose tab content
                        tabItem(tabName = "purpose", includeHTML("Purpose.html")),
                        
                        # Map tab content
                        tabItem(tabName = "map", fluidRow(
                          box(title = "Controls",
                              sliderInput("slider", "Year:", 1950, 2015, 1985)),
                          box(selectInput("region", "Choose a Region:",
                                          choices = c("IM", "MW", "NE", "PW", "SE"),
                                          multiple = TRUE,
                                          selected = "PW"))),
                          fluidRow(
                            box(width = 12, (plotOutput("usamap"))))),
                        
                        # Think tab content
                        tabItem(tabName = "think", includeHTML("Think.html")),
                        
                        
                        # Table tab content
                        tabItem(tabName = "table",
                                fluidRow(
                                  box(title = "Park Level Data Table",
                                      sliderInput("slider2", "Year:", min = 1950, max = 2015,
                                                  value = c(1980, 2015))),
                                  
                                  box(selectInput("parkname", "Choose your Favorite Park:",
                                                  choices = c("Acadia", "Arches", "Badlands", "Big Bend", "Biscayne", "Black Canyon of the Gunnison", "Bryce Canyon", "Canyonlands", 
                                                              "Capitol Reef", "Carlsbad Caverns", "Channel Islands", "Congaree", "Crater Lake", "Cuyahoga Valley", "Death Valley", 
                                                              "Dry Tortugas", "Everglades", "Glacier", "Grand Canyon", "Grand Teton", "Great Basin", "Great Sand Dunes", "Great Smoky Mountains", 
                                                              "Guadalupe Mountains", "Hot Springs", "Isle Royale", "Joshua Tree", "Kings Canyon", "Lassen Volcanic", "Mammoth Cave", 
                                                              "Mesa Verde", "Mount Rainier", "North Cascades", "Olympic", "Petrified Forest", "Pinnacles", "Redwood", "Rocky Mountain", 
                                                              "Saguaro", "Sequoia", "Shenandoah", "Theodore Roosevelt", "Voyageurs", "Wind Cave", "Yellowstone", "Yosemite", "Zion"),
                                                  multiple = TRUE,
                                                  selected = ""))),
                                fluidRow(
                                  box(width = 12, (tableOutput("table"))))),
                        
                        # Take Action tab content
                        tabItem(tabName = "action", includeHTML("TakeAction.html")),
                        
                        
                        # Chart tab content
                        tabItem(tabName = "chart", 
                                fluidRow(
                                  box(title = "Explanation", h5("The increase in visits per citizens has driven the utilization 
                                     of our parks. -visitpopper")),
                                  box(title = "Yearly Trends",
                                      selectizeInput("cnt", "Select Statistic:",
                                                     choices = c("yoygasper", "yoyvisitsper", "visitpopper", "yoypopper"), selected = "yoypopper",
                                                     multiple = TRUE))),
                                
                                
                                
                                fluidRow(
                                  box(width = 12, (plotOutput("plot")))),
                                
                                fluidRow(
                                  box(title = "Yearly Trends",
                                      sliderInput("slider3", "Year:", min = 1950, max = 2015,
                                                  value = c(1950, 1980)))),
                                
                                
                                fluidRow(
                                  box(width = 12, (plotOutput("plot2"))))
                        ),
                        
                        # About tab content
                        tabItem(tabName = "about", includeHTML("About.html")))
                    ))



server <- function(input, output) {
  
  #output for map tab  
  output$usamap <- renderPlot({
    
    usadata <- subset(map_data,
                      region %in% input$region &
                        year == input$slider)
    ggplot() +
      geom_polygon(data = usa1, aes(long, lat, group = group),
                   fill = "#3F7A44", color = "white", size = 0.5) +
      geom_point(data=usadata, aes(x=LONG, y=LAT, size=visitors), color="#F7D358")+
      ggtitle("Location and Attendance of National Parks")})
  
  #output for table tab
  output$table <- renderTable({
    tabledata <- map_data
    tabledata <- subset(tabledata,
                        Name %in% input$parkname & 
                          year >= input$slider2[1] & year <= input$slider2[2])})
  
  #output for chart tab 
  
  output$plot = renderPlot({
    plot.data <- melt(park_visits_yoy, id.vars = 'year')
    plot.data <- plot.data[plot.data$variable %in% input$cnt, ]
    
    ggplot(plot.data) +
      geom_line(mapping = aes(x = year, y = value, colour = variable)) + 
      labs (x = "Year", y = "Yearly Percentage Change", title = "Percentage Change in Gas, Population and Visits") + 
      theme(
        axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"),
        plot.title = element_text(color = "black", size = 18, face = "bold"),
        legend.title=element_blank(),
        panel.background = element_rect(fill = "white", colour = "lightblue", size = 1, linetype = "solid"))}) 
  
  
  output$plot2 = renderPlot({
    tabledata <- park_visits_yoy
    tabledata <- subset(tabledata, year >= input$slider3[1] & year <= input$slider3[2])
    
    ggplot() +
      geom_line(tabledata, mapping = aes(x = year, y = pop_total, color='USA Population'))+ 
      geom_line(tabledata, mapping = aes(x = year, y = visitors_total, color='Visitors'))+ 
      ggtitle("National Park Visitors vs. USA Population") +
      scale_y_continuous(limits = c(0, 400000000), labels= c("0", "100M", "200M", "300M", "400M"))+ 
      theme(
        axis.title.x = element_text(color = "black", size = 14, face = "bold"),
        axis.title.y = element_text(color = "black", size = 14, face = "bold"),
        plot.title = element_text(color = "black", size = 16, face = "bold"),
        legend.title=element_blank(),
        panel.background = element_rect(fill = "white", colour = "lightblue", size = 1, linetype = "solid"))+             
      ylab("Persons") + xlab("Year")+
      scale_color_hue(labels = c("Visitors", "USA Population"))
  })
}
shinyApp(ui = ui, server = server)
