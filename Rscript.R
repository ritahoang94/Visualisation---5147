################################ LOAD LIBRARY #################################################

library(shiny)
library(ggplot2) 
library(googleVis)
library(dplyr)
library(plotly)
library(tidyr)
library(DT)

################################ DATA PROCESSING ##############################################

#read information
data<- read.csv('merge.csv', header = TRUE)

#Merge with region
region <- read.csv('countryregion.csv',header = TRUE)

data = merge(data, 
             region[,c("alpha.3","region")], by.x = "code", by.y = "alpha.3")

#change value to ml per week
data$beer <- round(data$beer*1000/52,2)
data$wine <- round(data$wine*1000/52,2)
data$spirit <- round(data$spirit*1000/52,2)
data$other <- round(data$other*1000/52,2)
data$total <- round(data$total*1000/52,2)
data$prod <- round(data$prod,2)
data$gdp_per_cap <- round(data$gdp_per_cap,2)
data$pc_m_dis <- round(data$pc_m_dis,2)
data$pc_fm_dis <- round(data$pc_fm_dis,2)
data$de_per_mil <- round(data$de_per_mil,2)



# #Change the data type of bleaching percentage to numeric

country_summary = group_by(data, country)  %>%
  summarise(mean_gdp= mean(gdp_per_cap,na.rm = TRUE), mean_prd = mean(prod,na.rm = TRUE), 
            mean_life = mean(life_exp,na.rm = TRUE), mean_all = mean(total,na.rm = TRUE),
            wine = mean(wine,na.rm = TRUE), beer = mean(beer,na.rm = TRUE), other = mean(other,na.rm = TRUE),
            spirit = mean(spirit,na.rm = TRUE))

#Favourite
country_summary$favorite <- ifelse(country_summary$beer > country_summary$wine & country_summary$beer > country_summary$spirit, "beer", 
                                   ifelse(country_summary$wine > country_summary$beer & country_summary$wine > country_summary$spirit, "wine",
                                          ifelse(country_summary$spirit > country_summary$wine & country_summary$spirit > country_summary$beer, "spirit",NA)))
# Favourite over time
data$favorite <- ifelse(data$beer > data$wine & data$beer > data$spirit, "beer", 
                        ifelse(data$wine > data$beer & data$wine > data$spirit, "wine",
                               ifelse(data$spirit > data$wine & data$spirit > data$beer, "spirit","other")))

# grey if
country_summary$favoritevalue <- ifelse(country_summary$favorite == "beer",country_summary$beer,
                                        ifelse(country_summary$favorite == "wine", country_summary$wine,
                                               ifelse(country_summary$favorite == "spirit",country_summary$spirit, 0)))
country_summary = data.frame(country_summary)

# world summary

world_summary <- group_by(data, year)  %>%
  summarise( wine = mean(wine,na.rm = TRUE), beer = mean(beer,na.rm = TRUE), 
             spirit = mean(spirit,na.rm = TRUE),other = mean(other,na.rm = TRUE))
world_summary <- data.frame(gather(world_summary, type, con_level, wine:other, factor_key=TRUE))

#Region summary


region_summary <- data.frame(data %>% group_by(year, region)  %>%
                               summarise(beer = round(mean(beer,na.rm = TRUE),2), wine = round(mean(wine,na.rm = TRUE),2),
                                         spirit = round(mean(spirit,na.rm = TRUE),2), 
                                         other = round(mean(other,na.rm = TRUE),2)))

### data2
data2 =data[complete.cases(data),]


## UI

ui <- navbarPage("Visualization", id="nav",
           ### First page ###
           tabPanel("Favorite Map",includeHTML("abc.html"),
                    tags$i("Data source: World Health Organization"),
                    tags$br(),
                    ### Include the html file for the first page (Favourite Map)
                    tags$a(href="http://apps.who.int/gho/data/node.main.GISAH?lang=en", "Data Link"))
           ,
           ### Second page ##
           tabPanel("Consumption Level",
                    tags$head(
                      # Include css for the floating filter panel
                      includeCSS("datavis.css")
                    ),
                    
                    
                    h2("Alcohol consumption comparison"),
                    p("Each continent, due to the differences in climate, culture, might have different
                      patterns of drinking. The follow graph will show the average line of each continent over time."),
                    
                    plotlyOutput('view4', height = "200px", width = "900px"),
                    
                    hr (style="border:0.5px solid black;"),
                    
                    h2(textOutput("caption")),
                    tags$i("Unit: mililitre per person per week"),
                    p("Like a part of the culture, the drinking pattern of each countries are relatively stable over time."),
                    
                    htmlOutput("viewmap2"),
                    tags$i("Data source: World Health Organization"),
                    tags$br(),
                    tags$a(href="http://apps.who.int/gho/data/node.main.GISAH?lang=en", "Data Link"),
                    
                    
                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = TRUE, top = 150, right = 0, bottom = 60,
                                  width = 350, height = 800,
                                  
                                  h4("Filter"),
                                  
                                  selectInput("type", "Pick a alcoholic drink:",
                                              choices = c("beer", "wine", "spirit"), selected ="wine"),
                                  
                                  sliderInput('year', 'Year', min = min(data$year), max = 2013, 
                                              value = min(data$year), sep = "", step=1,
                                              animate= animationOptions(interval = 1500, loop = TRUE)),
                                  
                                  sliderInput("range", "Select alcohol level (Ml/pp/week):",min = 0, max = 200, value = c(0,200)),textOutput("SliderText"),
                                  
                                  selectInput("country","Select country",
                                              choices = c("All",
                                                          unique(as.character(country_summary$country))),
                                              multiple = TRUE,selected ="All"),
                                  
                                  htmlOutput("view2")
                                  
                    ))
           ,
           ## Third page ##
           tabPanel("Data Exploration",
                    h2("Data Exploration with Motion Chart"),
                    tags$i("Data source: World Health Organization"),
                    p("There are many possible causes and effects relatied to alcohol. Try your own Motion Chart!!!"),
                    tags$br(),
                    tags$a(href="http://apps.who.int/gho/data/node.main.GISAH?lang=en", "Data Link"),
                    htmlOutput("motionchart"),
                    hr (style="border:0.5 px solid black;"),
                    h2("Data Summary Table"),
                    tags$i("Data source: World Health Organization"),
                    p("If you want to find any information related to the charts provided, please search in the search box or follow the data link above."),
                    tags$br(),
                    tags$a(href="http://apps.who.int/gho/data/node.main.GISAH?lang=en", "Data Link"),
                    dataTableOutput("table")
           )
           )

server <- function(input, output) {
  output$caption <- renderText({
    paste("The world consumption level map of ", input$type)
  })
  
  
  ######################Timeline map################################
  
  
  output$viewmap2 <- renderGvis({
    #### customize colour
    color =""
    if (input$type == "beer"){
      color =  "{values:[0,40,80,120,160],
      colors:['#ffffd4','#fed98e','#fe9929','#d95f0e','#993404']}"
  }
    if (input$type == "wine"){
      color = "{values:[0,40,80,120,160],
      colors:['#fee5d9','#fcae91','#fb6a4a','#de2d26','#a50f15']}"
    }
    if (input$type == "spirit"){
      color = "{values:[0,40,80,120,160],
      colors:['#eff3ff','#bdd7e7','#6baed6','#3182bd','#08519c']}"
  }
    
    
    
    if (!("All" %in% input$country)) {
      
      mapdata = data2[(data2[input$type] >= as.numeric(input$range[1])) & 
                        (data2[input$type] <= as.numeric(input$range[2]))
                      & (data2$country %in% input$country) & (data2$year == input$year),]}
    if ("All" %in% input$country) {
      
      mapdata = data2[(data2[input$type] >= as.numeric(input$range[1])) & 
                        (data2[input$type] <= as.numeric(input$range[2])) &  (data2$year == input$year),]}
    
    gvisGeoChart(data = mapdata, 
                 locationvar="country", 
                 colorvar= input$type,
                 options=list(projection="kavrayskiy-vii", 
                              colorAxis= color,
                              title= paste("world ", input$type, "consumption map"),
                              width=1400, height=500))
    })
  
  
  ##############################################Sidebar piechart###########################
  output$view2 <- renderGvis({
    piedata <- world_summary[world_summary$year == input$year,c("type","con_level")]
    gvisPieChart(piedata, options=list(title='World Average Consumpiton',
                                       width=300, height=200,
                                       colors= "['#fe9929','#fb6a4a','#6baed6','#cccccc']"))
  })
  
  
  
  
  ###########Plotly bar char ####################################
  
  output$view4 <- renderPlotly({
    
    req(input$country)
    if (!("All" %in% input$country)) {
      region = as.character(data[data$country %in% input$country,]$region)
      region_summary = region_summary[region_summary$region %in%region,]
      region_summary = region_summary[region_summary$year== input$year,]
    } 
    if ("All" %in% input$country) {
      region_summary = region_summary[region_summary$year== input$year,]
    } 
    
    plot_ly() %>%
      add_bars(
        x = region_summary$region,
        y = region_summary$beer,
        marker = list(
          color = '#fe9929'),
        name = 'beer') %>%
      add_bars(
        x = region_summary$region,
        y = region_summary$spirit,
        marker = list(
          color = '#6baed6'),
        name = 'spirit')%>%
      add_bars(
        x = region_summary$region,
        y = region_summary$wine,
        marker = list(
          color = '#fb6a4a'),
        name = 'wine'
      )%>%
      add_bars(
        x = region_summary$region,
        y = region_summary$other,
        marker = list(
          color = '#cccccc'),
        name = 'other') %>% layout(font =list(
          family = "sans-serif",
          size = 11),
          
          xaxis = list(title = "Region"),
          yaxis = list(title = "Consumption level (ml/pp/week)"))%>% 
      layout(autosize = F, width =1000, height = 220)
  })
  
  
  ##### MOTION CHART#####
  output$motionchart <- renderGvis({
    motiondata = data2
    names(motiondata) <- c("code", "country", "year", "productivity", "GDP per capita", "Life expectancy",
                           "total", "beer", "other", "spirit", "wine","Alcohol disorder % in Males",
                           "Alcohol disorder % in Females","Death caused by alcohol (per million people)","region",
                           "favorite"
    )
    
    gvisMotionChart(motiondata, idvar = "country", timevar = "year", xvar = "GDP per capita",
                    yvar = "beer", colorvar = "region", sizevar = "productivity",
                    options = list(width= 1500, height = 600, showChartButtons=TRUE))
  })
  
  
  ############################################# Data table ############################ 
  output$table <- renderDataTable({
    tabledata = data2
    names(tabledata) <- c("code", "country", "year", "productivity", "GDP per capita", "Life expectancy",
                          "total", "beer", "other", "spirit", "wine","Alcohol disorder % in Males",
                          "Alcohol disorder % in Females","Death caused by alcohol (per million people)","region",
                          "favorite"
    )
    datatable(tabledata
              , colnames = c("code", "country", "year", "productivity", "GDP per capita", "Life expectancy",
                             "total", "beer", "spirit", "wine", "other","favorite", "Alcohol disorder % in Males",
                             "Alcohol disorder % in Females","Death caused by alcohol (per million people)","region"))
  })
}

shinyApp(ui, server)



