#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(leaflet)
library(tidyverse)
library(leaflet.extras)
library(ggplot2)
library(rmapshaper)
library(geogrid)
library(countrycode)
library(RColorBrewer)


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

source("map_data.R")



# Find most current date for slider
confirmed <- download_corona_data(1)
max_date <-names(confirmed)[ncol(confirmed)]
d <- strsplit(max_date, "_")
max_date <- paste0("20",d[[1]][3], "-", str_remove(d[[1]][1],"x"),"-",d[[1]][2])


server <- function(input, output, session) {
 
  
  # Read Eurostat basemap
  m1 <- read_map()
  
  # Download data from Johns Hopkins
  confirmed <- download_corona_data(1)
  recovered <- download_corona_data(2)
  death <- download_corona_data(3)
  
  # Merge Corona data to map
  m_confirmed <- prepare_map_data(m1, confirmed)
  m_recovered <- prepare_map_data(m1, recovered)
  m_death <- prepare_map_data(m1, death)
  
  # Create per capita (million inhabitatns) data
  m_confirmed_pc <- per_capita_data(m_confirmed)
  m_recovered_pc <- per_capita_data(m_recovered)
  m_death_pc <- per_capita_data(m_death)
  
  # Create precantage growth data
  m_confirmed_pct <- growth_rate(m_confirmed)
  m_recovered_pct <- growth_rate(m_recovered)
  m_death_pct <- growth_rate(m_death)
  

  
  
  # Create bins for coloring
  bins_abs <- c(0,50,100,1000,10000,Inf)
  bins_pc <- c(0,10,25,50, 100, 250, 500, 750, 1000,Inf)
  bins_pct <- c(0,5,15, 25, 50, 75, 100, 200, 500, 1000,Inf)
  
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addFullscreenControl() %>% 
      setMapWidgetStyle(list(background= rgb(170,218,255, maxColorValue = 255))) 
  })
  
  #170, 218, 225
  
  observe({
        req(input$tab_being_displayed == "COVID-19 Map")
        sel_date <- input$date
        
        d <- as.Date(sel_date)
        day <- as.numeric(format(d, "%d"))
        month <- as.numeric(format(d, "%m"))
        
        var <- paste0("x",month, "_", day,"_",20)
        
        if(input$info == 1){
          type_string <- "Infected"
          
          if(input$measure == 1){
            m_current <- m_confirmed_pc
            current_pal <- colorBin( palette="Reds", domain=m_current@data[[var]], na.color="white", bins= bins_pc)
          }
          
          if(input$measure == 2){
            m_current <- m_confirmed
            current_pal <- colorBin( palette="Reds", domain=m_current@data[[var]], na.color="white", bins= bins_abs)
            
          }

          
          if(input$measure == 3){
            m_current <- m_confirmed_pct
            current_pal <- colorBin( palette="Reds", domain=m_current@data[[var]], na.color="white", bins= bins_pct)
            
          }
          # # Prepare the text for tooltips:
          mytext <- paste(
            "Country: ", m_current@data$NAME_ENGL,"<br/>",
            paste("Number of", type_string, ": "), format(round(m_confirmed@data[[var]],2), big.mark = ","), "<br/>",
            paste(type_string, "per million inhabitants: "),format(round(m_confirmed_pc@data[[var]],2), big.mark = ","),"<br/>",
            paste("Growth rate: ",format(round(m_confirmed_pct@data[[var]],2), big.mark = ","), "%"),"<br/>",
            "Population: ", format(m_current@data$pop2018,big.mark = ","),
            sep="") %>%
            lapply(htmltools::HTML)

        }
        if(input$info == 2){
          type_string <- "Recovered"
          
          if(input$measure == 1){
            m_current <- m_recovered_pc
            current_pal <- colorBin( palette="Greens", domain=m_current@data[[var]], na.color="white", bins= bins_pc)
            
          }
          
          if(input$measure == 2){
            m_current <- m_recovered
            current_pal <- colorBin( palette="Greens", domain=m_current@data[[var]], na.color="white", bins= bins_abs)
          }  
          
          if(input$measure == 3){
            m_current <- m_recovered_pct
            current_pal <- colorBin( palette="Greens", domain=m_current@data[[var]], na.color="white", bins= bins_pct)
            
          }
          # # Prepare the text for tooltips:
          mytext <- paste(
            "Country: ", m_current@data$NAME_ENGL,"<br/>",
            paste("Number of", type_string, ": "), format(round(m_recovered@data[[var]],2), big.mark = ","), "<br/>",
            paste(type_string, "per million inhabitants: "),format(round(m_recovered_pc@data[[var]],2), big.mark = ","),"<br/>",
            paste("Growth rate: ",format(round(m_recovered_pct@data[[var]],2), big.mark = ","), "%"),"<br/>",
            "Population: ", format(m_current@data$pop2018,big.mark = ","),
            sep="") %>%
            lapply(htmltools::HTML)
        }
        if(input$info == 3){
          type_string <- "Deceased"
          
          if(input$measure == 1){
            m_current <- m_death_pc
            current_pal <- colorBin( palette="Purples", domain=m_current@data[[var]], na.color="white", bins= bins_pc)
          }
          
          if(input$measure == 2){
            m_current <- m_death
            current_pal <- colorBin( palette="Purples", domain=m_current@data[[var]], na.color="white", bins= bins_abs)
          } 
          if(input$measure == 3){
            m_current <- m_death_pct
            current_pal <- colorBin( palette="Purples", domain=m_current@data[[var]], na.color="white", bins= bins_pct)
            
          }
          # # Prepare the text for tooltips:
          mytext <- paste(
            "Country: ", m_current@data$NAME_ENGL,"<br/>",
            paste("Number of", type_string, ": "), format(round(m_death@data[[var]],2), big.mark = ","), "<br/>",
            paste(type_string, "per million inhabitants: "),format(round(m_death_pc@data[[var]],2), big.mark = ","),"<br/>",
            paste("Growth rate: ",format(round(m_recovered_pct@data[[var]],2), big.mark = ","), "%"),"<br/>",
            "Population: ", format(m_current@data$pop2018,big.mark = ","),
            sep="") %>%
            lapply(htmltools::HTML)
        }
        
        
        

        
        
        
          if(input$measure == 1){
            leafletProxy("map") %>%
              addPolygons(
                data = m_current,
                fillColor =  ~current_pal(get(var)),
                stroke=TRUE,
                fillOpacity = 0.9,
                color="grey",
                weight=0.3,
                label = mytext,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "13px",
                  direction = "auto"
                )
              )%>%  addLegend( pal=current_pal, values=bins_pc, opacity=0.9, title = paste("Number of", type_string, "per Million"), position = "bottomleft" , layerId = "leg")
          }
          if(input$measure == 2){
            leafletProxy("map") %>%
              addPolygons(
                data = m_current,
                fillColor =  ~current_pal(get(var)),
                stroke=TRUE,
                fillOpacity = 0.9,
                color="grey",
                weight=0.3,
                label = mytext,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "13px",
                  direction = "auto"
                )
              )%>%  addLegend( pal=current_pal, values=bins_abs, opacity=0.9, title = paste("Number of", type_string), position = "bottomleft" , layerId = "leg")
          }
          if(input$measure == 3){
          leafletProxy("map") %>%
            addPolygons(
              data = m_current,
              fillColor =  ~current_pal(get(var)),
              stroke=TRUE,
              fillOpacity = 0.9,
              color="grey",
              weight=0.3,
              label = mytext,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "13px",
                direction = "auto"
              )
            )%>%  addLegend( pal=current_pal, values=bins_abs, opacity=0.9, title = paste("Growth of", type_string, "in %"), position = "bottomleft" , layerId = "leg")
        }
    })
  
 
}
    


ui <- navbarPage(inverse =TRUE, "Easy COVID-19 Map",
                 id = "tab_being_displayed",
  tabPanel("About",
    includeHTML("about.html")
  ),
  tabPanel("COVID-19 Map",
  tags$style(type = "text/css", "#map {height: calc(100vh - 90px) !important;}"),
  leafletOutput("map"),
  absolutePanel(top = 75, right = 25,
                id = "panel",
                 selectInput("info", "Count",choices = list(
                   "Confirmed" = 1,
                   "Recoverd" = 2,
                   "Deceased" = 3
                 ), selected = 1,
                 width = '250px'),
                selectInput("measure", "Measure",choices = list(
                  "Per Million" = 1,
                  "Absolute" = 2,
                  "Growth Rate" = 3
                ), selected =  1,
                width = '250px'),
                sliderInput("date",
                           "Dates:",
                           min = as.Date("2020-01-22","%Y-%m-%d"),
                           max = as.Date(max_date,"%Y-%m-%d"),
                           value=as.Date(max_date),
                           timeFormat="%Y-%m-%d",
                           width = '220px'),

                helpText("Source: John's Hopkins")


  )
)
)




# Run the application 
shinyApp(ui = ui, server = server)

