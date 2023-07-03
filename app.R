library(shiny)
library(shinyWidgets)
library(dplyr)
library(bslib)
library(leaflet)
library(maps)
library(mapproj)
library(plotly)

thematic::thematic_shiny() 
unix::rlimit_memlock(3e8)
roads <- readxl::read_excel("data/road maintenance routine.xlsx")
roads <- roads %>%
  rename(Problem_extend = Defect...3,
         Defect_kind = Defect...6)
roads$Ranking <- as.factor(roads$Ranking)
roads$Speed <- as.factor(roads$Speed)
roads$Problem_extend <- as.factor(roads$Problem_extend)
roads$`Problem Localisation` <- as.factor(roads$`Problem Localisation`)
roads$`Road Type` <- as.factor(roads$`Road Type`)
roads$Defect_kind <- as.factor(roads$Defect_kind)
roads$`Suggested Actions` <- as.factor(roads$`Suggested Actions`)
roads$`Kind of Maintenance` <- as.factor(roads$`Kind of Maintenance`)
roads$`Kind of Maintenance` <- recode_factor(roads$`Kind of Maintenance`,
                                             None = "no")

# Problem Statement (data + chart)
kenya <- map_data("world") %>%
  filter(region == c("Kenya"))
world <- map_data("world") %>%
  filter(lat > -40) %>%
  filter(lat < 40) %>%
  filter(long > -20) %>%
  filter(long < 55)
names <- world %>%
  filter(region == c("Kenya", "South Africa",
                     "Namibia", "Angola",
                     "Tanzania", "Mali", 
                     "Sudan", "Egypt",
                     "Niger", "Mauritania",
                     "Algeria", "Libya",
                     "Ethiopia", "Somalia",
                     "Uganda", "Nigeria",
                     "Ghana", "Guinea",
                     "Senegal", "Zambia",
                     "Central African Republic",
                     "Democratic Republic of the Congo",
                     "Madagascar", "Chad",
                     "Mozambique")) %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat)) 

africa_map <- world %>%
  ggplot() +
  geom_map(aes(x = long, y = lat, map_id = region),
           fill = "#e7f5dc", color = "gray",
           map = world) +  
  coord_map("ortho", orientation = c(0, 20, 0)) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  geom_polygon(data = kenya, aes(x = long, y = lat, group = group), 
               fill = "#c0dfc2", color = "black") +
  geom_text(aes(x = long, y = lat, label = region), data = names,  size = 4, hjust = 0.5) +
  ylim(-35, 40)

rqi <- tibble(year = c(2013, 2014, 2015, 2016, 2017, 2018, 2019),
              RQI = c(4.10, 4.24, 4.19, 4.20, 4.30, 4.20, 4.10))
index_plot <- plot_ly(rqi, x = ~year, y = ~RQI, type = 'scatter', mode = 'lines',
                      line = list(color = '#719b25', width = 4),
                      width = 300, height = 180) %>%
  layout(title = list(text = 'Road Quality Index', font = list(family = "Segoe UI Semibold",
                                                               size = 16,
                                                               color = "#94948E")),
         xaxis = list(title = list(text = 'Year', 
                                   font = list(family = "Segoe UI",
                                                              size = 14,
                                                              color = "#94948E"))),
         yaxis = list(range = list(3.7, 4.5), title = list(text = "RQI", 
                                                           font = list(family = "Segoe UI",
                                                                                     size = 14,
                                                                                     color = "#94948E"))),
         font = list(family = "Segoe UI",
                     size = 14,
                     color = "#464241"),
         autosize = FALSE,
         margin = list(l = 20, r = 20, t = 30, b = 20),
         plot_bgcolor = '#F8F9FB')


# Road Map (data + map)
kenya_network <- sf::st_read("/vsicurl/https://github.com/OVI71397/Road-Maintenance/raw/main/data/kenya_roads/Kenya_roads_version2.shp")
kenya_network$CLASS <- replace(kenya_network$CLASS, 
                               is.na(kenya_network$CLASS),
                               "Unknown") %>%
  as.factor()
kenya_network$TYPE <- replace(kenya_network$TYPE, 
                              is.na(kenya_network$TYPE),
                              "Unknown") %>%
  as.factor()
kenya_network$CONDITION <- replace(kenya_network$CONDITION, 
                                   is.na(kenya_network$CONDITION),
                                   "Unknown") %>%
  as.factor()

kenya_network <- kenya_network %>%
  mutate(condition_reduc = recode_factor(kenya_network$CONDITION,
                                         Excellent = "Good",
                                         `Very Poor` = 'Poor'))
kenya_network <- kenya_network %>%
  filter(TYPE != "230")
kenya_network$TYPE <- droplevels(kenya_network$TYPE, exclude = "230")
kenya_network$TYPE <- recode_factor(kenya_network$TYPE,
                                    `Concrete (Jt-Plain)` = "Concrete",
                                    `Concrete Block` = "Concrete",
                                    Sand = "Earth",
                                    `Set Stone` = "Gap")

primary <- kenya_network %>%
  filter(CLASS == "Primary (Trunk)")
secondary <- kenya_network %>%
  filter(CLASS == "Secondary (Main)")
tert <- kenya_network %>%
  filter(CLASS == "Tertiary")



kenya_bound <- sf::st_read("/vsicurl/https://github.com/OVI71397/Road-Maintenance/raw/main/data/Kenya_County_Boundaries/Kenya_County_Boundaries.shp")

quality_pal <- colorFactor(palette = c("#B2AB2E", "#ED413E", "#686461", "#CFCBC8"), 
                           domain = kenya_network$condition_reduc,
                           na.color = "#CFCBC8")
map <- leaflet(kenya_network) %>% 
  addTiles() %>% 
  setView(36.906, 0.4, zoom = 6) %>%
  #fitBounds(lng1 = 33.4, lat1 = -4.8, lng2 = 41.4, lat2 = 5.3) %>%
  addLegend(position = "bottomright",
            pal = quality_pal,
            values = ~kenya_network$condition_reduc,
            opacity = 0.9,
            title = paste("<small>Road Condition 
                          <br><em>click on the road to see info</em></small>")) %>%
  addLayersControl(
    position = "topright",
    overlayGroups = c("Primary (Trunk) Road Network", 
                      "Secondary (Main) Road Network", 
                      "Tertiary Road Network"),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Secondary (Main) Road Network", 
              "Tertiary Road Network")) %>%
  addPolygons(data = kenya_bound, color = "#F8F9FB",
              opacity = 0.3,
              fillColor = "#F8F9FB",
              fillOpacity = 0.8) %>%
  addPolylines(data = primary, 
               weight = 3,
               group = "Primary (Trunk) Road Network",
               color = ~quality_pal(primary$condition_reduc),
               opacity = 1,
               popup = paste("<b>", primary$ROADNO, "road", "</b>", 
                             "<br> Length:", "<b>", primary$LENGTHKM, "km", "</b>",
                             "<br> Width:", "<b>", primary$WIDTH, "m", "</b>",
                             "<br> Lanes:", "<b>", primary$LANES, "</b>",
                             "<br> Pavement Type:", "<b>", primary$TYPE, "</b>")) %>%
  addPolylines(data = secondary, 
               weight = 2,
               group = "Secondary (Main) Road Network",
               color = ~quality_pal(secondary$condition_reduc),
               opacity = 1,
               popup = paste("<b>", secondary$ROADNO, "road", "</b>", 
                             "<br> Length:", "<b>", secondary$LENGTHKM, "km", "</b>",
                             "<br> Width:", "<b>", secondary$WIDTH, "m", "</b>",
                             "<br> Lanes:", "<b>", secondary$LANES, "</b>",
                             "<br> Pavement Type:", "<b>", secondary$TYPE, "</b>")) %>%
  addPolylines(data = tert, 
               weight = 1,
               group = "Tertiary Road Network",
               color = ~quality_pal(tert$condition_reduc),
               opacity = 1,
               popup = paste("<b>", tert$ROADNO, "road", "</b>", 
                             "<br> Length:", "<b>", tert$LENGTHKM, "km", "</b>",
                             "<br> Width:", "<b>", tert$WIDTH, "m", "</b>",
                             "<br> Lanes:", "<b>", tert$LANES, "</b>",
                             "<br> Pavement Type:", "<b>", tert$TYPE, "</b>"))
  
# Road Map (donut charts + bar charts)
color <- c("#B2AB2E", "#ED413E", "#686461", "#CFCBC8")
donut_chart_plot <- function(categ) {
  if (categ == "Primary (Trunk)") {
    pr <- primary %>%
      as.data.frame() %>%
      group_by(condition_reduc) %>%
      summarise(freq = n()) %>%
      mutate(perc = round(freq/sum(freq),3))
    donut <- plot_ly(pr, labels = ~condition_reduc, values = ~perc, sort = FALSE, textposition = "inside",
                     textinfo = 'percent', hoverinfo = "text", text = ~paste(condition_reduc, perc*100,"%"),
                     marker = list(colors = color, line = list(color = "#F8F9FB", width = 1.5)),
                     showlegend = FALSE, width = 220, height = 100) %>%
      add_pie(hole = 0.5) %>%
      layout(autosize = FALSE,
             margin = list(l = 20, r = 20, t = 10, b = 10))
  } else if (categ == "Secondary (Main)") {
    sc <- secondary %>%
      as.data.frame() %>%
      group_by(condition_reduc) %>%
      summarise(freq = n()) %>%
      mutate(perc = round(freq/sum(freq),3))
    donut <- plot_ly(sc, labels = ~condition_reduc, values = ~perc, sort = FALSE, textposition = "inside",
                     textinfo = 'percent', hoverinfo = "text", text = ~paste(condition_reduc, perc*100,"%"),
                     marker = list(colors = color, line = list(color = "#F8F9FB", width = 1.5)),
                     showlegend = FALSE, width = 220, height = 100) %>%
      add_pie(hole = 0.5) %>%
      layout(autosize = FALSE,
             margin = list(l = 20, r = 20, t = 10, b = 10))
  } else if (categ == "Tertiary") {
    tr <- tert %>%
      as.data.frame() %>%
      group_by(condition_reduc) %>%
      summarise(freq = n()) %>%
      mutate(perc = round(freq/sum(freq),3))
    donut <- plot_ly(tr, labels = ~condition_reduc, values = ~perc, sort = FALSE, textposition = "inside",
                     textinfo = 'percent', hoverinfo = "text", text = ~paste(condition_reduc, perc*100,"%"),
                     marker = list(colors = color, line = list(color = "#F8F9FB", width = 1.5)),
                     showlegend = FALSE, width = 220, height = 100) %>%
      add_pie(hole = 0.5) %>%
      layout(autosize = FALSE,
             margin = list(l = 20, r = 20, t = 10, b = 10))
  } else {
    all <- kenya_network %>%
      as.data.frame() %>%
      group_by(condition_reduc) %>%
      summarise(freq = n()) %>%
      mutate(perc = round(freq/sum(freq),3))
    donut <- plot_ly(all, labels = ~condition_reduc, values = ~perc, sort = FALSE, textposition = "inside",
                     textinfo = 'percent', hoverinfo = "text", text = ~paste(condition_reduc, perc*100,"%"),
                     marker = list(colors = color, line = list(color = "#F8F9FB", width = 1.5)),
                     showlegend = FALSE, width = 220, height = 100) %>%
      add_pie(hole = 0.5) %>%
      layout(autosize = FALSE,
             margin = list(l = 20, r = 20, t = 10, b = 10))
  }
  return(donut)
}
  
bar_chart_plot <- function(categ) {
  if (categ == "Primary (Trunk)") {
    df_prime <- kenya_network %>%
      filter(CLASS == "Primary (Trunk)") %>%
      filter(TYPE != "Unknown") %>%
      as.data.frame() %>%
      group_by(TYPE, condition_reduc) %>%
      summarise(freq = n()) %>%
      mutate(perc = round((freq/sum(freq))*100, 1))
    barchart <- plot_ly(data = df_prime, x = ~freq, y = ~TYPE, color = ~condition_reduc,
                        colors = color,type = "bar", width = 220, height = 210,
                        hoverinfo = "text", text = ~paste(condition_reduc, freq, "\n", perc, "%"),
                        textposition = "none",showlegend = FALSE) %>%
      layout(barmode = "stack", autosize = FALSE,
             xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE),
             yaxis = list(title = "", categoryorder = "total ascending"),
             margin = list(l = 5, r = 20, t = 20, b = 20))
  } else if (categ == "Secondary (Main)") {
    df_second <- kenya_network %>%
      filter(CLASS == "Secondary (Main)") %>%
      filter(TYPE != "Unknown") %>%
      as.data.frame() %>%
      group_by(TYPE, condition_reduc) %>%
      summarise(freq = n()) %>%
      mutate(perc = round((freq/sum(freq))*100, 1))
    barchart <- plot_ly(data = df_second, x = ~freq, y = ~TYPE, color = ~condition_reduc,
                        colors = color,type = "bar", width = 220, height = 210,
                        hoverinfo = "text", text = ~paste(condition_reduc, freq, "\n", perc, "%"),
                        textposition = "none", showlegend = FALSE) %>%
      layout(barmode = "stack", autosize = FALSE,
             xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE),
             yaxis = list(title = "", categoryorder = "total ascending"),
             margin = list(l = 5, r = 20, t = 20, b = 20))
  } else if (categ == "Tertiary") {
    df_ter <- kenya_network %>%
      filter(CLASS == "Tertiary") %>%
      filter(TYPE != "Unknown") %>%
      as.data.frame() %>%
      group_by(TYPE, condition_reduc) %>%
      summarise(freq = n()) %>%
      mutate(perc = round((freq/sum(freq))*100, 1))
    barchart <- plot_ly(data = df_ter, x = ~freq, y = ~TYPE, color = ~condition_reduc,
                        colors = color,type = "bar", width = 220, height = 210,
                        hoverinfo = "text", text = ~paste(condition_reduc, freq, "\n", perc, "%"),
                        textposition = "none", showlegend = FALSE) %>%
      layout(barmode = "stack", autosize = FALSE,
             xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE),
             yaxis = list(title = "", categoryorder = "total ascending"),
             margin = list(l = 5, r = 20, t = 20, b = 20))
  } else {
    df_all <- kenya_network %>%
      filter(TYPE != "Unknown") %>%
      as.data.frame() %>%
      group_by(TYPE, condition_reduc) %>%
      summarise(freq = n()) %>%
      mutate(perc = round((freq/sum(freq))*100, 1))
    barchart <- plot_ly(data = df_all, x = ~freq, y = ~TYPE, color = ~condition_reduc,
                        colors = color,type = "bar", width = 220, height = 210,
                        hoverinfo = "text", text = ~paste(condition_reduc, freq, "\n", perc, "%"),
                        textposition = "none",showlegend = FALSE) %>%
      layout(barmode = "stack", autosize = FALSE,
             xaxis= list(title = "", showticklabels = FALSE, showgrid = FALSE),
             yaxis = list(title = "", categoryorder = "total ascending"),
             margin = list(l = 5, r = 20, t = 20, b = 20))
  }
  return(barchart)
}













ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "materia", primary = "#719b25", bg = "#fefefe", fg = "#464241", font_scale = 1.2),
  
  fluidRow(
    column(width = 10,
      offset = 1,
      navbarPage("Kenya Roads", id = "main",
                  tabPanel("Problem Statement",
                           fluidRow(
                           column(width = 7,
                                  plotOutput("africa", width = "100%"),
                                  absolutePanel(id = "plot", class = "panel",
                                                bottom = 120, left = 150, width = 320, fixed = TRUE,
                                                draggable = TRUE, height = "auto",
                                                plotlyOutput("rq_index", width = 300, height = 180)
                                                )
                                  ),
                           column(width = 5,
                                  div(id = "intro",
                                    includeMarkdown('supplementary_files/problem_description.Rmd')),
                                  tags$style(type = "text/css", "#intro {font-size: 15px; display: block;}")
                                  )
                                   )
                           ),
                  tabPanel("Road Condition",
                             fluidRow(
                               column(width = 12,
                                        shinycssloaders::withSpinner(
                                        leaflet::leafletOutput('kenya_roads',
                                                           width = "100%",
                                                           height = 560), 
                                        size = 1, color = "#719b25"),
                                    absolutePanel(id = "summary", class = "panel",
                                                  bottom = 60, left = 170, width = 240, fixed = TRUE,
                                                  draggable = TRUE, height = "auto",
                                                  span(tags$i(tags$small("Road condition summary both general and by pavement type.")), style = "color:#686461"),
                                                  selectInput("class", 
                                                              label = "Select Road Class:",
                                                              choices = c("All", "Primary (Trunk)", "Secondary (Main)", "Tertiary"),
                                                              selected = "Primary (Trunk)"),
                                                  plotlyOutput("quality_donut", height = "100px", width = "100%"),
                                                  plotlyOutput("surf_bar", height = "210px", width = "100%"),
                                                  tags$style(type = 'text/css', ".selectize-input { font-size: 13px;}
                                                                                 .selectize-dropdown { font-size: 13px;}"))
                                   ), tags$style(type = "text/css", "#summary {background-color: white; opacity: 0.9; padding: 5px 5px 5px 5px; transition: opacity 500ms 1s; font-size: 15px}")
                                   )
                           ),
                  tabPanel("Maintenance Recommendation",
                           fluidRow(
                             column(4,
                                    sidebarPanel(width = 12,
                                                 selectInput(inputId = "surface",
                                                             label = strong("Choose the Pavement Type:"),
                                                             choices = levels(roads$`Road Type`)),
                                                 selectInput(inputId = "defect",
                                                             label = strong("Choose the Kind of Surface Defect:"),
                                                             choices = levels(roads$Defect_kind)),
                                                 selectInput(inputId = "speed",
                                                             label = strong("What is the possible speed vehicle that can develop at the site?"),
                                                             choices = levels(roads$Speed)))
                                    ),
                             column(8,
                                    wellPanel(h5(htmlOutput(outputId = "maintenance_rec"), align = "center")),
                                    h5("Common types of defects on paved roads", align = "center"),
                                    imageOutput("damage_photo", height="50%", width="50%")
                                    )
                                    )
                           )
          )
          )
  )
)

server <- function(input, output, session){
  
  # Problem Statement
  output$africa <- renderPlot({
    africa_map
  }, height = 570, width = 700)
  
  output$rq_index <- renderPlotly({
    index_plot
  })
  
  # Road Condition
  output$kenya_roads <- renderLeaflet({
    map
    })
  
  output$quality_donut <- renderPlotly({
    donut_chart_plot(input$class)
  })
  
  output$surf_bar <- renderPlotly({
    bar_chart_plot(input$class)
  })
  
  # Maintenance
  react_road_type <- reactive({
    roads %>%
      filter(`Road Type` == input$surface)
    })
  observe({
    freezeReactiveValue(input, "defect")
    updateSelectInput(session, "defect", choices = react_road_type()$Defect_kind)
    })
  
  reactive_road_defect <- reactive({
    react_road_type() %>%
      filter(Defect_kind == input$defect)
    })
  observe({
    freezeReactiveValue(input, "speed")
    updateSelectInput(session, "speed", choices = reactive_road_defect()$Speed)
    })
  
  reactive_speed <- reactive({
    reactive_road_defect() %>%
      filter(Speed %in% input$speed)
    })
  
  output$maintenance_rec <- renderUI({
    no_maint <- reactive_speed() %>%
      as.data.frame() %>%
      pull(Ranking) %>%
      unique()
    maint_num <- nrow(as.data.frame(reactive_speed()))
    if (no_maint == "A") {
      HTML(paste("Based on the provided information the severity of the road condition 
                  could be classified as a", "<b>", reactive_speed()$Problem_extend, "</b>", "problem of",  
                  "<b>", reactive_speed()$Ranking, "</b>", "type. As such, no maintenance 
                  is recommended. However, extra monitoring efforts are desirable on this section of the road."))
    } else if (no_maint != "A") {
      if (maint_num == 1) {
        HTML(paste("Based on the provided information the severity of the road condition 
                   could be classified as a", "<b>", reactive_speed()$Problem_extend, "</b>", "problem of",  
                   "<b>", reactive_speed()$Ranking, "</b>", "type. As such,", "<b>", reactive_speed()$`Kind of Maintenance`,
                   "maintenance", "</b>", "is recommended, which involves", reactive_speed()$`Suggested Actions`,
                   "."))
      } else {
        HTML(paste("Based on the provided information the severity of the road condition 
                   could be classified as a", "<b>", unique(reactive_speed()$Problem_extend), "</b>", "problem of",  
                   "<b>", unique(reactive_speed()$Ranking), "</b>", "type. As such, either", "<b>", as.data.frame(reactive_speed())[1, 8],
                   "or", as.data.frame(reactive_speed())[2, 8], "maintenance", "</b>", "is recommended.",
                   as.data.frame(reactive_speed())[1, 8], "maintenance involves", 
                   as.data.frame(reactive_speed())[1, 7], ", while", as.data.frame(reactive_speed())[2, 8],
                   "maintenance involves", as.data.frame(reactive_speed())[2, 7], "."))
      }
    }
    
    })
  
  output$damage_photo <- renderImage({
    list(
      src = file.path("supplementary_files/common_pavement_defects.jpg"),
      contentType = "image/jpeg"
      #width = 750,
     # height = 310
    )
  }, deleteFile = FALSE)
}


shinyApp(ui = ui, server = server)


