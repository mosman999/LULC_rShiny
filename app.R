
#

library(shiny)
library(shinycssloaders)
library(dplyr)
library(ggplot2)
library(leaflet)
library(sf)
library(tigris)
library(data.table)
library(DT)
# Define UI for application that draws a histogram


######################### Csvs #######################################

######################## Plots ######################## 
path_states <- "data/states"
csv_files <- list.files(path, pattern = "*.csv", full.names = TRUE)
states_list <- lapply(csv_files, function(file) read.csv(file, header = TRUE, check.names = FALSE, row.names = 'type_of_lulc'))
names(states_list) <- gsub(path_states, "", csv_files)
names(states_list) <- gsub(".csv", "", names(states_list))
names(states_list) <- gsub("^/", "", names(states_list))
wunschjahrliste<- colnames(states_list[[1]])
coloorspie<- c("#33ff77","#85adad","#bfff00","#d9ffb3","#dfbf9f","#80dfff")
coloorsbar <- c('Agricultural crops'="#33ff77", 'Barren land'=  "#85adad",'Forest'  = "#bfff00", 'Grass'=  "#d9ffb3", 'Urban'= "#dfbf9f", 'Water '=  "#80dfff")
######################## Plots ######################## 

######################## Maps ######################## 
path_years <- "data/years"

csv_files_years <- list.files(path_years, pattern = "*.csv", full.names = TRUE)
years_list <- lapply(csv_files_years, function(file) read.csv(file, header = TRUE, check.names = FALSE, row.names = 'type_of_lulc'))

names(years_list) <- gsub(path_years, "", csv_files_years)
names(years_list) <- gsub(".csv", "", names(years_list))
names(years_list) <- gsub("^/", "", names(years_list))


######################## Maps ######################## 




######################### Csvs #######################################

  ui <-fluidPage(
    titlePanel(title=h3("Detecting Land Use And Land Cover Changes In Germany", align="center")     
    ),
    sidebarPanel(textOutput('info'), tags$head(tags$style("#info{color: black;
                                 font-size: 24px;
                                 text-align: justify;
                                 white-space: pre-wrap;
                                 margin-left:10px;
                                 margin-right:10px;
                                 
                                 }"
    )
    ), tags$hr(style="border-color: purple;"),
    
    textOutput('developer'),
    actionButton(inputId='ab1_weblink', label="linkedIn", 
                 icon = icon("th"), 
                 onclick ="window.open('https://www.linkedin.com/in/mohamed-osman-20097a168/', '_blank')")
    ,
    tags$head(
      tags$style(HTML('#ab1_weblink{background-color:#e6f5ff}'))
    )
    ,
    tags$head(tags$style("#developer{color: #5c8a8a;
                                 font-size: 24px;
                                 text-align: justify;
                                 white-space: pre-wrap;
                                 margin-left:10px;
                                 margin-right:10px;
                                 
                                 }"
    )
    )
    
    ),
    mainPanel(
      tags$hr(style="border-color: purple;"),
      
      tabsetPanel(type = "tabs",
                  
                  
                  tabPanel("Plots", icon=icon("chart-bar"),
                           fluidRow(
                             column(6, 
                                    plotOutput('plot1')%>% withSpinner(color = "green"),
                                    selectInput('region1',
                                                'Targeted State (1):',
                                                names(states_list)),
                                    selectInput('yearsregion1',
                                                'Targeted Year (1):',
                                                unique(wunschjahrliste)),
                                    downloadButton('downloadPlot1','Download The Bar Chart')
                                    
                             ),
                             column(6, 
                                    plotOutput('plot2')%>% withSpinner(color = "green"),
                                    selectInput('region2',
                                                'Targeted State (2):',
                                                names(states_list)),
                                    selectInput('yearsregion2',
                                                'Targeted Year (2):',
                                                unique(wunschjahrliste)),
                                    downloadButton('downloadPlot2','Download The Bar Chart')
                                    
                             )
                           ),
                           tags$hr(style="border-color: purple;")
                           ,
                           fluidRow(
                             column(6, 
                                    plotOutput('plot3')%>% withSpinner(color = "green")
                                    , downloadButton('downloadPlot3','Download The Pie Chart')
                             ),
                             
                             column(6, 
                                    plotOutput('plot4')%>% withSpinner(color = "green"),
                                    downloadButton('downloadPlot4','Download The Pie Chart')
                                    
                             )
                           )              
                           
                           
                           
                  ),
                  tabPanel("Tables", icon=icon("table"),
                           fluidRow(
                             column(6, 
                                    selectInput('regiontable',
                                                'Targeted State:',
                                                names(states_list))
                             )),
                           fluidRow( column(6, 
                                            selectInput(
                                              inputId =  "TimeFinderMin", 
                                              label = "From:", 
                                              choices = 2010:2019,
                                              selected = 2010
                                            ),
                                            selectInput(
                                              inputId =  "TimeFinderMax", 
                                              label = "To:", 
                                              choices = 2010:2019,
                                              selected = 2019
                                            )
                                            
                                            
                           )),
                           
                           fluidRow(
                             checkboxGroupInput(inputId = "TypeFinder",
                                                label = "Targeted LULC Class:",
                                                choices = rownames(states_list[[1]] ),
                                                selected =  c('Water','Urban')  )
                             
                             
                           ),
                           tags$hr(style="border-color: purple;"), 
                           withSpinner(dataTableOutput(outputId = "typetableFinder"))
                           
                           
                           
                           
                  ),
                  tabPanel("Maps", 
                           icon=icon("map"),
                           fluidRow(
                             column(6, 
                                    div(
                                      selectInput('lulc_class',
                                                  'Targeted LULC Class:',
                                                  rownames(states_list[[1]])),
                                      style = "position:relative;z-index:10000"
                                      
                                    )  
                                    
                             ),
                             
                             column(6,
                                    selectInput(
                                      inputId =  "targetedyear", 
                                      label = "Targeted Year", 
                                      choices = 2010:2019,
                                      selected = 2010
                                    )
                             )
                           ),
                           
                           fluidRow(leafletOutput("map"))%>% withSpinner(color = "green")
                  )
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {

################### Plot 1################### 
  
  selectorpolt1 <- reactive({
    if (input$yearsregion1 %in% colnames(states_list[[1]]) )
    {
      n <-  which (wunschjahrliste==input$yearsregion1)
      return(n)
      
    }
  })
  
  
  selectarea1 <- reactive({
    for (state in names(states_list)) {
      if (input$region1 == state) {
        return(states_list[[state]])
      }
    }
  })
  
  
  output$plot1<- renderPlot(.plot1())
  .plot1 <- reactive( 
    ggplot(data=selectarea1() ,aes(x=rownames(selectarea1()), y= ( selectarea1()[,(selectorpolt1())]), fill=rownames(selectarea1())))+
      geom_bar(stat="identity", width=0.2)+
      theme(panel.background=element_blank()) +
      theme(plot.background=element_blank())+
      theme(panel.grid.major.y=element_line(color="grey"))+
      xlab('Land Cover Tpye')+ 
      guides(x =  guide_axis(angle = 90))+
      ylab("Area Km2")+
      ylim(0,1000)+
      scale_fill_manual(values=coloorspie)+
      theme(legend.position = "none")+
      labs(title = paste0("The Change of LULC  in "," ",input$yearsregion1," " ,"in"," ", input$region1  ))
    
  )
  
  plotInputdownload1 = function(){.plot1()}
  
  output$downloadPlot1 <- downloadHandler(
    filename = function() { paste("test", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInputdownload1() ,width=5, height=4 ,device = "png", dpi=700)
    }
  )
################### Plot 1################### 

  
################### Plot 2################### 
  selectorpolt2<- reactive({
    if (input$yearsregion2 %in% colnames(states_list[[1]]) )
    {
      n <-  which (wunschjahrliste==input$yearsregion2)
      return(n)
      
    }
  })

  selectarea2 <- reactive({
    for (state in names(states_list)) {
      if (input$region2 == state) {
        return(states_list[[state]])
      }
    }
  })
  
  output$plot2<- renderPlot(.plot2())
  .plot2 <- reactive( 
    ggplot(data=selectarea2() ,aes(x=rownames(selectarea2()), y= ( selectarea2()[,(selectorpolt2())]),fill=rownames(selectarea2()) ))+
      geom_bar(stat="identity", width=0.2)+
      theme(panel.background=element_blank()) +
      theme(plot.background=element_blank())+
      theme(panel.grid.major.y=element_line(color="grey"))+
      xlab('Land Cover Tpye')+ 
      guides(x =  guide_axis(angle = 90))+
      ylab("Area Km2")+
      ylim(0,1000)+
      scale_fill_manual(values=coloorspie)+
      theme(legend.position = "none")+
      labs(title = paste0("The Change of LULC  in "," ",input$yearsregion2," " ,"in"," ", input$region2 ))
    
  )
  
  plotInputdownload2 = function(){.plot2()}
  
  output$downloadPlot2 <- downloadHandler(
    filename = function() { paste("test", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInputdownload2() ,width=5, height=4 ,device = "png", dpi=700)
    }
  )
  
  
  
  
################### Plot 2################### 
  
  
  
################### Plot 3################### 

  output$plot3<-renderPlot(.plot3())
  .plot3<-
    reactive (
      ggplot(selectarea1(), aes(x="", y=((selectarea1()[,selectorpolt1()] )),fill=rownames(selectarea1()))) +
        geom_bar(stat="identity", width=1) + 
        theme(panel.background=element_blank()) +
        theme(plot.background=element_blank())+
        ylab("Percentages of the LULC ")+
        coord_polar("y", start=0)+scale_fill_manual(values =coloorspie, name='Land Cover Class' )+
        xlab('')+
        theme_void()+
        labs(title = paste0("The Change of LULC  in "," ",input$yearsregion1," " ,"in"," ", input$region1  ))+
        geom_text((aes(x = 1.3)),position = position_stack(vjust = 0.5) , 
                  label= paste0( round((selectarea1()[,selectorpolt1()] )/ sum((selectarea1()[,selectorpolt1()] )) * 100, 1), "%"))
    )
  plotInputdownload3 = function(){.plot3()}
  
  output$downloadPlot3 <- downloadHandler(
    filename = function() { paste("test", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInputdownload3() ,width=5, height=4 ,device = "png", dpi=700)
    }
  )
  
   
  
  
################### Plot 3###################   
  
  
################### Plot 4################### 
  
  output$plot4<-renderPlot(.plot4())
  .plot4<-
    reactive (
      ggplot(selectarea2(), aes(x="", y=((selectarea2()[,selectorpolt2()] )),fill=rownames(selectarea2()))) +
        geom_bar(stat="identity", width=1) + 
        theme(panel.background=element_blank()) +
        theme(plot.background=element_blank())+
        ylab("Percentages of the LULC ")+
        coord_polar("y", start=0)+scale_fill_manual(values =coloorspie, name='Land Cover Class' )+
        xlab('')+
        theme_void()+
        labs(title = paste0("The Change of LULC  in "," ",input$yearsregion2," " ,"in"," ", input$region2  ))+
        geom_text((aes(x = 1.3)),position = position_stack(vjust = 0.5) , 
                  label= paste0( round((selectarea2()[,selectorpolt2()] )/ sum((selectarea2()[,selectorpolt2()] )) * 100, 1), "%"))
    )
  plotInputdownload4 = function(){.plot4()}
  
  output$downloadPlot4 <- downloadHandler(
    filename = function() { paste("test", '.png', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInputdownload4() ,width=5, height=4 ,device = "png", dpi=700)
    }
  )
    
  
  
################### Plot 4###################     

  
  
################### Maps ################### 
  bins <- c(0, 10, 20, 50, 100, 150, 250,350, 450,550,650, 750,850, 1000)
  pal <- colorBin("YlOrRd", bins = bins)

  mapdataselector <- reactive({
    for (year in names(years_list)) {
      if (paste0("year_", input$targetedyear) == year) {
        return(years_list[[year]])
      }
    }
  })
  
  classselector  <- reactive({
    if (input$lulc_class %in% (rownames(mapdataselector())))
    {
      mapdataselector() %>%
        filter((rownames(mapdataselector())) == input$lulc_class)
    }
  })
  
  palvalue<-reactive({
    
    DF <-  as.data.frame(t(classselector()))
    colnames(DF) <- rownames(classselector())
    rownames(DF) <- colnames(classselector())
    names(DF) <- NULL
    DF<- DF[-1,]
    colnames(DF) <-'X1'
    DF <- tibble::rowid_to_column(DF, "index")
    merged <-geo_join (germeny_states, DF, 'ID_1', 'index')
    return(merged$X1)
    
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery",options = tileOptions(minZoom = 3 , maxZoom = 16)) %>%
      addPolygons(data =germeny_states ,
                  fillColor = ~pal(palvalue() ),
                  fillOpacity = 0.98,
                  color = "white",
                  weight = 1.2,
                  popup = paste0(
                    "<br><span style='color: blue;'><strong> State Name: </strong></span>",
                    germeny_states$names_name, 
                    "<br><span style='color: blue;'><strong> Land Cover Type: </strong></span>",
                    input$lulc_class,
                    "<br><span style='color: blue;'><strong> Area Km2: </strong></span>",
                    palvalue()
                  )
                  
      )%>%
      
      addLegend(pal = pal, values = palvalue(), opacity = 0.7, 
                title =paste0(input$lulc_class," ", " Areas Over the Germany States in "," ", input$targetedyear),
                position = "bottomright")
    
    
  })
  
  
  
  
################### Maps ################### 
  
  
################### Table ################### 
  
  
  selectregiontable <- reactive({
    for (state in names(states_list)) {
      if (input$regiontable == state) {
        return(states_list[[state]])
      }
    }
  })
  
  dataframe <- reactive({
    region_data <- selectregiontable()
    colnames_regiondata<-colnames(region_data)
    
    TimeFinderMin_Index <- grep(paste0("^",input$TimeFinderMin),colnames_regiondata)
    TimeFinderMax_Index <- grep(paste0("^",input$TimeFinderMax),colnames_regiondata)
  
    region_data <- region_data[,TimeFinderMin_Index:TimeFinderMax_Index]
    
    if(!is.null(input$TypeFinder)){
      region_data <- region_data[row.names(region_data) %in% input$TypeFinder,]
    }

    return(region_data)
    
  })
  # 
  # output$typetableFinder <- renderDataTable({
  #   withSpinner(
  #     DT::datatable(dataframe(), options = list(pageLength = 25))
  #   )
  # }, server = FALSE, escape = FALSE)
  
  
  
  output$typetableFinder <- renderDataTable({
    datatable ((dataframe()),
               extensions = c("Buttons"),
               options = list(
                 dom = 'Bfrtip',
                 buttons = list(
                   list(extend = "csv", text = "Download Current Page", filename = "page",
                        exportOptions = list(
                          modifier = list(page = "current")
                        )
                   ),
                   list(extend = "csv", text = "Download Full Results", filename = "data",
                        exportOptions = list(
                          modifier = list(page = "all")
                        )
                   )
                 )
               ) 
    )
  })
  

  
################### Table ################### 
  
}

# Run the application 
shinyApp(ui = ui, server = server)
