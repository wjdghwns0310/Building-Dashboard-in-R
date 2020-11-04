# import packages
library(tidyverse);library(shiny);library(shinydashboard); library(DT); library(ggplot2); library(dplyr); 
library(labeling); library(gapminder); library(leaflet); library(treemap);library(highcharter)

# read data
# source("finaldata.R")
finaldata=readRDS("finaldata.rds")


ui <- dashboardPage(skin="blue", # skin = c("blue", "black","purple", "green", "red", "yellow")

            dashboardHeader(title="InKo Career Consulting", titleWidth = 250),
            # sidebar content
            dashboardSidebar(color="Job Market Intelligence Dashboard ", width=250,
                sidebarMenu(
                        menuItem("Find your best Job Category", tabName="firsttab", icon=icon("dashboard")),
                        menuItem("Salary and Risk Associated", tabName="salary", icon=icon("star")),
                        menuItem("Job Search Engine Index", tabName="thirdtab", icon=icon("table"))
                        )
                    ),
                    # body content
             dashboardBody(
                tabItems(
                          ## first tab content 
                 tabItem("firsttab", # tabName is connected to'tabitem'
 
                        ### infocards
                     fluidRow(
                         infoBoxOutput("AverageSalary",width=3),
                         infoBoxOutput("AverageTurnover",width=3),
                         infoBoxOutput("AverageRating",width=3),
                         infoBoxOutput("TopCity",width=3),

                                        ),

                     fluidRow(
                     ### map
                           box(title="Job Distribution across the U.S.", #background = "light-blue", # set the background color of map ("red", "yellow", "aqua", "light-blue", "green", "navy", "teal", "olive", "lime", "orange", "fuchsia", "purple", "maroon", "black")
                           leafletOutput("mymap")
                                         , height='505px',solidHeader = F
                                        ),
                      ### plots
                            box(title="Job Dimensions", #background='light-blue', solidHeader = F, # set the background color of map
                            tabsetPanel(
                               tabPanel("Job vs Salary", plotOutput("ggplot")),
                               tabPanel("Job vs Word of Mouth", plotOutput("boxplot"), 
                                        )))
                                ),

                                    
                                    
                    ### controls
                    fluidRow(
                            box(title="Work Environment: ", #background = "light-blue",
                                selectizeInput(inputId = "InputSector", label="Sector:",
                                               finaldata$Sector, selected=c("Finance", "Information Technology", "Retail", "Consumer Services", "Insurance", "Business Services"), multiple=TRUE
                                )
                                ),
                            box(title="Your Preference: ",
                                    checkboxGroupInput(inputId = "CheckSize",label =  "Size of Company:", 
                                                   choices=c("Startup", "Medium", "Large"), selected=c("Startup","Medium","Large")),
                                    radioButtons(inputId = "ButtonEasyApply",
                                                     label = "Accept Easy Apply:",
                                                     choices = c("TRUE", "FALSE"), selected="FALSE")
                                ),
                            )               
                     ), #close 'firsttab'


                            
                            ## the second tab content
                 tabItem("salary", # making a secondary tab
                         fluidRow(
                           box(id = "SelectCateg" ,side = "left", width=3, background = "black",
                               radioButtons('jobCateg', 'Select the desired Job Category', choices = unique(finaldata$JobCategory))
                           ),
                           infoBoxOutput("avgSalaryRange",width=3),
                           infoBoxOutput("easyapplyinfo",width=3),
                           infoBoxOutput("cityinfo",width=3)
                           # box(id = "sectorinfobox", title = "Top 5 sectors for that job category" , width=4, 
                           #     htmlOutput("sectorinfo"))
                         ),
                         fluidRow(
                           box(id = "usMapPlot" ,side = "left", title = "Map Cluster of #Jobs based on Company Size",
                               leafletOutput("usMapPlot")
                           ),
                           tabBox(id = "heatMap" , title = "Job Stability",
                                  tabPanel(title = "Stability",plotOutput("heatMapPlot")),
                                  tabPanel(title = "What is Risk Factor?",plotOutput("RiskfactorFormula"))
                           )
                         ),
                         fluidRow(
                           box(id = "treeMap" ,side = "right", title = "Jobs distribution based on Sector" , width=12,
                               plotOutput("distPlot")
                           )
                         )
                         
                 ),
                            ## the third tab content
                    tabItem("thirdtab", # making a secondary tab
                                fluidPage(
                                h1("Search Your Job"),
                                dataTableOutput("finaldata")
                            )            )
                            
    )#close tabitems
    )#close 'dashboard body'
    )#close dashboard page


# this is where you render your output
## the first tab
server <- function(input, output) {    
  
### first tab: for interactive infoboxes 
  output$AverageSalary <- renderInfoBox({
    filtereddata = finaldata %>% filter(Sector==input$InputSector)  %>% 
                                 filter(SizeCategory==input$CheckSize) %>%
                                 filter(EasyApply==input$ButtonEasyApply)
    avgsal=round(mean(filtereddata$SalaryAvg), digits=0)
    infoBox("Average Salary", paste0("$ ", avgsal),  icon = icon("money"), color = "aqua", fill=TRUE, "of 3 Job Categories")
  })
  
  output$AverageTurnover <- renderInfoBox({
    filtereddata = finaldata %>% filter(Sector==input$InputSector)  %>% 
                                 filter(SizeCategory==input$CheckSize) %>%
                                 filter(EasyApply==input$ButtonEasyApply)
    avgturn = round(mean(filtereddata$RevenueAvg)/1000000000, digits=2)
    infoBox("Average Turnover", paste0("$ ", avgturn, " Billion"  ), icon = icon("bar-chart-o"), color = "aqua", fill=TRUE)
  })
  
  output$AverageRating <- renderInfoBox({
    filtereddata = finaldata %>% filter(Sector==input$InputSector)  %>% 
                                 filter(SizeCategory==input$CheckSize) %>%
                                 filter(EasyApply==input$ButtonEasyApply)
    avgrat = round(mean(filtereddata$Rating), digits=1)
    infoBox("Average Rating", paste0(avgrat, " / ", "5"),  icon = icon("thumbs-up"), color = "aqua", fill=TRUE, "★★★☆☆")    
    })
  
  
  output$TopCity <- renderInfoBox({
    filtereddata = finaldata %>% filter(Sector==input$InputSector)  %>% 
                                 filter(SizeCategory==input$CheckSize) %>%
                                 filter(EasyApply==input$ButtonEasyApply)
    topcity=names(sort(table(filtereddata$LocationCity), decreasing = TRUE)[1])
    top2city=names(sort(table(filtereddata$LocationCity), decreasing = TRUE)[2])
    infoBox("Top Employing CIty", topcity,  icon = icon("rocket"), color = "aqua", fill=TRUE, paste0("followed by ", top2city ))
  })
  
### the first tab: leaflet map    
  x <- reactive( finaldata 
                %>% filter(SizeCategory==input$CheckSize)
                %>% filter(Sector==input$InputSector)
                %>% filter(EasyApply==input$ButtonEasyApply)

    )

  output$mymap <- renderLeaflet({
  filtereddata = finaldata %>%
     filter(SizeCategory==input$CheckSize) %>%
     filter(Sector==input$InputSector)%>%
     filter(EasyApply==input$ButtonEasyApply)
    
  filteredloc = filtereddata %>%
                select(Latitude = CityLatitude, Longitude= CityLongitude)
  filteredloc %>% 
        leaflet() %>%
        addTiles() %>%
        setView(lng = -98.439032, lat = 39.232253, zoom = 4.3) %>%
        addCircleMarkers( popup = filtereddata$Location, label=paste(filtereddata$JobCategory," ,", filtereddata$CompanyName),
                          weight = 7, radius=10, fillOpacity = 0.3, color=factor(filtereddata$JobCategory, levels=c('Business Analyst', 'Data Analyst', 'Data Scientist'), labels = c('#ffa500','#13ED3F', '#3F2D91')),
                          stroke = F, clusterOptions = NULL, clusterId = NULL) %>%
        addLegend("bottomright", colors= c("#ffa500", "#13ED3F", "#3F2D91"), labels=c("Business Analyst", "Data Analyst", "Data Scientist"), title="Job Category")
    })

    
### the first tab: ggplot
    output$ggplot <- renderPlot({
        x() %>% ggplot(aes(SizeAvg, SalaryAvg, size=RevenueMillion, color=JobCategoryNumeric))+ 
                geom_point(alpha=0.2)+
                geom_smooth(aes(group=JobCategory), method="lm", show.legend = TRUE)+
                facet_wrap(~JobCategory, scales="free")+
                labs(x="Size of Company", y="Salary")+
                # scale_size(range=c(.5,15))+
                theme_grey(base_size = 15, base_family = "")+
                theme(legend.position="none")
                                 })
    
    output$boxplot <- renderPlot({
        x() %>% ggplot (aes(x=JobCategory, y=Rating, fill=factor(JobCategory)))+
                geom_boxplot()+
                geom_point(shape=21, position=position_jitterdodge(), width=0.2, alpha=0.15, size=3,stroke=1)+ #the pile
                geom_jitter(width=0.2, alpha=0.3)+
                labs(x="Job Category", y="Rating", fill="Job Category")+
                theme_grey(base_size = 15, base_family = "")

                                }) #close renderplot
    
  ## the second tab
  ### the second tab infoboxes
    output$avgSalaryRange <- renderInfoBox({
      filtereddata = finaldata %>% filter(finaldata$JobCategory==input$jobCateg)
      print(mean(filtereddata$SalaryEstimateMin))
      salaryMin = round(mean(filtereddata$SalaryEstimateMin)/1000, digits=0)
      salaryMax = round(mean(filtereddata$SalaryEstimateMax)/1000, digits = 0)
      infoBox("Average salary range",paste0("$", salaryMin, "K - ", salaryMax, "K"),  icon = icon("money"), color = "purple", fill=TRUE)
    })
 
    output$easyapplyinfo <- renderInfoBox({
      filtereddata = finaldata %>% filter(finaldata$JobCategory==input$jobCateg)
      easyApplynumber = table(filtereddata$EasyApply)[2]
      infoBox("Jobs with Easy Apply option", easyApplynumber[1],  icon = icon("linkedin"), color = "red", fill=TRUE)
      
    })
    
    output$cityinfo <- renderInfoBox({
      filtereddata = finaldata %>% filter(finaldata$JobCategory==input$jobCateg)
      topCity = names(sort(table(filtereddata$LocationCity),decreasing = TRUE)[1])
      infoBox("City with Most Jobs",topCity,  icon = icon("city"), fill=TRUE)
    })
  ### the second tab: maps  
    output$usMapPlot <- renderLeaflet({
      filtereddata = finaldata %>% filter(finaldata$JobCategory==input$jobCateg)
      filteredloc = filtereddata %>% group_by(SizeCategory) %>% select(Latitude = CityLatitude, Longitude= CityLongitude)
      # Create a palette that maps factor levels to colors
      pal <- colorFactor(c("green", "navy", "red"), domain = c("Large", "Medium", "Startup"))
      
      filteredloc %>% 
        leaflet() %>%
        addTiles() %>%
        setView(lng = -95.695312, lat = 39.056198, zoom = 4) %>%
        addCircleMarkers( label = filteredloc$SizeCategory, color=pal(factor(filteredloc$SizeCategory)), stroke = FALSE, fillOpacity = 0.5, clusterOptions = markerClusterOptions()) %>%
        addLegend("bottomright", pal = pal, values = ~unique(filteredloc$SizeCategory),
                  title = "Size Category",
                  opacity = 1
        )
      
    })
    
    # output$sectorinfo <- renderUI({
    #   filtereddata = finaldata %>% filter(finaldata$JobCategory==input$jobCateg)
    #   top5Sector = names(sort(table(filtereddata$Sector),decreasing = TRUE)[1:5])
    #   HTML(paste(top5Sector[1],top5Sector[2],top5Sector[3],top5Sector[4],top5Sector[5], sep = "<br/>"))
    # })
    
  ### the second tab: eatmap
    output$heatMapPlot <- renderPlot({
      x <- finaldata %>% filter(finaldata$JobCategory==input$jobCateg) #Job stability
      x %>% ggplot(aes(x=SizeCategory, y=Sector)) + geom_tile(aes(fill=RiskFactor)) + theme_minimal() + scale_fill_gradient(high = "red")
      # datatree= x %>% group_by(Sector) %>% summarise(SalaryAvg = mean(SalaryAvg), RiskAvg = mean(RiskFactor))
      # datatree %>% hchart("treemap", hcaes(x=Sector , value = SalaryAvg, color = RiskAvg))
      # # x() %>% group_by(RiskFactor) %>% summarise(meanSA=mean(SalaryAvg), meanSalaryMin=mean(SalaryEstimateMin)) %>% ggplot(aes(x=meanSalaryMin, xend= meanSA, y=RiskFactor)) + geom_dumbbell(color="#a3c4dc", 
      #                                                                                                                            size=0.75, 
      #                                                                                                                            point.colour.l="#0e668b")
    })
   ### the second tab: image
    output$RiskfactorFormula <- renderImage({
      #filename <- normalizePath(file.path(paste('RFactor', '.png', sep='')))
      
      # Return a list containing the filename
      #list(src = filename)
      list(src = "RFactor.PNG",
           contentType = 'image/png')
    }, deleteFile = FALSE)
    
   ### the second tab: decision tree map
    output$distPlot <- renderPlot({
      x <- finaldata %>% filter(finaldata$JobCategory==input$jobCateg) #Job stability
      filteredframe <- x %>% group_by(SizeCategory) %>% summarise(sector = names(table(Sector)), JobSector = as.numeric(table(Sector))) %>% ungroup()
      print(filteredframe)
      filteredframe %>% treemap(index = c("SizeCategory","sector"), vSize = "JobSector", type="index", 
                                fontsize.labels=c(15,12),                # size of labels. Give the size per level of aggregation: size for group, size for subgroup, sub-subgroups...
                                fontcolor.labels=c("white","black"),    # Color of labels
                                fontface.labels=c(2,3),                  # Font of labels: 1,2,3,4 for normal, bold, italic, bold-italic...
                                bg.labels=c("transparent"),
                                border.col=c("black","white"),             # Color of borders of groups, of subgroups, of subsubgroups ....
                                border.lwds=c(7,2)
      )
      # # x() %>% group_by(RiskFactor) %>% summarise(meanSA=mean(SalaryAvg), meanSalaryMin=mean(SalaryEstimateMin)) %>% ggplot(aes(x=meanSalaryMin, xend= meanSA, y=RiskFactor)) + geom_dumbbell(color="#a3c4dc", 
      #                                                                                                                            size=0.75, 
      #                                                                                                                            point.colour.l="#0e668b")
    })
    
  
## the third tab:  rendering table for 'Job Search Engine Tab'
    output$finaldata = renderDataTable(
        DT::datatable(finaldata, escape=FALSE, 
                      options = list(
                          pageLength = 10, autoWidth = FALSE,
                          columnDefs = list(list( targets = 2, width = '600px', 
                                                  visible=FALSE
                          )),
                          scrollX = TRUE
                      ))) # simply put the name of variable in the parenthesis. This is available by library(DT), which imports an interactive table.
    
}

# to run the app
shinyApp(ui, server)


