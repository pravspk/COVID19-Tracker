library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(plotly)
library(dplyr)
library(ggplot2)
library(htmlwidgets)

dataurl1 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
dataurl2 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
dataurl3 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
data1 <- read.csv(file = dataurl1,stringsAsFactors = FALSE,check.names = FALSE)
data2 <- read.csv(file = dataurl2,stringsAsFactors = FALSE,check.names = FALSE)
data3 <- read.csv(file = dataurl3,stringsAsFactors = FALSE,check.names = FALSE)

##select data columns
covidx <- data1 %>% select(-1,-3,-4)

##sum duplicates
covid1x <- aggregate(covidx[-1], by=covidx[1], sum)

##countries
items <- covid1x[[1]]

##FOR INDIA DISTRICTS
inddata = read.csv("https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv")
df <- inddata %>% select(1,2,3)
df <- df[complete.cases(df), ]
distitems <- unique(df$State)

##FOR TEST COUNTRIES
testdata = read.csv("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv")
testdata <- testdata[ ! testdata$Entity %in% c("India - people tested","Indonesia - units unclear","Italy - tests performed","Japan - people tested","Singapore - swabs tested","United Kingdom - tests performed","United States - specimens tested (CDC)"),]
testdf <- testdata %>% select(1,3,7)
#testdf <- testdata[complete.cases(testdata), ]
ctryitems <- unique(testdata$Entity)
ctryitems <- strsplit(as.character(ctryitems), " - ")
ctrylist <- list()
for(x in c(1:length(ctryitems))){
  print(ctryitems[[x]][1])
  ctrylist[[x]] <- ctryitems[[x]][1]
}

header <- dashboardHeader(title = "COVID-19 Tracker")

sidebar <- dashboardSidebar(
  sidebarMenu(
    selectInput("select", "Choose visualization type:",
                                  c("Line graph" = "lg",
                                    "Heat map" = "hm")),
    selectInput("variable", "Choose data:",
                                 c("Confirmed cases" = "cnf",
                                   "Recovered cases" = "rec",
                                   "Deaths" = "death")),
    conditionalPanel(
      condition = "input.select == 'lg'",
      checkboxInput("add", "Add/Remove filter", FALSE),
    ),
    conditionalPanel(
      condition = "input.add == true",
      selectizeInput(
        'country', 'Filter by country:', choices = items,
        options = list(
          placeholder = 'Select a country',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    ),
    conditionalPanel(
      condition = paste0("input.add == true && input.country!='' && '",paste(ctrylist,collapse=''), "'.includes(input.country) == true"),
      checkboxInput("gen", "Visualize countrywise tests", FALSE)
    ),
    conditionalPanel(
      condition = 'input.country == "India"',
      checkboxInput("tests", "Visualize districtwise tests", FALSE)
    ),
    conditionalPanel(
      condition = 'input.tests == true',
      selectizeInput(
        'dist', 'Filter by district:', choices = distitems,
        options = list(
          placeholder = 'Select a district',
          onInitialize = I('function() { this.setValue(""); }')
        )
      )
    )
  )
)

body <- dashboardBody(
  #plotlyOutput("covidPlot", width = "100%", height = "100%")
  tags$head(tags$script('
      // Define function to set height of "map" and "map_container"
      setHeight = function() {
        var window_height = $(window).height();
        var header_height = $(".main-header").height();

        var boxHeight = window_height - header_height - 75;

        $("#map_container").height(boxHeight);
        $("#map").height(boxHeight - 20);
      };

      // Set input$box_height when the connection is established
      $(document).on("shiny:connected", function(event) {
        setHeight();
      });

      // Refresh the box height on every window resize event    
      $(window).on("resize", function(){
        setHeight();
      });
    ')),
  # Boxes need to be put in a row (or column)
  fluidRow(
    box(width = 12, id = "map_container",
        plotlyOutput("covidPlot", height = "100%")
    )
  )
)

shinyApp(
  ui = dashboardPage(header, sidebar, body),
  server = function(input, output) {
    output$covidPlot <- renderPlotly({
      ##load data
      if(input$variable == 'cnf'){
        #dataurl1 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
        #data <- read.csv(file = dataurl1,
        #                 stringsAsFactors = FALSE,check.names = FALSE)
        title1 = "No. of confirmed cases"
        data <- data1
      }
      if(input$variable == 'death'){
        #dataurl3 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
        #data <- read.csv(file = dataurl3,
        #                 stringsAsFactors = FALSE,check.names = FALSE)
        title1 = "No. of deaths"
        data <- data3
      }
      if(input$variable == 'rec'){
        #dataurl2 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
        #data <- read.csv(file = dataurl2,
        #                 stringsAsFactors = FALSE,check.names = FALSE)
        title1 = "No. of recovered cases"
        data <- data2
      }
      #subset(data4, startsWith(data4$Entity, "Argentina")==TRUE)
      ##linegraph
      if(input$select == "lg"){
        #data = read.csv("/home/pravs/Downloads/novel-corona-virus-2019-dataset/time_series_covid_19_confirmed.csv",check.names=FALSE)
        ##select data
        ##select data columns
        covid <- data %>% select(-1,-3,-4)
        ##sum duplicates
        covid1 <- aggregate(covid[-1], by=covid[1], sum)
        ##get date cols
        dates <- colnames(data[-1:-4])
        ##format dates
        betterDates <- as.Date(dates,
                               format = "%m/%d/%y")
        ##select ctry
        ctrytemp1 <- covid1$`Country/Region`[(order(covid1[length(covid1)], decreasing=TRUE)[1])]
        ##preprocess  
        #check filter
        if(input$add == TRUE && input$country!=""){
          ##select ctry data
          ctry1 <- subset(covid1, covid1$`Country/Region`==input$country)
        }else{
          ##select ctry data
          ctry1 <- covid1[(order(covid1[length(covid1)], decreasing=TRUE)[1]),]
        }
        ##plot variables
        x1 <- as.Date(colnames(ctry1[-1]),format="%m/%d/%y")
        y1 <- t(ctry1[-1])[,1]
        if(input$add == TRUE && input$country!=""){
          if(input$tests && input$dist!=""){
            sub <- subset(df, df$State==input$dist)
            fig <- plot_ly(x = as.Date(sub$Updated.On, format = '%d/%m/%y'), y = sub$Total.Tested, yaxis = "y2", type='scatter', mode='lines+markers',name = as.character(subset(covid1$`Country/Region`, covid1$`Country/Region`==input$country))) %>% layout(title=paste("Number of tests conducted - ",input$dist),margin = list(t=60,l=20,r=40)) %>% config(displayModeBar = FALSE)
            fig <- fig %>% layout(yaxis2 = list(overlaying = "y",side = "right"))
            fig
          }else if(input$gen==TRUE && input$country %in% ctrylist){
            sub2 <- subset(testdf, startsWith(as.character(testdf$Entity), input$country)==TRUE)
            fig <- plot_ly(x = as.Date(sub2$Date), y = sub2$Cumulative.total, yaxis = "y2", type='scatter', mode='lines+markers',name = as.character(subset(covid1$`Country/Region`, covid1$`Country/Region`==input$country))) %>% layout(title=paste("Number of tests conducted - ",input$country),margin = list(t=60,l=20,r=40)) %>% config(displayModeBar = FALSE)
            fig <- fig %>% layout(yaxis2 = list(overlaying = "y",side = "right"))
            fig
          }else{
            fig <- plot_ly(ctry1[-1], x = x1, y = y1, yaxis="y2",type="scatter", mode="lines", name = as.character(subset(covid1$`Country/Region`, covid1$`Country/Region`==input$country))) %>% layout(title=paste(title1," - ",input$country),margin = list(t=60,l=20,r=40)) %>% config(displayModeBar = FALSE)
            fig <- fig %>% layout(yaxis2 = list(overlaying = "y",side = "right"))
            #fig <- fig %>% add_lines(x = x1, y = fitted(lm(y1 ~ poly(x1, 2), data = ctry1[-1])))
            fig
          }
        }else{
          ##plot
          fig <- plot_ly(ctry1[-1], x = x1, y = y1, yaxis="y2",type="scatter", mode="lines", name = covid1$`Country/Region`[(order(covid1[length(covid1)], decreasing=TRUE)[1])]) %>% layout(title=title1,margin = list(t=60,l=20,r=40)) %>% config(displayModeBar = FALSE)
          fig <- fig %>% layout(yaxis2 = list(overlaying = "y",side = "right"))
          for(i in 2:5){
            ctry <- covid1[(order(covid1[length(covid1)], decreasing=TRUE)[i]),]
            fig <- fig %>% add_trace(y = t(ctry[-1])[,1], name = covid1$`Country/Region`[(order(covid1[length(covid1)], decreasing=TRUE)[i])], mode = "lines")
          }
          fig <- fig %>% layout(legend = list(x = 0.025, y = 0.975))
          fig
        }
      }
  
      ##heatmap
      else if(input$select == "hm"){
        ##select data
        #library(dplyr)
        datax <- data %>% select(2,length(data))
        ##sum duplicates
        datax <- aggregate(datax[-1], by=datax[1], sum)
        # light grey boundaries
        l <- list(color = toRGB("grey"), width = 0.5)
        # specify map projection/options
        g <- list(
          showframe = FALSE,
          showcoastlines = FALSE,
          projection = list(type = 'Mercator')
        )
        dfx <- data.frame(region=unname(unlist(datax[[1]])),value=unname(unlist(datax[[length(datax)]])),stringsAsFactors = FALSE)
        figx <- plot_geo(dfx,locationmode = 'country names') %>% layout(title=title1,margin = list(r = 0,t = 40,b = 0,l = 0)) %>% config(displayModeBar = FALSE) 
        figx <- figx %>% add_trace(
          z = dfx$value, color = dfx$value, colors = 'Blues',
          text = "", locations = dfx$region, marker = list(line = l), showscale = FALSE 
        ) #%>% hide_colorbar()
        figx
      }
    })
  }
)