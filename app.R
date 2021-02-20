library(shiny)
library(tmap)
library(viridis)
library(tidyverse)


monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}
tmap_mode("view")
load("ts_combi_sf.Rda")
load("ny_demographics.Rda")
vir <- viridis(9)


library(readr)
covid_cases_nyc <- read_csv("input/covid_cases_nyc.csv")
covid_2 <- covid_cases_nyc %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>% 
  filter(date <= "2020-12-31")
#View(covid_cases_nyc)

no_covid_dates <- seq.Date(from = as.Date("2020-01-01"), to = as.Date("2020-02-28"), "days")
length(no_covid_dates)
no_covid <- data.frame("date" = no_covid_dates,
                       "confirmed_cases" = integer(59),
                       "cases_per_day" = integer(59),
                       "deaths" = integer(59),
                       "deaths_per_day" = integer(59))

COVID <- rbind(no_covid,covid_2) 




###################### UI #########################

ui <- fluidPage(
  titlePanel("MTA Subway Ridership"),
    sliderInput("date", "Time",
                min = as.Date("2020-01-01"),
                max =as.Date("2020-12-31"),
                value=as.Date("2020-01-01"),
                timeFormat="%b %Y", 
                animate = animationOptions(interval = 100)),
    radioButtons("demographic",
                 "Demographic Data", 
                 c("Median Income", 
                   "Race: Percent White",
                   "Race: Percent Black",
                   "Race: Percent Asian",
                   "Race: Percent Other",
                   "Race: Percent 2orMore",
                   "Primary Work Transport: PT",
                   "Primary Work Transport: Car")),
  
  splitLayout(tmapOutput(outputId = "map"),
              plotOutput("covid")
              )
  
)

server <- function(input, output, session) { 

  output$map <- renderTmap({
  
    if(input$demographic == "Median Income") {
      tm_shape(ny_demographics) +
        tm_polygons(col = "income_median", 
                    breaks = c(0, 50000, 100000, 150000, 250000),
                    palette = vir, 
                    alpha = 0.6, 
                    border.alpha = 0.5, 
                    title = "Median Household Income in 2019, USD",
                    id = "income_median")
    } else if(input$demographic == "Race: Percent White")  {
      tm_shape(ny_demographics) +
        tm_polygons(col = "race_white", 
                    palette = vir, alpha = 0.6, border.alpha = 0.5, 
                    title = "Race: Pct White",
                    id = "race_white")
    }else if(input$demographic == "Race: Percent Black")  {
      tm_shape(ny_demographics) +
        tm_polygons(col = "race_black", 
                    palette = vir, alpha = 0.6, border.alpha = 0.5, 
                    title = "Race: Pct Black",
                    id = "race_black")
    }else if(input$demographic == "Race: Percent Asian")  {
      tm_shape(ny_demographics) +
        tm_polygons(col = "race_asian", 
                    palette = vir, alpha = 0.6, border.alpha = 0.5, 
                    title = "Race: Pct Asian",
                    id = "race_asian")
    }else if(input$demographic == "Race: Percent Other")  {
      tm_shape(ny_demographics) +
        tm_polygons(col = "race_other", 
                    palette = vir, alpha = 0.6, border.alpha = 0.5, 
                    title = "Race: Pct Other",
                    id = "race_other")
    }else if(input$demographic == "Race: Percent 2orMore")  {
      tm_shape(ny_demographics) +
        tm_polygons(col = "race_2ormore", 
                    palette = vir, alpha = 0.6, border.alpha = 0.5, 
                    title = "Race: Pct 2orMore",
                    id = "race_2ormore")
    }else if(input$demographic == "Primary Work Transport: PT")  {
      tm_shape(ny_demographics) +
        tm_polygons(col = "transport_pt", 
                    palette = vir, alpha = 0.6, border.alpha = 0.5, 
                    title = "Primary Work Transport: PT",
                    id = "transport_pt")
    }else if(input$demographic == "Primary Work Transport: Car")  {
      tm_shape(ny_demographics) +
        tm_polygons(col = "transport_car", 
                    palette = vir, alpha = 0.6, border.alpha = 0.5, 
                    title = "Primary Work Transport: Car",
                    id = "transport_car")
    }else {
      tm_shape(ny_demographics) +
        tm_polygons()
    
    }
      #tm_dots(zindex = 401)
    
  })
  
  observe({
    ts_filter <- ts_combi_sf %>% filter(as.numeric(month) == as.numeric(format(input$date, "%m")))
    
    tmapProxy("map", session, {
      #tm_remove_layer(401) +
        tm_shape(ts_filter)+
        tm_symbols(col = "remaining_ridership", 
                   breaks = c(0, 10, 20, 30, 40,50,60,70,80,90, 100),
                   palette = inferno(4, direction = -1),
                   size = 0.05, 
                   alpha = 1, 
                   title.col = "Remaining Ridership")
    }) 
  })
  
  output$covid <- renderPlot({
    g1 <- subset(COVID, date == input$date)
    ggplot(COVID, aes(x = date, y = cases_per_day)) + geom_point() + 
    geom_point(data = g1, colour = "red", mapping = aes(size = 10) )})
  
  output$date_text <- renderPrint( {str(input$date)})
}

shinyApp(ui, server)

#server <- function(input, output) {
#  output$hist <- renderPlot({
#    hist(rnorm(input$num))
#  })





