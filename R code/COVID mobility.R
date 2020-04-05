#ADS 1
setwd("/Users/andywilson1/Desktop/COVID Mobility") 
set.seed(54321)

library(leaflet)
library(tidyverse)
library(missRanger)
library(readxl)
library(leaflet.extras)

#Pull & save local copy: may find reliable feed in future

mobility<-read.csv(url("https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/mobility_report_US.csv"))

mobility <- mobility %>%
  select( -Date) %>%
  filter(Region == "Total")

mobility_scores<-select(mobility, -c(State, Region))
fit <- factanal(mobility_scores, 2, rotation="promax", scores="regression")
print(fit, digits=2, cutoff=.3, sort=TRUE)

mobility <- as.data.frame(cbind(mobility, fit$scores))
mobility$F<- (mobility$Factor1 + mobility$Factor2)/2
mobility$NAME <- mobility$State





states <- geojsonio::geojson_read("us_state_polygons.json", what = "sp")


mydf <- sp::merge(states, mobility, by="NAME", all=F)

## Velocities

CovJHU_3.28 <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-04-2020.csv"))
CovJHU_3.27 <- read.csv(url("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/04-03-2020.csv"))

#Get cases by state

CovJHU_3.28_sum <- CovJHU_3.28 %>%
  filter(Country_Region =="US") %>%
  group_by(Province_State) %>%
  summarise(total_confirmed28 = sum(Confirmed), total_deaths28= sum(Deaths))

CovJHU_3.27_sum <- CovJHU_3.27 %>%
  filter(Country_Region =="US") %>%
  group_by(Province_State) %>%
  summarise(total_confirmed27 = sum(Confirmed), total_deaths27= sum(Deaths))

COV27_28 <-merge(CovJHU_3.28_sum, CovJHU_3.27_sum)

COV27_28$confirm_change <-((COV27_28$total_confirmed28-COV27_28$total_confirmed27)/COV27_28$total_confirmed27)*100
COV27_28$deaths_change <-((COV27_28$total_deaths28-COV27_28$total_deaths27)/COV27_28$total_deaths27)*100
COV27_28$NAME <- COV27_28$Province_State

mydf<- sp::merge(mydf, COV27_28, by="NAME", all=F)

repub<-read_excel("ContUSStates.xlsx")

mydf<- sp::merge(mydf, repub, by="NAME", all=F)

head(mydf@data)

m <- leaflet(mydf, options = leafletOptions(dragging=TRUE, 
                                          minZoom=4, 
                                          maxZoom=6))%>%
  setView(-96, 37.8, 5) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

labels <- paste("<b>", mydf$State,"</b>",
                "<br/>",
                "<br/>",
                "<b>", "F1 = ", round(mydf$Factor1,3),"</b>",
                "<br/>",
                "Parks change: ", mydf$Parks, " (Low is good)",
                "<br/>",
                "Transit stations change: ", mydf$Transit.stations, " (Low is good)",
                "<br/>",
                "Workplaces change: ", mydf$Workplaces, " (Low is good)",
                "<br/>",
                "Residential change: ", mydf$Residential, " (High is good)",
                "<br/>",
                "<b>", "F2 = ", round(mydf$Factor2,3), "</b>",
                "<br/>",
                "Retail/recreation change: ", mydf$Retail...recreation, " (Low is good)",
                "<br/>",
                "Grocery/pharmacy: ", mydf$Grocery...pharmacy, " (Low is good)",
                "<br/>",
                "COVID Total cases :", mydf$total_confirmed28,
                "<br/>",
                "Percent change in deaths = ", round(mydf$deaths_change, 2), "%",
                "<br/>",
                "Percent daily change = " , round(mydf$confirm_change, 2) , "%",
                "<br/>",
                "<br/>",
                "<b>", "Percent Leaning Republican", mydf$Republican , "%", "</b>")

pal <- colorNumeric(palette = "RdYlBu", domain = c(-1,1), reverse = TRUE)

mm<- m %>% addPolygons(
  fillColor = pal(mydf$F),
  weight = 2,
  opacity = 1,
  color = "black",
  dashArray = "1",
  fillOpacity = 0.3,
  label = mydf$State,
  popup  = labels,
  highlight = highlightOptions(
    weight = 2,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.6,
    bringToFront = TRUE))%>%
  addLegend(title = "Composite mobility factor", 
            position = "bottomleft",
            pal=pal,
            values = c(-1,1))


mmm <- mm %>% addCircleMarkers(
                   lng = CovJHU_3.28$Long_,
                   lat = CovJHU_3.28$Lat,
                   radius = log(CovJHU_3.28$Confirmed),
                   color = "red",
                   weight = 1, opacity = 0.1, fillOpacity = 0.1) %>% 
  addResetMapButton()


library(htmlwidgets)

saveWidget(mmm, file = "Garbage1.html")

blab<-mydf@data
blap2<-read_excel("ContUSStates.xlsx")

blap3<-merge(blap, blap2, by = "State")

cor.test(blap3$Factor1, blap3$Republican)

