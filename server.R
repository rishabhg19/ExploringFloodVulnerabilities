#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
###############################Install Related Packages #######################
if (!require("shiny")) {
    install.packages("shiny")
    library(shiny)
}
if (!require("leaflet")) {
    install.packages("leaflet")
    library(leaflet)
}
if (!require("leaflet.extras")) {
    install.packages("leaflet.extras")
    library(leaflet.extras)
}
if (!require("dplyr")) {
    install.packages("dplyr")
    library(dplyr)
}
if (!require("magrittr")) {
    install.packages("magrittr")
    library(magrittr)
}
library(shiny)
library(ggplot2)
library(httr)
library(jsonlite)
library(dplyr)
library(DT)
library(leaflet)
library(plotly)
library(lubridate)
library(corrplot)

#get data
url <- GET("https://www.fema.gov/api/open/v2/FimaNfipClaims")
my_content <- content(url, as = 'text')
json_content <- fromJSON(my_content)
data <- fromJSON(rawToChar(url$content))
claims <- data$FimaNfipClaims

# NFIP claim years
claims$dateOfLoss <- as.Date((claims$dateOfLoss), format = "%Y-%m-%d")
claims$year <- format(claims$dateOfLoss, "%Y")

#building types
color_palette <- colorNumeric(palette = "PuOr", domain = claims$buildingDamageAmount)
numeric_damage <- as.numeric(claims$buildingDamageAmount)
building_labels <- c("1" = "Main building",
                     "2" = "Detached guest house",
                     "3" = "Detached garage",
                     "5" = "Warehouse",
                     "6" = "Pool House, Clubhouse, Recreation Building",
                     "8" = "Other",
                     "10" = "Apartment building",
                     "NA" = "Not Applicable")

#AL zip code visualization
text1p1 <-"According to the chart, there seems to be no visible trend. There were peaks in NFIP claims around 2004-2005 and 2020."

AL_zipcodes = table(subset(claims, state == 'AL', select = reportedZipCode))
zipcode_to_remove <- c(0, 112)

# Remove the specific ZIP code from the table
AL_zipcodes <- AL_zipcodes[!names(AL_zipcodes) %in% zipcode_to_remove]
lbls <- as.numeric(AL_zipcodes)
zipcodes_data <- as.data.frame(AL_zipcodes)
labels_with_freq <- ifelse(lbls > 10, paste(names(AL_zipcodes), " (", lbls, ")", sep = ""), "")
#labels_with_freq <- labels_with_freq[labels_with_freq != ""]
zip_freq_table <- table(claims$reportedZipCode)


#SFHA states
states_SFHA = table(subset(claims, floodZoneCurrent == 'A' | floodZoneCurrent == 'A06' | floodZoneCurrent == 'A08' | floodZoneCurrent == 'A09' | floodZoneCurrent == 'A11' | floodZoneCurrent == 'AE' | floodZoneCurrent == 'AOB' | floodZoneCurrent == 'V09' | floodZoneCurrent == 'V10' | floodZoneCurrent == 'V12' | floodZoneCurrent == 'V15' | floodZoneCurrent == 'VE', select = state))
lbls <- as.numeric(states_SFHA)
labels_with_freq_SFHA <- paste(names(states_SFHA), " (", lbls, ")", sep = "")

#MFHA states
states_MFHA = table(subset(claims, floodZoneCurrent == 'X', select = state))
lbls <- as.numeric(states_MFHA)
labels_with_freq_MFHA <- paste(names(states_MFHA), " (", lbls, ")", sep = "")

#flood causes
cause_labels <- c("0" = "Other causes",
                  "1" = "Tidal water overflow",
                  "2" = "Stream, river, or lake overflow",
                  "3" = "Alluvial fan overflow",
                  "4" = "Accumulation of rainfall or snowmelt",
                  "B" = "Expedited claim",
                  "NA" = "Not Applicable")

#AL causes
AL_damage = subset(claims, state == 'AL')

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # Render the plot
  output$claimsTable <- renderDT({
    datatable(claims, options = list(searching = TRUE))
  })
  output$claimsFreq <- renderPlotly({
    ggplotly
    (
      ggplot(claims, aes(x = year)) +
        geom_bar(fill = "brown", color = "white", alpha = 0.7) + 
        labs(title = "Annual NFIP Claims",
             x = "Year",
             y = "Frequency") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black", color = "white")
      
    )
  })
  
  output$explanation1 <- renderUI({
    HTML(paste("<h4>", text1p1, "</h4>"))
  })
  
  output$mapText <- renderUI({
    HTML(paste("<h4>", "This map shows the locations where buildings were the most damaged by floods. In each National Flood Insurance Program claim, the damages to buildings are quantified, so this metric is used to locate the areas that have the most claims, and are therefore the areas that are most significantly impacted by floods. For the most part, they are all concentrated in Alabama. States like New Jersey, Florida, Louisiana, Arizona, and Texas, however, have also made NFIP claims. From looking through the details behind thousands of NFIP claims, it becomes more apparent why these floods are happening, and how the NFIP is being used to mitigate further flood damage, and whether it has been effective in doing so.", "</h4>"))
  })
  
  output$externalLink <- renderUI({
    HTML(paste("<h4>News sources record Alabama, Florida, and Louisiana being the major regions of floods and NFIP claims. These regions were affected by Hurricane Sally. Read more at <a href=\"https://www.eenews.net/articles/analysis-8-counties-got-half-of-2020-flood-claims/\">", "E&E News", "</a>.</h4><br>"))
  })
  output$floodMap <- renderLeaflet({
    leaflet(data = claims) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = 5,
        color = ~color_palette(buildingDamageAmount),
        fillOpacity = 0.8,
        popup = ~paste("Flood Damage: ", buildingDamageAmount)
      )
  })
  
  # error in here
  output$buildingTypes <- renderPlotly({
    ggplotly(
      ggplot(claims, aes(x = factor(buildingDescriptionCode, labels = building_labels))) +
        geom_bar(fill = "blue", color = "white", alpha = 0.7) + 
        labs(title = "Types of Buildings associated with NFIP claims",
             x = "Building Description",
             y = "Frequency") + 
        scale_y_continuous(limits = c(0, 900)) + 
        geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black") +
        theme(axis.text.x = element_text(angle = 15, hjust = 1))
    )
  })
  
  output$stateClaims <- renderPlotly({
    ggplotly(
      ggplot(claims, aes(x = state)) +
        geom_bar(fill = "lightyellow", color = "white", alpha = 0.7) + 
        labs(title = "States with Most Frequent NFIP Claims",
             x = "State", y = "Frequency") + theme(axis.text.x = element_text(angle=0, vjust=.5, hjust=1)) + scale_y_continuous(limits = c(0,1020)) + geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black")
    )
  })
  
  output$zipAL <- renderPlotly({
    plot_ly(
      labels = labels_with_freq,
      values = AL_zipcodes,
      type = "pie",
      textinfo = "percent",
      textposition = "inside",
      hole = 0.6,  # Adjust hole size for a donut chart
      hoverinfo = "label+percent"
    ) %>%
      layout(title = "NFIP Claims in AL Zip Codes")
  })
  
  output$zipAnalysis <- renderUI({
    HTML(paste("<h4>Alabama clearly has the most NFIP claims. These are the most frequent zip codes:</h4><div><h4><li>36542- Baldwin County</li><li>36561- Baldwin County</li><li>36528- Dauphin Island</li></h4><div><h4>Baldwin County is on the Gulf Coast and borders the Gulf of Mexico. This explains the number of claims especially in 2020, when Hurricane Sally hit the Gulf Coast.</h4><br>"))
  })
  
  output$floodZone <- renderPlotly({
    ggplotly(
      ggplot(claims, aes(x = floodZoneCurrent)) +
        geom_bar(fill = "brown", color = "white", alpha = 0.7) + 
        labs(title = "Types of Flood Zones assoicated with NFIP claims ",
             x = "Flood Zone",
             y = "Frequency") + scale_y_continuous(limits = c(0,610)) + geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, colour = "black")
    )
  })
  
  output$zoneAnalysis <- renderUI({
    HTML(paste("<h4>Zone A, Zones A1-A30, Zone AE, Zone AO, Zone V, Zone VE, and Zones V1-V30 are Special Flood Hazard Areas (SFHAs). These are defined as areas that will likely be inundated by the flood event having a 1-percent chance of being equaled or exceeded in any given year.</h4><h4>Zone X is a moderate flood hazard area which is between the limits of the base flood and the 0.2-percent-annual-chance flood.</h4><br>"))
  })
  
  output$sfhaPie <- renderPlotly({
    plot_ly(
      labels = labels_with_freq_SFHA,
      values = states_SFHA,
      type = "pie",
      textinfo = "percent",
      textposition = "inside",
      hole = 0.6,  # Adjust hole size for a donut chart
      hoverinfo = "label+percent"
    ) %>%
      layout(title = "States with Special Flood Hazard Areas (SFHA)")
  })
  
  output$mfhaText <- renderUI({
    HTML(paste("<h4>There is another type of flood zone called Moderate Flood Zone Area (MFHA). The only state with MFHAs is Alabama. With such a high concentration of SFHAs, it makes sense for there to be MFHAs nearby.<br></h4>"))
  })
  
  
  
  #error in here
  output$causeAL <- renderPlotly({
    ggplotly(
      ggplot(AL_damage, aes(x = factor(causeOfDamage, labels = cause_labels))) +
        geom_bar(fill = "beige", color = "white", alpha = 0.7) + 
        labs(title = "Common Causes of Flood Damage",
             x = "Cause of Flood Damage",
             y = "Frequency") + 
        scale_y_continuous(limits = c(0, 400)) + 
        geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, color = "black") +
        theme(axis.text.x = element_text(angle = 15, hjust = 1))
    )
  })
  
  output$floodProofing <- renderPlotly({
    ggplotly(
      ggplot(claims, aes(x = state, fill = floodproofedIndicator)) +
        geom_bar(fill = "cyan", color = "white", alpha = 0.7) + 
        labs(title = "Floodproofing Status of Building by State",
             x = "Floodproof Indicator",
             y = "Frequency") + scale_y_continuous(limits = c(0,1100)) + geom_text(aes(label = ..count..), stat = "count", vjust = -0.5, color = "black")
    )
  })
  
  output$floodProofText <- renderUI({
    HTML(paste("<h4>Regions that have sufficient flood proofing and regions that do not experience many floods would not have to file many NFIP claims. It makes sense that Alabama, despite its significant floodproofing, is making the most claims. If there continues to be flood damage in Alabama with its flood proofing, Alabama will continue to claim its NFIP insurance.<br></h4>"))
  })
  
  output$conclusions <- renderUI({
    HTML(paste("<br><h2>Conclusions</h2><h5><li>Flood proofing in Alabama is insufficient. To better prevent future damages and decrease the frequency of NFIP claims in Alabama, looking at the best flood proofing in other coastal states like Florida would be effective.</li><br><li>NFIP claims are likely tied to damages that occur despite flood proofing. Claiming insurance is the best course of action when damage mitigation has already failed, so improving mitigation, which is likely improving the effectiveness of flood proofing, may impact insurance rates, which can change the frequency of claims.</li></h5><br><br>"))
  })
  
  output$space <- renderUI({
    HTML(paste("<br>"))
  })
  
})