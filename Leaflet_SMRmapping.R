library(SpatialEpi)
library(leaflet)
library(sp)

# You MUST run MapMaking.R first in order to run this and produce the map - MapMaking.R creates the polygon from which Leaflet builds the Austin map

# Race Data ------------

test_nhgis <- read_csv("/Users/brie/Desktop/Mol290C Project/FilteredEvenMore_NHGIS.csv")
test_nhgis

test_nhgis <- test_nhgis %>% pivot_longer(c(White, `Black or African American`, `Asian alone`, `Hispanic or Latino`), names_to = "race", values_to = "population")
test_nhgis

test_nhgis <- test_nhgis[order(test_nhgis$Zipcode, test_nhgis$population, test_nhgis$race), ]
test_nhgis

# calculatign expected using SpatialEpi library
E <- expected(population = test_nhgis$population, cases = test_nhgis$Cases, n.strata = 4)
E

# Cases by Zip Code
zipCases <- read_csv("/Users/brie/Desktop/Mol290C Project/zipCases.csv")
zipCases

g <- zipCases
g$E <- E
g
g$SMR <- g$Cases/g$E
g

race_stats <- g

austinMap$cases <- g$Cases
austinMap$E <- g$E
austinMap$SMR <- g$SMR

austinMap@data


l <- leaflet(austinMap) %>% addTiles()

pal <- colorNumeric(palette = "YlOrRd", domain = austinMap$SMR)

l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")

labels <- sprintf("<strong>%s</strong><br/>Observed: %s <br/>Expected: %s <br/>SMR: %s",
                  austinMap$ZCTA5CE10, austinMap$cases,  round(austinMap$E, 2), round(austinMap$SMR, 2)) %>%
  lapply(htmltools::HTML)

l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5,
                  highlightOptions = highlightOptions(weight = 4),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px", direction = "auto")) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")


# SMR = 1 which indicates observed cases are the same as expected,
# SMR > 1 which indicates observed cases are higher than expected,
# SMR < 1 which indicates observed cases are lower than expected.




# Transit Data ----------------

nhgis <- read_csv("/Users/brie/Desktop/Mol290C Project/FilteredEvenMore_NHGIS.csv")
cols <- colnames(nhgis)
cols
test_nhgis <- nhgis

test_nhgis <- test_nhgis %>% pivot_longer(c(`Car, truck, or van`, `Public transportation (excluding taxicab)`, `Worked at home`), names_to = "transit_type", values_to = "population")
test_nhgis

test_nhgis <- test_nhgis[order(test_nhgis$Zipcode, test_nhgis$population, test_nhgis$transit_type), ]
test_nhgis

# calculatign expected using SpatialEpi library
E <- expected(population = test_nhgis$population, cases = test_nhgis$Cases, n.strata = 3)
E

# Cases by Zip Code
zipCases <- read_csv("/Users/brie/Desktop/Mol290C Project/zipCases.csv")
zipCases

g <- zipCases
g$E <- E
g
g$SMR <- g$Cases/g$E
g

transit_stats <- g

austinMap$cases <- g$Cases
austinMap$E <- g$E
austinMap$SMR <- g$SMR

austinMap@data


l <- leaflet(austinMap) %>% addTiles()

pal <- colorNumeric(palette = "YlOrRd", domain = austinMap$SMR)

l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")

labels <- sprintf("<strong>%s</strong><br/>Observed: %s <br/>Expected: %s <br/>SMR: %s",
                  austinMap$ZCTA5CE10, austinMap$cases,  round(austinMap$E, 2), round(austinMap$SMR, 2)) %>%
  lapply(htmltools::HTML)

l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5,
                  highlightOptions = highlightOptions(weight = 4),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px", direction = "auto")) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")


# SMR = 1 which indicates observed cases are the same as expected,
# SMR > 1 which indicates observed cases are higher than expected,
# SMR < 1 which indicates observed cases are lower than expected.



# Educational Attainment Data ---------------------
nhgis <- read_csv("/Users/brie/Desktop/Mol290C Project/FilteredEvenMore_NHGIS.csv")
cols <- colnames(nhgis)
cols
test_nhgis <- nhgis

test_nhgis <- test_nhgis %>% pivot_longer(c(`Regular high school diploma`, `Bachelor's degree`, `Doctorate degree`), names_to = "edu_type", values_to = "population")
test_nhgis

test_nhgis <- test_nhgis[order(test_nhgis$Zipcode, test_nhgis$population, test_nhgis$edu_type), ]
test_nhgis

# calculatign expected using SpatialEpi library
E <- expected(population = test_nhgis$population, cases = test_nhgis$Cases, n.strata = 3)
E

# Cases by Zip Code
zipCases <- read_csv("/Users/brie/Desktop/Mol290C Project/zipCases.csv")
zipCases

g <- zipCases
g$E <- E
g
g$SMR <- g$Cases/g$E
g

education_stats <- g

austinMap$cases <- g$Cases
austinMap$E <- g$E
austinMap$SMR <- g$SMR

austinMap@data


l <- leaflet(austinMap) %>% addTiles()

pal <- colorNumeric(palette = "YlOrRd", domain = austinMap$SMR)

l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")

labels <- sprintf("<strong>%s</strong><br/>Observed: %s <br/>Expected: %s <br/>SMR: %s",
                  austinMap$ZCTA5CE10, austinMap$cases,  round(austinMap$E, 2), round(austinMap$SMR, 2)) %>%
  lapply(htmltools::HTML)

l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5,
                  highlightOptions = highlightOptions(weight = 4),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px", direction = "auto")) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")


# SMR = 1 which indicates observed cases are the same as expected,
# SMR > 1 which indicates observed cases are higher than expected,
# SMR < 1 which indicates observed cases are lower than expected.



# Health Insurance Data -------------------------
nhgis <- read_csv("/Users/brie/Desktop/Mol290C Project/FilteredEvenMore_NHGIS.csv")
cols <- colnames(nhgis)
cols
test_nhgis <- nhgis

test_nhgis <- test_nhgis %>% pivot_longer(c(`Total With Health Insurance`, `Total Without Health Insurance`), names_to = "insur_type", values_to = "population")
test_nhgis

test_nhgis <- test_nhgis[order(test_nhgis$Zipcode, test_nhgis$population, test_nhgis$insur_type), ]
test_nhgis

# calculatign expected using SpatialEpi library
E <- expected(population = test_nhgis$population, cases = test_nhgis$Cases, n.strata = 2)
E

# Cases by Zip Code
zipCases <- read_csv("/Users/brie/Desktop/Mol290C Project/zipCases.csv")
zipCases

g <- zipCases
g$E <- E
g
g$SMR <- g$Cases/g$E
g

insur_stats <- g

austinMap$cases <- g$Cases
austinMap$E <- g$E
austinMap$SMR <- g$SMR

austinMap@data


l <- leaflet(austinMap) %>% addTiles()

pal <- colorNumeric(palette = "YlOrRd", domain = austinMap$SMR)

l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")

labels <- sprintf("<strong>%s</strong><br/>Observed: %s <br/>Expected: %s <br/>SMR: %s",
                  austinMap$ZCTA5CE10, austinMap$cases,  round(austinMap$E, 2), round(austinMap$SMR, 2)) %>%
  lapply(htmltools::HTML)

l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5,
                  highlightOptions = highlightOptions(weight = 4),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px", direction = "auto")) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")


# SMR = 1 which indicates observed cases are the same as expected,
# SMR > 1 which indicates observed cases are higher than expected,
# SMR < 1 which indicates observed cases are lower than expected.









#TODO !!!!!!!!!
# Income Data ------------------------

nhgis <- read_csv("/Users/brie/Desktop/Mol290C Project/FilteredEvenMore_NHGIS.csv")
cols <- colnames(nhgis)
cols
test_nhgis <- nhgis

#test_nhgis <- test_nhgis %>% pivot_longer(c(Q1, Q2, Q3, Q4), names_to = "income_quartiles", values_to = "population")


test_nhgis <- test_nhgis %>% pivot_longer(c(`With cash public assistance or Food Stamps/SNAP`, `No cash public assistance or Food Stamps/SNAP`), names_to = "income_type", values_to = "population")
test_nhgis

test_nhgis <- test_nhgis[order(test_nhgis$Zipcode, test_nhgis$population, test_nhgis$income_type), ]
test_nhgis

# calculating expected using SpatialEpi library
E <- expected(population = test_nhgis$population, cases = test_nhgis$Cases, n.strata = 2)
E

# Cases by Zip Code
zipCases <- read_csv("/Users/brie/Desktop/Mol290C Project/zipCases.csv")
zipCases

g <- zipCases
g$E <- E
g$E[g$E==0] <- NA
#g<-g[!(g$E==0),]
g
g$SMR <- g$Cases/g$E
g


income_stats <- g

austinMap$cases <- g$Cases

austinMap$E <- g$E
austinMap$SMR <- g$SMR

austinMap@data


l <- leaflet(austinMap) %>% addTiles()

pal <- colorNumeric(palette = "YlOrRd", domain = austinMap$SMR)

l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")

labels <- sprintf("<strong>%s</strong><br/>Observed: %s <br/>Expected: %s <br/>SMR: %s",
                  austinMap$ZCTA5CE10, austinMap$cases,  round(austinMap$E, 2), round(austinMap$SMR, 2)) %>%
  lapply(htmltools::HTML)

l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(SMR), fillOpacity = 0.5,
                  highlightOptions = highlightOptions(weight = 4),
                  label = labels,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                              textsize = "15px", direction = "auto")) %>%
  addLegend(pal = pal, values = ~SMR, opacity = 0.5, title = "SMR", position = "bottomright")


# SMR = 1 which indicates observed cases are the same as expected,
# SMR > 1 which indicates observed cases are higher than expected,
# SMR < 1 which indicates observed cases are lower than expected.

 

# Softmax function to normalize the SMRs ----------------------

# raceSMR <- race_stats$SMR
# raceSMR <- exp(raceSMR)
# raceSMR
# sumRaceSMR <- sum(raceSMR)
# sumRaceSMR
# raceSoftmax <- raceSMR/sumRaceSMR
# sum(raceSoftmax)
# 
# eduSMR <- education_stats$SMR
# eduSMR <- exp(eduSMR)
# eduSMR
# sumeduSMR <- sum(eduSMR)
# sumeduSMR
# eduSoftmax <- eduSMR/sumeduSMR
# sum(eduSoftmax)
# 
# 

incomeSMR <- income_stats$SMR
incomeSMR
summary(incomeSMR)

incomeSMR
incomeSMR <- exp(incomeSMR)
sumincSMR <- sum(incomeSMR)
sumincSMR
incSoftmax <- incomeSMR/sumincSMR
sum(incSoftmax)
# 
# insurSMR <- insur_stats$SMR
# insureSMR <- exp(insurSMR) 
# suminsurSMR <- sum(insurSMR)
# suminsurSMR
# insurSoftmax <- insurSMR/suminsurSMR
# sum(insurSoftmax)
# 
# transitSMR <- transit_stats$SMR
# transitSMR <- exp(transitSMR) 
# sumtransSMR <- sum(transitSMR)
# sumtransSMR
# transitSoftmax <- transitSMR/sumtransSMR
# sum(transitSoftmax)
# 
# 
# allSMR <- raceSoftmax+eduSoftmax+incSoftmax+insurSoftmax+transitSoftmax
# 
# allSMR <- exp(allSMR)
# allSMR
# 
# sumSMR <- sum(allSMR)
# sumSMR
# 
# allSMR
# 
# softmax <- allSMR/sumSMR
# sum(softmax)
# 
# austinMap$softmax <- softmax
# 
# austinMap@data
# 
# softmax
# 
# l <- leaflet(austinMap) %>% addTiles()
# 
# pal <- colorNumeric(palette = "YlOrRd", domain = austinMap$softmax)
# 
# l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(softmax), fillOpacity = 0.5) %>%
#   addLegend(pal = pal, values = ~softmax, opacity = 0.5, title = "Softmax SMR", position = "bottomright")
# 
# labels <- sprintf("<strong>%s</strong><br/>",
#                   austinMap$ZCTA5CE10) %>%
#   lapply(htmltools::HTML)
# 
# l %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(softmax), fillOpacity = 0.5,
#                   highlightOptions = highlightOptions(weight = 4),
#                   label = labels,
#                   labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
#                                               textsize = "15px", direction = "auto")) %>%
#   addLegend(pal = pal, values = ~softmax, opacity = 0.5, title = "Softmax", position = "bottomright")
# 
# 
# # SMR = 1 which indicates observed cases are the same as expected,
# # SMR > 1 which indicates observed cases are higher than expected,
# # SMR < 1 which indicates observed cases are lower than expected.
# 
# 
