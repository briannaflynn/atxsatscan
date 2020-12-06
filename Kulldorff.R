library(SpatialEpi)
library(leaflet)
##### Kulldorff Test ----------------------------------------
kulldorff_test <-
  function(geo, cases, population, expected.cases=NULL, pop.upper.bound, 
           n.simulations, alpha.level, plot=TRUE){
    
    # Determine likelihood type: binomial or poisson
    if(is.null(expected.cases)){
      type <- "binomial"
      denominator <- population
      expected.cases <- sum(cases) * (denominator/sum(denominator))
      # poisson case	
    }else{
      type <- "poisson"
      denominator <- expected.cases
    }
    
    # Get geographic information
    geo.results <- zones(geo, population, pop.upper.bound)
    nearest.neighbors <- geo.results$nearest.neighbors
    cluster.coords <- geo.results$cluster.coords
    n.zones <- nrow(cluster.coords)
    
    lkhd <- computeAllLogLkhd(cases, denominator, nearest.neighbors, n.zones, type)
    
    # Get areas included in most likely cluster
    cluster.index <- which.max(lkhd)
    
    # cluster center and radial area
    center <- cluster.coords[cluster.index,1]
    end <- cluster.coords[cluster.index,2]
    
    # list of all areas included in cluster	
    cluster <- nearest.neighbors[[center]]
    cluster <- cluster[1:which(cluster == end)]
    
    #-------------------------------------------------------------------------------
    # Compute Monte Carlo randomized p-value
    #-------------------------------------------------------------------------------
    # Simulate cases under null hypothesis of no area effects i.e. conditioned on E
    perm <- rmultinom(n.simulations, round(sum(cases)), prob=denominator)
    
    # Compute simulated lambda's:  max log-lkhd in region
    sim.lambda <- kulldorffMC(perm, denominator, nearest.neighbors, n.zones, type)
    
    # Compute Monte Carlo p-value
    combined.lambda <- c(sim.lambda, max(lkhd))
    p.value <- 1-mean(combined.lambda < max(lkhd))
    
    # Plot histogram
    if(plot){
      hist(combined.lambda, 
           main="Monte Carlo Distribution of Lambda",
           xlab=expression(log(lambda)))
      abline(v=max(lkhd), col="red")
      legend("top",
             c(paste("Obs. log(Lambda) = ",round(max(lkhd),3),sep=""), 
               paste("p-value = ", round(p.value,log10(n.simulations + 1)),sep="")),
             lty=c(1, 1), 
             col=c("red","white"), 
             bty="n"
      )
    }
    
    
    most.likely.cluster = list(
      location.IDs.included = cluster,
      population = sum(population[cluster]),	
      number.of.cases = sum(cases[cluster]),
      expected.cases = sum(expected.cases[cluster]),
      SMR = sum(cases[cluster])/sum(expected.cases[cluster]),
      log.likelihood.ratio = lkhd[cluster.index],
      monte.carlo.rank = sum(combined.lambda >= lkhd[cluster.index]),
      p.value = p.value
    )
    
    results <- list(
      most.likely.cluster=most.likely.cluster,
      
      type = type,
      log.lkhd=lkhd,
      simulated.log.lkhd=sim.lambda
    )
    return(results)
  }



#### Income Level (southern cluster)-----------------------------------------------------------
## Set Parameters
pop.upper.bound <- 0.10
n.simulations <- 999
alpha.level <- 0.05
plot <- TRUE

nhgis <- read_csv("/Users/brie/Desktop/Mol290C Project/FilteredEvenMore_NHGIS.csv")

income_nhgis <- nhgis %>% pivot_longer(c(`With cash public assistance or Food Stamps/SNAP`, `No cash public assistance or Food Stamps/SNAP`), names_to = "income_type", values_to = "population")





coords <- latlong2grid(centers)
c <- income_stats$Cases
p <- income_nhgis$population
E <- income_stats$E

E

E[is.na(E)] <- 1
E
poisson_income <- kulldorff_test(coords, c, p, E, pop.upper.bound, 
                                 n.simulations, alpha.level, plot)

clus_income <- poisson_income$most.likely.cluster$location.IDs.included
clus_income 
plot(austinMap)
for (val in clus_income)
{
  plot(austinMap[val, ], add=TRUE, col="red")
}
## P-Value = 0.001

P_values <- tibble(
  income = 0.001
)



#### Race (northern cluster)------------
pop.upper.bound <- 0.10
n.simulations <- 999
alpha.level <- 0.05
plot <- TRUE

nhgis <- read_csv("/Users/brie/Desktop/Mol290C Project/FilteredEvenMore_NHGIS.csv")
race_nhgis <- nhgis %>% pivot_longer(c(White, `Black or African American`, `Asian alone`, `Hispanic or Latino`), names_to = "race", values_to = "population")
coords <- latlong2grid(centers)
c <- race_stats$Cases
p <- race_nhgis$population
E <- race_stats$E

E
poisson_race <- kulldorff_test(coords, c, p, E, pop.upper.bound, 
                               n.simulations, alpha.level, plot)

clus_race <- poisson_race$most.likely.cluster$location.IDs.included
clus_race 
plot(austinMap)
for (val in clus_race)
{
  plot(austinMap[val, ], add=TRUE, col="red")
}
## P-Value = 0.001

P_values <- P_values %>% add_column(
  race = 0.001
)



#### Education (northern cluster) ------------
pop.upper.bound <- 0.10
n.simulations <- 999
alpha.level <- 0.05
plot <- TRUE
nhgis <- read_csv("/Users/brie/Desktop/Mol290C Project/FilteredEvenMore_NHGIS.csv")
edu_nhgis <- nhgis %>% pivot_longer(c(`Regular high school diploma`, `Bachelor's degree`, `Doctorate degree`), names_to = "edu_type", values_to = "population")

coords <- latlong2grid(centers)
c <- education_stats$Cases
p <- edu_nhgis$population
E <- education_stats$E

E
poisson_education <- kulldorff_test(coords, c, p, E, pop.upper.bound, 
                                    n.simulations, alpha.level, plot)

clus_education <- poisson_education$most.likely.cluster$location.IDs.included
clus_education 
plot(austinMap)
for (val in clus_education)
{
  plot(austinMap[val, ], add=TRUE, col="red", alpha=125)
}
## P-Value = 0.001

P_values <- P_values %>% add_column(
  education = 0.001
)


#### Transit (southern cluster) ---------------------

pop.upper.bound <- 0.10
n.simulations <- 999
alpha.level <- 0.05
plot <- TRUE

nhgis <- read_csv("/Users/brie/Desktop/Mol290C Project/FilteredEvenMore_NHGIS.csv")
transit_nhgis <- nhgis %>% pivot_longer(c(`Car, truck, or van`, `Public transportation (excluding taxicab)`, `Worked at home`), names_to = "transit_type", values_to = "population")

coords <- latlong2grid(centers)
c <- transit_stats$Cases
p <- transit_nhgis$population
E <- transit_stats$E

poisson_transit <- kulldorff_test(coords, c, p, E, pop.upper.bound, 
                                  n.simulations, alpha.level, plot)

clus_transit <- poisson_transit$most.likely.cluster$location.IDs.included
clus_transit
plot(austinMap)
for (val in clus_transit)
{
  plot(austinMap[val, ], add=TRUE, col="red", alpha=125)
}
## P-Value = 0.001

P_values <- P_values %>% add_column(
  transit = 0.001
)


#### Insurance status (northern Cluster) -------------
pop.upper.bound <- 0.10
n.simulations <- 999
alpha.level <- 0.05
plot <- TRUE

nhgis <- read_csv("/Users/brie/Desktop/Mol290C Project/FilteredEvenMore_NHGIS.csv")
insur_nhgis <- nhgis %>% pivot_longer(c(`Total With Health Insurance`, `Total Without Health Insurance`), names_to = "insur_type", values_to = "population")

coords <- latlong2grid(centers)
c <- insur_stats$Cases
p <- insur_nhgis$population
E <- insur_stats$E

poisson_insur <- kulldorff_test(coords, c, p, E, pop.upper.bound, 
                                n.simulations, alpha.level, plot)

clus_insur <- poisson_insur$most.likely.cluster$location.IDs.included
clus_insur
plot(austinMap)
for (val in clus_insur)
{
  plot(austinMap[val, ], add=TRUE, col="red")
}
## P-Value = 0.001

P_values <- P_values %>% add_column(
  insurance = 0.001
)



#### Kulldorff Plot -------------------------
## These share the same cluster
nhgis <- read_csv("/Users/brie/Desktop/Mol290C Project/FilteredEvenMore_NHGIS.csv")

for (val in clus_race)
{
  print(austinMap.df[val, ]$ZCTA5CE10)
}

for (val in clus_education)
{
  print(austinMap.df[val, ]$ZCTA5CE10)
}

for (val in clus_insur)
{
  print(austinMap.df[val, ]$ZCTA5CE10)
}

## These do not share the same cluster
for (val in clus_transit)
{
  print(austinMap.df[val, ]$ZCTA5CE10)
}
for (val in clus_income)
{
  print(austinMap.df[val, ]$ZCTA5CE10)
}

# for every strata that has cluster at zip code, +1
# else, 0
# Kulldorff column is 0, or number of strata that cluster at a particular zipcode (range from 1 to 4)

austinMap$cases <- g$Cases

austinMap$Kulldorff <- nhgis$Kulldorff

austinMap@data

pal <- colorNumeric(palette = "YlOrRd", domain = austinMap$SMR)
labels <- sprintf("<strong>%s</strong><br/>Observed: %s <br/>Number of Contextual Factors: %s ",
                  austinMap$ZCTA5CE10, austinMap$cases, austinMap$Kulldorff) %>%
  lapply(htmltools::HTML)

map_kulldorff <- leaflet(austinMap, width = 900, height = 400) %>% addTiles() %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(Kulldorff), fillOpacity = 0.5) %>%
  addLegend(pal = pal, values = ~Kulldorff, opacity = 0.5, title = "Kulldorff Clusters", position = "bottomright") %>% addPolygons(color = "grey", weight = 1, fillColor = ~pal(Kulldorff), fillOpacity = 0.5,
                                                                                                                                   highlightOptions = highlightOptions(weight = 4),
                                                                                                                                   label = labels,
                                                                                                                                   labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),
                                                                                                                                                               textsize = "15px", direction = "auto"))
map_kulldorff
