## 01_calculate_diversity
## cm last amended 08/12/21

## harmonise column names -------------------------
b_biomass_2019 <- b_biomass_2019[, which(names(b_biomass_2019) %in% c("site", "block", "plot", "subplot", "taxa", "mass", "mass.g.m2..mass.x.5.", "date")),]
b_biomass_2020 <- b_biomass_2020[, which(names(b_biomass_2020) %in% c("site", "block", "plot", "subplot", "taxa", "mass", "mass.g.m2..mass.x.5.", "date")),]
b_biomass_2019$date <- 2019
b_biomass_2020$date <- 2020
b_cover_2019 <- b_cover_2019[, which(names(b_cover_2019) %in% c("site", "block", "plot", "subplot", "taxa", "cover", "Date")),]
b_cover_2020 <- b_cover_2020[, which(names(b_cover_2020) %in% c("site", "block", "plot", "subplot", "taxa", "cover", "Date")),]
b_cover_2019$Date <- 2019
b_cover_2020$Date <- 2020

## harmonise biomass data  -------------------------
b_biomass2 <- rbind(b_biomass_2019, b_biomass_2020)

b_biomass2$year <- b_biomass2$date
b_biomass2 <- 
  b_biomass2[, which(names(b_biomass2) %in% 
                       c("site_code", "block", "plot", "subplot", "mass.g.m2..mass.x.5.", "year"))]

names(b_biomass2)[names(b_biomass2) == 'site'] <- 'site_code'
names(b_biomass2)[names(b_biomass2) == 'mass.g.m2..mass.x.5.'] <- 'mass'
b_biomass2$site_code <- "burren.ie"

b_biomass <- 
  b_biomass[, which(names(b_biomass) %in% c("site_code", "block", "plot", "subplot",  
                                            "mass", "year"))]

b_biomass <- rbind(b_biomass,b_biomass2)
rm(b_biomass2)


## add biomass per block per plot per year -------------------------

b_biomass$mass_trt <- 0

mass_trt <- c()

for (l in unique(b_biomass$year)){
  
  for (i in unique(b_biomass$block#[b_biomass$year_trt == l]
  )){
    for (j in unique(b_biomass$plot#[b_biomass$block == i]
    )){
        b_biomass$mass_trt[b_biomass$year == l & b_biomass$block == i & b_biomass$plot == j] <- append(mass_trt, sum(b_biomass$mass[b_biomass$year == l &  
                                                                                                                                          b_biomass$block == i & 
                                                                                                                                          b_biomass$plot == j 
        ]))
      }
    }
  }

b_biomass <- b_biomass[, which(names(b_biomass) != "mass")]
b_biomass <- unique(b_biomass)
## get plot and treatment match
b_biomass <-merge(b_biomass, unique(trt_match[, which(names(trt_match) %in% c("trt", "plot"))]), by = "plot", all.x = TRUE)


## harmonise cover data  -------------------------
## harmonise plot naming system (individual plot numbers rather than 1:10 within each Block) -------------------------
b_cover_2019$taxa <- tolower(b_cover_2019$taxa)
b_cover_2020$taxa <- tolower(b_cover_2020$taxa)

x <- as.data.frame(11:20)
x2 <- as.data.frame(21:30)
for(i in 1:10){
  b_cover_2020$plot[b_cover_2020$block == 2 & b_cover_2020$plot == i] <- x[i,]
  b_cover_2020$plot[b_cover_2020$block == 3 & b_cover_2020$plot == i] <- x2[i,]
}

names(b_cover)[names(b_cover) == "Taxon"] <- "taxa"
b_cover$taxa <- tolower(b_cover$taxa)

b_cover2 <- rbind(b_cover_2019, b_cover_2020)
b_cover2$taxa <- tolower(b_cover2$taxa)
b_cover2$year <- b_cover2$Date
names(b_cover2)[names(b_cover2) == 'site'] <- 'site_code'
names(b_cover2)[names(b_cover2) == "cover"] <- "max_cover"
b_cover2$site_code <- "burren.ie"

b_cover2 <- b_cover2[, which(names(b_cover2) %in% c("year", "site_code", "block", "plot", "taxa", 
                                                    "max_cover"))]

b_cover <- b_cover[, which(names(b_cover) %in% c("year", "site_code", "block", "plot", "max_cover",
                                                 "taxa"))]

 
b_cover <- rbind(b_cover, b_cover2)
rm(b_cover2)

## get rid of random entries that are definitely not Burren endemics
ground <- c("ground", "other",  "bare ground", "other disturbed soil", "other rock", "ground rock")
b_cover$taxa <- gsub(paste(ground, collapse="|"),"ground", b_cover$taxa)

litter <- c("litter", "other litter", "ground animal droppings", "other animal droppings", "ground litter")
b_cover$taxa <- gsub(paste(litter, collapse="|"),"litter", b_cover$taxa)

b_cover$taxa <- gsub("bryophytes", "bryophyte", b_cover$taxa) 

random <- c("ground","litter","bryophyte")

b_cover$taxa <- gsub("species", "sp.", b_cover$taxa) 
b_cover$taxa <- gsub("  ", " ", b_cover$taxa) 
b_cover <- as.data.frame(unique(b_cover))

## get plot and treatment match
b_cover <-merge(b_cover, unique(trt_match[, which(names(trt_match) %in% c("trt", "plot"))]), by = "plot", all.x = TRUE)
b_cover$taxa <- tolower(b_cover$taxa)
## save all taxa in cover for dissimilarity analysis later
saveRDS(b_cover, "b_cover_taxa.rds")

## get total species list -------------------------
all_spp <- as.data.frame(unique(b_cover$taxa))
names(all_spp) <- "spp"


## Number of species per treatment per year -------------------------
## add length of unique species per plot per block per year
b_cover$species_trt <- 0

species_trt <-c()
for (l in unique(b_cover$year)){
  for (i in unique(b_cover$block#[b_cover$year_trt == l]
  )){
    for (j in unique(b_cover$plot#[b_cover$block == i]
    )){
        b_cover$species_trt[b_cover$year == l & b_cover$block == i & b_cover$plot == j] <- append(species_trt, 
                                                                                                      length(unique(b_cover$taxa[b_cover$year == l & 
                                                                                                                                    b_cover$block == i & 
                                                                                                                                    b_cover$plot == j 
                                                                                                      ]))) 
        }
    }
  }



b_cover <- unique(b_cover[, which(names(b_cover) %nin% c("max_cover", "taxa"))])


## Calculate 2019 and 2020 diversity -------------------------
#use ashley asumus's nutnet code to recalculate the diversity stats for the Burren 2019 and 2020 



# make a separate data frame to hold identifiers and summary variables -------------------------
b_diversity_2019 <- data.frame(site_code=b_cover_2019$site, year = 2019,
                               block=b_cover_2019$block,
                               plot=b_cover_2019$plot)

b_diversity_2020 <- data.frame(site_code=b_cover_2020$site, year = 2020,
                               block=b_cover_2020$block,
                               plot=b_cover_2020$plot)

## clean 2019 and 2020 names
b_cover_2019$taxa <- gsub("species", "sp.", b_cover_2019$taxa) 
b_cover_2019$taxa <- gsub("  ", " ", b_cover_2019$taxa) 
b_cover_2019 <- as.data.frame(unique(b_cover_2019))

b_cover_2020$taxa <- gsub("species", "sp.", b_cover_2020$taxa) 
b_cover_2020$taxa <- gsub("  ", " ", b_cover_2020$taxa) 
b_cover_2020 <- as.data.frame(unique(b_cover_2020))

## get species richness -------------------------
richness_vegan  <-c()
for (i in unique(b_cover_2019$block)){
  for (j in unique(b_cover_2019$plot)){
    b_diversity_2019$richness_vegan[b_diversity_2019$block == i & 
                                      b_diversity_2019$plot == j] <- append(richness_vegan, length(unique(b_cover_2019$taxa[b_cover_2019$block == i & 
                                                                                                                              b_cover_2019$plot == j])))
  }
}
richness_vegan  <-c()
for (i in unique(b_cover_2020$block)){
  for (j in unique(b_cover_2020$plot)){
    b_diversity_2020$richness_vegan[b_diversity_2020$block == i & 
                                      b_diversity_2020$plot == j] <- append(richness_vegan, length(unique(b_cover_2020$taxa[b_cover_2020$block == i & 
                                                                                                                              b_cover_2020$plot == j])))
  }
}

## get shannon diversity -------------------------
# shannon diversity is sum(p[i]*ln(p[i])) where p[i] is proportion of total abundance
shannon <- c()

for(i in b_cover_2019$block){
  for(j in b_cover_2019$plot){
    b_diversity_2019$shannon[b_diversity_2019$block == i & 
                               b_diversity_2019$plot == j] <- append(shannon, vegan::diversity(b_cover_2019$cover[b_cover_2019$block == i & b_cover_2019$plot ==j]))
  }
}

shannon <- c()

for(i in b_cover_2020$block){
  for(j in b_cover_2020$plot){
    b_diversity_2020$shannon[b_diversity_2020$block == i & 
                               b_diversity_2020$plot == j] <- append(shannon, vegan::diversity(b_cover_2020$cover[b_cover_2020$block == i & b_cover_2020$plot ==j]))
  }
}

## get inverse simpsons diversity -------------------------
invsimpson <- c()

for(i in b_cover_2019$block){
  for(j in b_cover_2019$plot){
    b_diversity_2019$inverse_simpson[b_diversity_2019$block == i & 
                                       b_diversity_2019$plot == j] <- append(invsimpson, vegan::diversity(b_cover_2019$cover[b_cover_2019$block == i & b_cover_2019$plot ==j],index='invsimpson'))
  }
}

invsimpson <- c()

for(i in b_cover_2020$block){
  for(j in b_cover_2020$plot){
    b_diversity_2020$inverse_simpson[b_diversity_2020$block == i & 
                                       b_diversity_2020$plot == j] <- append(invsimpson, vegan::diversity(b_cover_2020$cover[b_cover_2020$block == i & b_cover_2020$plot ==j],index='invsimpson'))
  }
}

## get evenness -------------------------
# Evenness metric is shannon diversity / log(S)
b_diversity_2019$evenness <- b_diversity_2019$shannon/log(b_diversity_2019$richness_vegan)
b_diversity_2020$evenness <- b_diversity_2020$shannon/log(b_diversity_2020$richness_vegan)

## create diversity data frame with all years of data  -------------------------
b_diversity <- rbind(b_diversity, unique(b_diversity_2019), unique(b_diversity_2020))

## add treatments to diversity dataframe -------------------------
## get plot and treatment match
b_diversity <-merge(b_diversity, unique(trt_match[, which(names(trt_match) %in% c("trt", "plot"))]), by = "plot", all.x = TRUE)
b_diversity <- unique(b_diversity)

rm(x,x2,trt_match,enviro,cover,biomass,b_enviro,b_diversity_2019, b_diversity_2020,b_cover_2019,b_cover_2020,b_biomass_2019, b_biomass_2020)
## end -------------------------









