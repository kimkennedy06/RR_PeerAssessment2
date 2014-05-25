Title: Briefly Summarizes data analysis
========================================================

## Synopsis
-At most 10 sentences.  

## Data Retrieval
Data retrieval and store raw CSV

```r
# Download File and Save Date Downloaded
NOAA.data.file <- download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
    destfile = "StormData.csv.bz2", method = "curl")
dateDownloaded <- date()

# Unzip the bz2 archive
open.connection <- bzfile("StormData.csv.bz2", "r")
NOAA.data <- read.csv(open.connection)
close(open.connection)
unlink("StormData.csv.bz2")
```

## Data Processing
-Removing un-necessary columns for data analysis questions

```r
# Subsetting the Data Frame to only include columns relevant for analysis
keeps <- c("COUNTY", "COUNTYNAME", "STATE", "EVTYPE", "FATALITIES", "INJURIES", 
    "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP", "REMARKS")
NOAA.data.keeps <- NOAA.data[, keeps]
```

-Focus on only data with damages to population or economic health

```r
NOAA.data.with.damages <- subset(NOAA.data.keeps, NOAA.data.keeps$FATALITIES > 
    0 | NOAA.data.keeps$INJURIES > 0 | NOAA.data.keeps$PROPDMG > 0 | NOAA.data.keeps$CROPDMG > 
    0)
```

-General cleaning

```r
# Change all casing to upper
NOAA.data.with.damages$EVTYPE <- toupper(as.character(NOAA.data.with.damages$EVTYPE))
# Remove spaces from the front of EVTYPE
NOAA.data.with.damages$EVTYPE <- sub("^(\\s)+", "", NOAA.data.with.damages$EVTYPE)
# Remove extra spaces from EVTYPE
NOAA.data.with.damages$EVTYPE <- gsub("[[:space:]]+", " ", NOAA.data.with.damages$EVTYPE)
```

-Renaming data so that naming style is the same

```r
# Renaming various mispellings of THUNDERSTORM
NOAA.data.with.damages$EVTYPE <- sub("(TSTM)|(THUDERSTORM)|(TUNDERSTORM)|(THUNDERSTROM)|(THUNDERTORM)|(THUNERSTORM)|(THUNDEERSTORM)|(THUNDERESTORM)", 
    "THUNDERSTORM", NOAA.data.with.damages$EVTYPE)
# Renaming TORNDAO as TORNADO
NOAA.data.with.damages$EVTYPE <- sub("TORNDAO", "TORNADO", NOAA.data.with.damages$EVTYPE)
# Renaming Avalance to Avalanche
NOAA.data.with.damages$EVTYPE <- sub("AVALANCE", "AVALANCHE", NOAA.data.with.damages$EVTYPE)
# Renaming MIRCO to MICRO
NOAA.data.with.damages$EVTYPE <- sub("MIRCO", "MICRO", NOAA.data.with.damages$EVTYPE)
# Renaming CSTL to COASTAL
NOAA.data.with.damages$EVTYPE <- sub("CSTL", "COASTAL", NOAA.data.with.damages$EVTYPE)
# Renaming WINDCHILL to WIND CHILL
NOAA.data.with.damages$EVTYPE <- sub("WINDCHILL", "WINDCHILL", NOAA.data.with.damages$EVTYPE)
# Renaming various misordering of FLASH FLOOD
NOAA.data.with.damages$EVTYPE <- sub("(FLOOD FLASH)|(FLOOD\\/FLASH)", "FLASH FLOOD", 
    NOAA.data.with.damages$EVTYPE)
# Renaming FLD to FLOOD
NOAA.data.with.damages$EVTYPE <- sub("FLD", "FLOOD", NOAA.data.with.damages$EVTYPE)
# Renaming LIGHTING to LIGHTNING
NOAA.data.with.damages$EVTYPE <- sub("LIGHTING", "LIGHTNING", NOAA.data.with.damages$EVTYPE)
# Renaming FLOES to FLOWS
NOAA.data.with.damages$EVTYPE <- sub("FLOES", "FLOWS", NOAA.data.with.damages$EVTYPE)
# Renaming Non-Thunderstorm Wind to Wind
NOAA.data.with.damages$EVTYPE <- sub("(NON-THUNDERSTORM)|(NON THUNDERSTORM)", 
    "", NOAA.data.with.damages$EVTYPE)
```

-Create column to group events into many of the categories listed in FAQ

```r
# Create Subset for Tide or Storm Surge Storm Events
NOAA.data.tides <- NOAA.data.with.damages[grepl("(tide)|(surge)", NOAA.data.with.damages$EVTYPE, 
    ignore.case = TRUE), ]
NOAA.data.tides$General.Storm.Event <- "Storm Surge/Tide"

# Create Subset for Avalanche Storm Events
NOAA.data.avalanche <- NOAA.data.with.damages[grepl("avalanche", NOAA.data.with.damages$EVTYPE, 
    ignore.case = TRUE), ]
NOAA.data.avalanche$General.Storm.Event <- "Avalanche"

# Create Subset for Blizzard Storm Events
NOAA.data.blizzard <- NOAA.data.with.damages[grepl("blizzard", NOAA.data.with.damages$EVTYPE, 
    ignore.case = TRUE), ]
NOAA.data.blizzard$General.Storm.Event <- "Blizzard"

# Create Subset for Coastal Storm Events
NOAA.data.coastal <- NOAA.data.with.damages[grepl("(coastal)|(beach) ", NOAA.data.with.damages$EVTYPE, 
    ignore.case = TRUE), ]
NOAA.data.coastal$General.Storm.Event <- "Coastal"

# Create Subset for Cold or Wind Chill Storm Events
NOAA.data.cold.windchill <- NOAA.data.with.damages[grepl("(cold)|(wind chill)|(hypothermia)|(low temperature)", 
    NOAA.data.with.damages$EVTYPE, ignore.case = TRUE), ]
NOAA.data.cold.windchill$General.Storm.Event <- "Cold/Wind Chill"

# Create Subset for Debris Flow Storm Events
NOAA.data.debrisflow <- NOAA.data.with.damages[grepl("(slide)|(flows)|(slump)", 
    NOAA.data.with.damages$EVTYPE, ignore.case = TRUE), ]
NOAA.data.debrisflow$General.Storm.Event <- "Debris Flow"

# Create Subset for Dense Fog Storm Events
NOAA.data.densefogsmoke <- NOAA.data.with.damages[grepl("(fog)|(smoke)", NOAA.data.with.damages$EVTYPE, 
    ignore.case = TRUE), ]
NOAA.data.densefogsmoke$General.Storm.Event <- "Dense Fog or Freezing Fog or Dense Smoke"

# Create Subset for Drought Storm Events
NOAA.data.drought <- NOAA.data.with.damages[grepl("(drought)", NOAA.data.with.damages$EVTYPE, 
    ignore.case = TRUE), ]
NOAA.data.drought$General.Storm.Event <- "Drought"

# Create Subset for Dust Storm Events
NOAA.data.duststorm <- NOAA.data.with.damages[grepl("(dust devil)|(dust storm)|(dust)", 
    NOAA.data.with.damages$EVTYPE, ignore.case = TRUE), ]
NOAA.data.duststorm$General.Storm.Event <- "Dust Devil or Dust Storm"

# Create Subset for Heat Storm Events
NOAA.data.heat <- NOAA.data.with.damages[grepl("(heat)|(hyperthermia)|(warm)", 
    NOAA.data.with.damages$EVTYPE, ignore.case = TRUE), ]
NOAA.data.heat$General.Storm.Event <- "Heat"

# Create Subset for Flood Storm Events
NOAA.data.flood <- NOAA.data.with.damages[grepl("(flood)|(high water)|(rapidly rising water)", 
    NOAA.data.with.damages$EVTYPE, ignore.case = TRUE), ]
NOAA.data.flood$General.Storm.Event <- "Flash Flood or Flood"

# Create Subset for Frost/Freeze Storm Events
NOAA.data.frost <- NOAA.data.with.damages[grepl("(frost)|(freeze)", NOAA.data.with.damages$EVTYPE, 
    ignore.case = TRUE), ]
NOAA.data.frost$General.Storm.Event <- "Frost/Freeze"

# Create Subset for Funnel Cloud Storm Events
NOAA.data.funnel <- NOAA.data.with.damages[grepl("(funnel)", NOAA.data.with.damages$EVTYPE, 
    ignore.case = TRUE), ]
NOAA.data.funnel$General.Storm.Event <- "Funnel Cloud"

# Create Subset for Hail Storm Events
NOAA.data.hail <- NOAA.data.with.damages[grepl("(hail)", NOAA.data.with.damages$EVTYPE, 
    ignore.case = TRUE), ]
NOAA.data.hail$General.Storm.Event <- "Hail or Marine Hail"

# Create Subset for Thunderstorm Storm Events
NOAA.data.thunderstorm <- NOAA.data.with.damages[grepl("(thunderstorm)|(thundersnow)|(gustnado)|(apache)", 
    NOAA.data.with.damages$EVTYPE, ignore.case = TRUE), ]
NOAA.data.thunderstorm$General.Storm.Event <- "Thunderstorm Winds or Marine Thunderstorm Winds"

# Create Subset for Tornado Storm Events
NOAA.data.tornado <- NOAA.data.with.damages[grepl("(tornado)|(landspout)|(waterspout)", 
    NOAA.data.with.damages$EVTYPE, ignore.case = TRUE), ]
NOAA.data.tornado$General.Storm.Event <- "Tornado"

# Create Subset for Wind Storm Events
NOAA.data.wind <- NOAA.data.with.damages[grepl("(wind)|(microburst)|(downburst)|(whirlwind)|(turbulence)|(marine mishap)", 
    NOAA.data.with.damages$EVTYPE, ignore.case = TRUE), ]
NOAA.data.wind <- rbind(NOAA.data.wind, {
    subset(NOAA.data.with.damages, NOAA.data.with.damages$EVTYPE == "HIGH")
})
NOAA.data.wind$General.Storm.Event <- "Wind"

# Create Subset for Rain Storm Events
NOAA.data.rain <- NOAA.data.with.damages[grepl("(rain)|(shower)|(wet)", NOAA.data.with.damages$EVTYPE, 
    ignore.case = TRUE), ]
NOAA.data.rain$General.Storm.Event <- "Rain"

# Create Subset for Snow Storm Events
NOAA.data.snow <- NOAA.data.with.damages[grepl("(snow)", NOAA.data.with.damages$EVTYPE, 
    ignore.case = TRUE), ]
NOAA.data.snow$General.Storm.Event <- "Snow"

# Create Subset for Surf Storm Events
NOAA.data.surf <- NOAA.data.with.damages[grepl("(surf)|(swells)", NOAA.data.with.damages$EVTYPE, 
    ignore.case = TRUE), ]
NOAA.data.surf$General.Storm.Event <- "Surf"

# Create Subset for Hurricane(Typhoon) Storm Events
NOAA.data.hurricane <- NOAA.data.with.damages[grepl("(hurricane)|(typhoon)", 
    NOAA.data.with.damages$EVTYPE, ignore.case = TRUE), ]
NOAA.data.hurricane$General.Storm.Event <- "Hurricane(Typhoon)"

# Create Subset for Lightning Storm Events
NOAA.data.lightning <- NOAA.data.with.damages[grepl("(lightning)", NOAA.data.with.damages$EVTYPE, 
    ignore.case = TRUE), ]
NOAA.data.lightning$General.Storm.Event <- "Lightning"

# Create Subset for Ice Storm Events
NOAA.data.ice <- NOAA.data.with.damages[grepl("(ice)|(icy)|(glaze)", NOAA.data.with.damages$EVTYPE, 
    ignore.case = TRUE), ]
NOAA.data.ice$General.Storm.Event <- "Ice"

# Create Subset for Tropical Depression Storm Events
NOAA.data.tropicaldepression <- NOAA.data.with.damages[grepl("(tropical depression)", 
    NOAA.data.with.damages$EVTYPE, ignore.case = TRUE), ]
NOAA.data.tropicaldepression$General.Storm.Event <- "Tropical Depression"

# Create Subset for Tropical Storm Events
NOAA.data.tropicalstorm <- NOAA.data.with.damages[grepl("(tropical storm)", 
    NOAA.data.with.damages$EVTYPE, ignore.case = TRUE), ]
NOAA.data.tropicalstorm$General.Storm.Event <- "Tropical Storm"

# Create Subset for Winter Storm/Weather Storm Events
NOAA.data.winter <- NOAA.data.with.damages[grepl("(winter storm)|(winter weather)|(wintry))", 
    NOAA.data.with.damages$EVTYPE, ignore.case = TRUE), ]
NOAA.data.winter$General.Storm.Event <- "Winter Storm/Weather"

# Create Subset for Different Fire Storm Events
NOAA.data.fire <- NOAA.data.with.damages[grepl("(fire)", NOAA.data.with.damages$EVTYPE, 
    ignore.case = TRUE), ]
NOAA.data.fire$General.Storm.Event <- "Fire"

# Create Subset for Seiche Storm Events
NOAA.data.seiche <- NOAA.data.with.damages[grepl("(seiche)|(wave)|(high seas)|(heavy seas)|(rough seas)|(marine accident)", 
    NOAA.data.with.damages$EVTYPE, ignore.case = TRUE), ]
NOAA.data.seiche$General.Storm.Event <- "Seiche"

# Create Subset for Volcanic Ash Storm Events
NOAA.data.volcanicash <- NOAA.data.with.damages[grepl("(volcanic)", NOAA.data.with.damages$EVTYPE, 
    ignore.case = TRUE), ]
NOAA.data.volcanicash$General.Storm.Event <- "Volcanic Ash"

# Create Subset for Mix Precipitation Storm Events
NOAA.data.mixprecip <- NOAA.data.with.damages[grepl("(freezing drizzle)|(freezing spray)|(mix)|(precipitation)|(sleet)", 
    NOAA.data.with.damages$EVTYPE, ignore.case = TRUE), ]
NOAA.data.mixprecip$General.Storm.Event <- "Mixed Precipitation"

# Create Subset for RIP Current Storm Events
NOAA.data.ripcurrent <- NOAA.data.with.damages[grepl("(rip current)", NOAA.data.with.damages$EVTYPE, 
    ignore.case = TRUE), ]
NOAA.data.ripcurrent$General.Storm.Event <- "RIP Currents"

# Create Subset for Tsunami Storm Events
NOAA.data.tsunami <- NOAA.data.with.damages[grepl("(tsunami)", NOAA.data.with.damages$EVTYPE, 
    ignore.case = TRUE), ]
NOAA.data.tsunami$General.Storm.Event <- "Tsunami"

# Create Subset for Other Storm Events
NOAA.data.other <- NOAA.data.with.damages[grepl("(other)|(dam)|(drown)", NOAA.data.with.damages$EVTYPE, 
    ignore.case = TRUE), ]
NOAA.data.dust <- NOAA.data.other[grepl("dust", NOAA.data.other$REMARKS, ignore.case = TRUE), 
    ]
NOAA.data.dust$General.Storm.Event <- "Dust Devil or Dust Storm"
NOAA.data.rainother <- NOAA.data.other[grepl("rain", NOAA.data.other$REMARKS, 
    ignore.case = TRUE), ]
NOAA.data.rainother$General.Storm.Event <- "Rain"
NOAA.data.windother <- NOAA.data.other[grepl("wind", NOAA.data.other$REMARKS, 
    ignore.case = TRUE), ]
NOAA.data.windother$General.Storm.Event <- "Wind"
NOAA.data.other$General.Storm.Event <- "Other"

# Combine all the subsets into one overall dataframe
NOAA.data.categorized <- rbind(NOAA.data.tides, NOAA.data.avalanche, NOAA.data.blizzard, 
    NOAA.data.coastal, NOAA.data.cold.windchill, NOAA.data.debrisflow, NOAA.data.densefogsmoke, 
    NOAA.data.drought, NOAA.data.duststorm, NOAA.data.dust, NOAA.data.heat, 
    NOAA.data.flood, NOAA.data.frost, NOAA.data.funnel, NOAA.data.hail, NOAA.data.thunderstorm, 
    NOAA.data.tornado, NOAA.data.wind, NOAA.data.rain, NOAA.data.rainother, 
    NOAA.data.snow, NOAA.data.surf, NOAA.data.hurricane, NOAA.data.lightning, 
    NOAA.data.ice, NOAA.data.tropicaldepression, NOAA.data.tropicalstorm, NOAA.data.winter, 
    NOAA.data.fire, NOAA.data.seiche, NOAA.data.volcanicash, NOAA.data.mixprecip, 
    NOAA.data.ripcurrent, NOAA.data.tsunami, NOAA.data.windother)
```

 
## Results

```r
NOAA.data.summary <- ddply(NOAA.data.categorized, ~General.Storm.Event, summarize, 
    avgFatalities = mean(Fatalities, na.rm = TRUE), avgInjuries = mean(Injuries, 
        na.rm = TRUE))
```

```
## Error: could not find function "ddply"
```

```r
head(NOAA.data.summary)
```

```
## Error: object 'NOAA.data.summary' not found
```

                              
*No more than three figures
*Must contain at least one figure with a plot

## Questions that must be addressed
1. Across the United States, which types of events (as indicated in the `EVTYPE` variable) are most harmful with respect to population health?
1. Across the United States, which types of events have the greatest economic consequences?

Consider writing your report as if it were to be read by a government or municipal manager who might be responsible for preparing for severe weather events and will need to prioritize resources for different types of events. However, there is no need to make any specific recommendations in your report.

## Grading Rubric
1. Has either a (1) valid RPubs URL pointing to a data analysis document for this assignment been submitted; or (2) a complete PDF file presenting the data analysis been uploaded?
1. Is the document written in English?
1. Does the document have a title that briefly summarizes the data analysis?
1. Does the document have a synopsis that describes and summarizes the data analysis in less than 10 sentences?
1. Is there a section titled "Data Processing" that describes how the data were loaded into R and processed for analysis?
1. Is there a section titled "Results" where the main results are presented?
1. Is there at least one figure in the document that contains a plot?
1. Are there at most 3 figures in this document?
1. Does the analysis address the question of which types of events are most harmful to population health?
1. Does the analysis address the question of which types of events have the greatest economic consequences?
1. Do all the results of the analysis (i.e. figures, tables, numerical summaries) appear to be reproducible?
1. Do the figure(s) have descriptive captions (i.e. there is a description near the figure of what is happening in the figure)?
1. Does the analysis include description and justification for any data transformations?
