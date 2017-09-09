#1 Attempt using ABS SDMX service

library(rsdmx)

#read the annual population of each SA2 region
sdmx2 <- readSDMX(providerId = "ABS", resource = "data", flowRef = "ABS_ANNUAL_ERP_ASGS2016",
                 key = list("ERP", "SA2", NULL), start = 2006, end = 2016, dsd = TRUE)
df2 <- as.data.frame(sdmx2, labels = TRUE)
head(df2)

#results in a NULL df, despite their being data in the sdmx output
---
  
#2 Attempt using ABS JSON and httr package

library(httr)
library(jsonlite)
library(tidyverse)

sa2.url <- "http://stat.data.abs.gov.au/sdmx-json/data/ABS_ANNUAL_ERP_ASGS2016/ERP.AUS+STE+SA4+SA3+SA2+GCCSA.0+1+1GSYD+102+10201+102011028+102011029+102011030+997+99797+997979799.A/all?startTime=2001&endTime=2016&dimensionAtObservation=allDimensions"
  
sa2.api<- GET(sa2.url)
sa2.pop <- sa2.api$content
head(sa2.pop)

#weird... just returns some hexadecimal values.....
---

#Attempt 3 If ABS can't map Lat/Long to SA2, them maybe we may Lat/Long to LGA using Geonames  
  
library(geonames)

LGA<- GNfindNearby(
  lat=-33.883091,
  lng=151.149364,
  username="ajdncnsn",
  featureCode="ADM2"
  )  
LGA$geonames[[1]]$name

#returns "Ashfield" (even though that's out by a few hundred metres), 
#so if RMS data has LGA names that match, maybe this works?

# Attempt 4 - Rohan
# SA2
SA2url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/ABS_ANNUAL_ERP_ASGS2016/all?startTime=2006&endTime=2016&format=compact_v2"
SA2pop_raw <- readSDMX(SA2url)
SA2pop <- as.data.frame(SA2pop_raw)

# SA2 Names
SA2dsd_url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetDataStructure/ABS_ANNUAL_ERP_ASGS2016"
SA2dsd <- readSDMX(SA2dsd_url)

#get codelists from DSD
SA2cls <- slot(SA2dsd, "codelists")
SA2codelists <- sapply(slot(SA2cls, "codelists"), function(x) slot(x, "id")) #get list of codelists
SA2codelist <- as.data.frame(slot(SA2dsd, "codelists"), codelistId = "CL_ABS_ANNUAL_ERP_ASGS2016_ASGS_2016") #get a codelist
#get concepts from DSD
SA2concepts <- as.data.frame(slot(SA2dsd, "concepts"))

## Merge names to SA2 codes
SA2pop_full <- inner_join(SA2pop,SA2codelist,c("ASGS_2016" = "id")) 


# LGA
LGAurl <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetData/ABS_ERP_LGA2016/all?startTime=2006&endTime=2016&format=compact_v2"
LGApop_raw <- readSDMX(LGAurl)
LGApop <- as.data.frame(LGApop_raw)

# SA2 Names
LGAdsd_url <- "http://stat.data.abs.gov.au/restsdmx/sdmx.ashx/GetDataStructure/ABS_ERP_LGA2016"
LGAdsd <- readSDMX(LGAdsd_url)

#get codelists from DSD
LGAcls <- slot(LGAdsd, "codelists")
LGAcodelists <- sapply(slot(LGAcls, "codelists"), function(x) slot(x, "id")) #get list of codelists
LGAcodelists
LGAcodelist <- as.data.frame(slot(LGAdsd, "codelists"), codelistId = "CL_ABS_ERP_LGA2016_LGA_2016") #get a codelist
#get concepts from DSD
LGAconcepts <- as.data.frame(slot(LGAdsd, "concepts"))

LGApop_full <- inner_join(LGApop,LGAcodelist,c("LGA_2016" = "id")) 
