library(rsdmx)

#read the annual population of each SA2 region
sdmx2 <- readSDMX(providerId = "ABS", resource = "data", flowRef = "ABS_ANNUAL_ERP_ASGS2016",
                 key = list("ERP", "SA2", NULL), start = 2006, end = 2016, dsd = TRUE)
df2 <- as.data.frame(sdmx2)
head(df2)

#but while I can see the XML data in sdmx2, the df is NULL
#why???

#the same code works on this OECD dataset, so why not mine?
sdmx <- readSDMX(providerId = "OECD", resource = "data", flowRef = "MIG",
                 key = list("TOT", NULL, NULL), start = 2010, end = 2011)
df <- as.data.frame(sdmx)
head(df)