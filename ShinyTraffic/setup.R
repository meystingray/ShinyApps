#PI <- readRDS("C:/Users/sconroy/Documents/DallasPoliceData/PoliceIncidents1-9-21.RDS")
setDT(PI)
traffic <- PI[grepl("TRAFFICKING",offincident),]
traffic[,NumIncidentsPerYear := .N,by = "servyr"]

traffic[,rowid := 1:.N]
traffic[,Date := as.Date(substr(date1,1,10))]
traffic[,LatLongStart := regexpr("(",geocoded_column,fixed = TRUE)[1] + 1,by = rowid]
traffic[,LatLongEnd := regexpr(")",geocoded_column,fixed = TRUE)[1] - 1,by = rowid]
traffic[,LatLong := substr(geocoded_column,start = LatLongStart,stop = LatLongEnd)]
traffic[,LatLongComma := regexpr(",",LatLong,fixed = TRUE)[1],by = rowid]
traffic[,Latitude := substr(LatLong,start = 1,stop = LatLongComma - 1)]
traffic[,Longitude := substr(LatLong,start = LatLongComma + 1,stop = 1000)]
traffic[,Longitude := as.numeric(Longitude)]
traffic[,Latitude := as.numeric(Latitude)]

traffic[is.na(Latitude) & incident_address == "14040 N STEMMONS SERV",Latitude := 32.93769989950343]
traffic[is.na(Longitude) & incident_address == "14040 N STEMMONS SERV",Longitude := -96.90205446873641]

traffic[is.na(Latitude) & incident_address == "7815 L B J FWY",Latitude := 32.925475281010286]
traffic[is.na(Longitude) & incident_address == "7815 L B J FWY",Longitude := -96.77161085979215]

traffic[,Year := as.factor(servyr)]

keepCols <- c("incidentnum", "servyr", "Date", "Year", "watch", "signal", "offincident", "premise", "incident_address", "apt", "ra", "beat", "division", "sector", "district",
"involvement", "victimtype", "comprace", "compethnicity", "compsex", "compage", "followup1", "followup2", "status", "ucr_disp", "victiminjurydesc", "victimcond", "mo",
"Latitude","Longitude","NumIncidentsPerYear")
traffic <- traffic[,..keepCols]

#saveRDS(traffic,"C:/Users/sconroy/Documents/DallasPoliceData/Traffic.RDS")
#traffic <- readRDS("C:/Users/sconroy/Documents/DallasPoliceData/Traffic.RDS")
traffic$Date
