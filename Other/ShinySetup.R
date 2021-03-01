rsconnect::setAccountInfo(name='sean-conroy', token='BF1B412FC32FB289B60F6F19E4715E85', secret='tBz2CyN1rOqSUAK+sYGixo/vAGkm1x9QppmxjKdj')
rsconnect::configureApp("ShinyCrimeExplorer", size="xlarge")

# Setup Shiny
setwd("C:/Users/sconroy/Documents/ShinyApps/ShinyMurderMap")
setwd("C:/Users/sconroy/Documents/ShinyApps/ShinyTraffic")
setwd("C:/Users/sconroy/Documents/ShinyApps/ShinyCrimeExplorer")

library(shiny)
library(rsconnect)

runApp("C:/Users/sconroy/Documents/ShinyApps/ShinyMurdermap")
runApp("C:/Users/sconroy/Documents/ShinyApps/ShinyTraffic")
runApp("C:/Users/sconroy/Documents/ShinyApps/ShinyCrimeExplorer")

rsconnect::deployApp("C:/Users/sconroy/Documents/ShinyApps/ShinyMurderMap")
rsconnect::deployApp("C:/Users/sconroy/Documents/ShinyApps/ShinyTraffic")
rsconnect::deployApp("C:/Users/sconroy/Documents/ShinyApps/ShinyCrimeExplorer")

#library(rdrop2)
#token <- drop_auth()
#saveRDS(token, "droptoken.rds")

