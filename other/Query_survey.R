#Query SVDBS
library(RODBC); library(data.table)

if(Sys.info()['sysname']=="Windows"){
    data.dir <- "L:\\Rworkspace\\RSurvey\\"
    out.dir  <- "L:\\EcoAP\\Data\\survey\\"
    memory.limit(4000)
}
if(Sys.info()['sysname']=="Linux"){
    data.dir <- "slucey/Rworkspace/RSurvey/"
    out.dir  <- "slucey/EcoAP/Data/survey/"
    uid      <- 'slucey'
    cat('Oracle Password:')
    pwd <- readLines(n=1) #If reading from source, need to manually add pwd here
}

if(Sys.info()['sysname']=="Windows"){
    channel <- odbcDriverConnect()
} else {
    channel <- odbcConnect('sole', uid, pwd)
}

tables <- as.data.table(sqlTables(channel))

SVDBS <- tables[TABLE_SCHEM == 'SVDBS', ]

svspp <- as.data.table(sqlQuery(channel, 'select * from SVSPECIES_LIST'))

