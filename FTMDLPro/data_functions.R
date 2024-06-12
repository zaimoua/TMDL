library(RSQLite)
library(data.table)
library(logging)

# Set up logging
basicConfig()

# Define helper functions
YC <- function(OBJECT) {
  loginfo("Calculating yearly counts.")
  mdat <- setDT(OBJECT)
  mdat <- melt(mdat, id.vars = "year", measure.vars = names(OBJECT)[-(1:4)])
  mdat <- mdat[complete.cases(mdat), ]
  mdat.ag <- dcast(aggregate(value ~ year + variable, data = mdat, FUN = length), year ~ variable, length)
  setnames(mdat.ag, "year", "Year")
  return(mdat.ag)
}

GSC <- function(OBJECT, TYPE="") {
  loginfo("Calculating geometric mean sample count.")
  OBJECT <- setDT(OBJECT)
  mdat <- ifelse(TYPE == "raw", OBJECT, OBJECT)
  mdat$season <- ifelse(mdat$month > 4 & mdat$month <= 10, 1, 0)
  mdat <- melt(mdat, id.vars = c("year", "season"), measure.vars = names(OBJECT)[-(1:4)])
  mdat <- mdat[complete.cases(mdat), ]
  mdat$value <- as.numeric(mdat$season)
  mdat.ag <- dcast(aggregate(value ~ year + variable, data = mdat, FUN = sum), year ~ variable, sum)
  setnames(mdat.ag, "year", "Year")
  return(mdat.ag)
}

NGSC <- function(OBJECT, TYPE="") {
  loginfo("Calculating non-geometric mean sample count.")
  OBJECT <- setDT(OBJECT)
  mdat <- ifelse(TYPE == "raw", OBJECT, OBJECT)
  mdat$season <- ifelse(mdat$month > 4 & mdat$month <= 10, 1, 0)
  mdat <- melt(mdat, id.vars = c("year", "season"), measure.vars = names(OBJECT)[-(1:4)])
  mdat <- mdat[complete.cases(mdat), ]
  mdat$value <- as.numeric(mdat$season)
  mdat.ag <- dcast(aggregate(value ~ year + variable, data = mdat, FUN = sum), year ~ variable, sum)
  setnames(mdat.ag, "year", "Year")
  return(mdat.ag)
}

create_where_clause <- function(column, values) {
  values <- sapply(values, function(x) paste0("'", x, "'"))
  clause <- paste(column, "IN (", paste(values, collapse = ", "), ")", sep = " ")
  return(clause)
}

data_extraction <- function(WBID = NULL, STATIONS = NULL, PARAM = NULL, LAKEWATCH = FALSE, IWR = NULL, year_range = NULL, station_id = NULL) {
  loginfo("Validating inputs.")
  if (is.null(WBID)) stop("WBID cannot be NULL.")
  if (is.null(PARAM)) stop("PARAM cannot be NULL.")
  if (!file.exists(IWR)) stop("IWR database file does not exist.")
  
  loginfo("Creating SQL where clauses.")
  WBID.SQL <- create_where_clause("wbid", WBID)
  PARAM.SQL <- create_where_clause("mastercode", PARAM)
  YEAR.SQL <- if (!is.null(year_range)) {
    paste("year BETWEEN", year_range[1], "AND", year_range[2])
  } else {
    NULL
  }
  STATIONS.SQL <- if (!is.null(station_id) && station_id != "") create_where_clause("sta", station_id) else NULL
  
  loginfo("Constructing SQL query.")
  SQL.String <- paste("SELECT wbid, sta, year, month, day, time, mastercode, result, rcode, mdl FROM RawData WHERE", WBID.SQL, "AND", PARAM.SQL)
  if (!is.null(YEAR.SQL)) {
    SQL.String <- paste(SQL.String, "AND", YEAR.SQL)
  }
  if (!is.null(STATIONS.SQL)) {
    SQL.String <- paste(SQL.String, "AND", STATIONS.SQL)
  }
  
  loginfo("Connecting to the database.")
  con <- dbConnect(SQLite(), IWR)
  on.exit(dbDisconnect(con), add = TRUE)
  
  loginfo("Executing SQL query.")
  data.raw <- tryCatch({
    dbGetQuery(con, SQL.String)
  }, error = function(e) {
    stop("Failed to execute SQL query: ", e$message)
  })
  
  loginfo("Checking for empty data.")
  if (nrow(data.raw) == 0) {
    stop("No data found for the specified query.")
  }
  
  loginfo("Converting raw data to data.table.")
  data.raw <- setDT(data.raw)
  
  loginfo("Processing data.")
  data.raw[, `:=`(year = as.numeric(year), month = as.numeric(month), day = as.numeric(day))]
  
  loginfo("Calculating yearly geometric means.")
  data.wbid <- data.raw[, .(geomean = exp(mean(log(result), na.rm = TRUE))), by = .(wbid, year, mastercode)]
  
  loginfo("Pivoting data to wide format and sorting by year.")
  data.geomean.wide <- dcast(data.wbid, wbid + year ~ mastercode, value.var = "geomean")
  setorder(data.geomean.wide, year)
  setorder(data.raw, year)
  
  loginfo("Summarizing data.")
  data.sum <- data.raw[, .(Min = min(result, na.rm = TRUE), `1st Qu.` = quantile(result, 0.25, na.rm = TRUE),
                           Median = median(result, na.rm = TRUE), Mean = mean(result, na.rm = TRUE),
                           `3rd Qu.` = quantile(result, 0.75, na.rm = TRUE), Max = max(result, na.rm = TRUE)), by = mastercode]
  
  loginfo("Preparing output.")
  Out.list <- list("WBID" = WBID, "summary" = data.sum, "geomeans" = data.geomean.wide, "rawdata" = data.raw)
  
  return(Out.list)
}
