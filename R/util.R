# util.R



# =======================================================================================
# PACKAGE UTIL 
# =======================================================================================





# =======================================================================================
# PACKAGE RELATED 
# =======================================================================================



# Get time series data
#
# @param con          A character. String conection to the data source
# @param sps          An SpatialPoints object
# @return             
.getTimeSeries <- function(con, sps){
  samples.list <- list()  
  if(con$type == "scidb"){
    # project to MODIS SINUSOIDAL
    modis.proj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
    sps.mod <- spTransform(sps, CRS(modis.proj))
    # project to col_id, row_id
    cr.id <- .sinusoidal2gmpi(lonlat.Matrix = coordinates(sps.mod), pixelSize = .calcPixelSize(4800, .calcTileWidth()))
    # prepare the query
    cr.id <- cbind(con$array, cr.id, 0, cr.id, 500)
    afl.vec <- do.call(paste, c(as.data.frame(cr.id), sep = ","))
    afl.vec <- paste("between(", afl.vec, ")", sep = "")
    # get the data
    scidbconnect(host = con$host, port = con$port)                              # db connection
    for(i in 1:length(afl.vec)){
      samples.list[[i]] <- tryCatch({
        iquery(query = afl.vec[i], return = T, binary = T)
      }, error = function(e){
        return(NA)                                                              # error while retrieving data from SciDB
      })
    }
  }else if(con$type == "wtss"){
    ts_server = WTSS(con$server)
    cv <- describeCoverage(ts_server, con$coverage)
    xy.mat <- coordinates(sps)
    for(i in 1:nrow(xy.mat)){
      samples.list[[i]] <- tryCatch({
        samples.list[[i]] <- timeSeries(
          ts_server, 
          coverages = con$coverage, 
          attributes = cv[[1]]$attributes$name, 
          longitude = xy.mat[i, 1], 
          latitude = xy.mat[i, 2], 
          start = "2000-02-18", 
          end = "2020-01-01"
        )
      }, error = function(e){
        return(NA)                                                              # error while retrieving data from a WTSS server
      })
    }
  }else{
    stop("Unknown data source")
  }

  # compute DTW distances for each attribute
  res <- parallel::mclapply(1:length(samples.list), function(y, samples){
    dts.df <- data.frame()
    for(i in 1:length(samples)){
      test <- samples.list[[i]]
      ref <- samples.list[[y]]
      dts.vec <- rep(0, time = ncol(ref))
      if(i != y){
        for(j in 1:ncol(ref)){
          dts.vec[j] <- dtw(x = test[, j], y = ref[, j])$distance
        }
      }
      dts.df <- rbind(dts.df, dts.vec)
    }
    colnames(dts.df) <- colnames(samples.list[[y]])
    return(dts.df)
  }, 
  samples = samples.list)
  
}







