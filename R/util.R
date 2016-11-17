# util.R



# =======================================================================================
# PACKAGE UTIL 
# =======================================================================================


.triangularMatrix <- function(n, up, down){
  trian.mat <- matrix(up, ncol = n, nrow = n)
  for(i in 1:nrow(trian.mat)){for(j in 1:ncol(trian.mat)){if(i > j){trian.mat[i, j] <- down}}}
  return(trian.mat)
}





# =======================================================================================
# PACKAGE RELATED 
# =======================================================================================



# Get time series data
#
# @param con          A character. String conection to the data source
# @param spdf         An SpatialPointsDataFrame object. It contais the sample points (SpatialPoints) and a data.frame made of  id, class, start, and end
# @return             A list of data.frame. Each data.frame is the data retrieved for each sample point
.getTimeSeries <- function(con, spdf){
  samples.list <- list()  
  sps <- SpatialPoints(coords = coordinates(spdf), proj4string = CRS(proj4string(spdf)))
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
        df <- iquery(query = afl.vec[i], return = T, binary = T)
        df["tiddate"] <- as.Date(as.vector(unlist(.time_id2date(unlist(df["time_id"]), period = 16))))
        df
      }, error = function(e){
        return(NA)                                                              # error while retrieving data from SciDB
      })
    }
  }else if(con$type == "wtss"){
    ts_server = WTSS(con$server)
    cv <- describeCoverage(ts_server, con$coverage)
    xy.mat <- coordinates(sps)
    for(i in 1:nrow(xy.mat)){
      tryCatch({
        samples.list[[i]] <- timeSeries(
          ts_server, 
          coverages = con$coverage, 
          attributes = cv[[1]]$attributes$name, 
          longitude = xy.mat[i, 1], 
          latitude = xy.mat[i, 2], 
          start = "2000-02-18", 
          end = "2016-01-01"
        )
      }, error = function(e){
        return(NA)                                                              # error while retrieving data from a WTSS server
      })
    }
    samples.list <- lapply(samples.list, function(x){
      df <- as.data.frame(coredata(x[[1]][["attributes"]]))
      ind <- index(x[[1]][["attributes"]])
      df["tiddate"] <- ind
      return(df)
    })
  }else{
    stop("Unknown data source")
  }
  # filter by dates
  samples.list <- lapply(1:length(samples.list), function(x, spdf, samples.list){
    df <- samples.list[[x]]
    df <- df[df$tiddate > as.Date(spdf[["from"]][x]) & df$tiddate < as.Date(spdf[["to"]][x]), ]
  }, spdf = spdf, samples.list = samples.list)
  return(samples.list)
}


# compute the DTW distance
#
# @param samples.list A list of data.frame. Each data.frame is the data of a sample point
# @param label.vec A vector of character. The label or for each time series in samples.list
# @return             A list of matrices, one metrix per unique label. These matrices are the DTW distances between time series of sample points
.dtwDistance <- function(samples.list, label.vec){
  # compute DTW distances for each attribute
  res <- list()
  labs <- unique(label.vec)
  for(l in labs){
    subsamples.list <- samples.list[label.vec == l]
    dtw.dists <- parallel::mclapply(1:length(subsamples.list), function(y, subsamples.list){
      dts.df <- data.frame()
      #samples <- subsamples.list[[y]]
      for(i in 1:length(subsamples.list)){                                      # iterate subssamples
        test <- subsamples.list[[i]]
        ref <- subsamples.list[[y]]
        dts.vec <- rep(0, time = ncol(ref))
        if(i != y){
          for(j in 1:ncol(ref)){                                                # iterate columns
            dts.vec[j] <- dtw(x = test[, j], y = ref[, j])$distance
          }
        }
        dts.df <- rbind(dts.df, dts.vec)
      }
      colnames(dts.df) <- colnames(subsamples.list[[y]])
      return(dts.df)
    }, 
    subsamples.list = subsamples.list, 
    mc.cores = detectCores())
    res[[l]] <- dtw.dists
  }
  return(res)
}




