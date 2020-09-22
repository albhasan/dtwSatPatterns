# util.R



# =======================================================================================
# PACKAGE UTIL 
# =======================================================================================



# Turn a string logical to logical
#
# @param val      A character in c("0", "1")
# @return         A logical or NA
.tn2bool <- function(val){ 
  if(val == "0") return(FALSE)
  if(val == "1") return(TRUE)
  return(NA)
}



# Build a tringular matrix
#
# @param n            A numeric. The dimension of the resulting matrix
# @param up           A numeric. The number for filling the top of the matrix, including the diagonal
# @param down         A numeric. The number for filling the bottom of the matrix
# @return             A character vector
.triangularMatrix <- function(n, up, down){
  trian.mat <- matrix(up, ncol = n, nrow = n)
  for(i in 1:nrow(trian.mat)){
    for(j in 1:ncol(trian.mat)){
      if(i > j){
        trian.mat[i, j] <- down
      }
    }
  }
  return(trian.mat)
}



# Invert a character
#
# @param x            A character vector
# @return             A character vector
.invertString <- function(x){
  sapply(lapply(strsplit(x, NULL), rev), paste, collapse="")  
}



# Add readable quality data to a MOD13Q1 time series according to ist data quality
#
# @param x A data frame containing the column c("quality")
# @return A data frame with additional columns.
.addTSqua <- function(x){
  # get the codes
  rqa <- .invertString(R.utils::intToBin(x$quality)) # inverted binary quality
  x$MODLAND_QA <- .invertString(substr(rqa, 1, 2))
  x$VI_useful <- .invertString(substr(rqa, 2, 5))
  x$AerQuantity <- .invertString(substr(rqa, 6, 7))
  x$AdjCloud <- .invertString(substr(rqa, 8, 8))
  x$AtmBRDF <- .invertString(substr(rqa, 9, 9))
  x$MixCloud <- .invertString(substr(rqa, 10, 10))
  x$LandWater <- .invertString(substr(rqa, 11, 13))
  x$snowice <- .invertString(substr(rqa, 14, 14))
  x$shadow <-  .invertString(substr(rqa, 15, 15))
  # convet codesz to factors
  x$MODLAND_QA <- as.factor(x$MODLAND_QA)
  levels(x$MODLAND_QA)[levels(x$MODLAND_QA)=="00"] <- "VI produced, good quality"
  levels(x$MODLAND_QA)[levels(x$MODLAND_QA)=="01"] <- "VI produced, but check other QA"
  levels(x$MODLAND_QA)[levels(x$MODLAND_QA)=="10"] <- "Pixel produced, but most probably cloudy"
  levels(x$MODLAND_QA)[levels(x$MODLAND_QA)=="11"] <- "Pixel not produced due to other reasons than clouds"
  x$VI_useful <- as.factor(x$VI_useful)
  levels(x$VI_useful)[levels(x$VI_useful)=="0000"] <- "Highest quality"
  levels(x$VI_useful)[levels(x$VI_useful)=="0001"] <- "Lower quality"
  levels(x$VI_useful)[levels(x$VI_useful)=="0010"] <- "Decreasing quality"
  levels(x$VI_useful)[levels(x$VI_useful)=="0100"] <- "Decreasing quality"
  levels(x$VI_useful)[levels(x$VI_useful)=="1000"] <- "Decreasing quality"
  levels(x$VI_useful)[levels(x$VI_useful)=="1001"] <- "Decreasing quality"
  levels(x$VI_useful)[levels(x$VI_useful)=="1010"] <- "Decreasing quality"
  levels(x$VI_useful)[levels(x$VI_useful)=="1100"] <- "Lowest quality"
  levels(x$VI_useful)[levels(x$VI_useful)=="1101"] <- "Quality so low that it is not useful"
  levels(x$VI_useful)[levels(x$VI_useful)=="1110"] <- "L1B data faulty"
  levels(x$VI_useful)[levels(x$VI_useful)=="1111"] <- "Not useful for any other reason/not processed"
  x$AerQuantity <- as.factor(x$AerQuantity)
  levels(x$AerQuantity)[levels(x$AerQuantity)=="00"] <- "Climatology"
  levels(x$AerQuantity)[levels(x$AerQuantity)=="01"] <- "Low"
  levels(x$AerQuantity)[levels(x$AerQuantity)=="10"] <- "Average"
  levels(x$AerQuantity)[levels(x$AerQuantity)=="11"] <- "High"
  x$AdjCloud <- sapply(x$AdjCloud, .tn2bool)
  x$AtmBRDF <- sapply(x$AtmBRDF, .tn2bool)
  x$MixCloud <- sapply(x$MixCloud, .tn2bool)
  x$LandWater <- as.factor(x$LandWater)
  levels(x$LandWater)[levels(x$LandWater)=="000"] <- "Shallow ocean"
  levels(x$LandWater)[levels(x$LandWater)=="001"] <- "Land (Nothing else but land)"
  levels(x$LandWater)[levels(x$LandWater)=="010"] <- "Ocean coastlines and lake shorelines"
  levels(x$LandWater)[levels(x$LandWater)=="011"] <- "Shallow inland water"
  levels(x$LandWater)[levels(x$LandWater)=="100"] <- "Ephemeral water"
  levels(x$LandWater)[levels(x$LandWater)=="101"] <- "Deep inland water"
  levels(x$LandWater)[levels(x$LandWater)=="110"] <- "Moderate or continental ocean"
  levels(x$LandWater)[levels(x$LandWater)=="111"] <- "Deep ocean"
  x$snowice <- sapply(x$snowice, .tn2bool)
  x$shadow <- sapply(x$shadow, .tn2bool)
  return(x)
}



# Given a list of data.frames, this function adds the list names as columns on each of the data.frames
#
# @param df.list  A list of data.frames
# @param colname  A character. The name of the new column.
# @return         A list of data frames
.listname2data.frame <- function(df.list, colname){
  res <- parallel::mclapply(1:length(df.list), 
                            function(x, df.list){
                              df.list[[x]][colname] <- names(df.list)[x]
df.list[[x]]["xorder"] <- 1:nrow(df.list[[x]])
                              return(df.list[[x]])
                            }, 
                            df.list = df.list
  )
  names(res) <- names(df.list)
  return(res)
}


# cast the factors in a data.frame to characters
# NOTE: This has truble with factors representing double precision numbers
#
# @param adf  A data.frame
# @return     A data.frame.
.dfFactors2character <- function(adf){
  for(i in 1:ncol(adf)){
    if(is.factor(adf[,i])){
      adf[, i] <- as.character(adf[, i])
    }
  }
  return(adf)  
}




# =======================================================================================
# PACKAGE RELATED 
# =======================================================================================



# compute statistics for a time step, over clusters on a set of time series
#
# @param subsamples.df  A data.frame. Contains all the time-series of a single label
# @param attname        A character. The name of an attribute in the data.frame
# @return               A data.frame.
.computeStatsClusterAtt <- function(subsamples.df, attname){
  tsStats.df <- data.frame(stringsAsFactors = FALSE)
  clusterIds <- unique(subsamples.df$clusterID)
  if(is.factor(subsamples.df$clusterID)){
    clusterIds <- levels(subsamples.df$clusterID)
  }
  clus.list <- split(subsamples.df, subsamples.df$clusterID, drop = TRUE)       # one data.frame for each cluster
  clusStats.list <- lapply(clus.list, 
                           function(x.df, attname){
                             sel <- as.vector(unlist(x.df[attname]))
                             tsstats <- c(mean(sel, na.rm = TRUE), 
                                          median(sel, na.rm = TRUE), 
                                          sd(sel, na.rm = TRUE), 
                                          min(sel, na.rm = TRUE), 
                                          max(sel, na.rm = TRUE)
                             )
                             return(tsstats)
                           }, 
                           attname = attname
  )
  clusStats.df <- as.data.frame(do.call("rbind", clusStats.list), stringsAsFactors = FALSE)

  
  
  colnames(clusStats.df) <- c("pmean", "pmedian", "psd", "pmin", "pmax", "clusterID", "timeStep")
  return(clusStats.df)
}


# compute statistics for each attribute
#
# @param attname          A character. Name of the attribute to compute (e.g. "evi").
# @param clist            A list of cluster objects.
# @param subsamples.list  A list of data.frame. Each data.frame contains a time series with various attribtes.
# @return                 A data.frame of statistics for each time step.
.computeStatsCluster <- function(attname, clist, subsamples.list){
  catt <- clist[[attname]]
  names(subsamples.list) <- paste(names(subsamples.list),                       # add the cluster ID to the list's names
                                  catt$pamobject$clustering, sep = "-")
  subsamples.list <- .listname2data.frame(df.list = subsamples.list,            # add the list names to each data.frame as a new column
                                          colname = "metadata")
  subsamples.df <- do.call("rbind", subsamples.list)                            # merge all data.frames into one
  subsamples.df <- cbind(subsamples.df,                                         # split the metadata into columns and add them
                         do.call(rbind, 
                                 strsplit(subsamples.df$metadata, 
                                          split = "-"
                                 )
                         )
  )
  names(subsamples.df) <- c("ndvi", "evi", "metadata", "xorder", "col_id",      # rename columns
                            "row_id", "label", "clusterID"
  )
  tsID <- paste(subsamples.df$label, subsamples.df$col_id,                      # build a time-series ID
                subsamples.df$row_id, sep = "-"
  )
  subsamples.df ["tsID"] <- tsID                                                 # add the time-series ID
  subsamples.df <- .dfFactors2character(adf = subsamples.df)                    # cast factor columns to strings
  timesteps <- unique(subsamples.df$xorder)                                     # how many time steps are there?
  clusterstats.list <- lapply(timesteps, 
                              FUN = .computeStatsClusterAtt, 
                              subsamples.df = subsamples.df, 
                              attname = attname
  )
  clusterstats.df <- as.data.frame(
    do.call("rbind", clusterstats.list), 
    stringsAsFactors = FALSE
  )
  clusterstats.df["attname"] <- attname
  return(clusterstats.df)
}


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




