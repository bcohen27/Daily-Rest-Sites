library('move2')
library('lubridate')
library('foreach')
library('sf') # maptools deprecated
library('geosphere')
library('suncalc')

# for now have removed pdf map, could add it again with use of MoveApps OSM mirror

rFunction <- function(data, window="all", upX=0, downX=0, speedvar="speed", maxspeed=NULL, duration=NULL, radius=NULL,stamen_key=NULL)
{
  Sys.setenv(tz="UTC")

  # add species info
  if (any(names(mt_track_data(data))=="taxon_canonical_name")) 
  {
    data <- mt_as_event_attribute(data,"taxon_canonical_name",.keep=TRUE)
    data$species <- data$taxon_canonical_name
  }
  if (any(names(mt_track_data(data))=="individual_taxon_canonical_name")) 
  {
    data <- mt_as_event_attribute(data,"individual_taxon_canonical_name",.keep=TRUE)
    data$species <- data$individual_taxon_canonical_name
  }
  data.df <- as.data.frame(data)

  n.all <- length(mt_time(data))
  data <- data[!duplicated(paste0(round_date(mt_time(data), "5 mins"), mt_track_id(data))),]
  logger.info(paste0("For better performance, the data have been thinned to max 5 minute resolution. From the total ",n.all," positions, the algorithm retained ",length(mt_time(data))," positions for calculation."))
  
  if (is.null(window)) 
  {
    logger.info("You have not selected if to extract positions during night, day or 24 hours. All resting sites of the 24h day will be extracted (starting midnight)")
    window <- "all"
  } else
  {
    if (window=="sundownup") 
    {
      logger.info(paste("Your have selected to extract positions from sunset +",downX,"minutes until sunrise +",upX,"minutes. The algorithm starts from the sunrise side back."))
    } else if (window=="sunupdown") 
    {
      logger.info(paste("Your have selected to extract positions from sunrise +",upX,"minutes until sunset +",downX,"minutes. The algorithm starts from the sunset side back."))
    } else if (window=="all") 
    {
      logger.info(paste("Your have selected to extract positions from all the 24h of the day. The algorithm starts midnight."))
    } else
    {
      logger.info("Your selected day/night selection option is not valid. Here we use 'all' as default, thus all resting sites of the 24h day will be extracted (starting midnight).")
      window <- "all"
    }
  }
  
  #transform to lonlat
  if (!st_is_longlat(data))
  {
    data_proj <- st_crs(data)
    data <- data |> 
      sf::st_transform(4326)
    logger.info("Transformed data to LonLat for calcualtions.")
  }

  if (is.null(maxspeed))
  {
    logger.info("You have not selected a maximum speed to filter out positions in flight. Therefore, all positions kept in the data set, but flight might corrupt the results.")
    data.ground <- data
  } else
  {
    maxspeed <- units::set_units(maxspeed,m/s)
    data.split <- split(data,mt_track_id(data)) #here was the order mistake! solved :)
    data.ground <- foreach(datai = data.split) %do% {
      if (speedvar=="speed") 
      {
        if (nrow(datai)>1) #cannot calculated between-loc speed if only one location, therefore keep
        {
          ix <- which(mt_speed(datai) < maxspeed) #na not selected (last location not included here)
          res <- datai[sort(unique(c(ix,ix+1))),] #this uses the speed between positions, select locations before and after low speed (poss. incl. last)
        } else
        {
          logger.info("One of your tracks contains only one location, so between-location speed cannot be calculated. The location is kept in the data set, but might corrupt the results.")
          res <- datai
        }
      } else if (speedvar %in% names(datai)) 
      {
        if (any(is.na(dataispeedvar))) logger.info("Your speed variable contains NA, these are kept in the data set of rest positions.")
        eval(parse(text=paste0("dataispeedvar <- datai$",speedvar)))
        res <- datai[sort(which(dataispeedvar < maxspeed | is.na(dataispeedvar))),] # this allows also NA speed to be selected
      } else 
      {
        logger.info("You have not selected a viable speed variable. Therefore the fallback between-location speed is calculated.")
        if (nrow(datai)>1)
        {
          ix <- which(mt_speed(datai) < maxspeed)
          res <- datai[sort(unique(c(ix,ix+1))),]#this uses the speed between positions
        } else
        {
          logger.info("One of your tracks contains only one location, so between-location speed cannot be calculated. The location is kept in the data set, but might corrupt the results.")
          res <- datai
        }
      }
      res #somehow, here the timestamps are not ordered any more!! ???
    }
    #names(data.ground) <- names(data.split)
    data.ground <- mt_stack(data.ground,.track_combine="merge")
  }
  
  if (is.null(duration) & is.null(radius)) 
  {
    logger.info("You didnt provide any rest site radius or minimum rest duration. Please go back and configure them. Here return NULL.") 
    result <- NULL
  } else
  {
    if (is.null(duration) & !is.null(radius)) 
    {
      logger.info(paste0("You have selected a rest site radius of ",radius,"m, but no minimum rest duration. We here use 1h by default. If that is not what you need, please go back and configure the parameters."))
      duration <- 1
    }
    if (!is.null(duration) & is.null(radius))
    {
      logger.info(paste0("You have selected a minimum rest duration of ",duration,"h, but no rest site radius. We here use 1000m = 1km by default. If that is not what you need, please go back and configure the parameters."))
      radius <- 1000
    }
    
    # select night or day positions (use data.ground, call it "night")
    data.ground.split <- split(data.ground,mt_track_id(data.ground))
    data.night <- foreach(data.groundi = data.ground.split) %do% {
      #print(namesIndiv(data.groundi))
      logger.info(unique(mt_track_id(data.groundi)))
      
      lonlat <- sf::st_coordinates(data.groundi)
      
      sun_rise_set <- data.frame(
        date = as.Date(mt_time(data.groundi)),
        lon = lonlat[, "X"],
        lat = lonlat[, "Y"]
      ) |> 
        suncalc::getSunlightTimes(data = _, keep = c("sunrise", "sunset"), tz = "UTC")
      
      # bind new cols
      data.groundi <- data.groundi |> 
        dplyr::mutate(
          sunupx = sun_rise_set$sunrise + upX*60,
          sundownx = sun_rise_set$sunset + downX*60
        )

      # there are no sunup or sundown during Arctic summer, then only day positions possible "sunupdown". respectively for Arctic winter

      if (window=="all") #day and night, so no need to remove any locations
      {
        data.nighti <- data.groundi
        data.nighti$year <- as.POSIXlt(mt_time(data.nighti))$year+1900
        data.nighti$yday <- as.POSIXlt(mt_time(data.nighti))$yday
      } else
      {
        if (window=="sundownup") #night roosts, so remove locations/timestamps with polar day
        {
          ix <- which(is.na(data.groundi$sunupx) | is.na(data.groundi$sundownx))
          ix_ArcSum <- ix[st_coordinates(data.groundi)[ix,2]>50 & as.POSIXlt(mt_time(data.groundi[ix,]))$mon %in% c(4:8)]
          if (length(ix_ArcSum)>0)
          {
            logger.info(paste0("The data set of track ",unique(mt_track_id(data.groundi))," includes positions above the Arctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcSum)," northern summer positions without 'night' (May-Sep) are taken out for the calculations."))
            data.groundi <- data.groundi[-ix_ArcSum,]
          }
          
          ix <- which(is.na(data.groundi$sunupx) | is.na(data.groundi$sundownx))
          ix_AntSum <- ix[st_coordinates(data.groundi)[ix,2]<(-50) & as.POSIXlt(mt_time(data.groundi[ix,]))$mon %in% c(10:11,0:2)]
          if (length(ix_AntSum)>0)
          {
            logger.info(paste0("The data set of track ",unique(mt_track_id(data.groundi))," includes positions below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_AntSum)," southern summer positions without 'night' (Nov-Mar) are taken out for the calculations."))
            data.groundi <- data.groundi[-ix_AntSum,]
          }
          
          ix <- which(is.na(data.groundi$sunupx) | is.na(data.groundi$sundownx))
          ix_ArcWin <- ix[st_coordinates(data.groundi)[ix,2]>50 & as.POSIXlt(mt_time(data.groundi[ix,]))$mon %in% c(10:11,0:2)]
          ix_AntWin <- ix[st_coordinates(data.groundi)[ix,2]<(-50) & as.POSIXlt(mt_time(data.groundi[ix,]))$mon %in% c(4:8)]
          
          if (length(ix_ArcWin)>0 & length(ix_AntWin)>0) #needs to be so complicated, to keep correct ix relations?
          {
            logger.info(paste0("The data set of track ",unique(mt_track_id(data.groundi))," includes positions above the Arctic circle and below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcWin)," northern winter positions without 'day' (Nov-Mar) and", length(ix_AntWin), " southern winter positions without 'day' (May-Sep) are kept in the data fully."))
            data.groundi.ND <- data.groundi[-c(ix_ArcWin,ix_AntWin),]
            ix.ND <- seq(along=data.groundi)[-c(ix_ArcWin,ix_AntWin)]
            selND <- which(mt_time(data.groundi.ND)<=data.groundi.ND$sunupx | mt_time(data.groundi.ND)>=data.groundi.ND$sundownx)
            data.nighti <- data.groundi[sort(c(ix.ND[selND],ix_ArcWin,ix_AntWin)),]
          } else if (length(ix_ArcWin)>0 & length(ix_AntWin)==0)
          {
            logger.info(paste0("The data set of individual ",unique(mt_track_id(data.groundi))," includes positions above the Arctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcWin)," northern winter positions without 'day' (Nov-Mar) are kept in the data fully."))
            data.groundi.ND <- data.groundi[-c(ix_ArcWin),]
            ix.ND <- seq(along=data.groundi)[-c(ix_ArcWin)]
            selND <- which(mt_time(data.groundi.ND)<=data.groundi.ND$sunupx | mt_time(data.groundi.ND)>=data.groundi.ND$sundownx)
            data.nighti <- data.groundi[sort(c(ix.ND[selND],ix_ArcWin)),]
          } else if (length(ix_ArcWin)==0 & length(ix_AntWin)>0)
          {
            logger.info(paste0("The data set of individual ",unique(mt_track_id(data.groundi))," includes positions below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_AntWin)," southern winter positions without 'day' (May-Sep) are kept in the data fully."))
            data.groundi.ND <- data.groundi[-c(ix_AntWin),]
            ix.ND <- seq(along=data.groundi)[-c(ix_AntWin)]
            selND <- which(mt_time(data.groundi.ND)<=data.groundi.ND$sunupx | mt_time(data.groundi.ND)>=data.groundi.ND$sundownx)
            data.nighti <- data.groundi[sort(c(ix.ND[selND],ix_AntWin)),]
          } else data.nighti <- data.groundi[mt_time(data.groundi)<=data.groundi$sunupx | mt_time(data.groundi)>=data.groundi$sundownx,]
          
          year <- as.POSIXlt(mt_time(data.nighti))$year+1900
          yday <- as.POSIXlt(mt_time(data.nighti))$yday
          ynight <- yday
          
          ixx <- which(is.na(data.nighti$sundownx))
          if (length(ixx)>0)
          {
            ynight[mt_time(data.nighti[-ixx])>data.nighti$sundownx[-ixx]] <- ynight[mt_time(data.nighti[-ixx])>data.nighti$sundownx[-ixx]]+1
            
            # for Arctic/Antarctic nights the night goes from midday to midday, which depends on the location..
            midday_ixx <- solarnoon(st_coordinates(data.nighti[ixx]),mt_time(data.nighti[ixx]),POSIXct.out=TRUE)$time
            ynight[mt_time(data.nighti[ixx])>midday_ixx] <- ynight[mt_time(data.nighti[ixx])>midday_ixx]+1
          } else ynight[mt_time(data.nighti)>data.nighti$sundownx] <- ynight[mt_time(data.nighti)>data.nighti$sundownx]+1
          
          # adapt for New Year's Eve
          year[as.POSIXlt(mt_time(data.nighti))$mday==31 & as.POSIXlt(mt_time(data.nighti))$mon==11 & mt_time(data.nighti)>data.nighti$sundownx] <- year[as.POSIXlt(mt_time(data.nighti))$mday==31 & as.POSIXlt(mt_time(data.nighti))$mon==11 & mt_time(data.nighti)>data.nighti$sundownx]+1
          ynight[as.POSIXlt(mt_time(data.nighti))$mday==31 & as.POSIXlt(mt_time(data.nighti))$mon==11 & mt_time(data.nighti)>data.nighti$sundownx] <- 0
          
          data.nighti$year <- year
          data.nighti$yday <- yday
          data.nighti$ynight <- ynight
        }
        
        if (window=="sunupdown") # day rests
        {
          ix <- which(is.na(data.groundi$sunupx) | is.na(data.groundi$sundownx))
          ix_ArcWin <- ix[st_coordinates(data.groundi)[ix,2]>50 & as.POSIXlt(mt_time(data.groundi[ix,]))$mon %in% c(10:11,0:2)]
          if (length(ix_ArcWin)>0)
          {
            logger.info(paste0("The data set of individual ",unique(mt_track_id(data.groundi))," includes positions above the Arctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcWin)," northern winter positions without 'day' (Nov-Mar) are taken out for the calculations."))
            data.groundi <- data.groundi[-ix_ArcWin,]
          }
          ix <- which(is.na(data.groundi$sunupx) | is.na(data.groundi$sundownx))
          ix_AntWin <- ix[st_coordinates(data.groundi)[ix,2]<(-50) & as.POSIXlt(mt_time(data.groundi[ix,]))$mon %in% c(4:8)]
          if (length(ix_AntWin)>0)
          {
            logger.info(paste0("The data set of individual ",unique(mt_track_id(data.groundi))," includes positions below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_AntWin)," southern winter positions without 'day' (May-Sep) are taken out for the calculations."))
            data.groundi <- data.groundi[-ix_AntWin,]
          }
          
          ix <- which(is.na(data.groundi$sunupx) | is.na(data.groundi$sundownx))
          ix_ArcSum <- ix[st_coordinates(data.groundi)[ix,2]>50 & as.POSIXlt(mt_time(data.groundi[ix,]))$mon %in% c(4:8)]
          ix_AntSum <- ix[st_coordinates(data.groundi)[ix,2]<(-50) & as.POSIXlt(mt_time(data.groundi[ix,]))$mon %in% c(10:11,0:2)]
          
          if (length(ix_ArcSum)>0 & length(ix_AntSum)>0)
          {
            logger.info(paste0("The data set of individual ",unique(mt_track_id(data.groundi))," includes positions above the Arctic circle and below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcSum)," northern summer positions without 'night' (May-Sep) and", length(ix_AntSum), " southern summer positions without 'night' (Nov-Mar) are kept in the data fully."))
            data.groundi.ND <- data.groundi[-c(ix_ArcSum,ix_AntSum),]
            ix.ND <- seq(along=data.groundi)[-c(ix_ArcSum,ix_AntSum)]
            selND <- which(mt_time(data.groundi.ND)>=data.groundi.ND$sunupx & mt_time(data.groundi.ND)<=data.groundi.ND$sundownx)
            data.nighti <- data.groundi[sort(c(ix.ND[selND],ix_ArcSum,ix_AntSum)),]
          } else if (length(ix_ArcSum)>0 & length(ix_AntSum)==0)
          {
            logger.info(paste0("The data set of individual ",unique(mt_track_id(data.groundi))," includes positions above the Arctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcSum)," northern summer positions without 'night' (May-Sep) are kept in the data fully."))
            data.groundi.ND <- data.groundi[-c(ix_ArcSum),]
            ix.ND <- seq(along=data.groundi)[-c(ix_ArcSum)]
            selND <- which(mt_time(data.groundi.ND)>=data.groundi.ND$sunupx & mt_time(data.groundi.ND)<=data.groundi.ND$sundownx)
            data.nighti <- data.groundi[sort(c(ix.ND[selND],ix_ArcSum)),]
          } else if (length(ix_ArcSum)==0 & length(ix_AntSum)>0)
          {
            logger.info(paste0("The data set of individual ",unique(mt_track_id(data.groundi))," includes positions below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_AntSum)," southern summer positions without 'night' (Nov-Mar) are kept in the data fully."))
            data.groundi.ND <- data.groundi[-c(ix_AntSum),]
            ix.ND <- seq(along=data.groundi)[-c(ix_AntSum)]
            selND <- which(mt_time(data.groundi.ND)>=data.groundi.ND$sunupx & mt_time(data.groundi.ND)<=data.groundi.ND$sundownx)
            data.nighti <- data.groundi[sort(c(ix.ND[selND],ix_AntSum)),]
          } else data.nighti <- data.groundi[mt_time(data.groundi)>=data.groundi$sunupx & mt_time(data.groundi)<=data.groundi$sundownx,]
          
          data.nighti$year <- as.POSIXlt(mt_time(data.nighti))$year+1900
          data.nighti$yday <- as.POSIXlt(mt_time(data.nighti))$yday
        }
      }
      return(data.nighti)
    }

    data.night <- mt_stack(data.night,.track_combine="rename")

    if (nrow(data.night)==0) #number of list elements
    {
      logger.info("Your data contain no night/day positions. No csv overview saved. Return NULL.")
      result <- NULL
    } else 
    {
      data.night.df <- as.data.frame(data.night)
      nacolx <- which(apply(data.night.df,2,function (x) all(is.na(x)))) #remove empty columns
      if (length(nacolx)>0) data.night.df.nna <- data.night.df[,-nacolx] else data.night.df.nna <- data.night.df #remove columns with all NA
      
      write.csv(data.night.df.nna,file = appArtifactPath("data_rest_selectedTime.csv"),row.names=FALSE) #csv artefakt of all ground and night (or day...) positions
      #write.csv(data.night.df.nna,file = "data_rest_selectedTime.csv",row.names=FALSE) #csv artefakt of all ground and night (or day...) positions
      
      # save all rest positions if is rest by given definition (radius, duration), goes backwards for last night/day rest
      data.night.split <- split(data.night,mt_track_id(data.night))
      
      prop.rest.df <- data.frame("local.identifier"=character(),"species"=character(),"year"=numeric(),"yday"=numeric(),"timestamp.first"=character(),"timestamp.last"=character(),"rest.mean.long"=numeric(),"rest.mean.lat"=numeric(),"rest.nposi"=numeric(),"rest.duration"=numeric(),"rest.radius"=numeric())
      if (window=="sundownup") names(prop.rest.df)[which(names(prop.rest.df)=="yday")] <- "ynight"

      data.rest <- foreach(data.nighti = data.night.split) %do% {
        logger.info(paste("Extracting rest sites of track ",unique(mt_track_id(data.nighti))))
        #data.resti.df <- data.frame(as.data.frame(data.nighti),st_coordinates(data.nighti))[0,] #empty df to fill
        #Nresti <- dim(data.resti.df)[2]
        #names(data.resti.df)[(Nresti-1):Nresti] <- c("location_long","location_lat")
        
        year <- unique(data.nighti$year)
        ix_sel_id <- numeric() #here to keep track of rest site indices of individual tacks
        
        for (k in seq(along=year))
        {
          data.nightik <- data.nighti[data.nighti$year==year[k],]
          
          if (window=="all")
          {
            night <- unique(data.nightik$yday)
          } else
          {
            if (window=="sundownup") night <- unique(data.nightik$ynight)
            if (window=="sunupdown") night <- unique(data.nightik$yday)
          }
          
          for (j in seq(along=night))
          {
            
            if (window=="all")
            {
              data.nightikj <- data.nightik[data.nightik$yday==night[j],]
            } else
            {
              if (window=="sundownup") data.nightikj <- data.nightik[data.nightik$ynight==night[j],]
              if (window=="sunupdown") data.nightikj <- data.nightik[data.nightik$yday==night[j],]
            }
            
            last <- Nikj <- nrow(data.nightikj)
            while (last>1) # as long as first night/day position is not the last
            {
              data.nightikj <- data.nightikj[1:last,]
              backdt <- as.numeric(difftime(mt_time(data.nightikj)[last],mt_time(data.nightikj)[-c(last:Nikj)],units="hours"))
              if (length(backdt)>=1) #changed this to allow for further away position (if in radius assume not moved), allows for worse resolution data
              {
                # note that here not yet checked that duration in site complete, later
                if (any(backdt<=duration)) 
                {
                  data.sel <- data.nightikj[c(which(backdt<=duration),last),] 
                  data.rem <- data.nightikj[-c(which(backdt<=duration),last),]
                } else
                {
                  data.sel <- data.nightikj[(last-1):last,]
                  data.rem <- data.nightikj[-c((last-1):last),]
                }
                m <- colMeans(st_coordinates(data.sel))
                dp0 <- distVincentyEllipsoid(m,st_coordinates(data.sel))
                p0 <- st_coordinates(data.sel)[min(which(dp0==max(dp0))),]
                dp1 <- distVincentyEllipsoid(p0,st_coordinates(data.sel))
                p1 <- st_coordinates(data.sel)[min(which(dp1==max(dp1))),]
                maxdist <- distVincentyEllipsoid(p0,p1)
                
                if (maxdist<radius)
                {
                  ## check if already longer at this rest site
                  mid <- midPoint(p0,p1)
                  data.bef <- data.rem
                  if (nrow(data.bef)>=1)
                  {
                    dist.bef <- distVincentyEllipsoid(mid,st_coordinates(data.bef))
                    if (any(dist.bef>radius))
                    {
                      data.selx <- data.nightikj[c(which(backdt>duration)[-(1:max(which(dist.bef>radius)))],which(backdt<=duration),last),]
                      data.remx <- data.nightikj[-c(which(backdt>duration)[-(1:max(which(dist.bef>radius)))],which(backdt<=duration),last),]
                    } else 
                    {
                      data.selx <- data.nightikj[c(which(backdt>duration),which(backdt<=duration),last),]
                      data.remx <- data.nightikj[-c(which(backdt>duration),which(backdt<=duration),last),]
                    }
                  } else 
                  {
                    data.selx <- data.sel
                    data.remx <- data.rem
                  }
                  
                  data.selx.df <- data.frame(as.data.frame(data.selx),st_coordinates(data.selx))
                  Nselx <- dim(data.selx.df)[2]
                  names(data.selx.df)[(Nselx-1):Nselx] <- c("location_long","location_lat")
                  
                  time0 <- min(mt_time(data.selx))
                  timeE <- max(mt_time(data.selx))
                  durx <- as.numeric(difftime(timeE,time0,unit="hour"))
                  radx <- max(distVincentyEllipsoid(mid,st_coordinates(data.selx)))
                  
                  if (durx>=duration & radx<=radius) #added this condition to only save rest sites of given duration (if this condition is left out also rest site with shorter duration are given back)
                  {
                    #data.resti.df <- rbind(data.resti.df,data.selx.df)
                    ix_sel_id <- c(ix_sel_id,which(mt_time(data.selx) %in% mt_time(data.nighti)))
                    
                    if (window=="all")
                    {
                      prop.rest.df <- rbind(prop.rest.df,data.frame("local.identifier"=unique(mt_track_id(data.selx)),"species"=data.selx.df$species[1],"year"=data.selx.df$year[1],"yday"=data.selx.df$yday[1],"timestamp.first"=as.character(time0),"timestamp.last"=as.character(timeE),"rest.mean.long"=mid[1,1],"rest.mean.lat"=mid[1,2],"rest.nposi"=nrow(data.selx),"rest.duration"=durx,"rest.radius"=radx))
                    } else
                    {
                      if (window=="sundownup") prop.rest.df <- rbind(prop.rest.df,data.frame("local.identifier"=unique(mt_track_id(data.selx)),"species"=data.selx.df$species[1],"year"=data.selx.df$year[1],"ynight"=data.selx.df$ynight[1],"timestamp.first"=as.character(time0),"timestamp.last"=as.character(timeE),"rest.mean.long"=mid[1,1],"rest.mean.lat"=mid[1,2],"rest.nposi"=nrow(data.selx),"rest.duration"=durx,"rest.radius"=radx))
                      if (window=="sunupdown") prop.rest.df <- rbind(prop.rest.df,data.frame("local.identifier"=unique(mt_track_id(data.selx)),"species"=data.selx.df$species[1],"year"=data.selx.df$year[1],"yday"=data.selx.df$yday[1],"timestamp.first"=as.character(time0),"timestamp.last"=as.character(timeE),"rest.mean.long"=mid[1,1],"rest.mean.lat"=mid[1,2],"rest.nposi"=nrow(data.selx),"rest.duration"=durx,"rest.radius"=radx))
                    }
                  }
                  
                  #A) if only need one, i.e. the last, rest site per day/night
                  #break 
                  
                  #B) if need all rest sites (irregardless if previous rest site was saved, check for another one in the remaining data)
                  data.nightikj <- data.remx
                  last <- Nikj <- nrow(data.nightikj) #this can be zero, will be stop indication in while() condition
                  
                } else last <- last-1 #shift one time step
              } else last <- last-1 # shift one time step also if not enough data in previous Xh time frame
            } #while
          } #for j
        } #for k
        data.nighti[ix_sel_id,] # those locations belong to any rest site, dont need to transform data like this
        #if (dim(data.resti.df)[1]>0) 
        #{
          #o <- order(data.resti.df$timestamp) #KEEP
          #data.resti <- mt_as_move2(data.resti.df[o,],coords=c("location_long","location_lat"),time_column="timestamp",track_id_column=mt_track_id_column(data),track_id_columns=names(mt_track_data(data))) #KEEP
        #} else data.resti <- NULL #move2 objects can have length 0
      }
      names(data.rest) <- names(data.night.split)
      data.rest <- mt_stack(data.rest,.track_combine="rename")
      
      if (nrow(data.rest)==0) 
      {
        logger.info("Your output file contains no positions. No csv overview and plot saved. Return NULL.")
        result <- NULL
      } else 
      {
        result <- data.rest
        write.csv(prop.rest.df,file = appArtifactPath("rest_overview.csv"),row.names=FALSE) #csv artefakt

        #stamen map does not work... better to use leaflet App after this one
      }
    }
  }
  
  if(!is.null(result))
  {
    result <- result |> 
      sf::st_transform(data_proj)
    logger.info(paste("Retransformed result to input projection:", data_proj))
  }
  
  return(result)
}



