library('move')
library('foreach')
library('maptools')
library('lubridate')

rFunction <- function(data, window=NULL, upX=0, downX=0, speedvar="speed", maxspeed=NULL, duration=NULL, radius=NULL)
{
  Sys.setenv(tz="GMT")
  
  n.all <- length(timestamps(data))
  data <- data[!duplicated(paste0(round_date(timestamps(data), "5 mins"), trackId(data))),]
  logger.info(paste0("For better performance, the data have been thinned to max 5 minute resolution. From the total ",n.all," positions, the algorithm retained ",length(timestamps(data))," positions for calculation."))

  if (is.null(window)) 
  {
    logger.info("You have not selected if to extract positions during night or day. All resting sites of the 24h day will be extracted (starting midnight)")
  } else
  {
    if (window=="sundownup") 
    {
      logger.info(paste("Your have selected to extract positions from sunset +",downX,"minutes until sunrise +",upX,"minutes. The algorithm starts from the sunrise side back."))
    } else if (window=="sunupdown") 
    {
      logger.info(paste("Your have selected to extract positions from sunrise +",upX,"minutes until sunset +",downX,"minutes. The algorithm starts from the sunset side back."))
    } else
    {
      logger.info("Your selected day/night selection option is not valid. Here we use NULL instead, thus all resting sites of the 24h day will be extracted (starting midnight).")
      window <- NULL
    }
  }
  
  if (is.null(maxspeed))
  {
    logger.info("You have not selected a maximum speed to filter out positions in flight. These positions are therefore kept in the data set, but might corrupt the results.")
    data.ground <- data
  } else
  {
    data.split <- move::split(data)
    data.ground <- foreach(datai = data.split) %do% {
      if (speedvar=="speed") 
      {
        ix <- which(speed(datai)<maxspeed)
        res <- datai[sort(unique(c(ix,ix+1))),]#this uses the speed between positions
      } else if (speedvar %in% names(datai)) 
      {
        res <- datai[datai@data[,speedvar]<maxspeed | is.na(datai@data[,speedvar]),] # this allows also NA speed to be selected
        logger.info("Your speed variable contains NA, these are kept in the data set of rest positions.")
      } else 
      {
        logger.info("You have not selected a viable speed variable. Therefore the fallback between location speed is calculated.")
        ix <- which(speed(datai)<maxspeed)
        res <- datai[sort(unique(c(ix,ix+1))),]#this uses the speed between positions
      }
        res
    }
    names(data.ground) <- names(data.split)
    data.ground <- moveStack(data.ground[unlist(lapply(data.ground, length) > 0)])
  }
  
  if (is.null(duration) & is.null(radius)) 
  {
    logger.info("You didnt provide any rest site radius or minimum rest duration. Please go back and configure them. Here return input data set.") 
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
    data.ground.split <- move::split(data.ground)
    data.night <- foreach(data.groundi = data.ground.split) %do% {
      #print(namesIndiv(data.groundi))
      sunupx <- data.frame(sunriset(coordinates(data.groundi), timestamps(data.groundi), direction="sunrise", POSIXct.out=TRUE))$time + upX*60
      sundownx <- data.frame(sunriset(coordinates(data.groundi), timestamps(data.groundi), direction="sunset", POSIXct.out=TRUE))$time + downX*60
      data.groundi@data <- cbind(data.groundi@data,sunupx,sundownx)
      
      # there are no sunup or sundown during Arctic summer, then only day positions possible "sunupdown". respectively for Arctic winter
      ix <- which(is.na(sunupx) | is.na(sundownx))
      
      ix_ArcSum <- ix[coordinates(data.groundi)[ix,2]>50 & as.POSIXlt(timestamps(data.groundi[ix,]))$mon %in% c(4:8)]
      ix_ArcWin <- ix[coordinates(data.groundi)[ix,2]>50 & as.POSIXlt(timestamps(data.groundi[ix,]))$mon %in% c(10:11,0:2)]
      ix_AntWin <- ix[coordinates(data.groundi)[ix,2]<(-50) & as.POSIXlt(timestamps(data.groundi[ix,]))$mon %in% c(4:8)]
      ix_AntSum <- ix[coordinates(data.groundi)[ix,2]<(-50) & as.POSIXlt(timestamps(data.groundi[ix,]))$mon %in% c(10:11,0:2)]
      
      
      if (is.null(window))
      {
        data.nighti <- data.groundi
        year <- as.POSIXlt(timestamps(data.nighti))$year+1900
        yday <- as.POSIXlt(timestamps(data.nighti))$yday
        data.nighti@data <- cbind(data.nighti@data,year,yday)
      } else
      {
        if (window=="sundownup") #night roosts
        {
          if (length(ix_ArcSum)>0)
          {
            logger.info(paste0("The data set of individual ",namesIndiv(data.groundi)," includes positions above the Arctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcSum)," northern summer positions without 'night' (May-Sep) are taken out for the calculations."))
            data.groundi <- data.groundi[-ix_ArcSum,]
          }
          if (length(ix_AntSum)>0)
          {
            logger.info(paste0("The data set of individual ",namesIndiv(data.groundi)," includes positions below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_AntSum)," southern summer positions without 'night' (Nov-Mar) are taken out for the calculations."))
            data.groundi <- data.groundi[-ix_AntSum,]
          }
          
          if (length(ix_ArcWin)>0 & length(ix_AntWin)>0)
          {
            logger.info(paste0("The data set of individual ",namesIndiv(data.groundi)," includes positions above the Arctic circle and below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcWin)," northern winter positions without 'day' (Nov-Mar) and", length(ix_AntWin), " southern winter positions without 'day' (May-Sep) are kept in the data fully."))
            data.groundi.ND <- data.groundi[-c(ix_ArcWin,ix_AntWin),]
            ix.ND <- seq(along=data.groundi)[-c(ix_ArcWin,ix_AntWin)]
            selND <- which(timestamps(data.groundi.ND)<=data.groundi.ND$sunupx | timestamps(data.groundi.ND)>=data.groundi.ND$sundownx)
            data.nighti <- data.groundi[sort(c(ix.ND[selND],ix_ArcWin,ix_AntWin)),]
          } else if (length(ix_ArcWin)>0 & length(ix_AntWin)==0)
          {
            logger.info(paste0("The data set of individual ",namesIndiv(data.groundi)," includes positions above the Arctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcWin)," northern winter positions without 'day' (Nov-Mar) are kept in the data fully."))
            data.groundi.ND <- data.groundi[-c(ix_ArcWin),]
            ix.ND <- seq(along=data.groundi)[-c(ix_ArcWin)]
            selND <- which(timestamps(data.groundi.ND)<=data.groundi.ND$sunupx | timestamps(data.groundi.ND)>=data.groundi.ND$sundownx)
            data.nighti <- data.groundi[sort(c(ix.ND[selND],ix_ArcWin)),]
          } else if (length(ix_ArcWin)==0 & length(ix_AntWin)>0)
          {
            logger.info(paste0("The data set of individual ",namesIndiv(data.groundi)," includes positions below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_AntWin)," southern winter positions without 'day' (May-Sep) are kept in the data fully."))
            data.groundi.ND <- data.groundi[-c(ix_AntWin),]
            ix.ND <- seq(along=data.groundi)[-c(ix_AntWin)]
            selND <- which(timestamps(data.groundi.ND)<=data.groundi.ND$sunupx | timestamps(data.groundi.ND)>=data.groundi.ND$sundownx)
            data.nighti <- data.groundi[sort(c(ix.ND[selND],ix_AntWin)),]
          } else data.nighti <- data.groundi[timestamps(data.groundi)<=data.groundi$sunupx | timestamps(data.groundi)>=data.groundi$sundownx,]
          
          year <- as.POSIXlt(timestamps(data.nighti))$year+1900
          yday <- as.POSIXlt(timestamps(data.nighti))$yday
          ynight <- yday
          
          ixx <- which(is.na(data.nighti$sundownx))
          if (length(ixx)>0)
          {
            ynight[timestamps(data.nighti[-ixx])>data.nighti$sundownx[-ixx]] <- ynight[timestamps(data.nighti[-ixx])>data.nighti$sundownx[-ixx]]+1
            
            # for Arctic/Antarctic nights the night goes from midday to midday, which depends on the location..
            midday_ixx <- solarnoon(coordinates(data.nighti[ixx]),timestamps(data.nighti[ixx]),POSIXct.out=TRUE)$time
            ynight[timestamps(data.nighti[ixx])>midday_ixx] <- ynight[timestamps(data.nighti[ixx])>midday_ixx]+1
          } else ynight[timestamps(data.nighti)>data.nighti$sundownx] <- ynight[timestamps(data.nighti)>data.nighti$sundownx]+1
          
          # adapt for New Year's Eve
          year[as.POSIXlt(timestamps(data.nighti))$mday==31 & as.POSIXlt(timestamps(data.nighti))$mon==11 & timestamps(data.nighti)>data.nighti$sundownx] <- year[as.POSIXlt(timestamps(data.nighti))$mday==31 & as.POSIXlt(timestamps(data.nighti))$mon==11 & timestamps(data.nighti)>data.nighti$sundownx]+1
          ynight[as.POSIXlt(timestamps(data.nighti))$mday==31 & as.POSIXlt(timestamps(data.nighti))$mon==11 & timestamps(data.nighti)>data.nighti$sundownx] <- 0
          
          data.nighti@data <- cbind(data.nighti@data,year,yday,ynight)
        }
        
        if (window=="sunupdown") # day rests
        {
          if (length(ix_ArcWin)>0)
          {
            logger.info(paste0("The data set of individual ",namesIndiv(data.groundi)," includes positions above the Arctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcWin)," northern winter positions without 'day' (Nov-Mar) are taken out for the calculations."))
            data.groundi <- data.groundi[-ix_ArcWin,]
          }
          if (length(ix_AntWin)>0)
          {
            logger.info(paste0("The data set of individual ",namesIndiv(data.groundi)," includes positions below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_AntWin)," southern winter positions without 'day' (May-Sep) are taken out for the calculations."))
            data.groundi <- data.groundi[-ix_AntWin,]
          }
          
          if (length(ix_ArcSum)>0 & length(ix_AntSum)>0)
          {
            logger.info(paste0("The data set of individual ",namesIndiv(data.groundi)," includes positions above the Arctic circle and below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcSum)," northern summer positions without 'night' (May-Sep) and", length(ix_AntSum), " southern summer positions without 'night' (Nov-Mar) are kept in the data fully."))
            data.groundi.ND <- data.groundi[-c(ix_ArcSum,ix_AntSum),]
            ix.ND <- seq(along=data.groundi)[-c(ix_ArcSum,ix_AntSum)]
            selND <- which(timestamps(data.groundi.ND)>=data.groundi.ND$sunupx & timestamps(data.groundi.ND)<=data.groundi.ND$sundownx)
            data.nighti <- data.groundi[sort(c(ix.ND[selND],ix_ArcSum,ix_AntSum)),]
          } else if (length(ix_ArcSum)>0 & length(ix_AntSum)==0)
          {
            logger.info(paste0("The data set of individual ",namesIndiv(data.groundi)," includes positions above the Arctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_ArcSum)," northern summer positions without 'night' (May-Sep) are kept in the data fully."))
            data.groundi.ND <- data.groundi[-c(ix_ArcSum),]
            ix.ND <- seq(along=data.groundi)[-c(ix_ArcSum)]
            selND <- which(timestamps(data.groundi.ND)>=data.groundi.ND$sunupx & timestamps(data.groundi.ND)<=data.groundi.ND$sundownx)
            data.nighti <- data.groundi[sort(c(ix.ND[selND],ix_ArcSum)),]
          } else if (length(ix_ArcSum)==0 & length(ix_AntSum)>0)
          {
            logger.info(paste0("The data set of individual ",namesIndiv(data.groundi)," includes positions below the Antarctic circle, so there are no sunup or sundown events during some time of the year. The relevant ",length(ix_AntSum)," southern summer positions without 'night' (Nov-Mar) are kept in the data fully."))
            data.groundi.ND <- data.groundi[-c(ix_AntSum),]
            ix.ND <- seq(along=data.groundi)[-c(ix_AntSum)]
            selND <- which(timestamps(data.groundi.ND)>=data.groundi.ND$sunupx & timestamps(data.groundi.ND)<=data.groundi.ND$sundownx)
            data.nighti <- data.groundi[sort(c(ix.ND[selND],ix_AntSum)),]
          } else data.nighti <- data.groundi[timestamps(data.groundi)>=data.groundi$sunupx & timestamps(data.groundi)<=data.groundi$sundownx,]
          
          year <- as.POSIXlt(timestamps(data.nighti))$year+1900
          yday <- as.POSIXlt(timestamps(data.nighti))$yday
          data.nighti@data <- cbind(data.nighti@data,year,yday)
        }
      }
      return(data.nighti)
    }
    names (data.night) <- names(data.ground.split)
    data.night.nozero <- data.night[unlist(lapply(data.night, length) > 0)]
    
    if (length(data.night.nozero)==0) 
    {
      logger.info("Your data contain no night/day positions. No csv overview saved. Return NULL.")
      result <- NULL
    } else 
    {
      data.night <- moveStack(data.night.nozero)
      data.night.df <- as.data.frame(data.night)
      data.night.df.nna <- data.night.df[,-which(apply(data.night.df,2,function (x) all(is.na(x))))] #remove columns with all NA
      
      write.csv(data.night.df.nna,file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"data_rest_selectedTime.csv"),row.names=FALSE) #csv artefakt of all ground and night (or day...) positions
      #write.csv(data.night.df.nna,file = "data_rest_selectedTime.csv",row.names=FALSE) #csv artefakt of all ground and night (or day...) positions
      
      # save all rest positions if is rest by given definition (radius, duration), goes backwards for last night/day rest
      data.night.split <- move::split(data.night)
      
      if (is.null(window))
      {
        prop.rest.df <- data.frame("local.identifier"=character(),"year"=numeric(),"yday"=numeric(),"timestamp.first"=character(),"timestamp.last"=character(),"rest.mean.long"=numeric(),"rest.mean.lat"=numeric(),"rest.nposi"=numeric(),"rest.duration"=numeric(),"rest.radius"=numeric())
      } else
      {
        if (window=="sundownup") prop.rest.df <- data.frame("local.identifier"=character(),"year"=numeric(),"ynight"=numeric(),"timestamp.first"=character(),"timestamp.last"=character(),"rest.mean.long"=numeric(),"rest.mean.lat"=numeric(),"rest.nposi"=numeric(),"rest.duration"=numeric(),"rest.radius"=numeric())
        if (window=="sunupdown") prop.rest.df <- data.frame("local.identifier"=character(),"year"=numeric(),"yday"=numeric(),"timestamp.first"=character(),"timestamp.last"=character(),"rest.mean.long"=numeric(),"rest.mean.lat"=numeric(),"rest.nposi"=numeric(),"rest.duration"=numeric(),"rest.radius"=numeric())
      }
      
      data.rest <- foreach(data.nighti = data.night.split) %do% {
        print(namesIndiv(data.nighti))
        data.resti.df <- as.data.frame(data.nighti)[0,]
        
        year <- unique(data.nighti@data$year)
        for (k in seq(along=year))
        {
          
          data.nightik <- data.nighti[data.nighti@data$year==year[k],]
          
          if (is.null(window))
          {
            night <- unique(data.nightik@data$yday)
          } else
          {
            if (window=="sundownup") night <- unique(data.nightik@data$ynight)
            if (window=="sunupdown") night <- unique(data.nightik@data$yday)
          }
          
          for (j in seq(along=night))
          {
            
            if (is.null(window))
            {
              data.nightikj <- data.nightik[data.nightik@data$yday==night[j],]
            } else
            {
              if (window=="sundownup") data.nightikj <- data.nightik[data.nightik@data$ynight==night[j],]
              if (window=="sunupdown") data.nightikj <- data.nightik[data.nightik@data$yday==night[j],]
            }
            
            last <- Nikj <- length(data.nightikj)
            while (last>1) # as long as first night/day position is not the last
            {
              data.nightikj <- data.nightikj[1:last,]
              backdt <- as.numeric(difftime(timestamps(data.nightikj)[last],timestamps(data.nightikj)[-c(last:Nikj)],units="hours"))
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
                m <- colMeans(coordinates(data.sel))
                dp0 <- distVincentyEllipsoid(m,coordinates(data.sel))
                p0 <- coordinates(data.sel)[min(which(dp0==max(dp0))),]
                dp1 <- distVincentyEllipsoid(p0,coordinates(data.sel))
                p1 <- coordinates(data.sel)[min(which(dp1==max(dp1))),]
                maxdist <- distVincentyEllipsoid(p0,p1)
                
                if (maxdist<radius)
                {
                  ## check if already longer at this rest site
                  mid <- midPoint(p0,p1)
                  data.bef <- data.rem
                  if (length(data.bef)>=1)
                  {
                    dist.bef <- distVincentyEllipsoid(mid,coordinates(data.bef))
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
                  
                  data.selx.df <- as.data.frame(data.selx)
                  
                  time0 <- min(timestamps(data.selx))
                  timeE <- max(timestamps(data.selx))
                  durx <- as.numeric(difftime(timeE,time0,unit="hour"))
                  radx <- max(distVincentyEllipsoid(mid,coordinates(data.selx)))
                  
                  if (durx>=duration & radx<=radius) #added this condition to only save rest sites of given duration (if this condition is left out also rest site with shorter duration are given back)
                  {
                    data.resti.df <- rbind(data.resti.df,data.selx.df)
                    
                    if (is.null(window))
                    {
                      prop.rest.df <- rbind(prop.rest.df,data.frame("local.identifier"=namesIndiv(data.selx),"year"=data.selx.df$year[1],"yday"=data.selx.df$yday[1],"timestamp.first"=as.character(time0),"timestamp.last"=as.character(timeE),"rest.mean.long"=mid[1,1],"rest.mean.lat"=mid[1,2],"rest.nposi"=length(data.selx),"rest.duration"=durx,"rest.radius"=radx))
                    } else
                    {
                      if (window=="sundownup") prop.rest.df <- rbind(prop.rest.df,data.frame("local.identifier"=namesIndiv(data.selx),"year"=data.selx.df$year[1],"ynight"=data.selx.df$ynight[1],"timestamp.first"=as.character(time0),"timestamp.last"=as.character(timeE),"rest.mean.long"=mid[1,1],"rest.mean.lat"=mid[1,2],"rest.nposi"=length(data.selx),"rest.duration"=durx,"rest.radius"=radx))
                      if (window=="sunupdown") prop.rest.df <- rbind(prop.rest.df,data.frame("local.identifier"=namesIndiv(data.selx),"year"=data.selx.df$year[1],"yday"=data.selx.df$yday[1],"timestamp.first"=as.character(time0),"timestamp.last"=as.character(timeE),"rest.mean.long"=mid[1,1],"rest.mean.lat"=mid[1,2],"rest.nposi"=length(data.selx),"rest.duration"=durx,"rest.radius"=radx))
                    }
                  }
                  
                  #A) if only need one, i.e. the last, rest site per day/night
                  #break 
                  
                  #B) if need all rest sites (irregardless if previous rest site was saved, check for another one in the remaining data)
                  data.nightikj <- data.remx
                  last <- Nikj <- length(data.nightikj) #this can be zero, will be stop indication in while() condition
                  
                } else last <- last-1 #shift one time step
              } else last <- last-1 # shift one time step also if not enough data in previous Xh time frame
            } #while
          } #for j
        } #for k
        if (dim(data.resti.df)[1]>0) 
        {
          o <- order(data.resti.df$timestamp)
          data.resti <- move(x=data.resti.df$location_long[o],y=data.resti.df$location_lat[o],time=as.POSIXct(data.resti.df$timestamp[o]),data=data.resti.df[o,],sensor=data.resti.df$sensor[o],animal=data.resti.df$local_identifier[o]) 
        } else data.resti <- NULL
      }
      names(data.rest) <- names(data.night.split)
      data.rest.nozero <- data.rest[unlist(lapply(data.rest, length) > 0)] #remove IDs with no data
      
      if (length(data.rest.nozero)==0) 
      {
        logger.info("Your output file contains no positions. No csv overview saved. Return NULL.")
        result <- NULL
      } else 
      {
        result <- moveStack(data.rest.nozero)
        write.csv(prop.rest.df,file = paste0(Sys.getenv(x = "APP_ARTIFACTS_DIR", "/tmp/"),"rest_overview.csv"),row.names=FALSE) #csv artefakt
        #write.csv(prop.rest.df,file = "rest_overview.csv",row.names=FALSE)
        # note that all timestamps are UTC!
      }
    }
  }
  return(result)
}

  
  
  
  
  
  
  
  
  
  
