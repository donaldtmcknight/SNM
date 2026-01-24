#' Distance moved over time intervals
#'
#' For a series of time intervals, calculate the distance moved between each point and the previous point matching the given time interval.
#' @param data (list) Output from \code{\link{prep.data}}.
#' @param coords (data.frame) Data frame containing coordinates for animal locations. It must include a column of animal IDs, columns of latitude and longitude (in decimal degrees projected to the same system as the rest of the data), and a column of dates/times (in POSIX format). All other columns will be ignored.
#' @param lon.name (character) For the \code{coords} object, the name of the column of longitudes (in quotes). Default = "lon"
#' @param lat.name (character) For the \code{coords} object, the name of the column of latitudes (in quotes). Default = "lat"
#' @param id.name (character) For the \code{coords} object, the name of the column of animal identities (in quotes). Default = "id"
#' @param date.time.name (character) For the \code{coords} object, the name of the column of dates and times (in a POSIX format). Default = "date.time"
#' @param units (character) One of "secs", "mins", "hours", or "days" specifying the units for all time and sensitivity inputs and outputs
#' @param time.diff (numeric) The amount of time to separate each analysis of distance (i.e., a time interval based on  \code{units}). Default = 1
#' @param diff.max (numeric) The maximum time interval to analyze. If NULL, it will use the maximum possible interval. Default = \code{NULL}
#' @param sensitivity.min (numeric) The minimum (starting) buffer (+/-) around \code{time.diff} (i.e., the buffer when \code{time.diff} = 1). Default = 0.1
#' @param sensitivity.max (numeric) The maximum buffer (+/-) around \code{time.diff}. Default = 0.1
#' @param sensitivity.change (numeric) The amount that the sensitivity should change (both + and -) each time interval. Default = 0
#' @param custom.times (numeric vector) Optional vector of custom time intervals to analyze (overrides time.diff and diff.max)
#' @param custom.sensitivity (numeric vector) Optional vector of custom sensitivities to apply to time intervals (overrides all other sensitivity arguments)
#'
#' @return A data frame containing the outputs for each point at each time interval.
#'
#' Columns \code{lat},\code{lon},\code{id}, and \code{date.time} are for the ending position for the interval analyzed
#'
#' Column \code{dist} = distance moved (m) during a given interval (during the \code{actual.diff} prior to the specified point)
#'
#' Column \code{actual.diff} = the exact time between the ending position and the previous point to which it was paired (in the time units specified by \code{units})
#'
#' Column \code{time.diff} = the time interval specified by \code{time.diff} (in the time units specified by \code{units})
#'
#' Column \code{sensitivity} = the time buffer (+/-) around the \code{time.diff} for the given point (in the time units specified by \code{units})
#'
#' \strong{Note}: Although each endpoint will only be matched with one previous point for a given time interval, there may be multiple different end points within that time interval. For example, if \code{time.diff} = 1, \code{sensitivity.min} = .1, \code{units} = "days", and for a given animal, there were points at "2023-06-01 12:00:00", "2023-06-02 12:00:00" and "2023-06-02 12:30:00", the results will include a row with "2023-06-02 12:00:00" as the endpoint and a row with "2023-06-02 12:30:00" as the endpoint (both showing the distance from "2023-06-01 12:00:00"). Depending on the data set and questions being asked, that may create undesirable pseudoreplication and you may need to filter the results based on date or some other criterion.
#'
#' \strong{Note}: When reading the \code{time.diff} column, remember that points are not necessarily sequential. Thus, days = 1, 2, 3 does not necessarily indicate movement over days 1, 2, and 3 of the study, rather it is movement over that many days, regardless of when in the study the pairs of points occurred. See the vignette for suggestions on visualizing and analyzing these data.
#'
#' @details
#' This function calculates movement over given intervals of time. For each coordinate (going from oldest to newest) it looks for matches to previous coordinates where the time difference between them matches the specified interval.
#'
#' \code{time.diff}, \code{diff.max}, and \code{units} specify the time intervals. \code{time.diff} specifies the amount of additional time in each sequential interval, with \code{diff.max} specifying the largest time interval. For example, if \code{time.diff} = 6, \code{diff.max}, = 24 and \code{units} = "hours" movement will be calculated over 6 hours, 12 hours, 18 hours, and 24 hours.
#'
#' Points can be used for multiple time intervals. For example, if \code{time.diff} = 1 and \code{units} = "days" and an animal was tracked at 12:00 on the 1st, 2nd, 3rd, 5th, and 6th of January, the following pairs of points would be used for each time interval:
#' \itemize{
#'  \item 1 day interval = days 1&2, 2&3, 5&6
#'  \item 2 day interval = days 1&3, 3&5
#'  \item 3 day interval = days 2&5, 3&6
#'  \item 4 day interval = days 1&5, 2&6
#'  \item 5 day interval = days 1&6
#'  }
#'
#' \code{sensitivity.min}, \code{sensitivity.max}, and \code{sensitivity.change} control a buffer of time (+/-) around each interval. This allows for situations such as radio telemetry where points will rarely match an exact time interval. For example, if the time interval is 24 hours with a 1 hour (+/-) buffer, then for each point, it will look for corresponding points 23-25 hours previously.
#'
#' In many cases, it may be desirable to increase the buffer slightly as the size of the time intervals increase. The \code{sensitivity.max} and \code{sensitivity.change} arguments control this. \code{sensitivity.max} specifies the maximum value a buffer can have, and \code{sensitivity.change} controls the amount of buffer increase for new time interval.
#'
#' \strong{Example}: If:
#'  \itemize{
#'   \item \code{time.diff} = 1
#'   \item \code{diff.max} = 6
#'   \item \code{units} = "days"
#'   \item \code{sensitivity.min} = 0.1
#'   \item \code{sensitivity.max} = 0.5
#'   \item \code{sensitivity.change} = 0.1
#'   }
#' The function will use intervals of 1, 2, 3, 4, 5, 6 days, and the corresponding buffers will be +/- 0.1, 0.2, 0.3, 0.4, 0.5, 0.5 days.
#'
#' Note that the units are the same for all entries, and the buffer caps at +/- 0.5 because of \code{diff.max}, Thus, for a given coordinate, it will look for corresponding coordinates 0.9-1.1, 1.8-2.2, 2.7-3.3, 3.6-4.4, 4.5-5.5, and 5.5-6.5 days previously.
#'
#' To remove the buffer entirely, set \code{sensitivity.min} = 0, \code{sensitivity.max} = 0, and \code{sensitivity.change} = 0
#'
#' To set a fixed buffer, set \code{sensitivity.min} and \code{sensitivity.max} to the same value and set \code{sensitivity.change} = 0
#'
#' \code{custom.times} and \code{custom.sensitivity} allow users to supply vectors of times and sensitivities that change at irregular intervals (e.g., 1, 3, 5, 10 days). These arguments override other time and sensitivity arguments.
#'
#' \strong{Note}: The default sensitivity values are largely arbitrary, and you should carefully select your own based on your study questions and system.
#'
#' \strong{Note}: For a given coordinate and time interval, only one match will be allowed with a previous coordinate. If multiple matches are available within a time interval, only the one that is closest to the specified interval will be retained (in the case of ties, the first is arbitrarily retained). For example, if the time interval is 1 day with a +/- .5 buffer and the following points are available: 2023-01-01 12:00:00, 2023-01-01 13:00:00, and 2023-01-02 12:00:00, then when calculating the distance for 2023-01-02 12:00:00, it will only calculate the distance to the point at 2023-01-01 12:00:00 because it was closer to the specified time interval of 1 day.
#'
#' @export
dist.over.time <- function(data,coords,lon.name="lon",lat.name="lat",id.name="id",date.time.name="date.time",units="days",time.diff=1,diff.max=NULL,sensitivity.min=.1,sensitivity.max=.1,sensitivity.change=0,custom.times=NULL,custom.sensitivity=NULL){

  colnames(coords)[which(colnames(coords)==lon.name)] <- "lon"
  colnames(coords)[which(colnames(coords)==lat.name)] <- "lat"
  colnames(coords)[which(colnames(coords)==id.name)] <- "id"
  colnames(coords)[which(colnames(coords)==date.time.name)] <- "date.time"

  coords <- coords[,c("lon","lat","id","date.time")]

  coords <- coords[with(coords, order(id, date.time)), ]

  #if not diff.max is supplied, it calculates it as the maximum spread of dates.times in the data set
  if(is.null(diff.max)==T){diff.max <- as.numeric(difftime(max(coords$date.time),min(coords$date.time),units=units))}

  if(is.null(custom.times)==T){hs <- seq(time.diff,diff.max,time.diff)
    print(paste("Calculating every",time.diff,units,"from",time.diff,units,"to",diff.max,units))}else{
      hs <- custom.times
      print("Using custom sequence of times")} #sequence of time intervals

  if(is.null(custom.sensitivity)==T){
    if(is.null(custom.times)==T){ #checks that times are not custom
    sens <- seq(sensitivity.min,sensitivity.max,sensitivity.change) #sequence of sensitivities
  if(length(sens)>length(hs)){sens <- sens[1:length(hs)]}else{ #is sequence is longer than sequence of times, subset accordingly, otherwise, continue sequence with required number of sensitivity.max
    sens <- c(sens,rep(sensitivity.max,length(hs-length(sens))))}
    print(paste("Sensitivity starts at +/-",sensitivity.min,units,"and increases by",sensitivity.change,units,"every",time.diff,units))
  }else{ #if times are custom
    print(paste("Sensitivity starts at +/-",sensitivity.min,units,"and increases by",sensitivity.change,units,"every",time.diff,units))
      sens <- sensitivity.change*hs-sensitivity.change+sensitivity.min
      if(length(which(sens > sensitivity.max))> 0){
      sens[which(sens > sensitivity.max)] <- sensitivity.max}}}else{ #if custom sensitivity is being used
    sens <- custom.sensitivity
    if(length(custom.sensitivity) != length(hs)){stop(paste('length of custom.sensitivity (',length(custom.sensitivity),') does not match length of time intervals (',length(hs),')',sep=""))}
    print("Using custom sequence of sensitivities")}

  data2 <- data
  res.a <- vector("list",length(unique(coords$id))) #will be filled with time (h) to previous point
  for(a in 1:length(unique(coords$id))){

    id.a <- coords[coords$id == unique(coords$id)[a],] #subset by individual

    print(paste("Analyzing individual",id.a$id[1]))

    res.b <- vector("list",nrow(id.a))
    for(b in 2:nrow(id.a)){

      id.b <- id.a[1:b,]

      #calculate time difference from all previous points
      difftime.b <- difftime(id.b[nrow(id.b),"date.time"],id.b[-nrow(id.b),"date.time"],units=units)

      res.c <- vector("list",length(hs))
      for(c in 1:length(hs)){

        time.diff.c <- hs[c]
        sens.c <- sens[c]

        #subset to rows within the specified time frame
        matches <- which(difftime.b >= (time.diff.c-sens.c) & difftime.b <= (time.diff.c+sens.c))
        row.c <- id.b[matches,]
        difftime.c <- difftime.b[matches]

        if(nrow(row.c) > 0){

          row.c <- row.c[which(abs(difftime.c-time.diff.c) == min(abs(difftime.c-time.diff.c))),] #if multiple options are present it subsets to the one closest to the set time.diff

          row.c <- row.c[1,] #in case of ties, in time it arbitrarily goes with the first one

          row2.c <- id.a[b,] #extracts the row we actually want to make comparisons to

          row2.c$dist <- if(row2.c$lat == row.c$lat & row2.c$lon == row.c$lon){0}else{ #if coordinates are the same, movement is 0, otherwise uses the function below to calculate distance between current point and previous
            calc.stream.dist(p1 = row2.c[1,c("lon","lat")],p2 = row.c[1,c("lon","lat")],data=data2)}#ends else
          row2.c$actual.diff <- as.numeric(difftime(row2.c$date.time,row.c$date.time,units = units)) #calculates difference in time between points (in hours)

          row2.c$time.diff <- time.diff.c
          row2.c$sensitivity <- sens.c

          res.c[[c]] <- row2.c} #ends if(nrow(row.c))
        }#ends for(c in )


      res.b[[b]] <- do.call("rbind.data.frame",res.c)

      }#end for b

    res.a[[a]] <- do.call("rbind.data.frame",res.b)

  } #end for a

  result <- do.call("rbind.data.frame",res.a)

    result
}#end function
