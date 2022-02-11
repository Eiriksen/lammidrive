#v2

# This function downloads the list of dead individuals (by PIT) from the online lab journal


#' get_mortalities
#'
#' Simplified function for defining the y axis with settings for tickmarks and labels.
#' The function is used as a ggplot object.
#' @param datepit_file the location of a datepit file to use for converting pit-tags to IDs
#' @export
#' @examples lammidrive::get_mortalities(datepit_file="lookup - lammisalmon pit to ID - March 2021.txt").
get_mortalities = function(datepit_file){
  require(tidyverse)
  if (missing(datepit_file)) datepit_file = "lookup - lammisalmon pit to ID - March 2021.txt"
  sheet <-
    googlesheets4::read_sheet(
      "https://docs.google.com/spreadsheets/d/1xTHxjR7bvZvMY_GvzJAA3EKjtswzdKPN17gY1u84WNA/edit#gid=1602740998",
      range="Mortalities"
    ) %>%
    select("...1","...5") %>%
    rename(date="...1",pit="...5") %>%
    filter(date!="Date") %>%
    filter(!is.na(pit)) %>%
    mutate(
      pit = toupper(pit),
      date = date %>% as.character() %>% dmy(),
      live=0
    ) %>%
    datepit::datepit_to_ID(tb_pit = read_delim(datepit_file))
  message("Mortality sheet obtained")
  sheet
}


#' get_tissuesamples
#'
#' obtains the table with the tissue sample data from the lammi salmon google drive
#' @export
#' @examples tb_samples <- lammidrive::get_tissuesamples().
get_tissueSamples = function(datepit_file){
  require(tidyverse)
  sheet <-
    googlesheets4::read_sheet(
      "https://docs.google.com/spreadsheets/d/1kVB87s3rcLkgx0pj9DKAiMD2RmxYRiXB"
      )%>%
    mutate(
      date = dmy(date),
      pit = toupper(pit)
      ) %>%
    datepit::datepit_to_ID(tb_pit = read_delim(datepit_file))
  message("tissue samples sheet obtained")
  return(sheet)
}





#' get_temperatures
#'
#' obtains the tale with the temperature logs (the manual ones) from the lammi salmon lab journal
#' @param extend A date. Lets you extend the temperature data into the future (assumes the coming year will be exactly like the last one)
#' @export
#' @examples tb_temps <- lammidrive::get_temperatures().
driveGet_temperatures = function(extend){
  require(tidyverse)

  sheet <- googlesheets4::read_sheet(
    "https://docs.google.com/spreadsheets/d/1xTHxjR7bvZvMY_GvzJAA3EKjtswzdKPN17gY1u84WNA/",
    range="Temp records",
    skip=2,
    col_types="cnnnnnnnnnnnnnnnnnnnnc"
    )

  # get the temperatures from the warm and cold tanks.  # for days without a reading, use yesterdays reading
  sheet$hot  <-  rowMeans( data.frame(sheet$Temp_T29_Arvo, sheet$Temp_T29_Man), na.rm=T ) %>% zoo::na.locf() %>% as.numeric()
  sheet$cold <-  rowMeans( data.frame(sheet$Temp_T30_Arvo, sheet$Temp_T30_Man), na.rm=T ) %>% zoo::na.locf() %>% as.numeric()
  sheet$date <-  dmy(sheet$date)

  # Extend dataset until december 2020
  if (!is.na(extend)){
    sheet <- sheet %>% tempData_extend(date=extend,columns=c("hot","cold"))
  }

  return(sheet)
}


driveGet_feed = function(){
  sheet <- read.data("https://docs.google.com/spreadsheets/d/1xTHxjR7bvZvMY_GvzJAA3EKjtswzdKPN17gY1u84WNA/",
                     skip=6,
                     range="Feed BT",
                     col_types="cnnnnnnnnnnnnnnnnnnnnnnnnnnnnn"
                     ) %>%
            mutate(date=dmy(date))


  # feedDays: One day per row, one column per tank: How much feed per tank per day
  dates <- datesBetween(sheet$date[1],today())
  df_feedDays <- data.frame(
                 date=rep(dates,12),
                 tank=c(rep(c("T21","T22","T23","T24","T25","T26","T27","T28","T29","T30","T31","T32"), each=length(dates)))
                 )

  # Thus, for each combination of tank and date:
  df_feedDays <- df_feedDays %>%
                 group_by(tank,date) %>%
                 mutate(feed = feedData_convert_feedPrDate(sheet,tank,date)) %>%
                 na.omit()

  return(df_feedDays)
  }

#extends a temperature dataset using temperature data from previous year
#df_temp has columnds "date", as well as those specified by the parameter "columns"  that contains temperatures, for-
#example from different channels
#parameter "date" is the date of etension
tempData_extend = function(df_temp, date, columns){
  require(tidyverse)

  date=dmy(date)
  # get the last date of the temperature dataset
  date_last <- df_temp$date[nrow(df_temp)]
  message("Last recorded date is: ", date_last)

  # get the days between that date and the extension date and make this into a dataframe (rows being dates)
  betweenDates <- datesBetween(date, date_last)
  df_new <- data.frame(date=betweenDates)
  df_new2<- data.frame(date=betweenDates)
  message("a")
  # for each column we want to extend:
  for (i in columns){
    # apply over the list of days, return the temperature of that date one year ago
    df_new2[[i]] = apply(df_new,MARGIN=1,FUN=function(j){
      day = ymd(j[["date"]])
      nyears=1
      if (day-date_last > 365) nyears=2
      day_prev = df_temp %>% filter(date == (day-years(nyears)))
      temp_prev = day_prev[[i]]
      return(temp_prev)
    })
  }
  print(df_new2)

  for (i in columns){
    df_new[[i]] = as.numeric(df_new2[[i]])
  }
  message("Data extended to ",date)
  # rbind the new dataset to the old one
  return(bind_rows(df_temp,df_new))
}


#' @title List dates between two dates
#' @description Function for gaining a list of all dates between two dates. Sollution to this problem by user "yifyan" at stackoverflow.com  https://stackoverflow.com/questions/14450384/create-a-vector-of-all-days-between-two-dates. date_a and date_b must be lubridate-date-objects
#' @export
datesBetween = function(date_a,date_b) {
  require(lubridate)

  n_days <- interval(date_a,date_b)/days(1)
  dates = date_a + days(0:n_days)
  return(dates)
}

