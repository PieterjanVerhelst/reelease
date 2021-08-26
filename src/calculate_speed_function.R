# Function to calculate movement speed between consecutive stations. It is based on an 'under the hood' function in Hugo Flavio's actel package
# https://github.com/hugomflavio/actel
# For more questions, ask Hugo
# hflavio@wlu.ca


movementSpeeds <- function(movements, speed.method, dist.mat) {
  #appendTo("debug", "Running movementSpeeds.")
  movements$swimtime_s[1] <- NA
  movements$swimdistance_m[1] <- NA
  movements$speed_m_s[1] <- NA
  if (nrow(movements) > 1) {
    capture <- lapply(2:nrow(movements), function(i) {
      if (movements$station_name[i] != movements$station_name[i - 1] & all(!grep("^Unknown$", movements$station_name[(i - 1):i]))) {
        if (speed.method == "last to first"){
          a.sec <- as.vector(difftime(movements$arrival[i], movements$departure[i - 1], units = "secs"))
          my.dist <- dist.mat[movements$station_name[i], gsub(" ", ".", movements$station_name[i - 1])]
        }
        if (speed.method == "last to last"){
          a.sec <- as.vector(difftime(movements$departure[i], movements$departure[i - 1], units = "secs"))
          my.dist <- dist.mat[movements$station_name[i], gsub(" ", ".", movements$station_name[i - 1])]
        }
        movements$swimtime_s[i] <<- round(a.sec, 6)
        movements$swimdistance_m[i] <<- round(my.dist, 6)
        movements$speed_m_s[i] <<- round(my.dist/a.sec, 6)
        rm(a.sec, my.dist)
      } else {
        movements$speed_m_s[i] <<- NA_real_
      }
    })
  }
  return(movements)
}


