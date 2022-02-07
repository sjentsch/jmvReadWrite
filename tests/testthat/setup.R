ts2w <- function(ts = NULL) {
    reshape(data.frame(Year = as.integer(floor(time(ts))), Month = as.integer(round((time(ts) %% 1) * 12)) + 1, X = as.matrix(ts)), idvar = "Year", timevar = "Month", direction = "wide")
}

ts2l <- function(ts = NULL) {
    data.frame(Year = floor(as.integer(time(ts))), Month = as.integer(round((time(ts) %% 1) * 12)) + 1, X = as.matrix(ts))
}
