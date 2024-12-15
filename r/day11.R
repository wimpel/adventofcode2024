library(data.table)

input <- as.integer64(c(41078, 18, 7, 0, 4785508, 535256, 8154, 447))



library(memoise)

input <- c(125,17)



max(input[!(floor(log10(input)) + 1) %% 2 == 0], na.rm = TRUE) * 2024


DT <- data.table(int = 0:(41078 * 2024))


input <- integer64(length = 2^25)
1 * 2024

20 24    2 0 2 4 4048 1 4048 9096        40 48  90 96



newinput <- function(input) {
    lapply(input, function(x) {
    if (x == 0) return(1)
    if (nchar(x) %% 2 == 0) {
      left <- substr(x, start = 0, stop = nchar(x) / 2) |>
        as.numeric()
      right <- substr(x, start = nchar(x) / 2 + 1, stop = nchar(x)) |>
        as.numeric()
      return(c(left,right))
    }
    return(x * 2024)
  }) |> unlist()
}

for (i in 1:75) {
  input <- newinput(input)
  print(i)
}

length(input)
