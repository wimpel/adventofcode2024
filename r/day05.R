library(data.table)
library(quickcode)
# inputfile <- "./input/day05_sample.txt"
inputfile <- "./input/day05.txt"
input_divide <- which(readLines(inputfile) == "")
rules <- fread(inputfile, nrows = input_divide - 1, col.names = c("print", "before"))
updates <- fread(inputfile, skip = input_divide, fill = TRUE)

# part 1
# split rules to a list based on number
L <- split(rules, by = "print")
# perform if the update passes
checks <- sapply(transpose(updates), function(x) {
  x <- rev(x[!is.na(x)])
  sapply(1:length(x), function(i) {
    if (quickcode::has.error(L[names(L) == x[i]][[1]]$before)) return(TRUE)
    forbidden <- L[names(L) == x[i]][[1]]$before
    sum(forbidden %in% x[i:length(x)]) == 0
  }) |> prod() == 1
})
# get middle number of passed updates
sapply(transpose(updates[checks,]), function(x) {
  x <- x[!is.na(x)]
  return(x[ceiling(length(x)/2)])
}) |> sum()

#part 2
errors <- transpose(updates[!checks,])
repaired <- sapply(errors, function(x) {
  x <- rev(x[!is.na(x)])
  for (i in 1:(length(x) - 1)) {
    repeat {
      error_found <- tail(intersect(x[(i+1):length(x)], rules[print == x[i], ]$before), 1)
      if (length(error_found) < 1) break
      x_new <- append(x[-i], x[i], after = which(x == error_found) - 1)
      x <- x_new
    }
  }
  return(x)
})

sapply(repaired, function(x) {
  x <- x[!is.na(x)]
  return(x[ceiling(length(x)/2)])
}) |> sum()



