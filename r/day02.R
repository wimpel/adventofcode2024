library(data.table)
#part 1
DT  <- fread("./input/day02_sample.txt", fill = TRUE)
DT  <- fread("./input/day02.txt", fill = TRUE)
# tests
test1 <- function(input) {
  input.diff <- diff(input)
  return(all(input.diff < 0) | all(input.diff > 0))
}
test2 <- function(input) {
  input.diff <- diff(input)
  !(any(abs(input.diff) > 3) | any(abs(input.diff) < 1))
}
# perform
sum(sapply(transpose(DT), function(x) {
  x <- x[!is.na(x)]
  test1(x) * test2(x)
}))

# part 2 BRUTE FORCE
sapply(data.table::transpose(DT), function(x) {
  #x <- transpose(DT)[[1]]
  x <- x[!is.na(x)]
   if (test1(x) & test2(x)) {
     return(TRUE)
   } else {
     #start removing numbers
     for (i in seq.int(x)) {
       if (test1(x[-i]) & test2(x[-i])) {
         return(TRUE)
       } else { 
         next
       }
     }
   } 
  return(FALSE)
}) |> sum()

