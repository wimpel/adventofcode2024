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


# part 2  NOT WORKING
test2.remove1 <- function(input) {
  input.diff <- diff(input)
  test21 <- sum(abs(input.diff) > 3) == 1
  test22 <- sum(abs(input.diff) < 1) == 1
  if (xor(test21, test22)) {
    if (test21) {
      return( test2(input[-(which(abs(input.diff) > 3) + 1)]) )
    } else {
      return( test2(input[-(which(abs(input.diff) < 1) + 1)]) )
    }
  } else {
    return(FALSE)
  }
}

test1.remove1 <- function(input) {
  input.diff <- diff(input)
  test11 <- length(input.diff[input.diff >= 0]) == 1 
  test12 <- length(input.diff[input.diff <= 0]) == 1
  if (xor(test11, test12)) {
    if (test11) {
      check <- input[-(which(input.diff >= 0) + 1)]
      return(  test2(input[-(which(input.diff >= 0) + 1)]) )
    } else {
      check <- input[-(which(input.diff <= 0) + 1)]
      return( test2(input[-(which(input.diff <= 0) + 1)]) )
    }
  } else {
    return(FALSE)
  }
}

# perform
sapply(transpose(DT), function(x) {
  #browser()
  x <- x[!is.na(x)]
  if (test1(x)) {
    if (test2(x)) {
      return(TRUE)
    } else {
      return(test2.remove1(x))
    }
  } else {
    return(test1.remove1(x))
  }
}) |> sum()
  
  
