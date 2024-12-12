library(data.table)
#input <- readLines("./input/day06_sample.txt")
input <- readLines("./input/day06.txt")

m <- input |>
  strsplit("") |>
  unlist() |>
  matrix(ncol = nchar(input[1]), byrow = TRUE)

# part 1
start.pos <- as.data.table(which(m == "^", arr.ind = TRUE))
obst.pos <- as.data.table(which(m == "#", arr.ind = TRUE))[,id := .I]

steps.taken <- function(curpos) {
  #browser()
  if (direction %in% c("up")) {
    found <- obst.pos[col == curpos$col & row < curpos$row, ]$row
    if (length(found) == 0) {
      leave <<- TRUE
      return(data.frame(row = c(curpos$row:1), col = curpos$col, dir = direction, towards = 9999999))
    }
    newrow <- found[which.min(abs(found - curpos$row))] + 1
    obst.id <- obst.pos[row == newrow - 1 & col == curpos$col, ]$id
    old.direction <- direction
    direction <<- "right"
    return(data.frame(row = c(curpos$row:newrow), col = curpos$col, dir = old.direction, towards = obst.id))
  }
  if (direction %in% c("right")) {
    found <- obst.pos[row == curpos$row & col > curpos$col, ]$col
    if (length(found) == 0) {
      leave <<- TRUE
      return(data.frame(row = curpos$row, col = c(curpos$col:ncol(m)), , dir = direction, towards = 9999999))
    }
    newcol <- found[which.min(abs(found - curpos$col))] - 1
    obst.id <- obst.pos[row == curpos$row & col == newcol + 1, ]$id
    old.direction <- direction
    direction <<- "down"
    return(data.frame(row = curpos$row, col = c(curpos$col:newcol), dir = old.direction, towards = obst.id))
  }
  if (direction %in% c("down")) {
    found <- obst.pos[col == curpos$col & row > curpos$row, ]$row
    if (length(found) == 0) {
      leave <<- TRUE
      return(data.frame(row = c(curpos$row:nrow(m)), col = curpos$col, dir = direction, towards = 9999999))
    }
    newrow <- found[which.min(abs(found - curpos$row))] - 1
    obst.id <- obst.pos[row == newrow + 1 & col == curpos$col, ]$id
    old.direction <- direction
    direction <<- "left"
    return(data.frame(row = c(curpos$row:newrow), col = curpos$col, dir = old.direction, towards = obst.id))
  }
  if (direction %in% c("left")) {
    found <- obst.pos[row == curpos$row & col < curpos$col, ]$col
    if (length(found) == 0) {
      leave <<- TRUE
      return(data.frame(row = curpos$row, col = c(curpos$col:1), dir = direction, towards = 9999999))
    }
    newcol <- found[which.min(abs(found - curpos$col))] + 1
    obst.id <- obst.pos[row == curpos$row & col == newcol - 1, ]$id
    old.direction <- direction
    direction <<- "up"
    return(data.frame(row = curpos$row, col = c(curpos$col:newcol), dir = old.direction, towards = obst.id))
  }
}

direction <- "up"
leave <- FALSE
covered <- start.pos[, dir := "up"][, towards := 0]
repeat {
  covered <- rbind(covered, steps.taken(tail(covered,1)))
  if (leave) break
}
# part 1
nrow(unique(covered, by = c("row", "col")))


# part 2

# let op, je hoeft niet meteemn een obstakel te zien.... 65000 stappen en dan 
#  nog gene exit >> loop

m2 <- copy(m)
m2[m2 == "#"] <- 1:nrow(obst.pos)

covered[dir == "up", look := "right"]
covered[dir == "right", look := "down"]
covered[dir == "down", look := "left"]
covered[dir == "left", look := "up"]

covered[look == "right", towards.obst := obst.pos[.SD, id, on = .(row = row, col = col), roll = -Inf]][]
covered[look == "down", towards.obst := obst.pos[.SD, id, on = .(col = col, row = row), roll = -Inf]][]
covered[look == "left", towards.obst := obst.pos[.SD, id, on = .(row = row, col = col), roll = Inf]][]
covered[look == "up", towards.obst := obst.pos[.SD, id, on = .(col = col, row = row), roll = Inf]][]

covered[, stepid := .I]
setkey(covered, stepid)

covered[covered, new.obs := {
  temp <- covered[stepid < i.stepid & towards == i.towards.obst & dir == i.look,]
  list(temp$stepid[1])
}, by = .EACHI][]

covered[dir == "up" & !is.na(new.obs), `:=`(row.obs = row - 1, col.obs = col)]
covered[dir == "down" & !is.na(new.obs), `:=`(row.obs = row + 1, col.obs = col)]
covered[dir == "left" & !is.na(new.obs), `:=`(row.obs = row, col.obs = col - 1)]
covered[dir == "right" & !is.na(new.obs), `:=`(row.obs = row, col.obs = col + 1)]

uniqueN(covered[!is.na(new.obs),], by = c("row.obs", "col.obs"))



