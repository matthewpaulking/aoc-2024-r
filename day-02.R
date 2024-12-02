library(tidyverse)

input = read_lines('data/day-02.txt')
reports = map(input, str_split, ' ') %>% reduce(c) %>% map(as.integer)

diff = function(x) {
  diffed_vector = c()
  for(i in 1:length(x)) {
    if(i == length(x)) {
      break
    }
    diffed_vector[i] = x[i] - x[i+1]
  }
  return(diffed_vector)
}

is_monotonic = function(x) {
  if(any(x == 0)) {
    return(FALSE)
  }
  if(x[1] < 0) {
    return (all(x < 0))
  } else {
    return (all(x > 0))
  }
}

levels_safe = function(x) {
  # takes in the diff
  return(all(abs(x) <= 3))
}

# check all reports and sum

safe_ones = map_int(reports, function(report) {
  d = diff(report)
  return(
    is_monotonic(d) & levels_safe(d)
  )
}) 

answer_1 = safe_ones %>% sum()


## Puzzle 2

unsafe_ones = reports[!safe_ones]


# Brute force time, couldn't figure out anything more elegant

dampened = rep(FALSE, length(unsafe_ones))
for(j in 1:length(unsafe_ones)){
  for(i in 1:length(unsafe_ones[[j]])) {
    r = unsafe_ones[[j]][-i]
    d = diff(r)
    a = is_monotonic(d) & levels_safe(d)
    if(a) {
      dampened[j] = TRUE
    }
  }
}
answer_2 = answer_1 + sum(dampened)
