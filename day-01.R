library(tidyverse)

# Puzzle 1

input = read_delim('data/day-01.txt', '   ', col_names = c('list_1', 'list_2'))

input$list_1 = sort(input$list_1)
input$list_2 = sort(input$list_2)

diff = abs(input$list_1 - input$list_2)

answer_1 = sum(diff)  # 3714264

# Puzzle 2

count_in_list_2 = map_int(input$list_1, function(x) {
  return (sum(input$list_2 == x))
})

answer_2 = sum(input$list_1 * count_in_list_2)
