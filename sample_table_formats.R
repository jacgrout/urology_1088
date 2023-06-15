p <- percent(c(0.1, 0.02, 0.03, 0.12))
p

## [1] 10.00% 2.00%  3.00%  12.00%

p + 0.05

## [1] 15.00% 7.00%  8.00%  17.00%


max(p)

## [1] 12.00%

balance <- accounting(c(1000, 500, 200, -150, 0, 1200))
balance

## [1] 1,000.00 500.00   200.00   (150.00) 0.00     1,200.00

balance + 1000

## [1] 2,000.00 1,500.00 1,200.00 850.00   1,000.00 2,200.00

p <- data.frame(
  id = c(1, 2, 3, 4, 5),
  name = c("A1", "A2", "B1", "B2", "C1"),
  balance = accounting(c(52500, 36150, 25000, 18300, 7600), format = "d"),
  growth = percent(c(0.3, 0.3, 0.1, 0.15, 0.15), format = "d"),
  ready = formattable(c(TRUE, TRUE, FALSE, FALSE, TRUE), "yes", "no"))
p

##   id name balance growth ready
## 1  1   A1  52,500    30%   yes
## 2  2   A2  36,150    30%   yes
## 3  3   B1  25,000    10%    no
## 4  4   B2  18,300    15%    no
## 5  5   C1   7,600    15%   yes
library(formattable)
df <- data.frame(
  id = 1:10,
  name = c("Bob", "Ashley", "James", "David", "Jenny",
           "Hans", "Leo", "John", "Emily", "Lee"),
  age = c(28, 27, 30, 28, 29, 29, 27, 27, 31, 30),
  grade = c("C", "A", "A", "C", "B", "B", "B", "A", "C", "C"),
  test1_score = c(8.9, 9.5, 9.6, 8.9, 9.1, 9.3, 9.3, 9.9, 8.5, 8.6),
  test2_score = c(9.1, 9.1, 9.2, 9.1, 8.9, 8.5, 9.2, 9.3, 9.1, 8.8),
  final_score = c(9, 9.3, 9.4, 9, 9, 8.9, 9.25, 9.6, 8.8, 8.7),
  registered = c(TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE),
  stringsAsFactors = FALSE)

formattable(df, list(
  age = color_tile("white", "orange"),
  grade = formatter("span", style = x ~ ifelse(x == "A",
                                               style(color = "green", font.weight = "bold"), NA)),
  area(col = c(test1_score, test2_score)) ~ normalize_bar("pink", 0.2),
  final_score = formatter("span",
                          style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray")),
                          x ~ sprintf("%.2f (rank: %02d)", x, rank(-x))),
  registered = formatter("span",
                         style = x ~ style(color = ifelse(x, "green", "red")),
                         x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No")))
))
