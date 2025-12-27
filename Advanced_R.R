# Advenced R notes

# Ch 2

library(lobstr)

x <- runif(1e6)
obj_size(x)
y <- list(x, x, x)
obj_size(y)

x <- c(1, 2, 3)
y <- x
obj_addr(x)
obj_addr(y)

a <- 1:10
b <- a
c <- b
d <- 1:10
obj_addr(a)
obj_addr(b)
obj_addr(c)
obj_addr(d)

x <- data.frame(c(1, 2, 3))
names(x) <- c("a")
str(x)
y  <- x %>% 
  select("_b 1" = a)
str(y)
y  <- x %>% 
  select(`_b 2` = a)
str(y)

?Memory 
?gc


a <- 1:10
b <- a
c <- b
d <- 1:10

lobstr::obj_addr(a)
lobstr::obj_addr(b)
lobstr::obj_addr(c)
lobstr::obj_addr(d)

mean
base::mean
get("mean")
evalq(mean)
match.fun("mean")

lobstr::obj_addr(mean)
lobstr::obj_addr(base::mean)
lobstr::obj_addr(get("mean"))
lobstr::obj_addr(evalq(mean))
lobstr::obj_addr(match.fun("mean"))

x <- c(1L, 2L, 3L)
tracemem(x)

x[[3]] <- 4


x <- 1e6L
x

x <- c(1,2,3)
class(x)
mode(x)
typeof(x)
is.double(x)
str(x)

attr(x, "class")

?raw 
?complex

"one" > 2

attributes(x)

setNames
unname

str(structure(1:5, comment = "my attribute"))

x <- factor(c("b", "a", "b", "b", "a"))
x
attributes(x)
str(x)
order(x)
x <- factor(c("b", "a", "b", "b", "a"), levels = c("b", "a"))
x
str(x)

grade <- ordered(c("b", "b", "a", "c"), levels = c("c", "b", "a"))
grade
str(grade)
attributes(grade)

ordered <- TRUE
c(if (ordered) "ordered", "factor")

date <- as.Date("1950-02-01")
date
typeof(date)
class(date)
attributes(date)
str(date)
unclass(date)

date1 <- as.Date("1070-02-01")
dates <- c(date, date1)
dates
class(dates)
attributes(dates)
str(dates)
unclass(dates)

x <- table(c(1,2,1,1), c(2,2,3,2), c(5,5,3,5))
x
str(x)
attributes(x)
typeof(x)

f1 <- factor(letters)
f1
str(f1)
levels(f1) <- rev(levels(f1))
f1
str(f1)

f2 <- rev(factor(letters))
f2
str(f2)
f3 <- factor(letters, levels = rev(letters))
f3
str(f3)

x <- list(1:3)
typeof(list(1:3))
unlist(x)
as.vector(x)
typeof(as.vector(x))
str(as.vector(x))

x <- as.list(1:3)
unlist(x)

x <- tibble(
  x = 1:3,
  y = x * 2
)
attributes(x)

x <- data.frame(
  x = 1:3,
  y =  2
)
attributes(x)

x <- data.frame()
x

View(statements)

x <- outer(1:5, 1:5, FUN = "*")
x[upper.tri(x)]

df <- tibble(a=c(1,NA,3), b=c(NA, NA,6))
is.na(df)
df[is.na(df)] <- 0
df

x <- c("m", "f", "u", "f", "f", "m", "m")
lookup <- c(m = "Male", f = "Female", u = NA)
str(lookup)
lookup[x]
unname(lookup[x])
lookup[c(1,1,1,1)]

x <- sample(10) < 4
which(x)

x <- numeric()
out <- vector("list", length(x))
for (i in 1:length(x)) {
  print(i)
  print(x[i])
  print(x[i] ^ 2)
  out[i] <- x[i] ^ 2
  print(out[i])
}
out
str(out)

y <- 10
f1 <- function(x = {y <- 1; 2}, y = 0) {
  c(x, y)
}
f1()
y


args(library)

`+`(1,2)
getMethod(`+`)
`+`

x <- 0
f <- function() {
  x <<- 1
}
str(withVisible(f()))
x

library(rlang)
e2a <- env(d = 4, e = 5)
str(e2a)
class(e2a)
attributes(e2a)
env_print(e2a)
env_parent(e2a)
env_parents(e2a)
env_parents(e2a, last = empty_env())

{print(1); print(2)}

tryCatch(stop("fred"),  error = function(e) e, finally = print("Hello"))
x <- "Default value"
x <- log("a")
x
z <- NULL
try(z <- log("a"), silent = TRUE)
z
z <- try(log("a"))
z
str(z)

catch_cnd(stop("An error"))
catch_cnd(abort("An error"))

log <- function(message, level = c("info", "error", "fatal")) {
  cat(level, "\n")
  level <- match.arg(level)
  cat(level)
  signal(message, "log", level = level)
}

log("this is logged", level="err")


log <- function(message, level = NULL) {
  cat(level, "\n")
  level <- match.arg(level, choices=c("info", "error", "fatal"))
  cat(level)
  signal(message, "log", level = level)
}

log("this is logged")

pair <- function(x) c(x, x)
map_dbl(1:2, pair)
# Error: Result 1 must be a single double, not an integer vector of length 2
map(1:2, pair)
#  works, result is the list of pairs (vectors)
# [[1]]
# [1] 1 1
# 
# [[2]]
# [1] 2 2

x <- map(1:3, ~ runif(2))
str(x)
x

as_mapper(~ runif(2))

map(1:3, runif(2))

str(mtcars)
mtcars
?mtcars

modify(mtcars, 1)



df <- data.frame(
  x = 1:3,
  y = 6:4
)
class(df)

class(map(df, ~ .x * 2))
class(modify(df, ~ .x * 2))

temp <- tempfile()
dir.create(temp)

cyls <- split(mtcars, mtcars$cyl)
paths <- file.path(temp, paste0("cyl-", names(cyls), ".csv"))
walk2(cyls, paths, write.csv)

x <- logical()
x <- c(1,1)
str(x)
length(x)
seq_along(x)
length(seq_along(x))

trims <- c(0, 0.1, 0.2, 0.5)
x <- rcauchy(1000)
pmap_dbl(list(trim = trims), mean, x = x)

modify(mtcars, 1)

trans <- list(
  disp = function(x) x * 0.0163871,
  am = function(x) factor(x, labels = c("auto", "manual"))
)

nm <- names(trans)
mtcars[nm] <- map2(trans, mtcars[nm], function(f, var) f(var))

f(
  f(f(1, 2), 
    3),
  4)

power1 <- function(exp) {
  function(x) {
    x ^ exp
  }
}

square <- power1(2)
square
envstr(square)
class(square)
square(2)

env_print(square)
fn_env(square)$exp

# construct some sample data with very different numbers in each cell
sd <- c(1, 5, 15)
n <- 100

df <- data.frame(x = rnorm(3 * n, sd = sd), sd = rep(sd, n))
str(df)
rnorm(4, sd=c(1,5))

library(sloop)
x <- c(1,2,3)
class(x)
attributes(x)
is.object(x)
attr(x, "class")
sloop::s3_class(x)
y <- c(1L)
sloop::s3_class(y)
class(y)

typeof(x)
typeof(y)
f <- function() 2
typeof(f)
typeof(function() 2)

f <- factor(c("a", "b", "c"))
f
typeof(f)
attributes(f)
str(f)
unclass(f)

sloop::s3_dispatch(print(f))
sloop::ftype(print.factor)


sloop::ftype(t.test)
sloop::ftype(t.data.frame)
sloop::s3_get_method(t.test)
sloop::s3_get_method(t.data.frame)

sloop::ftype(as.data.frame.data.frame)
sloop::s3_get_method(as.data.frame.data.frame)

x <- table(rpois(100, 5))
x
class(x)
sloop::s3_class(x)
attributes(x)

x <- data.frame(a=c(1,2), b=c(3,4))
sloop::otype(x)
typeof(x)
class(x)
attributes(x)

x <- tibble(a=c(1,2), b=c(3,4))
sloop::otype(x)
typeof(x)
class(x)
attributes(x)

x <- base::factor(c("a","b"), levels = c("a"))
x
str(x)

x <- structure(1:10, class = "test")
str(x)
t(x)

sloop::s3_dispatch(t(x))
sloop::ftype(t.test)

sloop::s3_methods_class("table")

library(R6)
Accumulator <- R6Class("Accumulator", list(
  sum = 0,
  add = function(x = 1) {
    self$sum <- self$sum + x 
    invisible(self)
  })
)
typeof(Accumulator)
attributes(Accumulator)
str(Accumulator)

x <- Accumulator$new() 
str(x)
typeof(x)
class(x)

library(methods)
setClass("Person", 
         slots = c(
           name = "character", 
           age = "numeric"
         ), 
         prototype = list(
           name = NA_character_,
           age = NA_real_
         )
)

me <- new("Person", name = "Hadley")
str(me)
typeof(me)
class(me)
attributes(me)

library(rlang)
library(lobstr)

sample(5)

z <- rlang::expr(y <- x * 10)
z
typeof(z)
str(z)
class(z)
attributes(z)
sloop::s3_class(z)

f <- expr(f(x = 1, y = 2))
str(f)
f
f$x
lobstr::ast(f(x = 1, y = 2))

x <- expr(kood == 3)
x
str(x)
lobstr::ast(x)
lobstr::ast(kood == 3)


with2 <- function(df, expr) {
  a <- 1000
  eval_tidy(enexpr(expr), df)
}

df <- data.frame(x = 1:3)
a <- 10
expr <- expr(x + a)
with2(df, x + a)
#> [1] 1001 1002 1003

with2 <- function(df, expr) {
  a <- 1000
  print(enquo(expr))
  eval_tidy(enquo(expr), df)
}

with2(df, x + a)
#> [1] 11 12 13

x <- 1
lobstr::ast(if(x == 1) 2 else if (x > 1) 3 else 4)

lobstr::ast(c(1, 2))

x <- expr(read.csv("foo.csv", header = TRUE))[-1]
str(x)

lobstr::ast(f((1)))

lobstr::ast(-2^2 )

parse_expr("x + 1; y + 1")

expr <- expr(g(a + b + c + d + e + f + g + h + i + j + k + l + 
                 m + n + o + p + q + r + s + t + u + v + w + x + y + z))
expr_text(expr) 

lobstr::ast(x = (y = 10))

z <- expr(y = 10)

lobstr::ast(mean(x, na.rm = T) )

lobstr::ast(function(x, na.rm = T) {FALSE})


f <- expr(function(x, y = 10) x + y)
lobstr::ast(function(x, y = 10) x + y)
lobstr::ast(!!f)
# f <- function(x, y = 10) x + y
f
typeof(f)
f[[1]]
typeof(f[[1]])
args <- f[[2]]
args
#> $x
#> 
#> 
#> $y
#> [1] 10
typeof(args)
#> [1] "pairlist"
f[[3]]
typeof(f[[3]])

f[[4]]
typeof(f[[4]])

ms <- list(missing_arg(), missing_arg())
ms[[1]]


lobstr::ast(f(x, "y", 1))
lobstr::ast(y<-x)
lobstr::ast(y< -x)

lobstr::ast(1 -> x)
eval(1 -> x)
expr(1 -> x)

lobstr::ast(list(1,2, list(3,4)))

lobstr::ast(function(x) x+1(3))
expr(function(x) x+1(3))
eval(expr(function(x) x+1(3)))
function(x) x+1(3)
typeof(function(x) x+1(3))
sloop::s3_class(function(x) x+1(3))

lobstr::ast(map(c(1,2), function(x) x+3))
lobstr::ast(map(c(1,2), sqrt))

x <- 1:10

call2(median, x, na.rm = TRUE)
call2(expr(median), x, na.rm = TRUE)
call2(median, expr(x), na.rm = TRUE)
call2(expr(median), expr(x), na.rm = TRUE)                

call_standardise(quote(mean( 1:10, 0, TRUE)))

x <- expr(foo(x = 1))
x
str(x)
lobstr::ast(expr(foo(x = 1)))
names(x) <- c("x", "y")
x

expr(`x`+x)
expr("x"+x)

lobstr::ast(f((1)))
lobstr::ast(`(`(1 + 1))

parse_exprs("x + 1; y + 1")
parse_expr("a+")

expr <- expr(g(a + b + c + d + e + f + g + h + i + j + k + l + 
                 m + n + o + p + q + r + s + t + u + v + w + x + y + z))
deparse(expr)
expr_text(expr)

lobstr::ast(x <- 2)

f <- expr(function(x, y = 10) x + y)
str(f)        
lobstr::ast(f)

lobstr::ast("x" <- 1)
x <- 0
"x" <- 1
x
str(x)

a <- sym("y")
a
str(a)
typeof(a)

y <- 100
!y
!!y
as.logical(100)

help(var)


dfs <- list(
  a = data.frame(x = 1, y = 2),
  b = data.frame(x = 3, y = 4)
)

dplyr::bind_rows(!!!dfs)
expr(dplyr::bind_rows(!!!dfs))
expr(rbind(!!!dfs))


greek <- c(
  "alpha", "theta", "tau", "beta", "vartheta", "pi", "upsilon",
  "gamma", "varpi", "phi", "delta", "kappa", "rho",
  "varphi", "epsilon", "lambda", "varrho", "chi", "varepsilon",
  "mu", "sigma", "psi", "zeta", "nu", "varsigma", "omega", "eta",
  "xi", "Gamma", "Lambda", "Sigma", "Psi", "Delta", "Xi",
  "Upsilon", "Omega", "Theta", "Pi", "Phi"
)
str(greek)
greek_list <- set_names(paste0("\\", greek), greek)
str(greek_list)
greek_env <- as_environment(greek_list)

library(profvis)
library(bench)

f <- function(n = 1e5) {
  x <- rep(1, n)
  rm(x)
}
profvis(f(n=1e15))


df <- data.frame(a=c(1,2), b=c(3,4))

rowAny <- function(x) rowSums(x) > 0
rowAll <- function(x) rowSums(x) == ncol(x)

rs <- rowSums(df)
str(rs)
ra <- rowAny(df)
rall <- rowAll(df)

lookup <- setNames(as.list(sample(100, 26)), letters)
str(lookup)

x1 <- "j"
x10 <- sample(letters, 10)
x100 <- sample(letters, 100, replace = TRUE)

lookup[x1]
lookup[x10]
lookup[x100]

random_string <- function() {
  paste(sample(letters, 50, replace = TRUE), collapse = "")
}
x <- random_string()
strings10 <- replicate(10, random_string())
strings100 <- replicate(100, random_string())

collapse <- function(xs) {
  out <- ""
  for (x in xs) {
    out <- paste0(out, x)
  }
  out
}

loop10  <-  collapse(strings10)
vec10   = paste(strings10, collapse = "")

df <- data.frame(runif(3), runif(3))
names(df) <- c(1, 2)
df
str(df)
dfCol <- names(df)
str(dfCol)

x <- 'x'

!TRUE
!c(TRUE, FALSE)

this_is_a_really_long_name <- 2.5


usethis::use_testthat()
use_test()

(r1 <- rank(x1 <- c(3, 1, 4, 15, 92)))
x2 <- c(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5)
names(x2) <- letters[1:11]
x2
(r2 <- rank(x2, ties.method = "average")) # ties are average

x <- c(1,2,3)
x[[3]]
x[[1:2]]
x[4]

models <- mtcars %>% 
  split(.$cyl) %>% 
  map(function(df) lm(mpg ~ wt, data = df))

c <- y ~ x
c
class(c)
typeof(c)

(r1 <- rank(x1 <- c(3, 1, 4, 15, 92)))

library(modelr)
sim1
sim2
sim3

x <- data.frame(a=c(1,2), b=c(3,4))
y <- x[, 1, drop = FALSE]
str(y)

rev(c(1,3,2))
x <- c(TRUE, FALSE)
!x

x <- tibble(a=c(1,2,0), b=c(NA, 1, NA))
is.na(x)
x[is.na(x)]
str(x[is.na(x)])
x == 0
x[x == 0]
str(x[x == 0])



x <- data.frame(a=c(2,1), b=c(3,4))
tracemem(x)
lobstr::ref(x)
x <- cbind(x, "c"=c(5,6))
lobstr::ref(x)

cat(tracemem(x), "\n")

x <- c(1,2)
x[2] <- 3

x[["c"]] <- c(5,6)
x

typeof(expr("abc"))
x$a
x[expr("a")]
select(x, expr("a"))
select(x, expr(a))
select(x, a)
select(x, "a")
col1 <- "a"
select(x, col1)
select(x, 1)
select(x, c(2,1))
select(x, 2:1)
select(x, 2, 1)
select(x, last_col())
select(x, last_col(offset = 1))
select(x, 1:last_col())

expr(arrange(x, a))
expr(arrange(x, "a"))
expr(arrange(x, !!eneval("a")))
expr(arrange(x, !!enexpr("a")))
expr(arrange(x, sym("a")))
expr(arrange(x, !!"a"))
expr(arrange(x, !!enquo("a")))
expr(arrange(x, !!quo("a")))
expr(arrange(x, !!expr("a")))
expr(arrange(x, !!sym("a")))
expr(arrange(x, !!sym(colnames(x)[1])))
sym("a")
typeof(sym("a"))

y <- expr(x)
str(y)
attributes(y)
typeof(y)

y <- expr(x + 2)
str(y)
attributes(y)
typeof(y)


x <- "a"
expr(!!x) #is equivalent to x.
eval(expr(!!x))

x <- 2
g03 <- function() {
  y <- 1
  x <- 10
  c(x, y)
}
g03()
x

sum <- sum(1, 2)

# Function facto

power1 <- function(exp) {
  function(x) {
    x ^ exp
  }
}

square <- power1(2)
cube <- power1(3)

library(rlang)
fn_env(square)
environment(square)
env_print(cube)
fn_env(cube)$exp


# Higher order functions wiki

twice <- function(f) {
  return(function(x) {
    f(f(x))
  })
}
twice

plusThree <- function(i) {
  return(i + 3)
}

g <- twice(plusThree)
str(g)
g

print(g(7))
