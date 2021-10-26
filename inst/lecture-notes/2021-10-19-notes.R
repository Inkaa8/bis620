library(palmerpenguins)

data(penguins)

fit <- lm(bill_length_mm ~ bill_depth_mm, data = penguins)

print.character <- function(x, ...){
  cat("you are here.\n")
}


my_list <- list(letter = letters, number = 1:26)

class(my_list) <- "letters_and_numbers"

print.just_letters <- function(x, ...){
  print(x$letter)
}
print.just_numbers <- function(x, ...){
  print(x$number)
}

class(my_list) <- c("just_letters", class(my_list))

class(my_list) <- c("just_numbers", class(my_list))

class(my_list) <- c("bunk", class(my_list))

attributes(my_list)$pi <- 3.14









plus <- function(x, y){
  UseMethod("plus", x)
}

plus.default <- function(x, y){
  stop(
    "Don't know how to add an object of type",
    paste(class(x), collopse = " "),
    "add type",
    paste(calss(y), collapse = " ")
  )
  x+y
}

plus.numeric <- function(x,y){
  if(is.numeric(y)){
    stop("The y argument is numeric ")
  }
  x+y
}

plus.character <- function(x,y){
  if(is.character(y)){
    stop("The y argument is character ")
  }
  paste0(x,y)
}


`%+%` <- function(x, y){
  UseMethod('%+%', x)
}

`%+%.character` <- function(x,y){
  plus(x,y)
}

`%+%` <- function(x, y){
  plus(x+y)
}

library(testthat)
expect(3 %+% 4, 7)
expect_equal("a" %+% "b", "ab")
expect_error("a" %+% 3)









# s4
setClass(Class = "Bus",
         slots = list(n_seats = "numeric",
                      top_speed = "numeric",
                      current_speed = "numeric",
                      brand = "character"))

setClass(Class = "SchoolBus",
         slots = list(num_student_riders = "integer",
                      extra = "ANY"),
         contains = "Bus")

my_bus <- new("Bus", n_seats = 20L, top_speed = 80L,
              current_speed = 0L, brand = "Volvo")

my_bus


get_brand <- function(bus){
  bus@brand
}

get_brand(my_bus)


setGeneric("is_bus_moving", function(x){
  standardGeneric("is_bus_moving")
})


setMethod(
  f = "is _bus_moving",
  signature = c(x = "Bus"),
  definition = function(x){
    x@current_speed > 0
    }
  )

is_bus_moving(my_bus)


plus(3, 3)
plus(3, "a")
plus("a", "b")





# reference classes
