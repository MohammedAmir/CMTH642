x <- function(sodium, sugar, fat)
{
  if(sodium == 1)
  {
    print ("High Sodium , Failed")
    if(sugar == 1 && sodium == 1)
    {
      print ("High Sugar, Failed")
      if (fat == 1 && sugar == 1 && sodium == 1)
      {
        print ("Low Fat, Failed")
      }
      else
      {
        print ("Low Fat, Pass")
      }
    }
    else
    {
      print ("Low Sugar, Pass")
    }
  }
  else
  {
    print ("Low Sodium, Pass")
  }
}

x(0,1,1)


------------------
  # create data frame
y <- c(1:10)
z <- c(3:12)
x <- data.frame(y,z)

# Create a function to print squares of numbers in sequence.
newfun <- function(a,b)
{
  c <- a * b
  print(c)
}

#Apply function to data frame
x$w <- mapply(newfun, x$y, x$z)
x
