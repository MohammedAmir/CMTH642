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

x(1,1,1)

