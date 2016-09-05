

customDM=function(roads,car,packages)
{
  nextMove=0
  toGo=0
  offset=0
  if (car$load==0) { #decides which package to pick up next 
    carXCoord = car$x
    carYCoord = car$y
    notpickedup=which(packages[,5]==0) #list of packages that haven't been picked up
    packageX = package[notpickedup, 1]
    packageY = package[notpickedup, 2]
    hVal = heuristic(carXCoord, carYCoord,packageX, packageY)
    
                      #for x in packages
                      #run heuristic on x
                      #store package with lowest x
                      #go to package with lowest x after loop terminates 
  }else { #decides which delivery to make 
    toGo=car$load  
    offset=2
  }
}

heuristic = function(carX, carY, packageX, packageY) #x1 and y1 are current loc, x2 and y2 are destination
{
  heuristicVal = c(1:length(packageX))
  for (i in 1:length(packageX))
  {
    print(i)
    heuristicVal[i] = abs(carX-packageX[i]) + abs(carY-packageY[i])
  }
  return(heuristicVal)
}






