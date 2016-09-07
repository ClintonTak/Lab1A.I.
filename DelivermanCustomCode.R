

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
    hVal = heuristic(carXCoord, carYCoord,packageX, packageY)#returns a list of the Heurstic vals of all available packages 
    #minHVal = min(hVal)
    #locationOfMinInhVal = which(hVal == minHVal) #gets location in hVal of lowest number
    #nextpickuplocation = notpickedup[locationOfMinInhVal]
    for (i in notpickedup){#iterates through available packages 
      currentpackageinfo = packages[i,]
      
      leftorRight = currentpackageinfo[1] - carXCoord
      if (leftorRight<0)#package is to the left of the car
      {               #check left cost
        hCost=roads$hroads[carXCoord-1]
      }
      else if(leftorRight==0) #package is on the horisontal axis   
      {#check right cost 
        hCost=0
      }
      else  #package is to the right of the car   
      {#check right cost 
        hCost=roads$hroads[carXCoord+1]
      }
      uporDown = currentpackageinfo[2] - carYCoord
      if (uporDown < 0)#package is below the car
      {#check down cost 
        
        vcost = roads$vroads[carYCoord-1]
        
      }
      else
      {#check up cost 
      else if(uporDown == 0){#package is left or right, don't do anything
        
        
      }
      else{#check up cost 
        vcost = roads$vroads[carYCoord+1]
        
      }
      
      currentpackagenumber = which(notpickedup == i)
      fStar = vcost + hVal[currentpackagenumber]
      c(fValues)
    }
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
    heuristicVal[i] = abs(carX-packageX[i]) + abs(carY-packageY[i])
  }
  return(heuristicVal)
}






