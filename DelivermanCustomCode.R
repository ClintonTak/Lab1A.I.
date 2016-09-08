

customDM=function(roads,car,packages)
{
  nextMove=0
  toGo=0
  offset=0
  if (car$load==0) { #decides which package to pick up next 
    carXCoord = car$x; carYCoord = car$y;
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
      if(uporDown==0){#package is left or right, don't do anything
        vcost=0
        
      }
      else{#check up cost 
        vcost = roads$vroads[carYCoord+1]
        
      }
      if(vcost==0 & hcost!=0){
        totalcost=hcost
      }else if(vcost!=0 & hcost==0){
        totalcost=vcost
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

avNodes <- function(x,y,roads){
  dim=length(roads$hroads[,1])
  #nodes=matrix(nrow=5,ncol=2)
  if(x==1){
    if(y==1){
      return(nodes=matrix(c(x,y+1,x+1,y),nrow=2,ncol=2))
    }else if(y==dim){
      return(nodes=matrix(c(x,y-1,x+1,y),nrow=2,ncol=2))
    }else{
      return(nodes=matrix(c(x,y+1,x,y-1,x+1,y),nrow=2,ncol=3))
    }
  }else if(x==dim){
    if(y==1){
      return(nodes=matrix(c(x,y+1,x-1,y),nrow=2,ncol=2))
    }else if(y==dim){
      return(nodes=matrix(c(x,y-1,x-1,y),nrow=2,ncol=2))
    }else{
      return(nodes=matrix(c(x,y+1,x,y-1,x-1,y),nrow=2,ncol=3))
    }
  }else if(y==1){
    return(nodes=matrix(c(x,y+1,x+1,y,x-1,y),nrow=2,ncol=3))
  }else if(y==dim){
    return(nodes=matrix(c(x,y-1,x+1,y,x-1,y),nrow=2,ncol=3))
  }else{
    return(nodes=matrix(c(x,y+1,x,y-1,x-1,y,x+1,y),nrow=2,ncol=4))
  }
}


makeDelivery = function(roads,car,packages)
{
  currentpackage = which(packages[,5]==1) 
  destinationX = currentpackage[3]
  destinationY = currentpackage[4] 
  while (currentpackage[5]==1){
    hVal = abs(car$x-destinationX) + abs(car$y-destinationY)
    if (car$y != 10){
      gUp = roads$hroads[car$y+1]
    }else{
      gUp = 99999#give high value to discourage driving off the map
    }
    if (car$y!= 1){
      gDown = roads$hroads[car$y-1]
    }else{
      gDown = 99999#give high value to discourage driving off the map
    }
    if (car$x != 10){
      gRight = roads$vroads[car$x+1]
    }else{
      gRight = 99999#give high value to discourage driving off the map
    }
    if (car$x != 1){
      gLeft = roads$vroads[car$x-1]
    }else{
      gLeft = 99999#give high value to discourage driving off the map 
    }
    gVals<-c(gUp,gDown,gRight,Gleft)
    gLowest<-c(min(gVals), which(gVals == min(gVals))) 
  }
}





