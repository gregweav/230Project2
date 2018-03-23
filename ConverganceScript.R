require(actuar)
require(fitdistrplus)
require(rmutil)
require(ggplot2)
require(survival)
require(reshape2)
require(e1071)


numberOfVars = 500


oldw <- getOption("warn")
options(warn = -1)



printAllSignals = function(){
  paretoSig = doAllForSVM(100, "pareto")
  weibullSig = doAllForSVM(100, "weibull")
  logNormSig = doAllForSVM(100, "logNorm")
  normalSig = doAllForSVM(100, "normal")
  gammaSig = doAllForSVM(100, "jfdkslf")
  
  
  colnames(myMatrix) = c("index","weibull", "logNorm", "levy", "fendo", "normal")
  molten = melt(myMatrix, id.vars = "index")
  print(ggplot(molten, aes(x = index, y = value, colour = variable)) + geom_line() + ggtitle(dist))
}


getStatistics = function(vecb, toDos){
  return_guy = list()
  vecOfToDos = toDos
  vecOfAnalysis = list()
  count = 1;
  wherethedudesareat12 = numeric();
  plot34.legend = character()
  if(toDos[1] == 0){
    weibullEstZLatqqq = try(fitdist(vecb, "weibull", method = "mle"), silent = T)
    if(class(weibullEstZLatqqq) != "try-error"){
      vecOfAnalysis[[count]] = weibullEstZLatqqq
      wherethedudesareat12[1] = count
      count = count + 1;
      plot34.legend = c(plot34.legend, "weibull")
    } else if(class(weibullEstZLatqqq) == "try-error"){
      #print("could not estimate the parameters for the weibull function")
      vecOfToDos[1] = 1;
      wherethedudesareat12[1] = 34
    }
  }else{
    wherethedudesareat12[1] = 34
  }
  #gammaEstZLatqqq = fitdist(vecb, "gamma", method = "mle")
  if(toDos[2] == 0){
    logNormEst3qqq = try(fitdist(vecb, "lnorm", method = "mle"), silent = T)
    if(class(logNormEst3qqq) != "try-error"){
      wherethedudesareat12[2] = count;
      vecOfAnalysis[[count]] = logNormEst3qqq
      count = count + 1
      plot34.legend = c(plot34.legend, "lnorm")
    } else if(class(logNormEst3qqq) == "try-error"){
      #print("could not estimate the log norm paramters")
      vecOfToDos[2] = 1
      wherethedudesareat12[2] = 34
    }
  }else{
    wherethedudesareat12[2] = 34
  }
  if(toDos[3] == 0){
    levyEst3qqq = try(fitdist(vecb, "pareto",  method = "mle"), silent = T);
    if(class(levyEst3qqq) != "try-error"){
      wherethedudesareat12[3] = count;
      vecOfAnalysis[[count]] = levyEst3qqq;
      count = count + 1;
      plot34.legend = c(plot34.legend, "pareto")
    } else if(class(levyEst3qqq) == "try-error"){
      # print("could not estimate the parameters for the levy-pareto distribution")
      vecOfToDos[3] = 1
      wherethedudesareat12[3] = 34
    }
  }else{
    wherethedudesareat12[3] = 34
  }
  if(toDos[4] == 0){
    fendoEst3qqq = try(fitdist(vecb, "llogis",  method = "mle"), silent = T);
    if(class(fendoEst3qqq) != "try-error"){
      vecOfAnalysis[[count]] = fendoEst3qqq;
      wherethedudesareat12[4] = count;
      count = count + 1;
      plot34.legend = c(plot34.legend, "llogis")
    } else if(class(fendoEst3qqq) == "try-error"){
      # print("could not estimate the parameters for the feno distribution")
      vecOfToDos[4] = 1
      wherethedudesareat12[4] = 34
    }
  }else{
    wherethedudesareat12[4] = 34
  }
  if(toDos[5] == 0){
    normEst12 = try(fitdist(vecb, "norm", method = "mle"), silent = T)
    if(class(normEst12) != "try-error"){
      vecOfAnalysis[[count]] = normEst12;
      wherethedudesareat12[5] = count;
      count = count + 1;
      plot34.legend = c(plot34.legend, "normal")
    } else if(class(normEst12) == "try-error"){
      #print("could not estimate the parameters for the normal distribution")
      vecOfToDos[5] = 1
      wherethedudesareat12[5] = 34
    }
  }else{
    wherethedudesareat12[5] = 34
  }
  zeroLat = gofstat(vecOfAnalysis, fitnames = plot34.legend)
  return_guy[[1]] = zeroLat
  return_guy[[2]] = vecOfToDos
  return_guy[[3]] = wherethedudesareat12
  return(return_guy)
}

generateListOfRVars = function(numberOfVars, densityString){
  vecOfVecs = list();
  if(densityString == "pareto"){
    for(n in 1:numberOfVars){
      myVec = rpareto(1, 1, 2);
      for(b in 1: 1000){
        temp = rpareto(1, 1, 2);
        while(temp > 100 || temp < .01){
          temp = rpareto(1,1,2);
        }
        myVec = c(myVec, temp);
      }
      vecOfVecs[[n]] = myVec;
    }
  }else if(densityString == "weibull"){
    for(n in 1:numberOfVars){
      myVec = rweibull(1, 1, 2);
      for(b in 1: 1000){
        temp = rweibull(1, 1, 2);
        while(temp > 100 || temp < .01){
          temp = rweibull(1,1,2);
        }
        myVec = c(myVec, temp);
      }
      vecOfVecs[[n]] = myVec;
    }
  }else if(densityString == "normal"){
    for(n in 1:numberOfVars){
      myVec = rnorm(1, 5, .5);
      for(b in 1: 1000){
        temp = rnorm(1, 5, .5);
        while(temp > 100 || temp < .01){
          temp = rnorm(1, 5, .5);
        }
        myVec = c(myVec, temp);
      }
      vecOfVecs[[n]] = myVec;
    }
  }else if(densityString == "logNorm"){
    for(n in 1:numberOfVars){
      myVec = rlnorm(1, 1, 2);
      for(b in 1: 1000){
        temp = rlnorm(1, 1, 2);
        while(temp > 100 || temp < .01){
          temp = rlnorm(1,1,2);
        }
        myVec = c(myVec, temp);
      }
      vecOfVecs[[n]] = myVec;
    }
  }
  else{
    for(n in 1:numberOfVars){
      myVec = rgamma(1, 1, 2);
      for(b in 1: 1000){
        temp = rgamma(1, 1, 2);
        while(temp > 100 || temp < .01){
          temp = rgamma(1,1,2);
        }
        myVec = c(myVec, temp);
      }
      vecOfVecs[[n]] = myVec;
    }
  }
  return(vecOfVecs)
}
mycumsum = function(myList){
  vecofsums = list()
  vecofsums[[1]] = myList[[1]];
  for(i in 2:length(myList)){
    tempVec = myList[[i]];
    vecofsums[[i]] = vecofsums[[i-1]] + tempVec;
  }
  return(vecofsums)
}



correctVec = function(myVec){
  if(length(myVec)> 25){
    for(i in 25:length(myVec)){
      if((myVec[i] - myVec[i-1]) > .7){
        myVec[i] = myVec[i-1];
      }
    }
  }
  return(myVec)
}




printConvergence = function(ksStatList, numberOfVars, dist){
  indexVec = 1:numberOfVars
  myMatrix = data.frame(indexVec)
  for(q in 1:length(ksStatList)){
    if(q == 4){
      fendoVec = ksStatList[[q]]
      fendoVec = correctVec(fendoVec)
      myMatrix = data.frame(myMatrix, fendoVec)
    }else{
      myMatrix = data.frame(myMatrix, ksStatList[[q]])
    }
  }
  colnames(myMatrix) = c("index","weibull", "logNorm", "levy", "fendo", "normal")
  molten = melt(myMatrix, id.vars = "index")
  print(ggplot(molten, aes(x = index, y = value, colour = variable)) + geom_line() + ggtitle(dist))
}




createSVMVec = function(ksStatList, numberOfVars, dist){
  myBigOlVec = numeric()
  for(q in 1:length(ksStatList)){
    if(q == 4){
      fendoVec = ksStatList[[q]]
      fendoVec = correctVec(fendoVec)
      myBigOlVec = c(myBigOlVec, fendoVec)
    }else{
      myBigOlVec = c(myBigOlVec, ksStatList[[q]])
    }
  }
  return(myBigOlVec)
}


ks_stat_collect = function(sumVec){
  ksStat = numeric()
  todoVec = integer(5)
  weibullKsStat = numeric()
  logNormKsStat = numeric()
  levyKsStat = numeric()
  fendoKsStat = numeric()
  normalKsStat = numeric()
  listOfDudeInfo = list()
  listOfDudeInfo[[1]] = weibullKsStat
  listOfDudeInfo[[2]] = logNormKsStat
  listOfDudeInfo[[3]] = levyKsStat
  listOfDudeInfo[[4]] = fendoKsStat
  listOfDudeInfo[[5]] = normalKsStat
  for(m in 1:length(sumVec)){
    tempVec = sumVec[[m]];
    tempStat = getStatistics(tempVec, todoVec)
    results = tempStat[[1]]
    wherethedudesisat = tempStat[[3]] # vector that has all indices of vector info
    ksStatInfoForAll = results$ks
    for(i in 1:length(wherethedudesisat)){
      vecToAddTo = listOfDudeInfo[[i]] # this is the specific ksStat vector for a unique density
      whatIndex = wherethedudesisat[i]
      if(whatIndex == 34){ # catch sentinel value, and put the ksStat for that index as 0.
        vecToAddTo = c(vecToAddTo, 0)
      }else{
        specificKsStat = ksStatInfoForAll[whatIndex]
        vecToAddTo = c(vecToAddTo, specificKsStat)
      }
      listOfDudeInfo[[i]] = vecToAddTo
    }
    todoVec = tempStat[[2]]
  }
  return(listOfDudeInfo)
}

doAllForPlot = function(times, calls){
  vecOfPureRandos = generateListOfRVars(times, calls)
  vecofSummedRandos = mycumsum(vecOfPureRandos)
  listOfSignals = ks_stat_collect(vecofSummedRandos)
  printConvergence(listOfSignals, times, calls)
}

doAllForAnalysis = function(xChagos, yChagos){
  distanceVec = getDistanceVec(xChagos, yChagos)
  distanceVecCumSum = mycumsum(distanceVec)
  getMySubset()
  ksForChagos = ks_stat_collect(distanceVecCumSum)
  
}



doAllForSVM = function(times, calls){
  vecOfPureRandos = generateListOfRVars(times, calls)
  vecofSummedRandos = mycumsum(vecOfPureRandos)
  listOfSignals = ks_stat_collect(vecofSummedRandos)
  return_guy = createSVMVec(listOfSignals, times, calls)
  return(return_guy)
}

doAllForValidation = function(times, calls){
  theMatrix = matrix(data = NA, nrow = 5, ncol = 500, byrow = TRUE)
  for(w in 1:5){
    singleVec  = rpareto(100, 1, 2)
    vecofVecs = list()
    vecofVecs[[1]] = singleVec
    for(b in 2:100){
      tempVec = numeric()
      for(i in 1:100){
        randIndex = runif(1, min = 1, max = 100)
        tempVar = singleVec[randIndex]
        tempVec[i] = tempVar
      }
      vecofVecs[[b]] = tempVec
    }
    sumProduct = mycumsum(vecofVecs)
    signals = ks_stat_collect(sumProduct)
    theVec = createSVMVec(signals,100, "pareto")
    print(length(theVec))
    theMatrix[w, ] = theVec
  }
  theRealRealKey = rep(3, times = 5)
  theDudesPredictions = predict(mySVM, theMatrix)
  factoredForm = as.numeric(levels(theDudesPredictions)[as.integer(theDudesPredictions)])
  rightRate = checkClass(factoredForm, theRealRealKey)
  print(rightRate)
}
doAllForValidation(100, "pareto")


##################
#    This function is going to create a main matrix whereby each row is the vector of the signal 
#    and a the matching vector that is going to be the response:
#    which is going to map as follows: [1:weibull, 2:logNorm, 3:levy, 4:gamma, 5:normal]
createData = function(){
  responseVec = numeric()
  myBigMatrix = matrix(data = NA, nrow = 1000, ncol = 500, byrow = TRUE)
  print("Here we go... ")
  for(i in 1:2000){
    pareto = "pareto"
    tempVec = doAllForSVM(100, "pareto")
    myBigMatrix[i,] = tempVec
    responseVec = c(responseVec, 3)
    print(i)
  }
  print("Part 1 done")
  for(b in 201:400){
    weibull = "weibull"
    tempVec = doAllForSVM(100, weibull)
    myBigMatrix[b,] = tempVec
    responseVec = c(responseVec, 1)
    print(b)
  }
  print("Part 2 done")
  for(h in 401: 600){
    logNorm = "logNorm"
    tempVec = doAllForSVM(100, logNorm)
    myBigMatrix[h, ] = tempVec
    responseVec = c(responseVec, 2)
    print(h)
  }
  print("Part 3 done")
  for(l in 601: 800){
    normal = "normal"
    tempVec = doAllForSVM(100, normal)
    myBigMatrix[l,] = tempVec
    responseVec = c(responseVec, 5)
    print(l)
  }
  print("Part 4 done")
  for(q in 801:1000){
    gammaStr = "fdsafds"
    tempVec = doAllForSVM(100, gammaStr)
    myBigMatrix[q, ] = tempVec
    responseVec = c(responseVec, 4)
    print(q)
  }
  print(myBigMatrix)
  write.csv(myBigMatrix, file = "/Users/Greg/Documents/230Project/biggerTestSet.csv")
  myFactor = factor(responseVec)
  myReturnPerson = list()
  myReturnPerson[[1]] = myFactor
  myReturnPerson[[2]] = myBigMatrix
  return(myReturnPerson)
}

newTrainingStuff = createData()
    jsnewTestStuff = createTestData()

createTestData = function(){
  responseVec = numeric()
  myBigMatrix2 = matrix(data = NA, nrow = 100, ncol = 500, byrow = TRUE)
  classesVec = numeric()
  return_dude = list()
  print("Here we go...again ")
  for(i in 1:20){
    pareto = "pareto"
    tempVec = doAllForSVM(100, "pareto")
    myBigMatrix2[i,] = tempVec
    responseVec = c(responseVec, 3)
  }
  print("Part 1 done")
  for(b in 21:40){
    weibull = "weibull"
    tempVec = doAllForSVM(100, weibull)
    myBigMatrix2[b,] = tempVec
    responseVec = c(responseVec, 1)
  }
  print("Part 2 done")
  for(h in 41: 60){
    logNorm = "logNorm"
    tempVec = doAllForSVM(100, logNorm)
    myBigMatrix2[h, ] = tempVec
    responseVec = c(responseVec, 2)
  }
  print("Part 3 done")
  for(l in 61: 80){
    normal = "normal"
    tempVec = doAllForSVM(100, normal)
    myBigMatrix2[l,] = tempVec
    responseVec = c(responseVec, 5)
  }
  print("Part 4 done")
  for(q in 81:100){
    gammaStr = "fdsafds"
    tempVec = doAllForSVM(100, gammaStr)
    myBigMatrix2[q, ] = tempVec
    responseVec = c(responseVec, 4)
  }
  print("Finished!")
  return_dude[[1]] = myBigMatrix2
  write.csv(myBigMatrix2,"/Users/Greg/Documents/230Project/newTestStuffNumber2.csv" )
  return_dude[[2]] = responseVec
  return(return_dude)
}
myTestStuff = createTestData()
testStuff12 = myTestStuff[[1]]
apply(testStuff12, 2, function(x) any(is.na(x)))
write.csv(testStuff12,"/Users/Greg/Documents/230Project/newTestStuffNumber2.csv" )



checkClass = function(SVMresponse, key){
  good = 0
  for(i in 1:length(SVMresponse)){
    if(key[i] == SVMresponse[i]){
      good = good + 1
    }
  }
  return(good/length(SVMresponse))
}





getMySubset = function(vec, lastIndex){
  myVec = numeric()
  if(length(vec) >= lastIndex){
    for(i in 1:lastIndex){
      myVec = c(myVec, vec[i])
    }
  }
  else{
    print("The index that you want to go to is longer than the original vector itself")
  }
  return(myVec)
}







options(warn = oldw)
