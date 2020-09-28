#__________________________Pull the market data_____________________________
# Get the market data
getPred = function(yahooData) {
    #date_begin_m <- strptime(as.character(date_begin), "%m/%d/%Y")
    #date_end_m <- strptime(as.character(date_end), "%m/%d/%Y")
    
    # Define the variables that we are going to use
    #__________________________Pull the market data_____________________________
    
    # Get the market data
    
    # Create the date list
    #yahooData = getSymbols(input$symb, src = "yahoo", from = input$dates[1], to = input$dates[2], auto.assign = FALSE)
    date_market <- data.frame(index(yahooData))   #Change to data frame
    
    # Get Open_Price, Close_Price, High_Price, Low_Price
    
    data_from_yahoo <- as.data.frame(yahooData) # 
    
    adjust_coff <- data_from_yahoo[,4]/data_from_yahoo[,6]  # Get the adjust index
    Open_Price <- data_from_yahoo[,1]/adjust_coff   # Get the adjusted value based on index, similar below
    Close_Price <- data_from_yahoo[,6]
    High_Price <- data_from_yahoo[,2]/adjust_coff
    Low_Price <- data_from_yahoo[,3]/adjust_coff
    
    #______________________Calulate the technical indicators_____________________________
    
    # Get the Stochastic Oscillator
    
    data_for_sto <- data.frame(High_Price,Low_Price,Close_Price,row.names = NULL)
    colnames(data_for_sto) <- c("High","Low","Close")    #Meet the format of fuction
    full_sto <- data.frame(stoch(data_for_sto))
    Stochastic_Oscillator <- full_sto$fastK * 100
    
    # Get the RSI_Move
    
    the_RSI <- RSI(Close_Price)     #Get the RSI
    RSI_Move <- diff(the_RSI)       #Get the difference as previous day
    RSI_Move[RSI_Move < 0] <- 0       # 0 means going down
    RSI_Move[RSI_Move > 0] <- 1       # 1 means going up
    RSI_Move <- data.frame(RSI_Move)  # Transfer to data frame
    RSI_Move <- rbind("N/A",RSI_Move) # Move down for one row
    
    #_______________________Temporary full data_________________________________
    
    fulldata_temp <- data.frame(date_market,Open_Price,Close_Price,
                                High_Price,Low_Price, Stochastic_Oscillator,
                                RSI_Move,row.names = NULL)
    colnames(fulldata_temp) <- c("Date","Open","Close","High","Low","Stochastic Oscillator", "RSI Move")
    
    # ______________________________Create the target_____________________________
    
    # Based on Target 2: O(i+1) - O(i)
    
    Target <- diff(Open_Price)
    temp = rep(NA, length(Target) + 1)
    temp[1:length(Target)] = Target
    Target = data.frame(temp)
    
    colnames(Target) <- "Target"
    
    Target[Target<0] <- 0       # 0 means going down
    Target[Target>0] <- 1       # 1 means going up
    Target <- data.frame(Target)  # Transfer to data frame
    
    fulldata_temp <- cbind(fulldata_temp,Target)
  
    
    #_________________Finalize the data_______________________
    
    
    fulldata <- fulldata_temp[20:nrow(fulldata_temp),]

    move_cols <- sapply(fulldata, is.character)
    move_cols[["RSI Move"]] <- TRUE
    move_cols[["Target"]] <- TRUE
    move_data <- as.data.frame(sapply(fulldata[,move_cols], as.factor))
    move_col_names <- names(move_data)
    non_move_data <- fulldata[, -which(names(fulldata) %in% move_col_names)]
    fulldata <- cbind(non_move_data,move_data)
    
    Target <- fulldata$Target
    fulldata$Target <- NULL
    fulldata <- cbind(fulldata,Target)

    
    fulldata = fulldata[,2:ncol(fulldata)]
    numRows = dim(fulldata)[1]
    lastday = fulldata[numRows, ] 
    fulldata = fulldata[1:numRows-1, ]
    # Create the training and testing data sets
    set.seed(123)
    splitIndex <- createDataPartition(fulldata$Target, p = .9, list = FALSE, times = 1)
    trainDF <- fulldata[splitIndex,]
    testDF <- fulldata[-splitIndex,]  
    
    # Create the model
    svm.model <- ksvm(Target ~., data = trainDF, kernel = "polydot", C=9)
    
    #Evaluating model performance
    svm.predict <- predict(svm.model,testDF)
    #table(svm.predict,testDF$Target)
    
    ##########improve model preformance (kernel selection)############
    # Train the model using different kernel, radial basis, linear, polynomial, hyperbalic tangertsigmoid
    
    svm.model.rbf <- ksvm(Target ~., data = trainDF, kernel = "rbfdot",C=9)
    svm.model.linear <- ksvm(Target ~., data = trainDF, kernel = "vanilladot",C=9)
    svm.model.poly <- ksvm(Target ~., data = trainDF, kernel = "polydot",C=9)
    svm.model.tanh <- ksvm(Target ~., data = trainDF, kernel = "tanhdot",C=9)
    
    #Get the confusion matrix
    svm.confusion.rbf <- predict(svm.model.rbf,trainDF)
    svm.confusion.linear <-  predict(svm.model.linear,trainDF)
    svm.confusion.poly <-  predict(svm.model.poly,trainDF)
    svm.confusion.tanh <-  predict(svm.model.tanh,trainDF)
    #table(svm.confusion.rbf,trainDF$Target)
    #table(svm.confusion.linear,trainDF$Target)
    #table(svm.confusion.poly,trainDF$Target)
    #table(svm.confusion.tanh,trainDF$Target)
    
    # get the predicted values for each model
    
    svm.predict.rbf <- predict(svm.model.rbf,testDF)
    svm.predict.linear <-  predict(svm.model.linear,testDF)
    svm.predict.poly <-  predict(svm.model.poly,testDF)
    svm.predict.tanh <-  predict(svm.model.tanh,testDF)
    
    # check the result
    #print(svm.predict.rbf)
    #print(svm.predict.linear)
    #print(svm.predict.poly)
    #print(svm.predict.tanh)
    
    #Compare the accuracy for each model
    agreement.rbf <- svm.predict.rbf == testDF$Target
    agreement.linear <- svm.predict.linear == testDF$Target
    agreement.poly <- svm.predict.poly == testDF$Target
    agreement.tanh <- svm.predict.tanh == testDF$Target
    
    #table(agreement.rbf)
    #prop.table(table(agreement.rbf))
    #table(agreement.linear)
    #prop.table(table(agreement.linear))
    #table(agreement.poly)
    #prop.table(table(agreement.poly))
    #table(agreement.tanh)
    #prop.table(table(agreement.tanh))
    
    accu.rbf = sum(agreement.rbf) / length(agreement.rbf)
    accu.linear = sum(agreement.linear) / length(agreement.linear)
    accu.poly = sum(agreement.poly) / length(agreement.poly)
    accu.tanh = sum(agreement.tanh) / length(agreement.tanh)
    accus = c(accu.rbf, accu.linear, accu.poly, accu.tanh)
    kernels = c(svm.model.rbf, svm.model.linear, svm.model.poly, svm.model.tanh)
    for (i in 1:length(accus)) {
        if (accus[i] == max(accu.rbf, accu.linear, accu.poly, accu.tanh))
            bestmod = kernels[i][[1]]
    }
    lastdayPred <- predict(bestmod, lastday)
    if (lastdayPred == 0)
        return("Going down")
    else
        return("Going up")
}

getNNmodel<-function(yahooData){
  #symb<-getSymbols(input$symb, src = "yahoo", from = as.Date("26March2019","%d%b%Y") - 730, to = as.Date("26March2019","%d%b%Y"), auto.assign = FALSE)
  yahooData<-data.frame(yahooData)
  yahooData<-tibble::rownames_to_column(yahooData)
  adjust_coff <- yahooData[,5]/yahooData[,7]  # Get the adjust index
  Open_Price <- yahooData[,2]/adjust_coff   # Get the adjusted value based on index, similar below
  Close_Price <- yahooData[,7]
  High_Price <- yahooData[,3]/adjust_coff
  Low_Price <- yahooData[,4]/adjust_coff
  
  #______________________Calulate the technical indicators_____________________________
  
  # Get the Stochastic Oscillator
  
  data_for_sto <- data.frame(High_Price,Low_Price,Close_Price,row.names = NULL)
  colnames(data_for_sto) <- c("High","Low","Close")    #Meet the format of fuction
  full_sto <- data.frame(stoch(data_for_sto))
  Stochastic_Oscillator <- full_sto$fastK * 100
  
  # Get the RSI_Move
  
  the_RSI <- RSI(Close_Price)     #Get the RSI
  RSI_Move <- diff(the_RSI)       #Get the difference as previous day
  RSI_Move[RSI_Move < 0] <- 0       # 0 means going down
  RSI_Move[RSI_Move > 0] <- 1       # 1 means going up
  RSI_Move <- data.frame(RSI_Move)  # Transfer to data frame
  RSI_Move <- rbind("N/A",RSI_Move) # Move down for one row
  date_market <- yahooData$rowname
  #_______________________Temporary full data_________________________________
  
  fulldata_temp <- data.frame(date_market,Open_Price,Close_Price,
                              High_Price,Low_Price, Stochastic_Oscillator,
                              RSI_Move,row.names = NULL)
  colnames(fulldata_temp) <- c("Date","Open","Close","High","Low","Stochastic Oscillator", "RSI Move")
  
  # ______________________________Create the target_____________________________
  
  # Based on Target 2: O(i+1) - O(i)
  
  Target <- diff(Open_Price)
  temp = rep(NA, length(Target) + 1)
  temp[1:length(Target)] = Target
  Target = data.frame(temp)
  
  colnames(Target) <- "Target"
  
  Target[Target<0] <- 0       # 0 means going down
  Target[Target>0] <- 1       # 1 means going up
  Target <- data.frame(Target)  # Transfer to data frame
  
  fulldata_temp <- cbind(fulldata_temp,Target)
  
  #_________________Finalize the data_______________________
  
  
  fulldata <- fulldata_temp[20:nrow(fulldata_temp),]
  
  move_cols <- sapply(fulldata, is.character)
  move_cols[["RSI Move"]] <- TRUE
  move_cols[["Target"]] <- TRUE
  move_data <- as.data.frame(sapply(fulldata[,move_cols], as.factor))
  move_col_names <- names(move_data)
  non_move_data <- fulldata[, -which(names(fulldata) %in% move_col_names)]
  fulldata <- cbind(non_move_data,move_data)
  
  Target <- fulldata$Target
  fulldata$Target <- NULL
  fulldata <- cbind(fulldata,Target)
  
  fulldata = fulldata[,2:ncol(fulldata)]
  numRows = dim(fulldata)[1]
  lastday = fulldata[numRows, ] 
  fulldata = fulldata[1:numRows-1, ]
  
  # Create the training and testing data sets
  set.seed(123)
  splitIndex <- createDataPartition(fulldata$Target, p = .9, list = FALSE, times = 1)
  trainDF <- fulldata[splitIndex,]
  testDF <- fulldata[-splitIndex,]  
  model <- train(Target ~ ., trainDF, method='nnet', trace = FALSE) # train
  # we also add parameter 'preProc = c("center", "scale"))' at train() for centering and scaling the data
  prediction <- predict(model, testDF)                           # predict
  table(prediction, testDF$Target) 
  lastdayPred <- predict(model, lastday)
  return(model)
  if (lastdayPred == 0)
    return("Going down")
  else
    return("Going up")

}

getNNmodel1<-function(yahooData){
  #symb<-getSymbols(input$symb, src = "yahoo", from = as.Date("26March2019","%d%b%Y") - 730, to = as.Date("26March2019","%d%b%Y"), auto.assign = FALSE)
  yahooData<-data.frame(yahooData)
  yahooData<-tibble::rownames_to_column(yahooData)
  adjust_coff <- yahooData[,5]/yahooData[,7]  # Get the adjust index
  Open_Price <- yahooData[,2]/adjust_coff   # Get the adjusted value based on index, similar below
  Close_Price <- yahooData[,7]
  High_Price <- yahooData[,3]/adjust_coff
  Low_Price <- yahooData[,4]/adjust_coff
  
  #______________________Calulate the technical indicators_____________________________
  
  # Get the Stochastic Oscillator
  
  data_for_sto <- data.frame(High_Price,Low_Price,Close_Price,row.names = NULL)
  colnames(data_for_sto) <- c("High","Low","Close")    #Meet the format of fuction
  full_sto <- data.frame(stoch(data_for_sto))
  Stochastic_Oscillator <- full_sto$fastK * 100
  
  # Get the RSI_Move
  
  the_RSI <- RSI(Close_Price)     #Get the RSI
  RSI_Move <- diff(the_RSI)       #Get the difference as previous day
  RSI_Move[RSI_Move < 0] <- 0       # 0 means going down
  RSI_Move[RSI_Move > 0] <- 1       # 1 means going up
  RSI_Move <- data.frame(RSI_Move)  # Transfer to data frame
  RSI_Move <- rbind("N/A",RSI_Move) # Move down for one row
  date_market <- yahooData$rowname
  #_______________________Temporary full data_________________________________
  
  fulldata_temp <- data.frame(date_market,Open_Price,Close_Price,
                              High_Price,Low_Price, Stochastic_Oscillator,
                              RSI_Move,row.names = NULL)
  colnames(fulldata_temp) <- c("Date","Open","Close","High","Low","Stochastic Oscillator", "RSI Move")
  
  # ______________________________Create the target_____________________________
  
  # Based on Target 2: O(i+1) - O(i)
  
  Target <- diff(Open_Price)
  temp = rep(NA, length(Target) + 1)
  temp[1:length(Target)] = Target
  Target = data.frame(temp)
  
  colnames(Target) <- "Target"
  
  Target[Target<0] <- 0       # 0 means going down
  Target[Target>0] <- 1       # 1 means going up
  Target <- data.frame(Target)  # Transfer to data frame
  
  fulldata_temp <- cbind(fulldata_temp,Target)
  
  #_________________Finalize the data_______________________
  
  
  fulldata <- fulldata_temp[20:nrow(fulldata_temp),]
  
  move_cols <- sapply(fulldata, is.character)
  move_cols[["RSI Move"]] <- TRUE
  move_cols[["Target"]] <- TRUE
  move_data <- as.data.frame(sapply(fulldata[,move_cols], as.factor))
  move_col_names <- names(move_data)
  non_move_data <- fulldata[, -which(names(fulldata) %in% move_col_names)]
  fulldata <- cbind(non_move_data,move_data)
  
  Target <- fulldata$Target
  fulldata$Target <- NULL
  fulldata <- cbind(fulldata,Target)
  
  fulldata = fulldata[,2:ncol(fulldata)]
  numRows = dim(fulldata)[1]
  lastday = fulldata[numRows, ] 
  fulldata = fulldata[1:numRows-1, ]
  
  # Create the training and testing data sets
  set.seed(123)
  splitIndex <- createDataPartition(fulldata$Target, p = .9, list = FALSE, times = 1)
  trainDF <- fulldata[splitIndex,]
  testDF <- fulldata[-splitIndex,]  
  model <- train(Target ~ ., trainDF, method='nnet', trace = FALSE) # train
  # we also add parameter 'preProc = c("center", "scale"))' at train() for centering and scaling the data
  prediction <- predict(model, testDF)                           # predict
  table(prediction, testDF$Target) 
  lastdayPred <- predict(model, lastday)
  if (lastdayPred == 0)
    return("Going down")
  else
    return("Going up")
}
