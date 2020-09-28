symb<-getSymbols("AAPL", src = "yahoo", from = as.Date("26March2019","%d%b%Y") - 730, to = as.Date("26March2019","%d%b%Y"), auto.assign = FALSE)
symb<-data.frame(symb)
symb<-tibble::rownames_to_column(symb)
adjust_coff <- symb[,5]/symb[,7]  # Get the adjust index
Open_Price <- symb[,2]/adjust_coff   # Get the adjusted value based on index, similar below
Close_Price <- symb[,7]
High_Price <- symb[,3]/adjust_coff
Low_Price <- symb[,4]/adjust_coff

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
date_market <- symb$rowname
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

nn=neuralnet(Target~Open+Close+High+Low,data=fulldata, hidden=3,algorithm="rprop+",act.fct = "logistic",
             linear.output = FALSE)
plot(nn)
