#Homework Number 7
#Collaborated on this homework with different people

require(ggplot2)
require(grid)

#First we will run a couple scenarios to create a combined function that will use different features

ExampleData <-diamonds[1:100, ]   #First we create a set of data for testing from an already existing dataframe in ggplot2; our example is the diamond data set


#Now we will determine if a vector is binary or not by using the is.binary function
#Our input is a vector, while our output is True or False (whether or not its binary)
is.binary <-function(v) 
{
  x <-unique(v)  #Here we check to make sure all the elements are unique/distinct and put them in a vector named x
  length(x) - sum(is.na(x)) == 2L  #Here we check to see if x only contains 2 distinct values
}


#Here we will use the freq_table function to find out all the factor class columns; it will also print a frequency table for each factor column
#Our input is a dataframe; our output is a factor class table
freq_table <-function(data) 
{
  lapply(data[, sapply(data,is.factor)], table)   #We used the sapply and lapply functions from the previous homework, and can use them here as well 
}


#Here we use the printSummary function, which prints out a summary table for a given dataframe
#Our input is a dataframe; our output is a summary table
printSummary <-function(data)
{
  lapply(data[, sapply(data,is.numeric)], summary)
}


#We used the Pearson function from the previous homework.
#It takes any dataframe as a parameter and returns a dataframe that contains each pair of column names in the first column as a single string
#seperated by a "-" and their corresponding Pearson correlation coefficient in the second column.
#Our input is a dataframe
#Our ouput is the combination of column names and their Pearson coefficients
pearson<- function(data)
{
  Numeric <-sapply(data, is.numeric)        #Here we check to see if the columns are numeric
  ExploreData <-data[,Numeric]              #Here we create a new dataframe to store the numeric columns 
  Names <-ColumnNames(Explore Data)         #Here we create vector to store the numeric column names 
  ComboNames <-combn(Names, 2)          #Here we find all the combinations of any 2 column names 
  Combo <- combn(length(ColumnNames(ExploreData)), 2)     #Here we find all of the combination of the indices of the column names
  a <-paste(ComboNames[1,],ComboNames[2,],sep = '-')      #Here we create vector to store the combinations
  pearson <-PearsonCoefficent <- c()    #Here we create vectors with empty sets
  
  for(i in 1:length(a))
{
    p <-cor(x= ExploreData[combo[1,i]], y = ExploreData[combo[2,i]])  #Here we calculate the correlations between any two of the columns
    PearsonCoefficent[i] <- p[1]      #Here we extract the number from a list 
  }
  return(data.frame(a, Pearsoncoefficient))    #Here we combine the information into dataframe
}



#The absolute_pearson function takes a dataframe, consisting of pearson correlations with 2 variables, and extracts the values that are
#greater than a threshold based on the user's input
#Our input is a dataframe
#Our ouput is the combination of column names and their Pearson coefficient values that are greater than a specified threshold
absolute_pearson <-function(dataset, threshold)
{
  row_index <- which(abs(dataset[,2]) > threshold)      #Here we determine which column is greater than the given threshold
  return(dataset[row_index, ])                          #Here we return a new dataframe with the updated coefficients
} 


#The Calculate_Rsquare function takes a dataframe and determines the R-square values of each of the variables
#Our input is a dataframe
#Our ouput is the combination of column names and their respective R-square values 
Calculate_Rsquare<- function(data)
{
  Numeric <-sapply(data, is.numeric)                                    #Here we check to see if columns are numeric
  ExploreData <-data[,Numeric]                                          #Here we create a new dataframe to store the numeric columns 
  Names <-ColumnNames(ExploreData)                                      #Here we create vector to store the numeric column names 
  ComboNames <-combn(Names, 2)                                          #Here we find all the combinations of any 2 column names 
  Combo <-combn(length(ColumnNames(ExploreData)), 2)                    #Here we find all the combination of the indices of the column names
  a <-paste(ComboNames[1,], ComboNames[2,], sep = '-')                  #Here we create vector to store the combinations
  Rsquare <- c()                                                        #Here we create vectors, consisting of the empty set, to store Rsquare values
  
  for(i in 1:length(a)){                       
    Regression <-paste0(ComboNames[1,i], " ~ ", ComboNames[2,i])              #Here we maunally utilize the regression formula to avoid the result of a list by using the lm() function
    r1 <- summary( lm(as.formula(Regression), data=ExploreData) )$r.squared      #Here we extract the Rsquare values
    Rsquare[i] <- r1                                          
  }
  return(data.frame(a, Rsquare))    #Here we combine the data into a dataframe
}


#I had help on this part of the homework. I was able to use the multiplot functionality from R-cookbook.
#It combines subplots plots into a grid and prints out the data 
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) 
{
  
  #Here we want to make a list from the ... arguments and plot the list
  plots <-c(list(...), plotlist)
  
  NumberPlots = length(plots)
  
  #If layout is NULL, then we use 'cols' to determine layout
  if (is.null(layout)) {
    #First we make the panel
    layout <- matrix(seq(1, cols * ceiling(NumberPlots/cols)),				#The nCol variable will be the number of columns of plots
                     nCol = cols, nRow = ceiling(NumberPlots/cols))			#The nRow variable will be the number of needed and will be calculated from the number of columns
  }
  
  if (NumberPlots==1) {	#If the number of plots is one, then we only print one plot
    print(plots[[1]])
    
  } else {
    grid.newpage()		#Here we set up the grids of plots on a new page if the number of plots is not one
    pushViewport(viewport(layout = grid.layout(nRow(layout), nCol(layout))))
    
    for (i in 1:numPlots) {		#Here we make sure each plot is in its appropriate location on the page; to do this, we will need to figure out its exact location:
      matchidx <-as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#The Numeric_Plot function executes in the following directions: 
#If the plot switch parameter is �on� or �grid�, then plot a pair of blue histograms with a vertical red line at the mean (one using counts and the other density) for 
#every numerical variable at each number of bins integer specified in the bin vector parameter. If the plot switch is set to �grid�, then the function prints a grid for each count-bin 
#combination and a separate grid for each density-bin size combination.
#Our input is a dataframe, a string, and an optional vector
#Our output is a grid with plots containing the count and desnity histograms

Numeric_Plot <- function(data, Plot_Switch, binVec) {
  Numeric <-sapply(data, is.numeric)         #Here we check to see which columns are numeric
  data <-data[,Numeric]                      #Here we extract the numeric columns
  for(name in ColumnNames(data)) {           #Here we will loop through the columns in the dataset
    
    if(Plot_Switch == "on"){                 #Here is the case when the switch is "on"
      grid.newpage()          		   #We put the output on a new page
      r <- lapply(data[name], mean)          # Here we find the mean of that currently iterated column
      plot1 <- ggplot(data, aes_string(name)) + geom_histogram(fill="blue") + geom_vline(xintercept = m[[1]], colour="red") 
      plot2 <- ggplot(data, aes_string(name)) + geom_histogram(aes(y= ..density..), fill="blue") + geom_vline(xintercept = m[[1]], colour="red")
      pushViewport(viewport(layout = grid.layout(1, 2)))
      print(plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
      print(plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
    }
    
    if(plot_switch == "grid"){          #Here is the case when switch is "grid"
      Count_Plots <-list()              #Here we create an  empty list to store the count for the histogram subplots of each bin size
      Density_Plots <-list()            #Here we create a empty list to store the density histograms subplots of each bin size
      if(missing(binVec)){              #This takes of the case when the vector is null, prints histogram with default bins 30
        print(ggplot(data, aes_string(name), color = "blue") + geom_histogram(fill="blue")+ labs(title= "default bins"))
        print(ggplot(data, aes_string(name), color = "blue") + geom_histogram(aes(y= ..density..), fill="blue")+ labs(title= "default bins"))
      }else{                            #This takes care of the case when the user enters a vector
        for(i in 1:length(binVec)) {    #loop through each bin size and create a subplot
        z <-ggplot(data, aes_string(name), color = "blue") + geom_histogram(fill="blue", bins = binVec[i])+ labs(title= paste(binVec[i], "bins"))
        count_plots[[i]] <- z           #Push each subplot to a list 
        }
        multiplot(plotlist = count_plots, cols = 2)     
      
        for(i in 1:length(binVec)) {    #Here we loop through each bin size and create a subplot
          z <- ggplot(data, aes_string(name), color = "blue") + geom_histogram(aes(y= ..density..), fill="blue", bins = binVec[i])+ labs(title= paste(binVec[i], "bins"))
          Density_Plots[[i]] <- z       #Here we combine each subplot into a list
        }
        multiplot(plotlist = Density_Plots, cols = 2)
      
      }
    }
  }
}


#The Categorical_Binary_Plot function plots a gray bar graph for every categorical and binary variable.
#We want this particular plot to appear when the plot switch parameter is �on� or �grid"
#Our input is a dataframe and a string ("on" or "grid")
#Our output are gray bar graphs
Categorical_Binary_Plot <-function(data, Plot_Switch_Parameter){
  Categorical_Binary_Plot <- sapply(data, function(x) (is.factor(x) || is.logical(x)) || is.binary(x))    #Here we check to make sure we have categorical and binary columns
  Categorical_Binary_Plot <- data[Categorical_Binary]     #extract those columns
  
  if(plot_switch == "on" || plot_switch == "grid") {      #Here we check condition for the sring input 
    for(name in ColumNames(Categorical_Binary_Data)) {             #loop through the sorted dataframe and plot bar graphs for each column
      k <-ggplot(Categorical_Binary_Data, aes_string(name), color = "grey") + geom_bar(fill="grey")
      print(k)
    }
  }
}

#Below is the main function that we will use all the parts above for. It will have several prompts to the user for input while providing error messages to the user if
#inproper data is inputted. 

ExploreData <- function(dataframe, Plot_Switch, threshold, binVec){
  
  while(!is.data.frame(dataframe)){                 #Here we want to check to see if the user inputs a dataframe. If he/she does not, we prompt the user to choose a new file, which will be converted to a dataframe
    print("Your input is not a dataframe: ")
    print("Please upload a new csv or text file: ")
    file1 <- file.choose()
    dataframe <- read.csv(file1, header = T)
  }
  
  Button <- plot_switch
  while(button != "off" && button != "on" && button != "grid"){   #Here we check to see if Plot_Switch is valid input
    print("This is an invalid input for the plot switch. Try again.")
    n <- readline(prompt="Enter your option(off / on / grid): ")  #Here we have the user re-enter the input
    button <- n
  }
  
  threshold2 <- threshold
  while(!is.numeric(threshold2) || threshold2 < 0 || threshold2 >1 ){    #Here we check to see if the threshold is a valid input
    print("The correlation threshold must be numeric value within the interval [0,1]. Try again.")
    a <- readline(prompt="Enter your correlation threshold: ")      #Here we have the user re-enter the threshold input
    threshold <- as.numeric(a)
  }
  
  while (!missing(binVec) && TRUE %in% (binVec <= 0)) {                        #Here we check to see if the bins are positive; if they are not, we prompt the user to input them again
    a <- readline(prompt="Enter a size of the bin Vector (or enter e to end): ")
    if(a == "e"){
      stop("End")
    }
    else{
      binVec <- c()
      size <- as.numeric(a)
      for(i in 1:size){
        bin <- readline(prompt="Enter the number of bins: ")
        bin <- as.numeric(bin)
        binVec <- c(binVec, bin)
      }
    }
    
  }
  
  if (!missing(binVec) && !is.integer(binVec)) {            #Here we check to see if the bins are integers; if they are not, then we round them 
    binVec <- round(binVec)
  }
  
  
  New_Dataframe <- freq_table(dataframe)
  allSummary <- printSummary(dataframe)
  Coefficient_Table <- pearson(dataframe)
  Absolute_Coefficient_Table <-absolute_pearson(Coefficient_Table, threshold2)
  Rsquare_Table <- Calculate_Rsquare(dataframe)
  Numeric_Plot(dataframe, button, binVec)
  Categorical_Binary_Plot(dataframe, button)
  New_List <-list(new_dataframe, allSummary, Rsquare_Table, Absolute_Coefficient_Table)
  return(New_List)
  
}
