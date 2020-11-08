## Title: Summaries
## Author: Nick Wawee
## Description:
#This function will find the letter values and outliers from a given data list. 
#The input is a named list of data that contains one column by n observations.
#The output is a list of lists which contain the letter values as well as mild and extreme outliers.
#The letter values for this version of the function include the max, upper fourth, median, lower fourth, minimum, inner and outer upper and lower fences.
#The other list contains the mild and extreme outliers.
#The terminology was adopted from Jim Albert's notes on EDA. His GitHub link is here: https://github.com/bayesball
#It can be further improved by incorporating lapply instead of the for loop, but for small scale purposes the for loop will suffice.
#An additional improvement would be to incorporate the round function so that the user can specify the number of decimal places to be reported.
summaries2 = function(datalist){
  letterlist = list()
  outlierlist = list()
  for (name in names(datalist)){
    x = sort(datalist[[name]],decreasing = F) #selecting data
  
    #calculating depths
    depth_m = (length(x) + 1)/2
    depth_f = (depth_m + 1)/2
  
    #calculating letter values
  
    #Mean and Median
    med = median(x)
    average = mean(x)
  
    #Fourth Calculations
    if (depth_m + depth_f %% 1 > 0){
      ind1 = depth_m + depth_f
      ind2 = depth_m - depth_f
      fourth1 = (x[floor(ind1)] + x[ceiling(ind1)])/2
      fourth2 = (x[floor(ind2)] + x[ceiling(ind2)])/2
    }else{
      fourth1 = x[depth_m + depth_f]
      fourth2 = x[depth_m - depth_f]
    }
  
    f_spread = fourth1 - fourth2

    #Max and min
    mx = max(x)
    mn = min(x)
  
  
    #Storing Results for Table
    letterlist[[name]]=data.frame(Letter = c("Maximum", "Upper Fourth", "Median", "Lower Fourth", "Minimum", "Outer Upper Fence", "Inner Upper Fence", "Inner Lower Fence", "Outer Lower Fence"), Value = c(mx, fourth1, med, fourth2, mn, fourth2 + 2*1.5*f_spread, fourth2 + 1.5*f_spread, fourth1- 1.5*f_spread, fourth1 - 2*1.5*f_spread))
  
    #Finding outliers
    mildind1 = which(x > fourth2 + 1.5*f_spread & x < fourth2 +2*1.5*f_spread)
    mildind2 = which(x < fourth1 - 1.5*f_spread & x > fourth1 - 2*1.5*f_spread)
    mild = x[c(mildind1, mildind2)]
    extreme = x[which(x > fourth2 + 2*1.5*f_spread | x < fourth1 - 2*1.5*f_spread)]
    outlierlist[[name]] = list(mi = mild, ex = extreme)
  }
  return(list(letters = letterlist, outliers = outlierlist))
}#This version of the function does not calculates eighths
