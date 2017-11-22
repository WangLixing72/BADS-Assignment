proj.class_col <- function(df,class){
  #This is a function returns the names of columns that are defined in a specific class.
  #df: The dataframe you want to operate.
  #class: a character variable in "numeric","factor","character","logical"
  evaluate <- function(x){
    temp <- paste("is.",class,sep = "")
    c1 <- call(temp,x)
    c2 <- eval(c1)
  }
  d.temp <- sapply(df,evaluate)
  d.logic <- subset(d.temp,d.temp == T)
  names(df[,names(df) %in% names(d.logic)])
}


proj.initial <- function(){
  #This is a function to set the work directory automatically.
  if(! "rstudioapi" %in% rownames(installed.packages())){install.packages("rstudioapi")}
  wd <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(wd)
}

proj.get.rawdata <- function(){
  #This is a function to get the raw data of this assignment.
  raw_d <- read.csv('BADS_WS1718_known.csv')
}

proj.as.date <- function(df){
  #This is a function to transfer time character in the raw dataframe into R date class.
  #It returns the dataframe that only contains the variable in R date class.
  c_data <- data.frame(order_date = as.Date(df$order_date),delivery_date = as.Date(df$delivery_date),
                       user_dob = as.Date(df$user_dob),user_reg_date = as.Date(df$user_reg_date))
  #temp <- subset(raw_d,select = -c(order_date,delivery_date,user_dob,user_reg_date))
  #c_data <- cbind(c_data,temp)
  return(c_data)
}


proj.create.date.var <- function(df){
  #Creat new attributes, such as delivery_time, age,days_reg
  #delivery_time: the time consumed for delivery
  #age:user's age
  #days_reg: days after registration
  df$delivery_time <- as.numeric(df$delivery_date - df$order_date)
  if(! "lubridate" %in% rownames(installed.packages())){install.packages("lubridate")}
  library(lubridate,warn.conflicts = F)
  df$age <- year(Sys.Date()) - year(df$user_dob)
  df$days_reg <- as.numeric(Sys.Date() - df$user_reg_date)
  return(df)
}


proj.trans.delivery_time <- function(date.data,numer.data){
  #Deal with Errors in delivery_time
  data.delivery <- cbind(date.data,numer.data)
  data.delivery.error <- subset(data.delivery,delivery_time < 0)

  #All error delivery_time data have delivery_date at 1990-12-31.
  #Generally, we consider using the median delivery date to replace them.
  data.delivery.ne <- subset(data.delivery,delivery_time >=  0)
  select<-data.delivery$delivery_time<0 & is.na(data.delivery$delivery_time) == FALSE
  data.delivery[select,c("delivery_time")] <- median(data.delivery.ne$delivery_time,na.rm =TRUE)

  #Deal with NAs in delivery_time.
  data.delivery.all <- data.delivery
  data.delivery.na <- subset(data.delivery.all,is.na(delivery_time))

  #We find that all orders with missing value in delivery_date are not returned.
  #Accordingly, we'd better create a new attribute to show this strong evidence.
  select<-is.na(data.delivery$delivery_time) == TRUE
  data.delivery[select,c("delivery_time")] <- median(data.delivery.ne$delivery_time,na.rm =TRUE)
  #Take a deeper look at the extreme value in delivery time.
  #Can't be sure the extreme values in delivery time are errors or not.
  #Return the cleaned data to c_data.

  numer.data$na_delivery_date <- as.factor(is.na(date.data$delivery_date))
  numer.data$delivery_time <- data.delivery$delivery_time
  date.data$delivery_date <- date.data$order_date + data.delivery$delivery_time

  return.data <- list(cleaned = numer.data,inf = date.data,plot = data.delivery.error)

  return(return.data)
}

proj.trans.age <- function(date.data,numer.data){
  #Deal with Errors in age.
  #This function returns 4 values
  #(1)The cleaned data which given median value of age to thoes outliers
  #(2)The indicator variables of age 117, extreme value, and NAs.



  c_data <- cbind(date.data,numer.data)
  data.age<- c_data
  data.age.error <- subset(c_data,age == max(c_data$age))
  #All observations have error values in age are with user_dob 1900-11-19.

  #Naive Solution: Assign a median age to these users
  select <- data.age$age == max(c_data$age,na.rm = T) & (is.na(data.age$age) == F)
  data.age[select,c("age")] = median(data.age$age,na.rm =TRUE)
  data.age$emax_age <- as.factor(select)

  #Same procedure applied to age < 18 or age > 70.
  #We naively assert that peole younger than 18 or older than 70 will not shop on website.
  data.age.error <- subset(data.age,age < 18)
  select <- data.age$age <18 & is.na(data.age$age) == F
  temp.s1 <- data.age$age <18 & data.age$age >70 & (is.na(data.age$age) == F)
  #This can be optimized by saving the original value but just giving them a NA mark index.
  data.age[select,c("age")] = median(data.age$age,na.rm =TRUE)

  #Same procedure applied to age > 70.
  select <- data.age$age >70 & (is.na(data.age$age) == F)
  #This can be optimized by saving the original value but just giving them a NA mark index.
  data.age[select,c("age")] = median(data.age$age,na.rm =TRUE)
  data.age$ext_age <- as.factor(temp.s1)

  #Deal with NA's.
  select<-is.na(data.age$age) == TRUE
  data.age[select,c("age")] <- median(data.age$age,na.rm =TRUE)
  data.age$na_age <- as.factor(select)

  return(data.age)

}

clean.size <- function(c_data){
  data.item_size <- data.frame(size = c_data$item_size)
  data.item_size$class <- 1
  #data.item_size
  #grep("[a-z]",c_data$item_size,value = TRUE)
  cloth1 <- grep("[a-z]",c_data$item_size)
  data.item_size[cloth1,2] <- "cloth1"
  cloth2 <- grep("[A-Z]",c_data$item_size)
  data.item_size[cloth2,2] <- "cloth2"
  shoes <- grep("[0-9]",c_data$item_size)
  data.item_size[shoes,2] <- "shoes"
  plus <- grep("[+]",c_data$item_size)
  data.item_size[plus,2] <- "plus"
  large <- grep("[0-9]{4}",c_data$item_size)
  data.item_size[large,2] <- "large"
  data.item_size$size <- as.character(data.item_size$size)

  temp <- subset(data.item_size,class == "shoes" & as.numeric(as.character(size)) <= 14)
  temp.row <- rownames(temp)
  data.item_size[temp.row,"class"] <- "unknown1"

  temp <- subset(data.item_size,class == "shoes" & as.numeric(as.character(size)) > 60)
  temp.row <- rownames(temp)
  data.item_size[temp.row,"class"] <- "unknown2"

  temp <- subset(data.item_size,class == "cloth1" & as.character(size) == "unsized")
  temp.row <- rownames(temp)
  data.item_size[temp.row,"class"] <- "unsize"

  temp <- subset(data.item_size,class == "cloth1" & size %in% c("l","m","xl"))
  temp.row <- rownames(temp)
  data.item_size[temp.row,"size"] <- "M"

  temp <- subset(data.item_size,class == "cloth1" & size %in% c("xxxl","xxl"))
  temp.row <- rownames(temp)
  data.item_size[temp.row,"size"] <- "L"

  temp <- subset(data.item_size,class == "cloth1" & size %in% c("s","xs"))
  temp.row <- rownames(temp)
  data.item_size[temp.row,"size"] <- "S"


  temp <- subset(data.item_size,class == "cloth2" & size %in% c("L","M","XL"))
  temp.row <- rownames(temp)
  data.item_size[temp.row,"size"] <- "M"

  temp <- subset(data.item_size,class == "cloth2" & size %in% c("XXXL","XXL"))
  temp.row <- rownames(temp)
  data.item_size[temp.row,"size"] <- "L"

  temp <- subset(data.item_size,class == "cloth2" & size %in% c("S","XS"))
  temp.row <- rownames(temp)
  data.item_size[temp.row,"size"] <- "S"

  #There maybe better classification rule for shoes.
  temp <- subset(data.item_size,class == "shoes" & as.numeric(size) > 44)
  temp.row <- rownames(temp)
  data.item_size[temp.row,"size"] <- "L"

  temp <- subset(data.item_size,class == "shoes" & as.numeric(size) < 36)
  temp.row <- rownames(temp)
  data.item_size[temp.row,"size"] <- "S"

  temp <- subset(data.item_size,class == "shoes" & as.numeric(size) >= 36 & as.numeric(size) <= 44)
  temp.row <- rownames(temp)
  data.item_size[temp.row,"size"] <- "M"

  #There maybe better classification rule for "large".
  temp <- subset(data.item_size,class == "large" & as.numeric(size) > 3634)
  temp.row <- rownames(temp)
  data.item_size[temp.row,"size"] <- "L"

  temp <- subset(data.item_size,class == "large" & as.numeric(size) < 3432)
  temp.row <- rownames(temp)
  data.item_size[temp.row,"size"] <- "S"

  temp <- subset(data.item_size,class == "large" & as.numeric(size) >= 3432 & as.numeric(size) <= 3634)
  temp.row <- rownames(temp)
  data.item_size[temp.row,"size"] <- "M"


  temp <- subset(data.item_size,class == "unknown1" & as.numeric(size) > 9)
  temp.row <- rownames(temp)
  data.item_size[temp.row,"size"] <- "L"

  temp <- subset(data.item_size,class == "unknown1" & as.numeric(size) < 6)
  temp.row <- rownames(temp)
  data.item_size[temp.row,"size"] <- "S"

  temp <- subset(data.item_size,class == "unknown1" & as.numeric(size) >= 6 & as.numeric(size) <= 9)
  temp.row <- rownames(temp)
  data.item_size[temp.row,"size"] <- "M"


  temp <- subset(data.item_size,class == "unknown2" & as.numeric(size) > 152)
  temp.row <- rownames(temp)
  data.item_size[temp.row,"size"] <- "L"

  temp <- subset(data.item_size,class == "unknown2" & as.numeric(size) <= 105)
  temp.row <- rownames(temp)
  data.item_size[temp.row,"size"] <- "S"

  temp <- subset(data.item_size,class == "unknown2" & as.numeric(size) > 105 & as.numeric(size) <= 152)
  temp.row <- rownames(temp)
  data.item_size[temp.row,"size"] <- "M"

  #Class "plus" is a little bit special.
  temp <- subset(data.item_size,class == "plus")

  char.sp <- function(x){
    char.a <- strsplit(x,"")
    paste(grep("[0-9]",char.a[[1]],value = TRUE),collapse = "")
  }

  temp$size <- sapply(temp$size,char.sp)

  temp1 <- subset(temp,size > 43)
  temp.row <- rownames(temp1)
  data.item_size[temp.row,"size"] <- "L"
  data.item_size[temp.row,"class"] <- "pshoes"

  temp1 <- subset(temp,size < 37 & size > 12)
  temp.row <- rownames(temp1)
  data.item_size[temp.row,"size"] <- "S"
  data.item_size[temp.row,"class"] <- "pshoes"

  temp1 <- subset(temp,size >= 37 & size <= 43)
  temp.row <- rownames(temp1)
  data.item_size[temp.row,"size"] <- "M"
  data.item_size[temp.row,"class"] <- "pshoes"

  temp1 <- subset(temp,size > 8 & size <=12)
  temp.row <- rownames(temp1)
  data.item_size[temp.row,"size"] <- "L"
  data.item_size[temp.row,"class"] <- "punknown"

  temp1 <- subset(temp,size >= 5 & size <= 8)
  temp.row <- rownames(temp1)
  data.item_size[temp.row,"size"] <- "M"
  data.item_size[temp.row,"class"] <- "punknown"

  temp1 <- subset(temp,size < 5)
  temp.row <- rownames(temp1)
  data.item_size[temp.row,"size"] <- "S"
  data.item_size[temp.row,"class"] <- "punknown"


  data.item_size$size <- as.factor(data.item_size$size)
  data.item_size$class <- as.factor(data.item_size$class)

  return(data.item_size)
}

clean.raw <- function(){
  proj.initial()
  r_data <- proj.get.rawdata()

  #In data cleaning session, 2 data set are intended to be specified.
  #The first one is the cleaned dataset, which saved as data.frame and contains the variables that are most relevant to the
  #model construction

  #The second one is the information data set, which saved as a list object. It contains the categorized informative variables that are temporarily
  #excluded from the cleaned data.

  #And some data need to be ploted are also saved.


  #First step: Dealing with the variables contain time character.
  c_data.date <- proj.as.date(r_data)

  #Second step: Cleaning and transforming the time varibales
  #3 new attributes are created:
  #delivery_time: the time consumed for delivery;
  #age:user's age;
  #days_reg: days after registration.
  c_data.date.nvar <- proj.create.date.var(c_data.date)
  name.date <- proj.class_col(c_data.date.nvar,"Date")

  #The variables that will be saved into cleaned data set.
  c_data.time.variable <- c_data.date.nvar[,names(c_data.date.nvar) %in% name.date == F]

  #The variables that will be saved into information data set.
  inf.date <- c_data.date.nvar[,name.date]
  inf <- list(date = inf.date)

  #Outliers and NAs in delivery_date.
  #Naive Solution: Median value is given to NAs and keeping the outliers.
  temp.delivery_date <- proj.trans.delivery_time(inf$date,c_data.time.variable)
  c_data.time.variable <- temp.delivery_date$cleaned
  inf$cleaned_delivery <- temp.delivery_date$inf$delivery_date
  pldata <- list(na_delivery <- temp.delivery_date$plot)
  pldata$ext_age <- c_data.time.variable$age

  #Outliers and NAs in age.
  #Naive Solution: Median value is given to NAs and other outliers.
  #na_age,ext_age,emax_age are 3 indicator variable indicating NAs, outliers, 117-age data,respectively.
  temp.age <- proj.trans.age(inf$date,c_data.time.variable)
  c_data.time.variable$age <- temp.age$age
  c_data.time.variable$emax_age <- temp.age$emax_age
  c_data.time.variable$ext_age <- temp.age$ext_age
  c_data.time.variable$na_age <- temp.age$na_age

  #Third step: Extracting good classes and size information in variable size.
  #9 classes of goods are identified:cloth1 cloth2 large pshoes punknown shoes unknown1 unknown2 unsize
  #All the values of size are standardized to S,M,L
  c_data.item_size <- clean.size(r_data)

  #Return the cleaned training data set.
  proj.class_col(r_data,"factor")
  r_v <- subset(r_data,select = -c(item_size,order_date,delivery_date,user_dob,user_reg_date))
  T.data <- list(c_data = cbind(c_data.time.variable,c_data.item_size,r_v),inf = inf,plot = pldata)
  return(T.data)
}


#data.brand <- data.frame(brand = as.numeric(c_data$brand_id),return = c_data$return)
#data.brand.table <- table(data.brand)
#data.brand.pr <- data.brand.table[,2]/apply(data.brand.table,1,sum)
#hist(data.brand.pr)
#data.brand.class <- ifelse(data.brand.pr > 0.60, 1,ifelse(data.brand.pr > 0.40,0,-1))
#data.brand.class <- as.matrix(data.brand.class)
#data.brand.a <- rownames(data.brand.class[data.brand.class == 1,,drop = FALSE])
#data.brand.b <- rownames(data.brand.class[data.brand.class == 0,,drop = FALSE])
#data.brand.c <- rownames(data.brand.class[data.brand.class == -1,,drop = FALSE])
#data.brand[data.brand$brand %in% as.numeric(data.brand.a),1] <- "high"
#data.brand[data.brand$brand %in% as.numeric(data.brand.b),1] <- "middle"
#data.brand[data.brand$brand %in% as.numeric(data.brand.c),1] <- "low"
#data.brand$brand <- as.factor(data.brand$brand)

d <- T.data$c_data
col2rgb(as.character(d$item_color), alpha = FALSE)

f <- function(x){
  result <- tryCatch({
    col2rgb(x, alpha = FALSE)
  }, warning = function(warning_condition) {
    message("wrong")
  }, error = function(error_condition) {
    message("wrong")
  })
}
names(M) <- unique(as.character(d$item_color))
M <- lapply(unique(as.character(d$item_color)),f)
names(subset(M,is.null(M)))
n <- lapply(M,is.null)
names(n[n == TRUE])
names(n[n == FALSE])
