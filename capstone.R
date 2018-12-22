' \vspace{8pt}
#' ## **Goal:**
#' \vspace{8pt}
#' ElecKart is an e-commerce firm specialising in electronic products. Over the 
#' last one year, they had spent a significant amount of money in marketing. 
#' Occasionally, they had also offered big-ticket promotions (similar to the Big 
#' Billion Day). They are about to create a marketing budget for the next year 
#' which includes spending on commercials, online campaigns, and pricing & promotion 
#' strategies. The CFO feels that the money spent over last 12 months on marketing 
#' was not sufficiently impactful and that they can either cut on the budget or 
#' reallocate it  optimally across marketing levers to improve the revenue response
#' \vspace{8pt}



# ***************************************************************************
#                   LOAD LIBRARY ----
# ***************************************************************************
#+ warning=FALSE, message=FALSE
library(lubridate)
library(dplyr)
library(ggplot2)
library(MASS)
library(car)
library(Hmisc)   # describe


# ***************************************************************************
#                   PROCs ----
# ***************************************************************************
# Function to compute the week number w.r.t origin date.
# It takes data and orgin in Date format as arguments.
nweek <- function(x, format="%Y-%m-%d", origin){
  if(missing(origin)){
    as.integer(format(strptime(x, format=format), "%W"))
  }else{
    x <- as.Date(x, format=format)
    o <- as.Date(origin, format=format)
    w <- as.integer(format(strptime(x, format=format), "%w"))
    2 + as.integer(x - o - w) %/% 7
  }
}


#' \newpage
# ***************************************************************************
#                   LOAD DATA ---- Transaction Data ----
# ***************************************************************************
# Make sure you are in current directory as in R-file is in. Should I do a commit?yes..

ce_data <- read.csv('./input/ConsumerElectronics.csv',stringsAsFactors = FALSE)
#' \vspace{8pt}
str(ce_data)
#' \vspace{8pt}



# ***************************************************************************
#                   DATA CLEANING ----
# ***************************************************************************


# . . . .   Outlier Treatment ----
# Remove orders before July'15 and after June'16
ce_data$order_date <- format(as.POSIXct(ce_data$order_date,format='%Y-%m-%d'),
                             format='%Y-%m-%d')
ce_data$order_date <- as.Date(ce_data$order_date, format = "%Y-%m-%d")

ce_data <- subset(ce_data, order_date > "2015-6-30" & order_date < "2016-7-1")


# . . . .   Correct Data Types ----

# 'order_id', 'order_item_id', 'cust_id', 'pincode' are qualitative data
#  having numeric values, let's convert them to character type

ce_data <- cbind(ce_data[,-c(5,6,11,12)],
                 sapply(ce_data[,c(5,6,11,12)],as.character) )  
# operate on interested columns


# . . . .   Missing Values ----

# Since 80% of the variable has 'No Expected delay'(\\N)
# will assume these scenarios as zero delay.
ce_data$deliverybdays[ce_data$deliverybdays=='\\N'] <- '0'
ce_data$deliverycdays[ce_data$deliverycdays=='\\N'] <- '0'
ce_data$deliverybdays <- as.integer(ce_data$deliverybdays)
ce_data$deliverycdays <- as.integer(ce_data$deliverycdays)

# delivery b/c days of 0-10 are appropriate, 
# lets omit rows where delivery b/c days are out of bounds
ce_data <- subset(ce_data, deliverybdays>=0 & deliverybdays<=10)
ce_data <- subset(ce_data, deliverycdays>=0 & deliverycdays<=10)

ce_data <- na.omit(ce_data)   # 4904 missing values, can be ignored





# ***************************************************************************
#                   FEATURE ENGINEERING ----
# ***************************************************************************

# create week,  week numbers start from min 'order date'
# . . . . Week Numbers ----
dates <- as.Date(
  gsub(" .*","",ce_data$order_date)
)
ce_data$week <- nweek(dates,origin = as.Date("2015-07-01"))


# . . . . Days, weeks, Month ----
# will compute Month, week, and no.of days per week (month, week)
# 
dys <- seq(as.Date("2015-07-01"),as.Date("2016-06-30"),'days')
weekdays <- data.frame('days'=dys, Month = month(dys), 
                       week = nweek(dys,origin = as.Date("2015-07-01")),
                       nweek = rep(1,length(dys)))
weekdays <- data.frame(weekdays %>% 
                         group_by(Month,week) %>% 
                         summarise(nweeks = sum(nweek)))
weekdays$fracDays <- weekdays$nweeks/7


# . . . . Strip Spaces ----
ce_data$product_analytic_vertical <- gsub(" +","",ce_data$product_analytic_vertical)


# . . . . Generate Discount ----
ce_data$discount <- ((ce_data$product_mrp - ce_data$gmv)/ce_data$product_mrp) * 100

# . . . .  Payment Type ----
ce_data$COD     = as.integer(ce_data$s1_fact.order_payment_type=='COD')
ce_data$Prepaid = as.integer(ce_data$s1_fact.order_payment_type!='COD')



#' \newpage
# ***************************************************************************
#                   LOAD DATA ---- Media & Inv Data ----
# ***************************************************************************
# . . . .   ProductList ----
productList_data      <- 
  read.csv("./input/ProductList.csv", stringsAsFactors = FALSE, 
           na.strings=c('\\N'))

# . . . .   Media Investment ----
mediaInvestment_data  <- 
  read.csv("./input/MediaInvestment.csv", stringsAsFactors = FALSE)

# . . . .  Special Sale Event ----

specialSale_data      <- 
  read.csv("./input/SpecialSale.csv", stringsAsFactors = FALSE)

# . . . .   Monthly NPS ----
monthlyNPS_data       <- 
  read.csv("./input/MonthlyNPSscore.csv", stringsAsFactors = FALSE )




# ***************************************************************************
#                   DATA PREPARATION ----
# ***************************************************************************

# . . . .   ProductList ----
productList_data <- na.omit(productList_data)

# . . . . . . . .  Correct Data types ----
productList_data$Frequency <- as.integer(productList_data$Frequency)
#' \vspace{8pt}
str(productList_data)
#' \vspace{8pt}

# . . . .   Media Investment ----
str(mediaInvestment_data)
#' \vspace{8pt}

# . . . . . . . .  Missing Values ----
mediaInvestment_data[is.na(mediaInvestment_data)] <- 0   # zero investment


# . . . . . . . .  Convert to weekly data ----
# convert montly spend to weekly
mediaInvestment_data <- cbind(Month=mediaInvestment_data[,c(2)],
                              mediaInvestment_data[,-c(1,2)]/4.30)
# Add weekly information
mediaInvestment_weekly <- merge(weekdays,mediaInvestment_data, by='Month', 
                                all.x = TRUE)

# Convert media Investment at weekly granularity
# pro-rate weekly investment as per the ratio of its days span over adjacent months
mediaInvestment_weekly <- 
  data.frame(
    mediaInvestment_weekly %>% 
      group_by(week) %>% 
      summarise(TotalInvestment = sum(Total.Investment*fracDays),
                TV = sum(TV*fracDays), 
                Digital=sum(Digital*fracDays),
                Sponsorship = sum(Sponsorship*fracDays), 
                ContentMarketing = sum(Content.Marketing*fracDays),
                OnlineMarketing = sum(Online.marketing*fracDays), 
                Affiliates = sum(Affiliates*fracDays),
                SEM = sum(SEM*fracDays), 
                Radio = sum(Radio*fracDays), 
                Other = sum(Other*fracDays))
  )

# . . . .   SPecialSale ----
str(specialSale_data)
#' \vspace{8pt}
specialSale_data$Date      <- as.Date(specialSale_data$Day, format = "%m/%d/%Y")
specialSale_data$week      <- nweek(specialSale_data$Date,origin = as.Date("2015-07-01"))
specialSale_data           <- data.frame(table(specialSale_data$week))
colnames(specialSale_data) <- c('week','n_saledays')


# . . . .   Monthly NPS ----
monthlyNPS_weekly   <- merge(weekdays, monthlyNPS_data, by='Month', all.x = TRUE)
monthlyNPS_weekly   <- as.data.frame(monthlyNPS_weekly %>% group_by(., week) %>% 
                                       summarise(., NPS = mean(NPS)))



#' \newpage
# ***************************************************************************
#                   WEEKLY DATA AGGREGATION ----
# ***************************************************************************
ce_data$sla <- as.integer(ce_data$sla)
ce_data_weekly <-  ce_data %>% 
  group_by(product_analytic_sub_category,
           week) %>% 
  summarise(gmv=sum(gmv), 
            product_mrp=mean(product_mrp), 
            units=sum(units),
            discount=mean(discount),
            sla=mean(sla), 
            procurement_sla=mean(product_procurement_sla),
            COD=sum(COD),
            Prepaid = sum(Prepaid),
            deliverybdays=mean(deliverybdays),
            deliverycdays=mean(deliverycdays))

ce_data_weekly <- as.data.frame(ce_data_weekly)   # type cast to data.frame



# ***************************************************************************
#                   MERGING DATA ----
# ***************************************************************************

# . . . .   Merge MediaInvestment & NPS ----
media_nps <- merge(mediaInvestment_weekly, monthlyNPS_weekly, by = 'week', 
                   all.x = TRUE)


# . . . .   Merge Sales & SaleDays
data <- merge(ce_data_weekly, specialSale_data, by = 'week', all.x = TRUE)
data[is.na(data$n_saledays),'n_saledays'] = 0
# data$Sales.Name[is.na(data$Sales.Name)] <- "No sale"


#. . . .   Merge Data & Media_NPS
data <- merge(data, media_nps, by = 'week', all.x = TRUE)


#' ---
#' title: 'MarketMixModeling DataPreparation'
#' author: 'Atchireddy chavva'
#' output: pdf_document
#' ---


#' \vspace{8pt}
#' ## **Goal:**
#' \vspace{8pt}
#' ElecKart is an e-commerce firm specialising in electronic products. Over the 
#' last one year, they had spent a significant amount of money in marketing. 
#' Occasionally, they had also offered big-ticket promotions (similar to the Big 
#' Billion Day). They are about to create a marketing budget for the next year 
#' which includes spending on commercials, online campaigns, and pricing & promotion 
#' strategies. The CFO feels that the money spent over last 12 months on marketing 
#' was not sufficiently impactful and that they can either cut on the budget or 
#' reallocate it  optimally across marketing levers to improve the revenue response
#' \vspace{8pt}



# ***************************************************************************
#                   LOAD LIBRARY ----
# ***************************************************************************
#+ warning=FALSE, message=FALSE
library(lubridate)
library(dplyr)
library(ggplot2)
library(MASS)
library(car)
library(Hmisc)   # describe


# ***************************************************************************
#                   PROCs ----
# ***************************************************************************
# Function to compute the week number w.r.t origin date.
# It takes data and orgin in Date format as arguments.
nweek <- function(x, format="%Y-%m-%d", origin){
  if(missing(origin)){
    as.integer(format(strptime(x, format=format), "%W"))
  }else{
    x <- as.Date(x, format=format)
    o <- as.Date(origin, format=format)
    w <- as.integer(format(strptime(x, format=format), "%w"))
    2 + as.integer(x - o - w) %/% 7
  }
}


#' \newpage
# ***************************************************************************
#                   LOAD DATA ---- Transaction Data ----
# ***************************************************************************
# Make sure you are in current directory as in R-file is in. Should I do a commit?yes..

ce_data <- read.csv('./input/ConsumerElectronics.csv',stringsAsFactors = FALSE)
#' \vspace{8pt}
str(ce_data)
#' \vspace{8pt}



# ***************************************************************************
#                   DATA CLEANING ----
# ***************************************************************************


# . . . .   Outlier Treatment ----
# Remove orders before July'15 and after June'16
ce_data$order_date <- format(as.POSIXct(ce_data$order_date,format='%Y-%m-%d'),
                             format='%Y-%m-%d')
ce_data$order_date <- as.Date(ce_data$order_date, format = "%Y-%m-%d")

ce_data <- subset(ce_data, order_date > "2015-6-30" & order_date < "2016-7-1")


# . . . .   Correct Data Types ----

# 'order_id', 'order_item_id', 'cust_id', 'pincode' are qualitative data
#  having numeric values, let's convert them to character type

ce_data <- cbind(ce_data[,-c(5,6,11,12)],
                 sapply(ce_data[,c(5,6,11,12)],as.character) )  
# operate on interested columns


# . . . .   Missing Values ----

# Since 80% of the variable has 'No Expected delay'(\\N)
# will assume these scenarios as zero delay.
ce_data$deliverybdays[ce_data$deliverybdays=='\\N'] <- '0'
ce_data$deliverycdays[ce_data$deliverycdays=='\\N'] <- '0'
ce_data$deliverybdays <- as.integer(ce_data$deliverybdays)
ce_data$deliverycdays <- as.integer(ce_data$deliverycdays)

# delivery b/c days of 0-10 are appropriate, 
# lets omit rows where delivery b/c days are out of bounds
ce_data <- subset(ce_data, deliverybdays>=0 & deliverybdays<=10)
ce_data <- subset(ce_data, deliverycdays>=0 & deliverycdays<=10)

ce_data <- na.omit(ce_data)   # 4904 missing values, can be ignored





# ***************************************************************************
#                   FEATURE ENGINEERING ----
# ***************************************************************************

# create week,  week numbers start from min 'order date'
# . . . . Week Numbers ----
dates <- as.Date(
  gsub(" .*","",ce_data$order_date)
)
ce_data$week <- nweek(dates,origin = as.Date("2015-07-01"))


# . . . . Days, weeks, Month ----
# will compute Month, week, and no.of days per week (month, week)
# 
dys <- seq(as.Date("2015-07-01"),as.Date("2016-06-30"),'days')
weekdays <- data.frame('days'=dys, Month = month(dys), 
                       week = nweek(dys,origin = as.Date("2015-07-01")),
                       nweek = rep(1,length(dys)))
weekdays <- data.frame(weekdays %>% 
                         group_by(Month,week) %>% 
                         summarise(nweeks = sum(nweek)))
weekdays$fracDays <- weekdays$nweeks/7


# . . . . Strip Spaces ----
ce_data$product_analytic_vertical <- gsub(" +","",ce_data$product_analytic_vertical)


# . . . . Generate List Price ----
ce_data$list_mrp <- as.integer(ce_data$gmv / ce_data$units)

# . . . . Generate Discount ----
ce_data$discount <- as.numeric((ce_data$product_mrp - ce_data$list_mrp) / ce_data$product_mrp)

# . . . .  Payment Type ----
ce_data$COD     = as.integer(ce_data$s1_fact.order_payment_type=='COD')
ce_data$Prepaid = as.integer(ce_data$s1_fact.order_payment_type!='COD')



#' \newpage
# ***************************************************************************
#                   LOAD DATA ---- Media & Inv Data ----
# ***************************************************************************
# . . . .   ProductList ----
productList_data      <- 
  read.csv("./input/ProductList.csv", stringsAsFactors = FALSE, 
           na.strings=c('\\N'))

# . . . .   Media Investment ----
mediaInvestment_data  <- 
  read.csv("./input/MediaInvestment.csv", stringsAsFactors = FALSE)

# . . . .  Special Sale Event ----

specialSale_data      <- 
  read.csv("./input/SpecialSale.csv", stringsAsFactors = FALSE)

# . . . .   Monthly NPS ----
monthlyNPS_data       <- 
  read.csv("./input/MonthlyNPSscore.csv", stringsAsFactors = FALSE )




# ***************************************************************************
#                   DATA PREPARATION ----
# ***************************************************************************

# . . . .   ProductList ----
productList_data <- na.omit(productList_data)

# . . . . . . . .  Correct Data types ----
productList_data$Frequency <- as.integer(productList_data$Frequency)
#' \vspace{8pt}
str(productList_data)
#' \vspace{8pt}

# . . . .   Media Investment ----
str(mediaInvestment_data)
#' \vspace{8pt}

# . . . . . . . .  Missing Values ----
mediaInvestment_data[is.na(mediaInvestment_data)] <- 0   # zero investment


# . . . . . . . .  Convert to weekly data ----
# convert montly spend to weekly
mediaInvestment_data <- cbind(Month=mediaInvestment_data[,c(2)],
                              mediaInvestment_data[,-c(1,2)]/4.30)
# Add weekly information
mediaInvestment_weekly <- merge(weekdays,mediaInvestment_data, by='Month', 
                                all.x = TRUE)

# Convert media Investment at weekly granularity
# pro-rate weekly investment as per the ratio of its days span over adjacent months
mediaInvestment_weekly <- 
  data.frame(
    mediaInvestment_weekly %>% 
      group_by(week) %>% 
      summarise(TotalInvestment = sum(Total.Investment*fracDays),
                TV = sum(TV*fracDays), 
                Digital=sum(Digital*fracDays),
                Sponsorship = sum(Sponsorship*fracDays), 
                ContentMarketing = sum(Content.Marketing*fracDays),
                OnlineMarketing = sum(Online.marketing*fracDays), 
                Affiliates = sum(Affiliates*fracDays),
                SEM = sum(SEM*fracDays), 
                Radio = sum(Radio*fracDays), 
                Other = sum(Other*fracDays))
  )

# . . . .   SPecialSale ----
str(specialSale_data)
#' \vspace{8pt}
specialSale_data$Date     <- as.Date(specialSale_data$Date, format = "%m/%d/%Y")
specialSale_data$week     <- nweek(specialSale_data$Date,origin = as.Date("2015-07-01"))
specialSale_data           <- data.frame(table(specialSale_data$week))
colnames(specialSale_data) <- c('week','n_saledays')


# . . . .   Monthly NPS ----
monthlyNPS_data$Date <- as.Date(monthlyNPS_data$Date, format = "%m/%d/%Y")
monthlyNPS_data$Month <- month(ymd(monthlyNPS_data$Date))
monthlyNPS_weekly   <- merge(weekdays, monthlyNPS_data, by='Month', all.x = TRUE)
monthlyNPS_weekly   <- as.data.frame(monthlyNPS_weekly %>% group_by(., week) %>% 
                                       summarise(., NPS = mean(NPS)))



#' \newpage
# ***************************************************************************
#                   WEEKLY DATA AGGREGATION ----
# ***************************************************************************
ce_data$sla <- as.integer(ce_data$sla)
ce_data_weekly <-  ce_data %>% 
  group_by(product_analytic_sub_category,
           week) %>% 
  summarise(gmv=sum(gmv), 
            product_mrp=mean(product_mrp),
            list_mrp=mean(list_mrp),
            units=sum(units),
            discount=mean(discount),
            sla=mean(sla), 
            procurement_sla=mean(product_procurement_sla),
            COD=sum(COD),
            Prepaid = sum(Prepaid),
            deliverybdays=mean(deliverybdays),
            deliverycdays=mean(deliverycdays))

ce_data_weekly <- as.data.frame(ce_data_weekly)   # type cast to data.frame



# ***************************************************************************
#                   MERGING DATA ----
# ***************************************************************************

# . . . .   Merge MediaInvestment & NPS ----
media_nps <- merge(mediaInvestment_weekly, monthlyNPS_weekly, by = 'week', 
                   all.x = TRUE)


# . . . .   Merge Sales & SaleDays
data <- merge(ce_data_weekly, specialSale_data, by = 'week', all.x = TRUE)
data[is.na(data$n_saledays),'n_saledays'] = 0



#. . . .   Merge Data & Media_NPS
data <- merge(data, media_nps, by = 'week', all.x = TRUE)




# . . . . Discount ----
data$discount <- (1-(data$list_mrp/data$product_mrp))*100



# ***************************************************************************
#                   SAVE DATA  ----
# ***************************************************************************
write.csv(data, file = "./intrim/eleckart.csv",row.names=FALSE)
#' \vspace{8pt}
str(data)
#' \vspace{8pt}





# ***************************************************************************
#                   FEATURE ENGINEERING -PASS2  ----
# ***************************************************************************

# . . . . List Price Inflation ----
data$chnglist <- c(0,diff(data$list_mrp))

# . . . . Discount ----
data$list_mrp     <- as.integer(data$gmv/data$units)
data$discount     <- (1-(data$list_mrp/data$product_mrp))*100

# . . . . Discount Inflation ----
data$chngdisc <- c(0,diff(data$discount))

# . . . . NPS Inflation ----
data$chngNPS  <- c(0,diff(data$NPS))

# . . . . Lag List Price ----
# Lag avg weekly list_mrp by 1 week
data$lagListMrp <- data.table::shift(data$list_mrp)

# . . . . Lag Discount ----
# Lag weekly avg discount by 1 week
data$lagDiscount <- data.table::shift(data$discount)

# . . . . Ad Stock ----
data$adTotalInvestment  <- as.numeric(
  stats::filter(data$TotalInvestment,filter=0.5,method='recursive'))
data$adTV               <- as.numeric(
  stats::filter(data$TV,filter=0.5,method='recursive'))
data$adDigital          <- as.numeric(
  stats::filter(data$Digital,filter=0.5,method='recursive'))
data$adSponsorship      <- as.numeric(
  stats::filter(data$Sponsorship,filter=0.5,method='recursive'))
data$adContentMarketing <- as.numeric(
  stats::filter(data$ContentMarketing,filter=0.5,method='recursive'))
data$adOnlineMarketing  <- as.numeric(
  stats::filter(data$OnlineMarketing,filter=0.5,method='recursive'))
data$adAffiliates       <- as.numeric(
  stats::filter(data$Affiliates,filter=0.5,method='recursive'))
data$adSEM              <- as.numeric(
  stats::filter(data$SEM,filter=0.5,method='recursive'))
data$adRadio            <- as.numeric(
  stats::filter(data$Radio,filter=0.5,method='recursive'))
data$adOther            <- as.numeric(
  stats::filter(data$Other,filter=0.5,method='recursive'))
data$adNPS              <- as.numeric(
  stats::filter(data$NPS,filter=0.5,method='recursive'))



# ***************************************************************************
#                   SAVE DATA  ----
# ***************************************************************************
write.csv(data[-1,], file = "./intrim/eleckart.csv",row.names=FALSE)
#' \vspace{8pt}
str(data)
#' \vspace{8pt}
#'  Load_Library ---- 
  #' ## Load Libraries:
  #'
  #'  Load required libraries. Will use `stepAIC`  from `MASS` package for model pruning.
  #'  `vif` variation inflation factor from `car` package
  #' 
  
  #+ warning=FALSE, message=FALSE
  library(MASS)
library(car)
library(DataCombine)   # Pair wise correlation
library(stargazer)
library(dplyr)         # Data aggregation
source('./code/atchircUtils.R')




#+ Load Data ---- 
#' ## Load Data:
#'
#'  Load blended data from sales and marketing datasets. Data is throughly
#' cleaned, pre-processed for model building. Refer to `DataCleaning.R`
#' script for data preparation steps. For this capstone project we limit
#' our model building focus to `camera_accessory`, `Home_audio` and 
#' `Gaming_accessory` product sub-categories. For simplicity will start
#' Linear model building with numerical features, later will consider 
#' categorical features.
#' \vspace{4pt}   

#+ load_data ----
data    <- read.csv('./intrim/eleckart.csv')


#' \vspace{8pt}
#' ## **Data Aggregation:**
#' \vspace{8pt}
#' Weekly aggregrated data is at granularity of `product_sub_category`, For Exploratory
#' analysis, will collapse `product_sub_category` level weekly data.
#' \vspace{8pt} 
data_week <- data %>% group_by(week) %>% 
  summarise(gmv=sum(gmv),
            product_mrp=mean(product_mrp),
            discount=mean(discount),
            sla=mean(sla),
            procurement_sla=mean(procurement_sla),
            n_saledays=mean(n_saledays),
            TV=mean(TV),Digital=mean(Digital),
            Sponsorship=mean(Sponsorship),
            ContentMarketing=mean(ContentMarketing),
            OnlineMarketing=mean(OnlineMarketing),
            Affiliates=mean(Affiliates),SEM=mean(SEM),
            Radio=mean(Radio),Other=mean(Other),
            TotalInvestment=mean(TotalInvestment),
            NPS=mean(NPS),
            list_mrp=mean(list_mrp),
            units=sum(units),
            COD=sum(COD),
            Prepaid=sum(Prepaid))

# deliverybdays=mean(deliverybdays),
# deliverycdays=mean(deliverycdays),
# chnglist=mean(chnglist),
# lagListMrp=mean(lagListMrp),
# lagDiscount=mean(lagDiscount),
# adTotalInvestment=mean(adTotalInvestment),
# adTV=mean(adTV),
# adDigital=mean(adDigital),
# adSponsorship=mean(adSponsorship),
# adContentMarketing=mean(adContentMarketing),
# adOnlineMarketing=mean(adOnlineMarketing),
# adAffiliates=mean(adAffiliates),
# adSEM=mean(adSEM),
# adRadio=mean(adRadio),
# adOther=mean(adOther),
# adNPS=mean(adNPS)


#' **Units Scaling:** `GMV`, `Product_mrp` are in terms of INR, while marketing
#' spend recorded in INR Cr. Lets convert marketing spend to INR. While models 
#' won't be sensitive to these parity in units, but for easy of model explanation, 
#' will standadize units.
#' \vspace{8pt}
data_week[,c(8:17)] <- data_week[,c(8:17)]*10000000
#' \vspace{8pt}


#+ EDA ----
#' ## **Exploratory Data Analysis:**

#' \vspace{8pt}
#+ . . . GMV analysis: ----
#' Will start analyzing the weekly `Gross Merchandize Value` the online website
#' generating in sales. Below chart shows per week `GMV` over a period of 52
#' weeks.  Horizontal dotted lines mark 25th 75th percentile of weekly expected 
#' `GMV`. Veritical lines represent whether any sale/promotion is carried out
#' during the week.
#' \vspace{8pt}
quant <- quantile(data_week$gmv,c(0.25,0.5,0.75))
matplot(data_week$week, cbind(data_week$gmv,rep(quant[1],53),rep(quant[2],53)),
        type='l',lwd=2,xlab = 'week',ylab = 'gmv')

saledays <- data_week$week[data_week$n_saledays > 1]
abline(v=saledays,col='blue',lwd=2)
legend('topright', inset = 0, legend = c('GMV','25th Percentile',
                                         '75th Percentile','sale days'), 
       lty = c(1:4), col=c(1,2,3,4), lwd = 2, cex = 0.75)
#' \vspace{8pt}
#' **Insights:**
#' \vspace{8pt}
#' One can clearly observe the peaks in weekly `GMV` aligns with promotion
#' markings. This clearly infers a positive impact of promotions on `GMV`.
#' Week 16 sales is kind of outlier, could be week long promitions/sales
#' events planned/festival sale. First 10 weeks sales were also seems outliers,
#' but on the lower side of 25th percentile.
#' \vspace{8pt}


#' \newpage
#+ . . . pricing analysis ----
#' #####**Pricing Analysis:**
#' \vspace{8pt}
#' Will look at average weekly `Max Retail Price` and `Listed MRP` of the 
#' products sold through the website. Clearly `List MRP` is consistently
#' lower w.r.t `product_mrp`. Next chart shows the average weekly discount.
#' \vspace{8pt}

matplot(data_week$week, cbind(data_week$product_mrp,data_week$list_mrp),
        type='l',lwd = 2,xlab='week',ylab='price')
abline(v=saledays,col='green',lwd=2)
legend('topright', inset = 0, legend = c('product mrp','list mrp','sale days'), 
       lty = c(1:3), lwd = 2, col=c(1,2,3), horiz = TRUE, cex = 0.75)
#' \vspace{8pt}
#+ . . . Discount ----
#' \vspace{8pt}
quant <- quantile(data_week$discount,c(0.25,0.5,0.75))
matplot(data_week$week, cbind(data_week$discount,rep(quant[1],53),
                              rep(quant[2],53),rep(quant[3],53)),
        type='l',lwd=2,xlab = 'week',ylab = 'Discount')
abline(v=saledays,col='cyan',lwd=2)
legend('topright', inset = 0, legend = c('Discount','25th Percentile',
                                         '50th Percentile','75th Percentile',
                                         'Sale days'), 
       lty = c(1:5), lwd = 2, col=c(1:5), cex = 0.75)
#' \vspace{8pt}
#' ######**Insights:**
#' \vspace{8pt}

#' 1. Avg `MRP` of products sold during promotions is high
#' 2. Discounts allowed throught out year
#' 3. During Non-Promotion weeks, discounts vary by 5%
#' 4. Discounts are higher by just 4% during promitions.
#' \vspace{8pt}


#+ . . . NPS ----
#' \newpage
#' #####**NPS(Net Promoter Score):**
#' \vspace{8pt}
#' Will study how `NPS` tall across the weeks for a given year. `NPS` can vary
#' anywhere between -100 and 100. `NPS` is a measure of customer satisfaction,
#' loyalty. Basically an approapriate `NPS` is arrived through regular customer
#' survey.
#' \vspace{8pt}
quant <- quantile(data_week$NPS,c(0.25,0.5,0.75))
matplot(data_week$week, cbind(data_week$NPS,rep(quant[1],53),
                              rep(quant[2],53),rep(quant[3],53)),
        type='l',lwd=2,xlab = 'week',ylab = 'NPS')
abline(v=saledays,col='cyan',lwd=2)
legend('topright', inset = 0, legend = c('NPS','25th Percentile','50th Percentile',
                                         '75th Percentile','sale days'), 
       lty = c(1:5), lwd = 2, col=c(1:5), cex = 0.75)
#' \vspace{8pt}
#' ######**Insights:**
#' \vspace{8pt}
#' Overall `NPS` for Eleckart is on the positive side ranging over 45-60
#' window. well the data around weeks 7-9 seems corrupted as seen on all
#' previous charts, lets exclude these weeks data for our insights.
#' I also overlay `sale/promotion` week markers, for better understanding
#' of `NPS` during weeks where no promotions being run. Relatively `NPS`
#' is higher during weeks where there are no promotions, kind of implying
#' due to high sales volumes during promitons customer satisfaction might
#' be compromised either due to delays in shipping or product quality.
#' If we were to relates `NPS` to our sales, we see a inverse relationship
#' between `NPS` and `GMV`, though it may not be granted, as a higher
#' `NPS` might be helping sales during weeks with no promotions running.


#+ . . . Sales Volume ----
#' \newpage
#' #####**Sales Volume:**
#' \vspace{8pt}
#' Below is weekly sales volume. Its pretty much follows `GMV` pattern.
#' Sales volume pretty much confines to a narrow band between 25th and
#' 75th percentile, expect a peak at week 16 and a poor sales reported
#' for first 9 weeks. we can ignore `Units` sold for model building as it
#' doesn't sound like an attriting feature that can influence sales.
#' \vspace{8pt}
quant <- quantile(data_week$units,c(0.25,0.5,0.75))
matplot(data_week$week, cbind(data_week$units,rep(quant[1],53),
                              rep(quant[2],53),rep(quant[3],53)),
        type='l',lwd=2,xlab = 'week',ylab = 'Sales Volume')
abline(v=saledays,col='cyan',lwd=2)
legend('topright', inset = 0, legend = c('Sales Volume','25th Percentile',
                                         '50th Percentile','75th Percentile',
                                         'Sale days'), 
       lty = c(1:5), lwd = 2, col=c(1:5), cex = 0.75)
#' \vspace{8pt}
#' ######**Insights:**
#' \vspace{8pt}
#' Between weeks 10 and 48, sales volume is kind of stable, also one can observe,
#' most of the promotions are being run during this period. By contrasting sales
#' numbers among 10-48 weeks and rest of the weeks, we can imply pretty confidently
#' promotions are kind of help sales.


#+ . . . Delivery Status ----
#' \newpage
#' #####**Delivery Status:**
#' \vspace{8pt}
#' For a developing country like India, where online transactions are not prevalent
#' mode of payment of those goods bought online, e-commerce websites have alternate
#' payment methods like `Cash on Delievery`. Will look at delivery status customer
#' choose at weekly level.
#' \vspace{8pt}
matplot(data_week$week, cbind(data_week$COD,data_week$Prepaid), type = 'l',
        lwd = 2, xlab = 'week', ylab = 'Order Status')
abline(v=saledays,col='cyan',lwd=2)
legend('topright', inset = 0, legend = c('COD','Prepaid','sale days'), 
       lty = c(1:3), lwd = 2, col=c(1:3), cex = 0.75)
#' \vspace{8pt}
#' ######**Insights:**
#' \vspace{8pt}
#' Irrespective of promotions, majority of customers heavily utilizing `Cash On
#' Delivery` facility consistently. we can say, there is a significant impact
#' on the sales with this alternate payment method.
#' 


#+ . . . SLA ----
#' \newpage
#' #####**Service Level Aggrement:**
#' \vspace{8pt}
#' `Service Level Aggrement` is a commitment to the customer to have his goods
#' delivered in with a certain number of days. Below chart shows the weekly
#' average `SLA` and `Procurement SLA`.
#' \vspace{8pt}
matplot(data_week$week, cbind(data_week$sla,data_week$procurement_sla), type = 'l',
        lwd = 2, xlab = 'week', ylab = 'SLA')
abline(v=saledays,col='cyan',lwd=2)
legend('topright', inset = 0, legend = c('SLA','Procurement SLA','sale days'), 
       lty = c(1:3), lwd = 2, col=c(1:3), cex = 0.75, horiz = TRUE)
#' \vspace{8pt}
#+ . . . . . . Insights ----
#' ######**Insights:**
#' \vspace{8pt}
#' `SLA` is kind of slightly higher than `Procurement SLA`, which is quiet good
#' metric, so as, one has margin buffer. During longer promotion periods like
#' week 16 and 20, `Procurement SLA` is lower compared to `SLA`. During weeks
#' where no promotion running, `Procurement SLA` is higher, could this be factor,
#' for lower sales during these periods.



#+ Correlation Among Non-Marketing spend ----
#' \newpage
#' #####**Correlation:**
#' \vspace{8pt}
#' Lets see how Non-Advertising KPIs were correlated
#+ eval=FALSE
pairs.panels(data_week[-c(8:16)])
#' ![](./output/NonMarketingSpendKPI.png)
#+
#' \vspace{8pt}
#' ######**Insights:**
#' \vspace{8pt}
#' `product_mrp` and `list_mrp` are highly correlated, this is kind of expected as we
#' observed, for most of the weeks the range of `discount` offered on the products
#' was with in narrow band.



#+ . . . GMV vs Marketing Spend ----
#' \newpage
#' #####**GMV vs Advertising Spend:**
#' \vspace{8pt}
#' Here is the comparision of weekly `GMV` against `Advertising` spend.
#' \vspace{8pt}
matplot(data_week$week, cbind(data_week$gmv, data_week$TotalInvestment),
        type='l',lwd = 2,xlab = 'week',ylab = 'GMV, MarketingInvestment')
abline(v=saledays,col='green',lwd=2)
legend('topright', inset = 0, legend = c('GMV','Marketing Investment','sale days'), 
       lty = c(1:3), lwd = 2, col=c(1,2,3), cex = 0.75)
#' \vspace{8pt}
#' ######**Insights:**
#' \vspace{8pt}
#' Total weekly advertising amount spend was consistently high compared to `GMV` for
#' all weeks. Well, either they were spending lot more on advertising than required,
#' (or) one really need to spend higher than `GMV` possible.
#' \vspace{8pt}



#+ . . . Marketing spend Across Channels ----
#' \newpage
#' #####**Marketing Spend Across Channels:**
#' \vspace{8pt}
#' Weekly aggregated marketing spend is confusing to draw clear insights, will analyze
#' marketing spend at monthly level.
#+ eval=FALSE
matplot(data_week$week, cbind(data_week$Digital, data_week$TV, data_week$Sponsorship,
                              data_week$ContentMarketing, data_week$OnlineMarketing,
                              data_week$Affiliates, data_week$SEM, data_week$Radio,
                              data_week$Other),
        type='l',lwd = 2,xlab = 'week',ylab = 'Marketing Spend')
legend('topright', inset = 0, legend = colnames(data_week[,c(8:16)]), 
       lty = c(1:9), lwd = 2, col=c(1:9), cex = 0.5)

#' \vspace{8pt}
#+ echo=TRUE
#' ![](./output/MarketingSpendBreakdown1.pdf)
#' \vspace{8pt}
#+
#' ######**Insights:**
#' \vspace{8pt}
#' A large portion of marketing spend goes towards sponsorships followed by online
#' marketing. On annaul basis, `SEM`, `Digital` channels receives significant funding
#' during month of October. `March`, `May` and `October` are the months when large
#' portion of money spent on marketing (or) running promotions.



#+ Correlations among explanatory variables ----
#' \newpage
#' #####**Correlation Among Advertising Channels:**
#' \vspace{8pt}
#+ eval=FALSE
pairs.panels(data_week[8:16])
#' ![](./output/Rplot.png)
#' \vspace{8pt}
#+
#' ######**Insights:**
#' \vspace{8pt}
#' There were multiple pairs of correlations exists among Advertising channel spends.
#' 1. `Radio` and `Other` Advertising channel spends are highly correlated.
#' 2. `SEM`, `Digital` and `Content Marketing` spends are highly correlated.
#' 3. `Affiliates` and `Online Marketing` are highly correlated.


#+ KPI Selection ----
#' \vspace{8pt}
#' ##### **KPI Selection:**
#' \vspace{8pt}
#' 