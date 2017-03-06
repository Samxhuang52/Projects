###################
#Test Accounting Project
####################

# crawling finance data for good company from yahoo Finance
require("quantmod")
A = 'GS'
df <- getQuote(A, what = yahooQF(c("EPS Estimate Current Year", "Volume")))
df

getQuote(yahooQF())
data <- read.csv('symbol_goodcompany.csv')

data[,1]<- as.character(data[,1])
data[,1]
#here

for(i in 1:nrow(data)){
  row <- data[i,1]
  df2 <- getQuote(row, what = yahooQF(c("EPS Estimate Current Year", "Volume")))
  df <-rbind(df,df2)
}

#export to add symbol
write.csv(df, file = 'symbol2_finance_goodcompany.csv')

#combine symbol_finance and symbol_fullname
x <- read.csv('symbol_finance_goodcompany.csv')
y <- read.csv('symbol_goodcompany.csv')

#crawling finnance data of bad company
datad <- read.csv('symbol_delisted.csv')
head(datad)
df <- getQuote('GS', what = yahooQF(c("EPS Estimate Current Year", "Volume")))
df



datad[,2]<- as.character(datad[,2])
datad[,2]


for(i in 1:nrow(datad)){
  row <- datad[i,2]
  df2 <- getQuote(row, what = yahooQF(c("EPS Estimate Current Year", "Volume")))
  df <-rbind(df,df2)
}
write.csv(df, file = 'symbol2_finance_delist.csv')


####################3
# T test for each variable
good <- read.csv('symbol_finance_goodcompany.csv')
good <- good[!(good$symbol == 'GS'),]

bad <- read.csv('symbol_finance_delist.csv')
good$suspend <- c('0')
head(good)
bad$suspend <- c('1')

company <- rbind(good,bad)
write.csv(company,file = 'company_finance.csv')

pairwise.t.test(x=company$Price.Sales, g=company$suspend, p.adjust="none")

# hist
y <- read.csv('company_convert.csv')
head(y)
good <- y[(y$suspend == '0'),]
bad <- y[(y$suspend == '1'),]

hist(good$EBITDA)
hist(bad$EBITDA)
hist(good$Price.Book)
hist(bad$Price.Book)
hist(good$Price.Book)
hist(good$EBITDA)
hist(bad$EBITDA)
hist(good$EBITDA)

##################
# logistic regression
##################

y <- read.csv('finaltext.csv')
head(y)
glm1 <- glm(suspend~Concept_technology+Concept_corrected+Concept_risks+Concept_change+Concept_terms+Concept_regulations+Concept_ended+Concept_customer+Concept_impact+Concept_prior+Concept_tableofcontents+Concept_amount+Concept_unableto+Concept_declined+Concept_reduced+Concept_sales+Concept_value+Concept_partner+Concept_year+Concept_required+Concept_commercial+Concept_patents+Concept_control+Concept_contract+Concept_issued+Concept_available+Concept_director+Concept_period+Concept_fair+Concept_shares+Concept_assets+Concept_requested+Concept_funds+Concept_price+Concept_market+Concept_financialcondition+Concept_factors+Concept_company+Concept_cash+Concept_delay+Concept_management+Concept_efficient+Concept_result+Concept_productcandidates+Concept_development+Concept_loss+Concept_increased+Concept_failed+Concept_net+Concept_clinicaltrials+Concept_estimate+Concept_handling+Concept_investment+Concept_costs+Concept_business+Concept_acquisition+Concept_sell+Concept_fda+Concept_offset+Concept_products+Concept_ability+Concept_revenue+Concept_resultsofoperations+Concept_payment+Concept_agreement+Concept_offer+Concept_thirdparty+Concept_research+Concept_liabilities+Concept_expense+Concept_commonstock+Concept_service+Concept_cashflow+Concept_claim+Concept_operations+Concept_part+Concept_notes
            ,data=y, family=binomial(link="logit"))
as.integer(y$suspend)
backward <- step(glm1,trace = 0)
formula(backward)
summary(backward)
glm1 <- glm(suspend ~ Concept_corrected + Concept_risks + Concept_change + 
              Concept_regulations + Concept_ended + Concept_customer + 
              Concept_impact + Concept_reduced + Concept_value + Concept_partner + 
              Concept_patents + Concept_contract + Concept_available + 
              Concept_director + Concept_shares + 
              Concept_assets + Concept_funds + Concept_price + Concept_market + 
              Concept_cash + Concept_delay + Concept_efficient + Concept_increased + 
              Concept_estimate + Concept_costs + Concept_sell + Concept_fda + 
              Concept_products + Concept_ability + Concept_agreement + 
              Concept_offer + Concept_research + Concept_liabilities + 
              Concept_commonstock + Concept_cashflow + Concept_operations + 
              Concept_notes, family = binomial(link = "logit"), data = y)

summary(glm1)

require(pROC)
roc(response = y$suspend, predictor = backward$fitted.values,plot=TRUE)
