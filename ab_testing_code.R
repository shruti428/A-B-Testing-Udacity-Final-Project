## A/B Testing Udacity Final Course Project ##
## Refer https://docs.google.com/document/u/1/d/1aCquhIqsUApgsxQ8-SQBAigFDcfWVVohLEXcV6jWbdI/pub?embedded=True for more information

##Udacity is adding a screener after the "join free trial" button to check the number for hours the student can devote 
##If the student can devote <5hrs then it will prompt a message to "access the materials"
##the hypothesis/rationale is that this might set clearer expectations for students upfront thus reducing the number of frustrated students who left the free trial  
#because they didn't have enough time—without significantly reducing the number of students to continue past the free trial and eventually complete the course.

##baseline data 
#C (Cookies),	CL (Clicks), ID (User-ids), CTP(Click-through-probability),	CG(Gross conversion), R(Retention), CN(Net conversion)
baseline_data<-data.frame("C"=40000, "CL"=3200, "ID"=660, "CTP"=0.08, "CG"=0.20625, "R"=0.53, "CN"=0.1093125)

##calculating the standard errors
##assuming that the metric has a binomial disrtibution and since the sample size is large it will approimate a normal distribution (central limit theorem)
##since the baseline data is provided for 40000 cookies and we need to calculate the results for 5000 cookies we need to rescale the measures

cal_stddev <-function(n, p){
  
  stddev<-sqrt(p*(1-p)/n)
  print(stddev)
}

##gross conversion
no_clicks <-40000/8 * 0.08
gross_conversion<- cal_stddev(n=no_clicks,p=0.20625)

#retention
no_enrolled <-40000/8 * 660/40000
retention<- cal_stddev(n=no_enrolled,p=0.53)

#net conversion
net_conversion<- cal_stddev(n=no_clicks,p=0.109313)

##sample calculation
#calculate sample size using library
#https://www.evanmiller.org/ab-testing/sample-size.html
#Clicks for Gross Conversion = 25835 (per variant)
#Clicks for Retention = 39115 (per variant)
#Clicks for Net Conversion = 27413 (per variant)

#pageviews for each metric
gross_conversion_pg_views <-25835 *2 /0.08
retention_pg_views <-39115 *2 /(660/40000)
net_conversion <-27413 * 2/0.08
#page views for gross conversion=645875, retention=4741212, net conversion=685325
#number of days needed for A/B testing each metric
#gross conversion=17 days, retention=119 days, net conversion=17 days
#since we don't want to run A/B test for longer duration (4 months) we can't use retention as a metric for evaluation
#use gross conversion and net conversion as the evaluation metric 

##analyzing the data from the experiment
data_experiment <-read.csv("/Users/shrutiparulekar/Desktop/data science/AB testing/ab_test_experiment.csv")
data_control <-read.csv("/Users/shrutiparulekar/Desktop/data science/AB testing/ab_test_control.csv")

head(data_experiment)
head(data_control)

#sanity checks
## the invariant metrics should be similar for the experiment and control groups
##check if the proportion of page views in experiment group should be around 50%
page_views_experiment <-sum(data_experiment$Pageviews)
page_views_control <-sum(data_control$Pageviews)

p_page_views <-page_views_experiment/(page_views_control+page_views_experiment)
sd_page_views <-sqrt(p_page_views*(1-p_page_views)/(page_views_control+page_views_experiment))
ci_interval_page_views <-paste0("lower_ci:",round(p_page_views -1.96 *sd_page_views,3), " ", "upper_ci:", round(p_page_views +1.96 *sd_page_views,3))

##check if the proportion of cookies views in experiment group should be around 50%
cookies_experiment <-sum(data_experiment$Clicks)
cookies_control <-sum(data_control$Clicks)

p_cookies <-cookies_experiment/(cookies_control+cookies_experiment)
sd_cookies <-sqrt(p_cookies*(1-p_cookies)/(cookies_control+cookies_experiment))
ci_interval_cookies <-paste0("lower_ci:",round(p_cookies -1.96 *sd_cookies,3), " ", "upper_ci:", round(p_cookies +1.96 *sd_cookies,3))

##check if the click through probablity is different between experiment and control groups
ctp_experiment <-cookies_experiment/page_views_experiment
ctp_control <-cookies_control/page_views_control

diff_prop<-round(ctp_experiment-ctp_control,3)
sd_diff_ctp <-sqrt((ctp_experiment*(1-ctp_experiment)/page_views_experiment) + (ctp_control *(1-ctp_control)/page_views_control))
ci_interval_ctp <-paste0("lower_ci:",round(diff_prop -1.96 *sd_diff_ctp,3), " ", "upper_ci:", round(diff_prop +1.96 *sd_diff_ctp,3))

##analyzing data
exp_enr_not_null <-subset(data_experiment, !is.na(data_experiment[,"Enrollments"]))
con_enr_not_null <-subset(data_control, !is.na(data_control[,"Enrollments"]))

##gross conversion metric
exp_enrollments <-sum(exp_enr_not_null["Enrollments"])
con_enrollments <-sum(con_enr_not_null["Enrollments"])

gross_con_exp <-exp_enrollments/sum(exp_enr_not_null["Clicks"])
gross_con_con <-con_enrollments/sum(con_enr_not_null["Clicks"])

diff_prop_gross_con<-round(gross_con_exp-gross_con_con,3)
sd_diff<-sqrt((gross_con_exp*(1-gross_con_exp)/sum(exp_enr_not_null["Clicks"])) + (gross_con_con *(1-gross_con_con)/sum(con_enr_not_null["Clicks"])))
ci_interval_ctp <-paste0("lower_ci:",round(diff_prop_gross_con -1.96 *sd_diff,3), " ", "upper_ci:", round(diff_prop_gross_con +1.96 *sd_diff,3))

##net conversion metric
exp_payments <-sum(exp_enr_not_null["Payments"])
con_payments <-sum(con_enr_not_null["Payments"])

net_con_exp <-exp_payments/sum(exp_enr_not_null["Clicks"])
net_con_con <-con_payments/sum(con_enr_not_null["Clicks"])

diff_prop_net_con<-round(net_con_exp-net_con_con,3)
sd_diff_net_con<-sqrt((net_con_exp*(1-net_con_exp)/sum(exp_enr_not_null["Clicks"])) + (net_con_con *(1-net_con_con)/sum(con_enr_not_null["Clicks"])))
ci_interval_ctp <-paste0("lower_ci:",round(diff_prop_net_con -1.96 *sd_diff_net_con,3), " ", "upper_ci:", round(diff_prop_net_con +1.96 *sd_diff_net_con,3))

##run sign test 
##number of days when experiment group has better rates
head(exp_enr_not_null)
data_merged <-merge(x=con_enr_not_null,y=exp_enr_not_null,on="inner",by.x="Date",by.y="Date")

data_merged['GrossConversion_con'] = data_merged["Enrollments.x"]/data_merged["Clicks.x"]
data_merged['GrossConversion_exp'] = data_merged["Enrollments.y"]/data_merged["Clicks.y"]
data_merged['NetConversion_con'] = data_merged["Payments.x"]/data_merged["Clicks.x"]
data_merged['NetConversion_exp'] = data_merged["Payments.y"]/data_merged["Clicks.y"]

cols = c('Date','GrossConversion_con','GrossConversion_exp','NetConversion_con','NetConversion_exp')

data_signtest <- data_merged[,cols]
nrow(data_signtest)
sum(data_signtest['GrossConversion_exp']>data_signtest['GrossConversion_con'])
sum(data_signtest['NetConversion_exp']>data_signtest['NetConversion_con'])

#use the following link https://www.graphpad.com/quickcalcs/binomial1/ or code below to calculate the pvalue for the sign test
gross_conversion_pvalue<-binom.test(4,23)
net_conversion_pvalue<-binom.test(10,23)
paste0("gross conversion pvalue is ", round(gross_conversion_pvalue$p.value,3))
paste0("net conversion pvalue is ", round(net_conversion_pvalue$p.value,3))

## the gross conversion rates are signiifcantly lower in the experimental group 
#the net conversion rate is not significantly different 
#since the gross conversion rates are significantly lower in the experiment group so we wouldn't recommend launching this screener
