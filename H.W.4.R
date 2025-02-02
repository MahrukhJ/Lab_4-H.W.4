LAB 4!
  
  #For this lab, we get to conduct regression models to explain wages. We want to understand how important is a college degree, in relation to income.
  
load("~/Desktop/School/Fall 2021/Statistics and Introduction to Econometrics/RStudio Stuff/Ecob2000_lecture1/acs2017_ny_data..RData")
attach(acs2017_ny)
use_varb <- (AGE >= 25) & (AGE <= 55) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 35)
dat_use <- subset(acs2017_ny,use_varb) # 
detach()
attach(dat_use)

#This selects people 25-55 (often called prime age), in labor force, working year round, full-time. Otherwise the regression would be trying to explain why so many people make zero wages despite so many qualifications.

> View(acs2017_ny)
> model_temp1 <- lm(INCWAGE ~ AGE + female + AfAm + Asian + Amindian + race_oth + Hispanic + educ_hs + educ_somecoll + educ_college + educ_advdeg)
> summary(model_temp1)

Call:
lm(formula = INCWAGE ~ AGE + female + AfAm + Asian + Amindian + 
  race_oth + Hispanic + educ_hs + educ_somecoll + educ_college + 
  educ_advdeg)

Residuals:
  Min      1Q  Median      3Q     Max 
-148088  -33205  -10708   13053  625543 

Coefficients:
                   Estimate  Std. Error t value Pr(>|t|)    
(Intercept)       -7096.25    2446.71  -2.900 0.003730 ** 
  AGE             1316.69      39.66  33.199  < 2e-16 ***
  female        -24939.46     720.43 -34.617  < 2e-16 ***
  AfAm          -11934.26    1130.37 -10.558  < 2e-16 ***
  Asian            566.53    1369.83   0.414 0.679188    
Amindian         -8858.57    6077.71  -1.458 0.144971    
race_oth         -7526.49    1272.49  -5.915 3.35e-09 ***
  Hispanic       -4224.82    1183.47  -3.570 0.000358 ***
  educ_hs        10592.37    1814.71   5.837 5.35e-09 ***
  educ_somecoll  22461.39    1857.67  12.091  < 2e-16 ***
  educ_college   57155.71    1830.96  31.216  < 2e-16 ***
  educ_advdeg    82766.43    1878.64  44.057  < 2e-16 ***
  ---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 76760 on 46959 degrees of freedom
Multiple R-squared:   0.15,	Adjusted R-squared:  0.1498 
F-statistic: 753.6 on 11 and 46959 DF,  p-value: < 2.2e-16

#dependent variable: total income
#independent variable: Age, female participants, specific identified races(Asian, American Indian, Hispanic, other) & education.
model_temp1 <- lm(INCWAGE ~ AGE + female + AfAm + Asian + Amindian + race_oth + Hispanic + educ_hs + educ_somecoll + educ_college + educ_advdeg)
summary(model_temp1)
plot(model_temp1)
# maybe get fancy
require(stargazer)
stargazer(model_temp1, type = "text")
# play with stargazer instead of summary, it can look nicer!
==================================================
                             Dependent variable:      
                    ------------------------------
                                  INCWAGE            
--------------------------------------------------
  AGE                          -189.515***          
                                  (8.038)            

female                      -17,005.830***        
                                 (306.035)           

AfAm                        -5,979.715***         
                                 (470.978)           

Asian                          -246.677           
                                (622.420)           

Amindian                     -4,434.481*          
                               (2,531.131)          

race_oth                    -3,473.873***         
                                 (572.419)           

Hispanic                     -1,001.109*          
                                 (521.614)           

educ_hs                     11,803.740***         
                                 (517.546)           

educ_somecoll               19,141.370***         
                                 (557.270)           

educ_college                45,536.170***         
                                 (570.431)           

educ_advdeg                 68,896.920***         
                                 (608.312)           

Constant                    26,745.370***         
                                  (630.833)           

--------------------------------------------------
Observations                   163,158            
R2                              0.134             
Adjusted R2                     0.134             
Residual Std. Error    61,589.210 (df = 163146)   
F Statistic         2,289.331*** (df = 11; 163146)
==================================================
  Note:                  *p<0.1; **p<0.05; ***p<0.01


# subset in order to plot...
NNobs <- length(INCWAGE)
set.seed(12345) # just so you can replicate and get same "random" choices
graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
dat_graph <-subset(dat_use,graph_obs)  

plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), data = dat_graph)
# ^^ that looks like crap since Wages are soooooooo skew!  So try to find some sensible ylim = c(0, ??)
plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph)
# discus what you see in this plot

# change this line to fit your regression
to_be_predicted2 <- data.frame(AGE = 25:55, female = 1, AfAm = 0, Asian = 0, Amindian = 1, race_oth = 1, Hispanic = 1, educ_hs = 0, educ_somecoll = 0, educ_college = 1, educ_advdeg = 0)
to_be_predicted2$yhat <- predict(model_temp1, newdata = to_be_predicted2)

lines(yhat ~ AGE, data = to_be_predicted2)

detach()

The results from the first linear regression portray a positive correlation between the independent and dependent variables (income(independent), ages ranging from 25-55 with a focus on female participants, race restrictions (African America, American Indian, Hispanic & other) and highest level of education attained(dependent))

#For the following linear regressions, I am interested in exploring the relationship between variables similar to the first regression analysis however, with restrictions placed on highest level of education attained, gender and increasing the age limit from 55 to 65. 
#The shift from 55 to 65 is to include the people who are still working until the full benefit retirement age of 65. 
#I also increased the number of average hours worked per week from 35 to 40. 
#The goal of this regression is to understand the reasoning behind any significant differences that may occur, when compared to the first analysis. 

dependent variable: income
independent variable: Age, female 
# I am omitting education in this particular regression in order to observe the impact of each variable in the subsequent regressions.   

My null hypothesis for the second regression analysis is that there will be no significant difference between the second regression analysis and the first. 
The alternative hypothesis is that there will be a significant difference between the first and the second analysis.

use_varb1 <- (AGE >= 25) & (AGE <= 65) & (LABFORCE == 2) & (WKSWORK2 > 4) & (UHRSWORK >= 40)
> dat_use1 <- subset(acs2017_ny,use_varb1) # 
> detach()
> attach(dat_use1)
View(dat_use1)
> model_temp2 <- lm(INCWAGE ~ AGE + female)
> summary(model_temp2)

Call:
  lm(formula = INCWAGE ~ AGE + female)

Residuals:
  Min     1Q Median     3Q    Max 
-99067 -42173 -19786  12229 585209 

Coefficients:
             Estimate    Std. Error t value Pr(>|t|)    
(Intercept)  53581.37    1619.28   33.09   <2e-16 ***
AGE            699.78      34.38   20.36   <2e-16 ***
female      -18285.20     788.63  -23.19   <2e-16 ***
  ---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 89040 on 52402 degrees of freedom
Multiple R-squared:  0.01823,	Adjusted R-squared:  0.0182 
F-statistic: 486.6 on 2 and 52402 DF,  p-value: < 2.2e-16

> suppressMessages(require(stargazer))
> stargazer(model_temp2, type = "text")
===============================================
                          Dependent variable:    
                    ---------------------------
                                INCWAGE          
-----------------------------------------------
  AGE                         699.777***         
                               (34.378)          

female                    -18,285.200***       
                                (788.629)         

Constant                   53,581.370***       
                              (1,619.285)        

-----------------------------------------------
Observations                  52,405           
R2                             0.018           
Adjusted R2                    0.018           
Residual Std. Error   89,036.620 (df = 52402)  
F Statistic         486.645*** (df = 2; 52402) 
===============================================
Note:               *p<0.1; **p<0.05; ***p<0.01

NNobs <- length(INCWAGE)
> set.seed(12345) # just so you can replicate and get same "random" choices
> graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
> dat_graph1 <-subset(dat_use1,graph_obs)
plot(INCWAGE ~ jitter(AGE, factor = 2), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,150000), data = dat_graph1)
> # discus what you see in this plot
  
#We can observe a positive linear relationship between income and age for females from this dataset. 

#Before I begin the third regression analysis, I want to try lm(log(INCWAGE) to compare the mean of the predicted values of the first version to the mean of the predicted values in the second (log) version. 
model_temp2a <- lm((log1p(INCWAGE)) ~ AGE + female)
> summary(model_temp2a)
Call:
  lm(formula = (log1p(INCWAGE)) ~ AGE + female)

Residuals:
  Min     1Q Median     3Q    Max 
-9.113 -4.584  1.805  4.378 10.597 

Coefficients:
                Estimate  Std. Error  t value P r(>|t|)    
(Intercept)   10.3978786  0.0367138  283.21   <2e-16 ***
  AGE         -0.0803009  0.0006654 -120.68   <2e-16 ***
  female      -0.6748504  0.0258823  -26.07   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.896 on 143763 degrees of freedom
(29316 observations deleted due to missingness)
Multiple R-squared:  0.09783,	Adjusted R-squared:  0.09781 
F-statistic:  7794 on 2 and 143763 DF,  p-value: < 2.2e-16

suppressMessages(require(stargazer))
> stargazer(model_temp2a, type = "text")

=================================================
                              Dependent variable:     
                    -----------------------------
                             (log1p(INCWAGE))       
-------------------------------------------------
  AGE                           -0.080***          
                                    (0.001)           

female                        -0.675***          
                                     (0.026)           

Constant                      10.398***          
                                     (0.037)           

-------------------------------------------------
  Observations                   143,766           
R2                              0.098            
Adjusted R2                     0.098            
Residual Std. Error      4.896 (df = 143763)     
F Statistic         7,794.474*** (df = 2; 143763)
=================================================
  Note:                 *p<0.1; **p<0.05; ***p<0.01

The log function allows us to view the percentage change of the dependent variable in response to changes in the independent variables. We can compare th log coefficients to the original coefficients through the following:
 
  'Coefficients:
                Estimate    Std. Error t value Pr(>|t|)    
(Intercept)    49537.279    490.035  101.09   <2e-16 ***
  AGE           -158.634      8.882  -17.86   <2e-16 ***
  female      -15517.583    345.462  -44.92   <2e-16 ***
  
  Coefficients:
               Estimate   Std. Error t value Pr(>|t|)    
(Intercept)   10.3978786  0.0367138  283.21   <2e-16 ***
  AGE         -0.0803009  0.0006654 -120.68   <2e-16 ***
  female      -0.6748504  0.0258823  -26.07   <2e-16 ***'

Overall, by observing the log model, we can see that there is defintiely a difference, especially in the female estimate and in the intercept t-value. 



For the third regression analysis, I want to include the education variables and compare against the results observed from the second regression. 
model_temp3 <- lm(INCTOT ~ AGE + female + educ_hs + educ_somecoll + educ_college + educ_advdeg)
> summary(model_temp3)

Call:
  lm(formula = INCTOT ~ AGE + female + educ_hs + educ_somecoll + 
       educ_college + educ_advdeg)

Residuals:
  Min      1Q  Median      3Q     Max 
-176489  -39010  -13717   12850 1270505 

Coefficients:
                Estimate     Std. Error t value  Pr(>|t|)    
(Intercept)     -11545.09    2430.40  -4.750 2.04e-06 ***
  AGE             1321.62      35.42  37.311  < 2e-16 ***
  female        -30249.63     807.32 -37.469  < 2e-16 ***
  educ_hs        13067.20    1936.76   6.747 1.53e-11 ***
  educ_somecoll  27187.27    1986.77  13.684  < 2e-16 ***
  educ_college   68818.29    1950.55  35.281  < 2e-16 ***
  educ_advdeg   104550.73    1993.06  52.457  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 90490 on 52398 degrees of freedom
Multiple R-squared:  0.1564,	Adjusted R-squared:  0.1563 
F-statistic:  1619 on 6 and 52398 DF,  p-value: < 2.2e-16

suppressMessages(require(stargazer))
stargazer(model_temp3, type = "text")
================================================
                           Dependent variable:     
                    ----------------------------
                                   INCTOT           
------------------------------------------------
  AGE                         1,321.622***        
                                (35.422)          

female                     -30,249.620***       
                                (807.324)          

educ_hs                    13,067.190***        
                               (1,936.764)         

educ_somecoll              27,187.270***        
                               (1,986.769)         

educ_college               68,818.290***        
                               (1,950.551)         

educ_advdeg                104,550.700***       
                               (1,993.059)         
    
Constant                   -11,545.090***       
                               (2,430.403)         

------------------------------------------------
Observations                   52,405           
R2                             0.156            
Adjusted R2                    0.156            
Residual Std. Error   90,492.270 (df = 52398)   
F Statistic         1,618.622*** (df = 6; 52398)
================================================
Note:                *p<0.1; **p<0.05; ***p<0.01

NNobs <- length(INCTOT)
set.seed(12345)
graph_obs2 <- (runif(NNobs) < 0.1) 
dat_graph2 <-subset(dat_use1,graph_obs2)  
plot(INCTOT ~ (AGE), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,200000), data = dat_graph2)
to_be_predicted2 <- data.frame(AGE = 25:65, female = 1, educ_hs = 1 , educ_somecoll = 1, educ_college = 1, educ_advdeg = 1)
to_be_predicted3$yhat <- predict(model_temp3, newdata = to_be_predicted3)
lines(yhat ~ AGE, data = to_be_predicted3)

#Next, we can compare the data from this regression to a log function. 
> model_temp3a <- lm((log1p(INCTOT)) ~ AGE + female + educ_hs + educ_somecoll + educ_college + educ_advdeg)
> summary(model_temp3a)

Call:
  lm(formula = (log1p(INCTOT)) ~ AGE + female + educ_hs + educ_somecoll + 
       educ_college + educ_advdeg)
Residuals:
  Min       1Q   Median       3Q      Max 
-13.3322  -0.5090   0.9287   1.9262   8.6517 

Coefficients:
                  Estimate  Std. Error t value  Pr(>|t|)    
(Intercept)      4.1317069  0.0305301  135.33   <2e-16 ***
  AGE            0.0511500  0.0004357  117.41   <2e-16 ***
  female        -0.8468228  0.0169368  -50.00   <2e-16 ***
  educ_hs        2.3176407  0.0275841   84.02   <2e-16 ***
  educ_somecoll  2.9271315  0.0296710   98.65   <2e-16 ***
  educ_college   3.8737704  0.0302874  127.90   <2e-16 ***
  educ_advdeg    4.3412661  0.0324505  133.78   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 3.221 on 145585 degrees of freedom
(27490 observations deleted due to missingness)
Multiple R-squared:  0.2221,	Adjusted R-squared:  0.2221 
F-statistic:  6929 on 6 and 145585 DF,  p-value: < 2.2e-16

> suppressMessages(require(stargazer))
> stargazer(model_temp3a, type = "text")

=================================================
                             Dependent variable:     
                    -----------------------------
                              (log1p(INCTOT))       
-------------------------------------------------
  AGE                           0.051***           
                                (0.0004)           

female                        -0.847***          
                                  (0.017)           

educ_hs                       2.318***           
                                   (0.028)           

educ_somecoll                 2.927***           
                                  (0.030)           

educ_college                  3.874***           
                                  (0.030)           

educ_advdeg                   4.341***           
                                  (0.032)           

Constant                      4.132***           
                                 (0.031)           

-------------------------------------------------
  Observations                   145,592           
R2                              0.222            
Adjusted R2                     0.222            
Residual Std. Error      3.221 (df = 145585)     
F Statistic         6,929.022*** (df = 6; 145585)
=================================================
  Note:                 *p<0.1; **p<0.05; ***p<0.01

-----------------Comparing the coefficients-------------------
  Coefficients:
                 Estimate   Std.Error t value  Pr(>|t|)    
(Intercept)     -11545.09    2430.40  -4.750 2.04e-06 ***
  AGE             1321.62      35.42  37.311  < 2e-16 ***
  female        -30249.63     807.32 -37.469  < 2e-16 ***
  educ_hs        13067.20    1936.76   6.747 1.53e-11 ***
  educ_somecoll  27187.27    1986.77  13.684  < 2e-16 ***
  educ_college   68818.29    1950.55  35.281  < 2e-16 ***
  educ_advdeg   104550.73    1993.06  52.457  < 2e-16 ***
  
  Coefficients:
                  Estimate  Std. Error  t value  Pr(>|t|)    
(Intercept)      4.1317069  0.0305301  135.33   <2e-16 ***
  AGE            0.0511500  0.0004357  117.41   <2e-16 ***
  female        -0.8468228  0.0169368  -50.00   <2e-16 ***
  educ_hs        2.3176407  0.0275841   84.02   <2e-16 ***
  educ_somecoll  2.9271315  0.0296710   98.65   <2e-16 ***
  educ_college   3.8737704  0.0302874  127.90   <2e-16 ***
  educ_advdeg    4.3412661  0.0324505  133.78   <2e-16 ***
  
#The next regression will follow the results of income, age, and education, omitting the female variable present from the previous regression in order to observe any differences. 


model_temp4 <- lm(INCTOT ~ AGE + educ_hs + educ_somecoll + educ_college + educ_advdeg)
> summary(model_temp4)
Residuals:
  Min      1Q  Median      3Q     Max 
-161551  -39191  -14793   10896 1283896 

Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)     -21544.13    2447.83  -8.801  < 2e-16 ***
  AGE             1333.08      35.89  37.142  < 2e-16 ***
  educ_hs        12083.35    1962.34   6.158 7.44e-10 ***
  educ_somecoll  23680.78    2010.96  11.776  < 2e-16 ***
  educ_college   64991.65    1973.78  32.928  < 2e-16 ***
  educ_advdeg    98878.19    2013.73  49.102  < 2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 91700 on 52399 degrees of freedom
Multiple R-squared:  0.1338,	Adjusted R-squared:  0.1337 
F-statistic:  1618 on 5 and 52399 DF,  p-value: < 2.2e-16

suppressMessages(require(stargazer))
stargazer(model_temp4, type = "text")

================================================
                            Dependent variable:     
                     ----------------------------
                                    INCTOT           
------------------------------------------------
  AGE                         1,333.078***        
                                (35.891)          

educ_hs                    12,083.350***        
                               (1,962.340)         

educ_somecoll              23,680.780***        
                               (2,010.956)         

educ_college               64,991.650***        
                                (1,973.780)         

educ_advdeg                98,878.190***        
                                (2,013.729)         

Constant                   -21,544.130***       
                              (2,447.833)         

------------------------------------------------
Observations                   52,405           
R2                             0.134            
Adjusted R2                    0.134            
Residual Std. Error   91,695.680 (df = 52399)   
F Statistic         1,618.235*** (df = 5; 52399)
================================================
  Note:                *p<0.1; **p<0.05; ***p<0.01

NNobs <- length(INCTOT)
set.seed(12345)
graph_obs2 <- (runif(NNobs) < 0.1) 
dat_graph2 <-subset(dat_use1,graph_obs2)  
plot(INCTOT ~ (AGE), pch = 16, col = rgb(0.5, 0.5, 0.5, alpha = 0.2), ylim = c(0,200000), data = dat_graph2)
to_be_predicted4 <- data.frame(AGE = 25:65, educ_hs = 1 , educ_somecoll = 1, educ_college = 1, educ_advdeg = 1)
to_be_predicted4$yhat <- predict(model_temp4, newdata = to_be_predicted4)
lines(yhat ~ AGE, data = to_be_predicted4)

#Interestingly, the average wages from model_temp3 surpasses the results observed from model_temp4. I had predicted that the increase in participant size for model_temp4 would impact the averages by increasing the results however, the results were significantly less. 

The p-value for all four linear regressions read 'p-value:< 2.2e-16' which is under the p<0.01 significance level. 
The p-value is a measure of how much evidence we have against the null hypothesis so the smaller the p-value, the more evidence against the null hypothesis. 
In this case, we can reject the null hypothesis and accept the significant differences in the mean values of the variables because a p-value less than 0.01 indicates substantial evidence against the null.

We can also analyze the t-values in the first regression and compare them to the values generated from the second, third and fourth regressions. 
Comparing the  coefficients for intercept, age & female, the t-value (for the second regression) increased for intercept,decreased for age and decreased for female. 
Comparing the third regression to the second regression, the t-value decreased for intercept, increased for age and decreased for female. 
Finally, comparing the fourth regression to the third regression, the t-value decreased for intercept and only slightly decreased for age.

Analyzing the results from the regressions conducted in this lab, there is evidence to demonstrate that age, education and gender impact the wage of an individual. 
#Interestingly, while the third regression depicted a positive correlation between age, education and gender, the fourth regression, albeit positive, depicted a correlation, however, not as greatly.  
This study focused solely on the differences and impact of age, education and gender on the income an individual earns but it should be noted that there are other variables and circumstances that can heavily impact wage, that have not been mentioned.
Additional information that would be useful in determining the importance or the impact of a degree on wages is the level of English fluency an individual holds. 
I would think that the lack of English fluency would impede opportunities for people even if they have the education required for a certain job. 
Following this, I would be interested in comparing data between people of similar education, age & gender, the only difference being their fluency of the language. 

#Just for fun: I am actually interested in comparing wages between people who are below the poverty line with an advanced degree and those above the poverty line with an advanced degree. 

model_temp5 <- lm(INCWAGE ~ AGE + educ_advdeg + below_povertyline)
summary(model_temp5)

Call:
  lm(formula = INCWAGE ~ AGE + educ_advdeg + below_povertyline)

Residuals:
  Min     1Q Median     3Q    Max 
-84697 -27405 -12810  10276 619433 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)        42941.682    454.885   94.40   <2e-16 ***
  AGE                 -256.578      8.543  -30.03   <2e-16 ***
  educ_advdeg        46887.095    475.637   98.58   <2e-16 ***
  below_povertyline -31079.949    529.226  -58.73   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 62750 on 143762 degrees of freedom
(29316 observations deleted due to missingness)
Multiple R-squared:  0.09334,	Adjusted R-squared:  0.09332 
F-statistic:  4933 on 3 and 143762 DF,  p-value: < 2.2e-16

> suppressMessages(require(stargazer))
> stargazer(model_temp5, type = "text")

=================================================
  Dependent variable:     
  -----------------------------
  INCWAGE           
-------------------------------------------------
  AGE                          -256.578***         
  (8.543)           

educ_advdeg                 46,887.100***        
  (475.637)          

below_povertyline          -31,079.950***        
  (529.226)          

Constant                    42,941.680***        
  (454.885)          

-------------------------------------------------
  Observations                   143,766           
R2                              0.093            
Adjusted R2                     0.093            
Residual Std. Error   62,749.820 (df = 143762)   
F Statistic         4,933.326*** (df = 3; 143762)
=================================================
  Note:                 *p<0.1; **p<0.05; ***p<0.01

model_temp5b <- lm(INCWAGE ~ AGE + educ_advdeg)
  > summary(model_temp5b)
Call:
  lm(formula = INCWAGE ~ AGE + educ_advdeg)

Residuals:
  Min     1Q Median     3Q    Max 
-82795 -26319 -18617  12048 622184 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 37987.958    452.324   83.98   <2e-16 ***
  AGE          -233.385      8.636  -27.03   <2e-16 ***
  educ_advdeg 49474.934    479.236  103.24   <2e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 63500 on 143763 degrees of freedom
(29316 observations deleted due to missingness)
Multiple R-squared:  0.07159,	Adjusted R-squared:  0.07157 
F-statistic:  5543 on 2 and 143763 DF,  p-value: < 2.2e-16

> suppressMessages(require(stargazer))
> stargazer(model_temp5b, type = "text")

=================================================
  Dependent variable:     
  -----------------------------
  INCWAGE           
-------------------------------------------------
  AGE                          -233.385***         
  (8.636)           

educ_advdeg                 49,474.930***        
  (479.236)          

Constant                    37,987.960***        
  (452.324)          

-------------------------------------------------
  Observations                   143,766           
R2                              0.072            
Adjusted R2                     0.072            
Residual Std. Error   63,497.830 (df = 143763)   
F Statistic         5,542.619*** (df = 2; 143763)
=================================================
  Note:                 *p<0.1; **p<0.05; ***p<0.01

To compare the coefficents:
Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)        42941.682    454.885   94.40   <2e-16 ***
  AGE                 -256.578      8.543  -30.03   <2e-16 ***
  educ_advdeg        46887.095    475.637   98.58   <2e-16 ***
  below_povertyline -31079.949    529.226  -58.73   <2e-16 ***
  
Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept) 37987.958    452.324   83.98   <2e-16 ***
  AGE          -233.385      8.636  -27.03   <2e-16 ***
  educ_advdeg 49474.934    479.236  103.24   <2e-16 ***
  
  
To conclude the entire study once more, correlation does not equate to causation and we cannot claim that any one variable increases or decreases income. 
However, there are certain variables that can aid in increasing or decreasing an individual's' income, such as highest level of education attained, number of years since working, etc. 
