# MechaCar_Statistical_Analysis

# Purpose
AutosRUs’ newest prototype, the MechaCar, is suffering from production troubles that are blocking the manufacturing
team’s progress. My purpose is to provide data analytics that may help the production team and to review the production
data for insights.

## Linear Regression to Predict MPG
Perform multiple linear regression analysis to identify which variables in the dataset predict the mpg of MechaCar prototypes.

<img width="520" alt="del1data" src="https://user-images.githubusercontent.com/86200136/136675712-b91dc6ca-ddfc-46cb-8c72-ea2642efd9ea.png">



-Which variables/coefficients provided a non-random amount of variance to the mpg values in the dataset?
  The ground_clearance, vehicle_length, and y Intercept are statistically unlikely to provide random amounts
  of variance to the linear model. This is based on their p-value below the .05 significance level.
  
-Is the slope of the linear model considered to be zero? Why or why not?
  The p-value of our model is 5.35 x 10^-11. This value is smaller than the .05 significance level. Therefore
  there is sufficent evidence to reject the null hypothesis, which means that the slope of our linear model
  is not zero.
  
-Does this linear model predict mpg of MechaCar prototypes effectively? Why or why not? 
  71% of the variablilty of our dependent variable (mpg) is explained using this linear model. Therefore the linear
  model does predict mpg of MechaCar prototypes effectively.
  
## Summary Statistics on Suspension Coils
Collect summary statistics on the pounds per square inch (PSI) of the suspension coils from the manufacturing lots.

The following PSI metrics for each lot: mean, median, variance, and standard deviation

totalsummary.png<img width="591" alt="image" src="https://user-images.githubusercontent.com/86200136/136700196-fb7d5d13-391e-4f6d-9b5b-c41fcf77a9cb.png">


The suspension coil’s PSI continuous variable across all manufacturing lots

lotsummary.png<img width="520" alt="image" src="https://user-images.githubusercontent.com/86200136/136700220-050ccf78-fd3e-4f75-a708-2d3cd02c69cb.png">


-The design specifications for the MechaCar suspension coils dictate that the variance of the suspension coils must not exceed 100 pounds per square inch. Does the current manufacturing data meet this design specification for all manufacturing lots in total and each lot individually? Why or why not?
  The lots as a total_summary number has a variance of 62.29psi which is below the 100 pounds limit. 
  Lots 1 and 2 have a variance of .97psi and 7.46psi which are below the 100 pounds limit.
  Lot 3 has a variance of 170.29psi which is way over the limit of 100 pounds.



## T-Tests on Suspension Coils
Run t-tests to determine if the manufacturing lots are statistically different from the mean population 

alllots.png<img width="431" alt="image" src="https://user-images.githubusercontent.com/86200136/136700464-5b9c9f29-188c-4625-8ec9-29feac1517df.png">

lot1.png<img width="652" alt="image" src="https://user-images.githubusercontent.com/86200136/136700473-14fa26b4-07eb-42f7-86cd-e8882ea50202.png">

lot2.png<img width="658" alt="image" src="https://user-images.githubusercontent.com/86200136/136700481-22740353-b3b1-4e3c-a670-f81c7675892d.png">

lot3.png<img width="647" alt="image" src="https://user-images.githubusercontent.com/86200136/136700487-e51e1331-9dcb-487c-9354-4bd8c3fc2ed5.png">

-Summary: PSI across all manufacturing lots is statistically different from the population mean of 1,500 pounds per square inch.

  All lots: p-value is .06 > .05(significance level) = p-value is above the assumed significance level. Therefore, we would state
  that there is not enough evidence to reject the null hypothesis and we can confirm our two samples are statistically similar.
  
  Lot 1: p-value is 1 > .05(significance level) = p-value is above the assumed significance level. Not enough evidence to reject
  the null hypothesis.
  
  Lot 2: p-value is .60 > .05(significance level) = p-value is above the assumed significance level. Not enough evidence to reject
  the null hypothesis.
  
  Lot 3: p-value is .04 < .05(significance level) = p-value is below the significance level, we can say there is evidence to reject
  the null hypothesis.
  




## Study Design: MechaCar vs Competition
Design a statistical study to compare speed of the MechaCar vehicles against speed from other manufacturers. We want to test if
MechaCars are faster based on the cars 0 to 60mph time. 

What metric or metrics are you going to test?
The dependent variable is the 0 to 60 time.
The independent variables are horsepower, vehicle weight, engine size, and fuel efficiency.

What is the null hypothesis or alternative hypothesis? 
There is no relationship between the 0 to 60pmg time and the  variables horsepower, vehicle weight, engine size, and fuel efficiency.

What statistical test would you use to test the hypothesis? And why? 
we would need to test our question with a multiple linear regression model. If we were to use a multiple linear regression model, we
would need to collect numerical variables, such as engine size, fuel efficiency, vehicle weight, and engine horsepower. Once we select the variables to collect, we would estimate sample size based on how low of a significance level is necessary and how sensitive the measurements are.

What data is needed to run the statistical test?
Continuous data from independent research or manufacturers' data.







