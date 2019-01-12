<img src="http://fightingcrimenc.com/wp-content/uploads/2017/09/fightingcrimelogo-1.jpg" align="right" height="190" width="220"/>

# Marshall_violent_crimes

**Date:** 2019-01-12

**License:** [MIT](https://opensource.org/licenses/MIT)

#### Authors

| Name | Github |
| ---- | -------|
| Akansha Vashisth |[akanshaVashisth](https://github.com/akanshaVashisth)|
| Daniel Lin | [danielglin](https://github.com/danielglin)|

### Overview (Section 1)

Within the United States, there is a lot of different kinds of crime, and there is a lot of variance between cities and years.  For a person deciding which city to live in, crime is an important factor to consider. To address this problem, we are going to create a visualization app using the Shiny package that allows the user to look at different types of crimes for a particular city and compare cities based on the type and number of crimes.  Our app will allow the user to view the trends for violent crimes in a city. Also, the user can compare cities based on data aggregated across types of violent crime. The user will be able to filter and reorder the types of crime.


### Description of the Data (Section 2)

Our dataset has 2829 records. The records are from 68 United States cities within the timeframe of 1975-2015. The data covers four types of violent crime, namely homicide, rape, robbery, and aggravated assault. Also, the data includes both the number of crimes (homs_sum, rape_sum, rob_sum, agg_ass_sum) and the rate of crimes (homs_per_100k, rape_per_100k, rob_per_100k, agg_ass_per_100k). Another field in the dataset gives the total population for a city in a specific year. The data also includes the total of all the four crime types (violent_crime), as well as the overall rate of violent crime (violent_per_100k). Each record contains the violent crime statistics for a particular city in a particular year.


### Usage Scenario and Tasks (Section 3)

Aaron is a young data scientist who is planning to move to the United States. Since he considers many factors when making all of his decisions, one of his concerns for moving to the US is how safe each city is. He wants to understand what the crime rate is in each city so he can make an informed decision. When Aaron logs onto the “Crime Rate in States” app, he will see an overview of types of crimes in each city. He can filter on the cities and look at the statistics for different types of violent crime in the cities. The app allows him to see trends in the types of crimes in each city. Also, once Aaron has looked at all the trends for his preferred cities, he can now compare the cities on his list of cities he is considering moving to.  When Aaron does so, he may notice that a few cities he was considering are not that safe and some of his less preferred cities on his list are safer to move to.

### Description of Your App and Sketch (Section 4)

The app contains a landing page where the total number of crimes per city is displayed.  The graph contains the total crime rate per city.  On the left side of the app the user will have access to the input field, where the user can enter the desired cities to compare the crime rate.  As the user enters specific cities, the graph will show the trends for the four specific crimes (homicide, robbery, rape, aggravated assault) for that city from 1975 to 2015.

