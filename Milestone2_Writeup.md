# Milestone 2 Writeup

## The question addressed in the app: **Which US city is the safest?**
We can approach this question in terms of overall crime. Or, we could also look at which city is the safest in terms of a specific type of crime. For example, a user might be more interested in homicides than in other types of crime.

## Raw Data

The dataset includes both raw counts of how many crimes were committed, as well as the crime rate per 100,000 people. The counts range from the single digits to hundreds of thousands. Crime rates range from less than one to the thousands.

## Explaintory Data Analysis

The Marshall Violent Crime dataset has a lot of data, encompassing 1975-2015 and including 68 US cities.  The dataset also has statistics for four different types of violent crime. To help the user hone in on what they care about most, we designed the app to let the user filter on the cities, types of crime, and time period. To compare cities on different types of crime, like homicides and robberies, the app shows a different graph for each type of crime. Having one graph for each type of crime allows the user to make a valid comparison across cities.

## Design Choices
1. Barplot: 
We decided to plot the counts of crimes using a bar graph, making it easy for any user to compare different cities to find out which US city is the safest. 

2. Dropdown Menu:
On the side pannel, we used a dropdown menu for the user to select specific cities which he/she interested in.

3. Checkboxes:
The second component on the side pannel is checkboxes for all the four types of crime.

4. Slider:
The graphs show crime statistics for a fixed time period. But we have used slider for the entire range of 1975 to 2013, which the user can play with to study specific years. 


## How the App has Changed Since the Proposal

In the proposal, the landing page of our app graphed all cities' total number of  violent crimes.  We decided that showing all cities would be too much information.   Therefore, now we choose to present only the total violent crime statistics for the top ten cities with the highest overall number of violent crimes. Also, now we have three value boxes which display the total number of crimes, average number of crimes and the decrease in number of crimes.

## Future Development

While we can rank cities easily for violent crime in total, our app does not yet sort the graphs looking at specific types of crime. For example, we are unable to find the safest city in terms of robberies. So even though we were unable to figure out how to add this functionality to the app for this second milestone, we will continue developing this feature.