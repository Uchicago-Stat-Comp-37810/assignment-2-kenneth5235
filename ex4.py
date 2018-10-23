#!/usr/bin/env python
# coding: utf-8

# In[5]:


# There are 100 cars
cars = 100
# There is space for 4 passengers in each car
space_in_a_car = 4
# There are 30 drivers
drivers = 30
# There are 90 passengers
passengers = 90
# The amount of cars that are not used is the total amount of cars minus the amount of drivers
cars_not_driven = cars - drivers
# The amount of cars that are used is equal to the amount of drivers available
cars_driven = drivers
# The amount of passengers that can be transported is the amount of cars used times the space in each car
carpool_capacity = cars_driven * space_in_a_car
# The average amount of passengers per car is the total amount of passengers devided by the amount of cars used
average_passengers_per_car = passengers / cars_driven


print("There are", cars, "cars available.")
print("There are only", drivers, "drivers available.")
print("There will be", cars_not_driven, "empty cars today.")
print("We can transport", carpool_capacity, "people today.")
print("We have", passengers, "to carpool today.")
print("We need to put about", average_passengers_per_car,
      "in each car.")

# Study drills

# The variable car_pool_capacity is not defined
# However the variable carpool_capacity is defined on line 7

# 4 instead of 4.0 would have worked as well
# It just means that both space_in_a_car and carpool_capacity will not be floating numbers

