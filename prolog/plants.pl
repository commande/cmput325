veg(carrot).
veg(tomato).
fruit(banana).
fruit(tomato).

plant(Item) :- veg(Item).
plant(Item) :- fruit(Item).
