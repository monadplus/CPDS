***************************
CPDS-Conc Lab 4
Safety & Progress (II).
***************************

Name: Arnau
Surname: Abella Gassol

Name: Pol
Surname: Barranchina Hernandez

******************************
Exercise 1


Both safety and progress are satisfied. It can be checked by loading `./first-exercise/neighbours.lts` on the `ltsa` tool.

******************************
Exercise 2

With the new implementation following the Peterson's algorithm, the greedy version of the neighbors problem does not halt accessing the shared resource (the field).

From the output we can see that both neighbours are cooperating sharing the resource alternating the turns:

```
try again, my name is: bob
try again, my name is: alice
alice waiting...
bob enter
alice waiting...
bob exits
try again, my name is: bob
bob waiting...
alice enter
bob waiting...
alice exits
try again, my name is: alice
alice waiting...
bob enter
alice waiting...
bob exits
alice waiting...
try again, my name is: bob
bob waiting...
alice enter
bob waiting...
alice exits
bob waiting...
try again, my name is: alice
alice waiting...
bob enter
alice waiting...
bob exits
try again, my name is: bob
alice enter
bob waiting...
```
