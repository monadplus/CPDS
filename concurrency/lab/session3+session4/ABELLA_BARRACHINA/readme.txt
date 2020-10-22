***************************

CPDS-Conc Lab 3 + 4
Safety & Progress

***************************

Name: Arnau
Surname: Abella Gassol

Name: Pol
Surname: Barranchina Hernandez

******************************

I prepared a GNU Make file for both parts to compile the java sources.

Just run `make` on the folder with the Makefile to build and execute the program

******************************

* Part I:

** Exercise 1

Complete the snippet.

See ./first-part/first-exercise/neighbours.lts

- Safety property

The property is not violated.

- Progress property

Under fair scheduling, they eventually enter to the field to pick berries.

- What if the neighbors are greedy?

If both neighbors raise the flag at, approximately, the same time, they will have to lower the flag and try again. In a hypothetical worst-case scenario, they will raise the flag indefinitely.

** Exercise 2

See the implementation at ./first-part/second-exercise/

******************************

* Part II:

** Exercise 1

Both safety and progress are satisfied. It can be checked by loading ./second-part/first-exercise/neighbours.lts on the ltsa tool.

** Exercise 2

See ./second-part/second-exercise/

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
