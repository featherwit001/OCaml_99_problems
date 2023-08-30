# OCaml_99_problems
from https://ocaml.org/problems 


important problems:

## 7 Flatten a List

more than two branch of a list 

## 8 Eliminate Duplicates

unique elts in a sorted sequences

take an unique elts when the next elt is different form the current one, including the end of the sequence(list) for the reason that distinction between the ending element and the one with successor is distinct and makes it easy to distinguish the two situation

## 9 Pack Consecutive Duplicates 

two acc in a recursive function to delivery two individual information

## 18 Extract a Slice From a List 

combine two easy actions or comprehensive solution

## 23 rand_select
 
use Random and return a tuple

## 26 Generate the C(K, N) K ELTs Chosen From the N Elements of a List
Generate the Combinations of K Distinct Objects Chosen From the N Elements of a List  

**binary recursion and binary acc**

can only handle the small-scala data

one branch for take the current elt, one for not
one acc represent the current combination, 
another takes the cur which is as long as k to be the result.

the base case is k <= 0 and lst = [],one for taking, another for not

**iter to simulate stack**

data scala could be bigger than recursive solution. 

No going back, there is hardly any room for optimization.

index form 0, the elt must be unique.
sequnece length : n
the sub sequence length: k

construct the data structure to get the index form elt and get the elt form the index quickly.

init the stack with the first element form index 0 to n - k

 - why n - k, because the later elt is impossible to construct a sequence as long as k

do until the stack is empty

pop the first combination from the stack


if the len is equal to K, push it into the result
else append the elt form the last elt in the current combination to ending elt of the sequence to the current, incr len and push both of them into Stack

done

There is some room for optimization that 
if the len of current combination plus amount of the elts whose index from i (the elt to be appended) to n - 1 (the ending elt) is less than k, which means it is impossile to meet the k requirement like the init stack situation, we can neglect the i elt to ending elt.
