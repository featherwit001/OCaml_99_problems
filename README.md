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

## 27 Group the Elements of a Set Into Disjoint Subsets 

like 26, but n recursion path (n is given)

the main diffculty is change the list elt

the main idea is one pass throught the lst whose elts will be ditributed into n sets

for each elt, it could put into set1, set2, ... , setn or just skip it. 
There are n + 1 recusion path

the base case is that all elts in sizes are 0.

each recusive function carry the acc(res) cur lst sizes

the interation solution is alse similar to <26> interation solution.

but add the last elt inerted to cur and the sizes to stack element.
(inspired by this, <26> inter solution is alse improved by recording the last elt inserted rather than pattern matching the cur list to get the head elt.)

the interation solution is better than recursion solution on stack sizes,
but it's a little bit complex.


## 34 Determine the Prime Factors of a Given Positive Integer (2)

O(log n ) ~ O(sqrt n)

## 37  A List of Prime Numbers 

sieve of Eratosthenes : O(n*loglogn)

linear sieve: O(n) 
get rid of composite numbers with its smallest prime factor.

quit loop like break


## 41 Truth Tables for Logical Expressions


## 42 Gray Code

for `n` bit code, there are 2^n Gray Code.

two method to generate Gray Code.


Recrusive:

the Gray code is symmetic:

```
0  ->   0   ->  01
1       1	    01
        1		11
        0		10
```

init with ["0","1"], recursive depth is `n`
       
Iterative: 

start from "000...0" which containing `n` "0".

change code is turning "0" to "1" and turning "1" to "0" 

step one :
	change the rightest bit
	get a new gray code
step two:
	change bit at the left of the rightest "1".
	e.g. "0001001" -> "0001011"
	get a new gray code
step three: 
	repeat the step1 and step2, 1 2 1 2
	until get 2^n - 1 gray code (and the first one "0000....0")
	or
	get the code "1000....0" (all bits are "0" except the first one)


## 43 Huffman Code 
use min heap

transform the charater-frequency tuple lst into the  Leaf of Huffman tree

put all the Leaves into the min heap

get the two minimum Huffman trees from the min heap

merge two trees, the value of new root is the sum of two trees' weight

put the new tree into the min heap

repeat pop two push one until the heap only contains one elements

the last element is the Huffman tree of the given charater-frequency tuple lst

get the Huffman code form the Huffman tree using in-order traversal by two accumulation recursive method.


## 44 Construct Completely Balanced Binary Trees 

all complete balance binary trees with [n] nodes, the balance factor is 0.

the big problem could be reduced to smaller problem, and it can be 0 elt in the Balanced Binary Trees

Cartesian product of two sub-trees

functional programming memoization

implement **print_binary_tree** and **print_binary_trees** with a recursive method and render many sets of point on a Cartesian plane (i.e. the cmd terminal) to print binary trees in form of a graph rather then nested indentation.

for instance: 
```
 x          x         x         x         x         x
/  \       / \       /  \      / \       / \       / \
x  x       x x       x  x      x x       x  x      x  x
 \        /         / \           \        /         / \
 x        x         x x           x        x         x x
```

## 48 Construct Height-Balanced Binary Trees

all balance binary trees with [h] height

alse combine left sub-trees wiht right sub-trees 

balance factor is 0 -1 or 1

## 49 Construct Height-Balanced Binary Trees With a Given Number of Nodes

Maximum nodes of h height is calculated by formula. 

so many recursive methods for the tree problems:
Minimum nodes of h height 
Minimum height of n nodes
Maximum height of n nodes

hbal_tree_nodes

all balanced binary tree with `n` nodes which is different form complete binary trees

**solution**

reduce the height and nodes by two mutually recursive functions
```
   top
  /   \
left  right

```


step 1:

form `n` nodes get the max height and min height

calculate `n` with each possible height `h`

step2 : for the combination fo (`n`, `h`) ,have three possible reduced sub-problems
```
				   left_h  right_h  nodes 
				   (h - 1) (h - 1) (n - 1)
the `h` of `top` = (h - 1) (h - 2) (n - 1)
				   (h - 2) (h - 1) (n - 1)
```

step3 : for each combinations of (`left_h`, `right_h`, `n`)

the left sub-tree nodes plus right sub-tree nodes equals to `n`
so just pay attention to left sub-tree nodes

```
the left sub-tree nodes is limited by 
	min_left_nodes    and   max_left_nodes
	 max of                      min of 
	min_nodes left_h        max_nodes left_h
 n - max_nodes right_h      n - min_nodes right_h
```

so we can get the sequences of every possible nodes of left sub tree.
now we get the left sub tree's height and nodes 
such as (left_h, left_n1); (left_h, left_n2) .... ; (left_h, left_na)

the right sub tree's nodes right_nx can be calculated by n - left_nx
thus get the similar combinations :
		(right_h, right_n1); (right_h, right_n1); .... ; (right_h, right_n1)

so the sub problems are homologous to the previous question in step 2.

the whole questions can be solved by reducing the scala of the data, i.e. recursive method.


**optimaization**:

List.rev_append (tr) and @ (not tr) is difference, 

the former could not raise stackover flow but the latter could

using acc and symmetric is always better than 
memo using Hashtbl no matter which functions was memoizated.

thus this question does not have so many overlapping sub-problems.