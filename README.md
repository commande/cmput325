# cmput325

At the top of each file:

Brett Commandeur

1255891

CMPUT 325 Wi17 - NON-PROCEDURAL PROG LANGUAGES Combined LAB LEC Wi17

For each question:

1. The question number
2. What does the function do
3. How does the function work (comment my solution)
4. Test cases. Needed if (2.) is not sufficiently clear

Programming Style:

- Put the most important function first. Do not begin with utils. 
- Indent your programs for clarity with consistency
- build complex functions from simple ones.
- Do not nest conditionals more than 2 deep. 
- Use meaningful function and variable names.
- Data structures need appropriate access functions

Example Style:

```lisp

#| Question 2.

The function zip takes two lists L1 and L2 and returns a single list where the 
elements of the two input lists alternate. If L1 and L2 are of different 
lengths, say n and m respectively, with n > m, then the last n-m elements in 
L1 are ignored. Similarly for the case m > n. 

test cases:
(zip '(a c e) '(b d f)) => (a b c d e f)
(zip '(a c e f) '(b d)) => (a b c d)
(zip '(a c) '(b d e f)) => (a b c d)

|#

(defun zip (L1 L2)
	(cond
		 ((null L1) nil) ; comment
		 ((null L2) nil)
		 ; comment
		 (t (cons (car L1)
				  (cons (car L2)
						(zip (cdr L1) (cdr L2)))))))

```
