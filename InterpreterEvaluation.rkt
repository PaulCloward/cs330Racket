Assignment: Rudimentary Interpreter
Name: Paul Cloward

Instructions: 
* Open this file in DrRacket (or your favorite plain-text editor) and add your answers 
  at the end of each line for each question. If you want to add more explanation or 
  justification, you may add one or more lines under the question.  Remember to add 
  your name as well.  Once complete, submit this to Learning Suite as a plain text file.
* For each of the documentation questions, indicate Yes (Y) or No (N).
* For each of the test case questions, indicate the line number of the corresponding
  test (or tests) usingthe number of the line.  For example, a test on line 61 of the
  file would be "61".  If you don't have a particular test, put "N".
* If you need to add any more explanation of justification, just add it on a line
  underneath the respective question.

Function: parse

 * Is the function correct? Y
 * Is the function documented correctly (i.e. contract and purpose statement)? Y

 Feature: literals
 * Is there an example of parsing a number expression properly? Y 97
     (eg, (parse '5))
 * Is there a test case for a literal that is not a number? (i.e., not a number, a list, or a symbol?) Y 98
     (eg, (parse "hello"))

 Feature: binary operators
 * Is there an example of parsing a + expression properly? Y 99
 * Is there an example of parsing a - expression properly? Y 100
 * Is there an example of parsing a * expression properly? Y 101
 * Is there an example of parsing a / expression properly? Y 102
 * Is there a test case for: too few pieces? Y 104
     (eg, (+ 5))
 * Is there a test case for: too many pieces? Y 103
     (eg, (+ 1 2 3))

 Feature: with
 * Is there an example of parsing a with expression properly? Y 107
 * Is there a test case for: too few pieces in the expression? Y 111
     (eg, [with ((x 5))] )
 * Is there a test case for: too many pieces in the expression? Y 113
     (eg, [with ((x 5)) (+ 1 x) (+ 2 x)] )
 * Is there a test case for: invalid bindings list (not a list)? Y 115
     (eg, [with x (+ 1 x) (+ 2 x)] )
 * Is there a test case for: invalid binding within the bindings (not a list)? Y 116
     (eg, [with (x 5) (+ 1 x) (+ 2 x)] )
 * Is there a test case for: invalid binding (too few pieces)? Y 117
     (eg, [with ((x)) (+ 1 x)] )
 * Is there a test case for: invalid binding (too many pieces)? Y 118 
     (eg, [with ((x 5 6)) (+ 1 x)] )
 * Is there a test case for: invalid binding (first item not a symbol)? Y 119
     (eg, [with ((42 5)) (+ 1 x)] )

 Feature: id
 * Is there an example of parsing a id expression properly? Y 122
 * Is there a test case for: not an id (+)? Y 123
 * Is there a test case for: not an id (-)? Y 124
 * Is there a test case for: not an id (*)? Y 125
 * Is there a test case for: not an id (/)? Y 126
 * Is there a test case for: not an id (with)? Y 127
 
 Other:
 * Is there a test case for an expression with no operator (an empty list)? Y 128
     (eg, (parse '()) )

Function: calc
 * Is the function correct? Y
 * Is the function documented correctly (i.e. contract and purpose statement)? Y
 * Is there a number case test? Y 223
 * Is there a + case test? Y 224
 * Is there a - case test? Y 225
 * Is there a * case test? Y 226
 * Is there a / case test? Y 227
 * Is there a divide by zero case test? Y 228
 * Is there a case that shows referencing an identifier? Y 229
 * Is there an id (unbound) case test? 230
 * Is there a with (basic, bound id) case test? Y 231
     (eg, [with ((x 5)) (+ x 5)] )
 * Is there a with (shadowing) case test? Y 232
     (eg, [with ((x 5)) (with ((x 6)) (+ x 5) ]] )
 * Is there a with (shadowing in body but not in initialization expression) case test? Y 233
     (eg, [with ((x 5)) (with ((x (+ x 1))) (+ x 5) ]] )
 * Is there a duplicate bindings test case? Y 234
     (eg, [with ((x 5) (x 6)) (+ x x)] )
 

