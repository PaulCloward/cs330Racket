Assignment: Extended Interpreter
Name: <insert your name here>

Instructions: 
* Open this file in DrRacket (or your favorite plain-text editor) and add your answers 
  at the end of each line for each question. If you want to add more explanation or 
  justification, you may add one or more lines under the question.  Remember to add 
  your name as well.  Once complete, submit this to Learning Suite as a plain text file.
* For each of the documentation questions, indicate Yes (Y) or No (N).
* For each of the test case questions, indicate the line number of the corresponding
  test (or tests) using "L" and the number of the line.  For example, a test on
  line 61 of the file would be "L61".  If you don't have a particular test, put "N".
* If you need to add any more explanation of justification, just add it on a line
  underneath the respective question.

Function: parse

 General:
 * Is the function correct? Y
 * Is the function documented correctly (i.e. contract and purpose statement)? Y

 Feature: literals
 * Is there an example of parsing a number expression properly? Y 201
 * Is there a test case for a literal that is not a number? Y 202

 Feature: binary operators
 * Is there an example of parsing a + expression properly? Y 204
 * Is there an example of parsing a - expression properly? Y 205
 * Is there an example of parsing a * expression properly? Y 206
 * Is there an example of parsing a / expression properly? Y 206
 * Is there a test case for: too few pieces? Y 209
 * Is there a test case for: too many pieces? Y 208

 Feature: id
 * Is there an example of parsing a id expression properly? Y 211
 * Is there a test case for: not an id (+)? Y 213
 * Is there a test case for: not an id (-)? Y 212 
 * Is there a test case for: not an id (*)? Y 215
 * Is there a test case for: not an id (/)? Y 214
 * Is there a test case for: not an id (with)? Y 216
 * Is there a test case for: not an id (if0)? Y 217 
 * Is there a test case for: not an id (fun)? Y 218

 Feature: if0
 * Is there an example of parsing a if0 expression properly? Y 220
 * Is there a test case for: too few pieces? Y 221
 * Is there a test case for: too many pieces? Y 222

 Feature: with
 * Is there an example of parsing a with expression properly? Y 224
 * Is there a test case for: too few pieces? Y 228
 * Is there a test case for: too many pieces? Y 229 
 * Is there a test case for: invalid bindings list (not a list)? Y 230
 * Is there a test case for: invalid binding (not a list)? Y 231
 * Is there a test case for: invalid binding (too few pieces)? Y 232
 * Is there a test case for: invalid binding (too many pieces)? Y 233
 * Is there a test case for: invalid binding (not a symbol)? Y 234 
 * Is there a test case for: invalid binding (not a valid id)? Y 236
 * Is there a test case for: invalid binding (duplicated id)? Y 237

 Feature: fun
 * Is there an example of parsing a fun expression properly? Y 240
 * Is there a test case for: too few pieces? Y 246
 * Is there a test case for: too many pieces? Y 247
 * Is there a test case for: invalid parameters (not a list)? Y 248
 * Is there a test case for: invalid parameter (not a symbol)? Y 249
 * Is there a test case for: invalid parameter (not a valid id)? Y 250
 * Is there a test case for: invalid parameter (duplicated id)? Y 251

 Feature: app
 * Is there an example of parsing an app expression properly? Y 253

 Other:
 * Is there a test case for an expression with no operator (an empty list)? Y 256


Function: interp

 General:
 * Is the function correct? Y 
 * Is the function documented correctly (i.e. contract and purpose statement)? Y
 
 Feature: literals
 * Is there a number case test? Y 339
 
 Feature: binary operators
 * Is there a + case test? Y 343
 * Is there a + (catch non-number, lhs) case test? Y 344
 * Is there a + (catch non-number, rhs) case test? Y 345
 * Is there a - case test? Y 346
 * Is there a - (catch non-number, lhs) case test? Y 347
 * Is there a - (catch non-number, rhs) case test? Y 348
 * Is there a * case test? Y 349
 * Is there a * (catch non-number, lhs) case test? Y 350
 * Is there a * (catch non-number, rhs) case test? Y 351
 * Is there a / case test? Y 352
 * Is there a / (catch non-number, lhs) case test? Y 353
 * Is there a / (catch non-number, rhs) case test? Y 354
 * Is there a / (catch div by 0) case test? Y 355
 
 Feature: id
 * Is there an id (unbound) case test? Y 358
 
 Feature: if0
 * Is there an if0 (true) case test? Y 361
 * Is there an if0 (false) case test? Y 362
 * Is there an if0 (catch non-number) case test? Y 363
 
 Feature: with
 * Is there a with (basic, bound id) case test? Y 366 
 * Is there a with (shadowing) case test? Y 367
 * Is there a with (shadowing in body but not in initialization expression) case test? Y 368 
 
 Feature: fun
 * Is there a fun (evaluates to closure) case test? Y 376
 * Is there a fun (evaluates to closure with captured binding) case test? Y 377
 
 Feature: app
 * Is there a working app test case? 382
 * Is there an app (catches non-function) case test? 383
 * Is there an app (catches too few args) case test? 384
 * Is there an app (catches too many args) case test? 385 
 * Is there an app (static, not dynamic scope) case test? 386

