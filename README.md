# cl-strings
cl-strings is a small, portable, dependency-free set of utilities that make it
even easier to manipulate text in Common Lisp.  
It has 100% test coverage and works at least on sbcl, ecl, ccl, abcl and clisp.

* [How do i use it?](#how-do-i-use-it)
* [Example](#example)
* [API](#api)
* [Contributing](#contributing)
* [License](#license)

## How do i use it?
This section assumes you use quicklisp. If you don't, you should! Download and
learn about it [here](https://www.quicklisp.org/beta/).

Once you have quicklisp loaded, simply do:  
```lisp
(ql:quickload :cl-strings)
```
And it's all up and running. To run the tests do:
```lisp
(ql:quickload :cl-strings-tests)
```
Please report if any tests fail in your Common Lisp implementation.

## Example
```lisp
> (ql:quickload :cl-strings)
(:CL-STRINGS)  

> (use-package :cl-strings)
T  

> (defparameter *num* (parse-number "-3.1e3"))
*NUM* ;; -3100.0  

> (format-number *num* :precision 3 :decimal-separator "." :order-separator ",")
"-3,100.000"
```

## API
#### (parse-number number-str &key (decimal-separator #\\.) (order-separator nil))
parse-number returns a number from a string, without using the reader (CL has
parse-integer but no equivalent for other number types). It accepts integers,
floats, fractional and scientific notations. It also accepts both chars and
one character strings for the separators. This method may signal *parse-error*.
```lisp
(parse-number "-3.1e2") ;; -310.0
(parse-number "1 234,9" :decimal-separator "," :order-separator " ") ;; 1234.9
```

#### (format-number number &key (precision 0) (decimal-separator ".") (order-separator ",")
format-number returns a string from a number. It's possible to set the precision
(decimals), and the separators as chars or strings of length one.
```lisp
(format-number 1234.326 :precision 2 :decimal-separator "," :order-separator " ") ;; "1 234,33"
```

#### (make-template-parser start-delimiter end-delimiter &key (ignore-case nil))
make-template-parser returns a function (a closure actually) that can substitute
known variables for their values. *start-delimiter* and *end-delimiter* define
the template tags. *ignore-case* defaults to nil and can be set to change the
way the returned function tests variable names. The argument list of the
returned lambda is **(string values)** where *string* is the template and
*values* is an association list. It is easier by example:
```lisp
(defvar *my-parser* (make-template-parser "{{" "}}")) ;; closure
(funcall *my-parser* "Hello {{name}}!" '(("name" . "Sam"))) ;; "Hello Sam!"
```

#### (starts-with string prefix &key (ignore-case nil))
starts-with checks if *string* starts with *prefix*. The key argument
*ignore-case* defaults to nil.
```lisp
(starts-with "fOo bar" "foo" :ignore-case t) ;; t
```

#### (ends-with string suffix &key (ignore-case nil))
ends-with checks if *string* ends with *suffix*. The key argument *ignore-case*
defaults to nil.
```lisp
(ends-with "fOo bar" "bAr" :ignore-case t) ;; t
```

#### (clean-diacritics string)
clean-diacritics returns a string with the diacritical characters replaced by
their closest counterparts.
```list
(clean-diacritics "Déjà vu") ;; "Deja vu"
```

#### (shorten string len &key (truncate-string "..."))
shorten returns a string consisting of *string* cut off to length *len*, and
then *truncate-string* (which defaults to "..." but can be nil or "") appended
to it.
```lisp
(shorten "and then the guy bit the dog!" 8) ;; "and then..."
```

#### (replace-all string part replacement &key (ignore-case nil))
replace-all returns a string with every instance of *part* replaced by
*replacement* in *string*. *ignore-case* defaults to nil.
```lisp
(replace-all "Fo3sale: baby shoes, neve3worn" "3" "r ")
;; "For sale: baby shoes, never worn"
```

#### (join lst &key (separator ""))
join receives a list of strings and concatenates them. They can be delimited
by *separator*.
```lisp
(join (list "Woot" "woot" "woot!") :separator ", ") ;; "Woot, woot, woot!"
```

#### (split string &optional separator &key (ignore-case nil))
split returns a list made up of *string* parts, delimited by *separator*.
*separator* can be a char or any string. It defaults to a space.  
Note: Two consecutive separators will be seen as
if there was an empty string between them.
```lisp
(split "this, is, crazy" ", ") ; ("this" "is" "crazy")
```

#### (insert string original &key (position nil))
insert returns a *original* with *string* inserted at *position*. If *position*
is not provided, the insertion is performed at the end of *original*, meaning it
 is the same as setting *position* to the length of *original*
```list
(insert "each day holds " "The infinite possibilities should stagger the mind" :position 27)
;; "The infinite possibilities each day holds should stagger the mind"
```

#### (repeat string count &key (separator ""))
repeat returns a string consisting of joining *string* with itself *count*
number of times, with *separator* in between
```lisp
(repeat "clap" 3 :separator " ") ;; "clap clap clap"
```

#### (chars string)
chars returns a list with all the characters in a given *string*
```lisp
(chars "foo bar") ;; (#\f #\o #\o #\  #\b #\a #\r)
```

#### (chop string step)
chop returns a list of parts of *string*, with *step* elements in each.
If step is less than 1, no chop is performed.
```lisp
(chop "take wrong turns" 5) ;; ("take " "wrong" " turn" "s")
```

#### (toggle-case string)
toggle-case returns a string with each character being the opposite case of the
original ones.
```lisp
(toggle-case "oPEN uNMARKED dOORS") ;; "Open Unmarked Doors"
```

#### (clean string &key (char #\\space))
clean removes *char* from the left and right sides of *string*, and replaces
consecutive sequences of *char* by a single one.
```lisp
(clean "  foo   bar ") ;; "foo bar"
```

#### (camel-case string &key (delimiter #\\space))
camel-case leaves the case of the first character of *string* as is.
The rest of the words are concatenated and the first letter of each is upcased.
Word separation defaults to a space but can be customized by *delimiter*.
```lisp
(camel-case "hello worLD") ;; "helloWorld"
```

#### (kebab-case string &key (delimiter #\\space))
kebab-case downcases *string* and joins every word by an hyphen.
Word separation defaults to space but can be customized by *delimiter*.
```lisp
(kebab-case "hello worLD") ;; "hello-world"
```

#### (snake-case string &key (delimiter #\\space))
snake-case leaves the case of the first character of *string* as is.
The rest of the words are lowercased and joined by an underscore.
Word separation defaults to space but can be customized by *delimiter*.
```lisp
(snake-case "hello worLD") ;; "hello_world"
```

#### (title-case string &key (remove-hyphens t))
title-case upcases the first letter of every word in *string*, and downcases
the rest. Word separation removes hyphens by default, but *remove-hyphens* can
be set to nil to change this behaviour.
```lisp
(title-case "hello-worLD") ;; "Hello World"
```

## Contributing
If you have any suggestions, bug reports, etc, please fill in an issue
describing it. If you have the time and want to contribute, that is even better!
Submit some tests too, let's try and keep coverage at 100%.

Here is what I'm thinking might make sense to implement next:
- url-encode / url-decode
- CL specific sanitization / escaping
- String distance measures (Levenshtein, etc.)

## License
MIT
