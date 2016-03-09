# cl-strings
cl-strings is small, portable, dependency-free set of utilities for manipulating strings in Common Lisp.

* [How do i use it?](#how-do-i-use-it)
* [Example](#example)
* [API](#api)
* [Contributing](#contributing)
* [License](#license)

## How do i use it?
This section assumes you use quicklisp. If you don't, you should! Download and learn about it [here](https://www.quicklisp.org/beta/).

While this library is not tracked by quicklisp, you can simply download the source code to your *local-projects* folder inside the quicklisp directory. Then simply:  
```lisp
(ql:quickload :cl-strings)
```
And it's all up and running. To run the tests do:
```lisp
(ql:quickload :cl-strings-tests)
```
Please report if any tests fail in your Common Lisp implementation.

## Example
To ease typing, the cl-strings package also has the nickname "s".
```lisp
> (ql:quickload :cl-strings)
(:CL-STRINGS)
> (defparmeter num (s:parse-number "-3.1e2"))
NUM ;; -3100.0
> (s:format-number num :precision 3 :decimal-separator "." :order-separator ",")
"-3,100.000"
```

## API
#### (parse-number (number-str &key (decimal-separator #\\.) (order-separator nil)))
parse-number returns a number from a string, without using the reader (CL has parse-integer but no equivalent for other number types). It accepts integers, floats, fractional and scientific notations. It also accepts both chars and one character strings for the separators. This method may throw *parse-error*.

```lisp
(s:parse-number "-3.1e2") ;; -3100.0
(s:parse-number "1 234,9" :decimal-separator "," :order-separator " ") ;; 1234.9
```

#### (format-number number &key (precision 0) (decimal-separator ".") (order-separator ",")
format-number returns a string from a number. It's possible to set the precision (decimals), and the separators as chars or strings of length one.

```lisp
(format-number 1234.326 :precision 2 :decimal-separator "," :order-separator " ") ;; "1 234,33"
```
#### (starts-with string target &key (ignore-case nil))
starts-with checks if *string* starts with *target*. The key argument ignore-case defaults to nil.

```lisp
(starts-with "fOo bar" "foo" :ignore-case t) ;; t
```

#### (ends-with string target &key (ignore-case nil))
ends-with checks if *string* ends with *target*. The key argument ignore-case defaults to nil.

```lisp
(ends-with "fOo bar" "bAr" :ignore-case t) ;; t
```

#### (truncate string len &key (truncate-string "..."))
truncate returns a string consisting of *string* cut off to length *len*, and then *truncate-string* (which defaults to "...") appended to it.
```lisp
(truncate "and then the man bit the dog!" 8) ;; "and then..."
```

#### (repeat string count &key (separator ""))
repeat returns a string consisting of joining *string* with itself *count* number of times, with *separator* in between
```lisp
(repeat "clap" 3 :separator " ") ;; "clap clap clap"
```

#### (join lst &key (separator ""))
join receives a list of strings and concatenates them. They can be delimited by *separator*.
```lisp
(join (list "Woot" "woot" "woot!") :separator ", ") ;; "Woot, woot, woot!"
```

## Contributing
If you have any suggestions, bug reports, etc, please fill in an issue describing it. If you have the time and want to contribute, that is even better! Submit some tests too, let's try and keep the test coverage at 100%.

Here is what i'm thinking might make sense to implement next:
- url-encode / url-decode
- strings to camel-case / kebab-case / snake-case
- CL specific sanitization / escaping
- String distance measures (Levenshtein, etc.)

## License
MIT
