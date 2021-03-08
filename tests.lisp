(in-package :cl-strings)

(prove:plan 20)

(prove:subtest "ends-with"
  (prove:ok (ends-with "1" "1" :ignore-case t) "equal strings")
  (prove:ok (not (ends-with "1" "12" :ignore-case t)) "suffix lengthier than string")
  (prove:ok (not (ends-with "123" "12" :ignore-case t)) "different strings")
  (prove:ok (ends-with "123" "123" :ignore-case nil) "equal strings")
  (prove:ok (ends-with "123A" "123a" :ignore-case t) "equal strings ignoring case")
  (prove:ok (not (ends-with "123A" "123a" :ignore-case nil))
    "different strings not ignoring case")
  (prove:ok (ends-with "123A" "123A" :ignore-case nil) "equal strings")
  (prove:ok (not (ends-with "123A" "123a")) "default to not ignore case")
  (prove:ok (ends-with "abcd" "") "empty suffix")
  (prove:ok (not (ends-with "" "abcd")) "empty string")
  (prove:ok (ends-with "" "") "both empty")
  (prove:ok (not (ends-with "" nil)) "nil suffix")
  (prove:ok (not (ends-with nil "")) "nil string")
  (prove:is-error (ends-with 3 3) 'type-error "Not strings"))

(prove:subtest "starts-with"
  (prove:ok (starts-with "1" "1" :ignore-case t) "equal strings")
  (prove:ok (not (starts-with "1" "12" :ignore-case t))
    "prefix lengthier than string")
  (prove:ok (starts-with "123" "12" :ignore-case t) "does start with prefix")
  (prove:ok (starts-with "123" "123" :ignore-case nil) "equal strings")
  (prove:ok (starts-with "a123A" "A123" :ignore-case t)
    "starts with if ignoring case")
  (prove:ok (not (starts-with "123A" "123a" :ignore-case nil))
    "doesn't start with if not ignoring case")
  (prove:ok (starts-with "123A" "123A" :ignore-case nil) "equal strings")
  (prove:ok (not (starts-with "123A" "123a")) "default to not ignore case")
  (prove:ok (starts-with "abcd" "") "empty prefix")
  (prove:ok (not (starts-with "" "abcd")) "empty string")
  (prove:ok (starts-with "" "") "both empty")
  (prove:ok (not (starts-with "" nil)) "nil prefix")
  (prove:ok (not (starts-with nil "")) "nil string")
  (prove:is-error (starts-with 3 3) 'type-error "Not strings"))

(prove:subtest "shorten"
  (prove:is (shorten "123456789" 5) "12345..." "shorten with default")
  (prove:is (shorten "123456789" 8) "12345678..." "shorten close to the end")
  (prove:is (shorten "123456789" 9) "123456789" "same length")
  (prove:is (shorten "123456789" 15) "123456789" "smaller length")
  (prove:is (shorten "123456789" 0) "..." "0 length")
  (prove:is (shorten "123456789" 5 :truncate-string ", etc.") "12345, etc."
    "different truncate-string")
  (prove:is (shorten "123456789" 5 :truncate-string nil) "12345"
    "truncate-string nil")
  (prove:is (shorten "123456789" 5 :truncate-string "") "12345"
    "truncate-string empty")
  (prove:is (shorten "123456789" 0 :truncate-string "") ""
    "truncate-string empty and no length"))

(prove:subtest "repeat"
  (prove:is (repeat "123" 2) "123123" "normal correct case")
  (prove:is (repeat "123" 2 :separator "") "123123" "using :separator")
  (prove:is (repeat "12345" 4 :separator ", ") "12345, 12345, 12345, 12345"
    "with custom :separator")
  (prove:is (repeat "123456789" 0) "" "0 repeats")
  (prove:is-error (repeat "123456789" nil) 'type-error "nil repeats")
  (prove:is (repeat "" 10) "" "empty string")
  (prove:is (repeat "" 5 :separator ".") "...." "empty string with separator"))

(prove:subtest "join"
  (prove:is (join (list "ab" "cd" "ef")) "abcdef" "normal case")
  (prove:is (join (list "ab" "cd" "ef") :separator "") "abcdef"
    "normal case with delimiter")
  (prove:is-error (join (list "ab" "cd" "ef") :separator nil) 'type-error
    "nil separator")
  (prove:is (join (list "ab") :separator ", ") "ab" "just one element")
  (prove:is (join nil :separator ", ") "" "null list")
  (prove:is (join (list "ab" "cd" "ef") :separator ", ") "ab, cd, ef"
    "normal case with delimiter"))

(prove:subtest "replace-all"
  (prove:is (replace-all "the man bit the dog" "man" "snake")
        "the snake bit the dog" "simple replace")
  (prove:is (replace-all "the man bit the dog" "MAN" "snake")
        "the man bit the dog" "does not ignore case by default")
  (prove:is (replace-all "the man bit the dog" "MAN" "snake" :ignore-case t)
        "the snake bit the dog" "simple replace")
  (prove:is (replace-all "the man bit the other man" "Man" "snake" :ignore-case t)
        "the snake bit the other snake" "multiple replaces")
  (prove:is (replace-all "the man bit the dog" "the " "")
        "man bit dog" "replace for nothing")
  (prove:is (replace-all "the man bit the dog" "" "snake")
        "the man bit the dog" "no replace"))

(prove:subtest "chars"
  (prove:is (chars "foo bar") (list #\f #\o #\o #\SPACE #\b #\a #\r) "standard chars")
  (prove:is (chars "") nil "empty string")
  (prove:is-error (chars 32) 'type-error "Test type error"))

(prove:subtest "split"
  (prove:is (split "crazy foo bar") (list "crazy" "foo" "bar")
    "simple split")
  (prove:is (split "crazy  foo bar") (list "crazy" "" "foo" "bar")
    "simple split with empty string")
  (prove:is (split "crazy foo bar" #\space) (list "crazy" "foo" "bar")
    "split by space character")
  (prove:is (split "crazy foo bar" " ") (list "crazy" "foo" "bar")
    "split by space string")
  (prove:is (split "crazy foo bar" #\f) (list "crazy " "oo bar")
    "split by character f")
  (prove:is (split "crazy foo bar" " foo") (list "crazy" " bar")
    "split by string \" foo\"")
  (prove:is (split "crazy foo bar" " Foo") (list "crazy foo bar")
    "split by string \" Foo\"")
  (prove:is (split "crazy foo bar" " Foo" :ignore-case t) (list "crazy" " bar")
    "split by string \" Foo\"")
  (prove:is (split "crazy bar" "")
    (list #\c #\r #\a #\z #\y #\space #\b #\a #\r) "split by the empty string")
  (prove:is (split "" "foo") (list "") "empty string")
  (prove:is (split "" "") nil "both-empty")
  (prove:is-error (split 34 "foo") 'type-error "Test type error"))

(prove:subtest "chop"
 (prove:is (chop "the man bit the dog" 4) (list "the " "man " "bit " "the " "dog")
  "Simple chop")
 (prove:is (chop "the man bit the dog" 12) (list "the man bit " "the dog")
  "Big chop")
 (prove:is (chop "the man bit the dog" 19) (list "the man bit the dog")
  "Chop with full length")
 (prove:is (chop "the man bit the dog" 30) (list "the man bit the dog")
  "Bigger chop than string")
 (prove:is (chop "foo bar" 1) (list "f" "o" "o" " " "b" "a" "r")
  "Chop by one")
 (prove:is (chop "foo bar" 0) (list "foo bar")
  "Chop by zero")
 (prove:is (chop "foo bar" -8) (list "foo bar")
  "Chop by negative number")
 (prove:is-error (chop 34 3) 'type-error "Test type error"))

(prove:subtest "toggle-case"
  (prove:is (toggle-case "This Stuff Is Crazy") "tHIS sTUFF iS cRAZY"
    "Simple case")
  (prove:is (toggle-case "mY aUNT hAS 3 dOGS!") "My Aunt Has 3 Dogs!"
    "Whith neutral characters")
  (prove:is (toggle-case "") ""
    "Empty string")
  (prove:is-error (toggle-case 34) 'type-error
    "Not a string"))

(prove:subtest "format-number"
  (prove:is (format-number 34) "34" "Simplest case")
  (prove:is (format-number 39.2) "39" "Round down")
  (prove:is (format-number 39.6) "40" "Round up")
  (prove:is (format-number -10.3) "-10" "Negative number")
  (prove:is (format-number 3.419 :precision 1) "3.4" "Precision 1")
  (prove:is (format-number 3.419 :precision 2) "3.42" "Precision 2")
  (prove:is (format-number 3.419 :precision 3) "3.419" "Precision 3")
  (prove:is (format-number 3.419 :precision 4) "3.4190" "Precision 4")
  (prove:is (format-number 3.419 :precision 5) "3.41900" "Precision 5")
  (prove:is (format-number -3.419 :precision 5) "-3.41900" "Precision 5, negative")
  (prove:is (format-number -3.419 :precision 5 :decimal-separator "!") "-3!41900"
    "Custom decimal separator")
  (prove:is (format-number 3419.26 :precision 2) "3,419.26" "Default delimiters")
  (prove:is (format-number 3419.26 :precision 1 :order-separator #\.) "3.419.3"
    "Custom order separator as char")
  (prove:is (format-number 3419.26 :precision 1 :order-separator ".") "3.419.3"
    "Custom order separator as string")
  (prove:is (format-number 212322.4473d0 :precision 4 :order-separator ",")
    "212,322.4473" "Right precision with big decimal number")
  (prove:is (format-number 22321.4 :precision 4 :order-separator "")
    "22321.4000" "Empty order separator")
  (prove:is (format-number 22321.4 :precision 4 :order-separator nil)
    "22321.4000" "Null order separator")
  (prove:is-error (format-number "3419") 'type-error "Number must be a number")
  (prove:is-error (format-number 3419.25 :precision -3) 'type-error
    "Precision must be 0 or higher")
  (prove:is-error (format-number 3419.25 :precision 3 :order-separator 4)
    'type-error "Separators must be a char or a string"))

(prove:subtest "parse-number"
  (prove:ok (= (parse-number "0") 0) "Zero")
  (prove:ok (= (parse-number "-5") -5) "Negative")
  (prove:ok (= (parse-number "3.1") 3.1) "Decimal")
  (prove:ok (= (parse-number "-10.9") -10.9) "Negative and decimal")
  (prove:ok (= (parse-number "-10,9" :decimal-separator #\,) -10.9)
    "Custom char separator")
  (prove:ok (= (parse-number "+2332") 2332) "Positive sign in front")
  (prove:ok (= (parse-number "-13,92" :decimal-separator ",") -13.92)
    "Custom string separator")
  (prove:ok (= (parse-number "1 234 456" :order-separator " ") 1234456)
    "With order separator")
  (prove:ok (= (parse-number "1,234,456.9" :order-separator #\,) 1234456.9)
    "Order separator and decimal")
  (prove:ok (= (parse-number "-1,234-22" :decimal-separator "-" :order-separator ",")
               -1234.22) "Order separator, custom decimal separator and negative")
  (prove:ok (= (parse-number "2e0") 2) "Scientific notation")
  (prove:ok (= (parse-number "-2.3e2") -230)
    "Scientific notation, decimal and negative")
  (prove:ok (= (parse-number "-20.1e-1") -2.01)
    "Scientific notation with negative exponent")
  (prove:ok (= (parse-number "-2,3e2" :decimal-separator ",") -230)
    "Scientific notation with custom decimal separator")
  (prove:ok (= (parse-number "10/2") 5) "Fractional number")
  (prove:ok (= (parse-number "-10/3") -10/3) "Fractional negative number")
  (prove:ok (= (parse-number "-.3") -0.3) "No integer part in decimal number")
  (prove:ok (= (parse-number "-3.") -3) "No decimal part in decimal number")
  (prove:is-error (parse-number "2ea") 'parse-error "Not a number")
  (prove:is-error (parse-number "2e3.4") 'parse-error
    "Decimal exponent not allowed")
  (prove:is-error (parse-number "10/3.4") 'parse-error
    "Decimal denominator not allowed")
  (prove:is-error (parse-number "3 123,123" :order-separator " ") 'parse-error
    "Two different order separators not allowed"))

(prove:subtest "clean-diacritics"
  (prove:is (clean-diacritics "") "" "Empty string")
  (prove:is (clean-diacritics "no diacritics") "no diacritics" "No diacritics")
  (prove:is (clean-diacritics "Déjà vu") "Deja vu" "Clean multiple diacritics")
  (prove:is (clean-diacritics "ÀÀÀÀÀÀÀhhh") "AAAAAAAhhh" "Upcase diacritics")
  (prove:is-error (clean-diacritics 3) 'type-error "Not a string")
  (prove:is (clean-diacritics nil) "" "Null argument"))

(prove:subtest "clean"
  (prove:is (clean "") "" "Empty string")
  (prove:is (clean "no double spaces") "no double spaces" "No double spaces")
  (prove:is (clean " foo bar") "foo bar" "Left trim")
  (prove:is (clean " foo bar ") "foo bar" "Left and right trim")
  (prove:is (clean " foo  bar ") "foo bar" "Left and right trim")
  (prove:is (clean "   the   man  bit  the    dog   ") "the man bit the dog"
    "Multiple spaces everywhere")
  (prove:is (clean "zzzthezzzzmanzzzzzbitzzzzzthezzzzzzdogz" :char #\z)
    "thezmanzbitzthezdog" "Multiple spaces everywhere")
  (prove:is-error (clean 3) 'type-error "Not a string")
  (prove:is-error (clean nil) 'type-error "Null argument"))

(prove:subtest "insert"
  (prove:is (insert "man ""the bit the dog" :position 4) "the man bit the dog"
    "Simple case")
  (prove:is (insert "the " "man bit the dog" :position 0) "the man bit the dog"
    "Insert at the start")
  (prove:is (insert "dog" "the man bit the ") "the man bit the dog"
    "Insert at the end by default")
  (prove:is (insert "" "foo" :position 1) "foo" "Insert empty string")
  (prove:is (insert "foo" "" :position 0) "foo" "Insert into empty string")
  (prove:is (insert "foo" "") "foo" "Into empty string with default pos")
  (prove:is-error (insert "foo" 3 :position 0) 'type-error "Original not a string")
  (prove:is-error (insert " bar" "foo" :position 10) 'type-error "Out of bounds")
  (prove:is-error (insert nil "foo" :position 0) 'type-error "Insert nil"))

(prove:subtest "camel-case"
  (prove:is (camel-case "the man bit the dog") "theManBitTheDog" "Simple case")
  (prove:is (camel-case "") "" "Empty string")
  (prove:is (camel-case "the") "the" "Only one word lowercase")
  (prove:is (camel-case "the ") "the" "Only one word and space")
  (prove:is (camel-case " the") "the" "One space followed by a word")
  (prove:is (camel-case "The man bit the dog") "TheManBitTheDog"
    "Already an uppercase letter")
  (prove:is (camel-case "893") "893" "String starting with numericals char")
  (prove:is (camel-case "The man bit the dog" :delimiter "e ") "ThMan bit thDog"
    "With a specified delimiter")
  (prove:is-error (camel-case 893) 'type-error "Not a string")
  (prove:is-error (camel-case "11" :delimiter 21) 'simple-type-error "Not a string"))

(prove:subtest "snake-case"
  (prove:is (snake-case "the man bit the dog") "the_man_bit_the_dog" "Simple case")
  (prove:is (snake-case "") "" "Empty string")
  (prove:is (snake-case "the") "the" "Only on word")
  (prove:is (snake-case " the") "the" "Leading white space")
  (prove:is (snake-case "the ") "the" "Trailing white space")
  (prove:is (snake-case "The man bit The dog" :delimiter "e ") "Th_man bit th_dog"
    "Replacing more than one char")
  (prove:is-error (snake-case 893) 'type-error "First arg not a string")
  (prove:is-error (snake-case "The man bit the dog" :delimiter 21) 'type-error
    "Second arg not a string"))

(prove:subtest "kebab-case"
  (prove:is (kebab-case "the man bit the dog") "the-man-bit-the-dog" "Simple case")
  (prove:is (kebab-case "") "" "Empty string")
  (prove:is (kebab-case "The man bit the dog" :delimiter "e ") "th-man bit th-dog"
    "Replacing more than one char")
  (prove:is (kebab-case "THE man BIT the DOG") "the-man-bit-the-dog"
    "Uppercase in the string")
  (prove:is-error (kebab-case 893) 'type-error "First arg not a string")
  (prove:is-error (kebab-case "The man bit the dog" :delimiter 21) 'type-error
    "Second arg not a string"))

(prove:subtest "title-case"
  (prove:is (title-case "the man bit the dog") "The Man Bit The Dog" "Simple case")
  (prove:is (title-case "") "" "Empty string")
  (prove:is (title-case "The man-bit-the dog" :remove-hyphens nil)
    "The Man-bit-the Dog" "Keeyping hyphens")
  (prove:is (title-case "THE-man-BIT the-DOG") "The Man Bit The Dog"
    "Uppercase in the string and removing hyphens")
  (prove:is-error (title-case 893) 'type-error "First arg not a string"))

(prove:subtest "make-template-parser"
  (prove:is (funcall (make-template-parser "{{" "}}")
                     "Hello {{name}}, welcome to {{website}}!"
                     '(("name" . "Sam") ("website" . "cl-strings")))
            "Hello Sam, welcome to cl-strings!"
            "Normal case")
  (prove:is (funcall (make-template-parser "<" ">")
                     "<Hello> <name>, welcome to <website><>!"
                     '(("name" . "Sam") ("website" . "cl-strings")))
            "<Hello> Sam, welcome to cl-strings<>!"
            "Whith unknown variables")
  (prove:is (funcall (make-template-parser "<" ">")
                     "<Hello> <name>, welcome to <website><>!"
                     '(("name" . "Sam") ("Website" . "cl-strings")))
            "<Hello> Sam, welcome to <website><>!"
            "Different case on a variable name")
  (prove:is (funcall (make-template-parser "<" ">" :ignore-case t)
                     "<Hello> <name>, welcome to <website><>!"
                     '(("name" . "Sam") ("Website" . "cl-strings")))
            "<Hello> Sam, welcome to cl-strings<>!"
            "Ignore case -> true")
  (prove:is (funcall (make-template-parser "<" ">" :ignore-case t)
                     "<Hello> <na<me>, welcome to <website><>!"
                     '(("na<me" . "Sam") ("Website" . "cl-strings")))
            "<Hello> Sam, welcome to cl-strings<>!"
            "With open separator in the middle")
  (prove:is (funcall (make-template-parser "<" ">" :ignore-case t)
                     "<Hello> <na>me>, welcome to <website><>!"
                     '(("na" . "Sam") ("Website" . "cl-strings")))
            "<Hello> Samme>, welcome to cl-strings<>!"
            "With two close separators")
  (prove:is (funcall (make-template-parser "$${{" "}}")
                     "$${{Hello}} $${{name}}, welcome to $${{website}}}}!"
                     '(("name" . "Sam") ("website" . "cl-strings")))
            "$${{Hello}} Sam, welcome to cl-strings}}!"
            "With different lengths and extra close tag")
  (prove:is-error (funcall (make-template-parser "" "")
                     "$${{Hello}} $${{name}}, welcome to $${{website}}}}!"
                     '(("name" . "Sam") ("website" . "cl-strings")))
            'type-error "Empty strings in template delimiters")
  (prove:is-error (funcall (make-template-parser 3 4)
                     "$${{Hello}} $${{name}}, welcome to $${{website}}}}!"
                     '(("name" . "Sam") ("website" . "cl-strings")))
            'type-error "Bad types in template delimiters")
  (prove:is (funcall (make-template-parser "{{" "}}")
                     ""
                     '(("name" . "Sam") ("website" . "cl-strings")))
            "" "Empty string")
  (prove:is-error (funcall (make-template-parser "{{" "}}")
                     45 '(("name" . "Sam") ("website" . "cl-strings")))
            'type-error "Bad type on string"))

(prove:finalize)
