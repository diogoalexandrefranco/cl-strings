(prove:plan 19)

(prove:subtest "ends-with"
  (prove:ok (s:ends-with "1" "1" :ignore-case t) "equal strings")
  (prove:ok (not (s:ends-with "1" "12" :ignore-case t)) "target lengthier than string")
  (prove:ok (not (s:ends-with "123" "12" :ignore-case t)) "different strings")
  (prove:ok (s:ends-with "123" "123" :ignore-case nil) "equal strings")
  (prove:ok (s:ends-with "123A" "123a" :ignore-case t) "equal strings ignoring case")
  (prove:ok (not (s:ends-with "123A" "123a" :ignore-case nil)) "different strings not ignoring case")
  (prove:ok (s:ends-with "123A" "123A" :ignore-case nil) "equal strings")
  (prove:ok (not (s:ends-with "123A" "123a")) "default to not ignore case")
  (prove:ok (s:ends-with "abcd" "") "empty target")
  (prove:ok (not (s:ends-with "" "abcd")) "empty string")
  (prove:ok (s:ends-with "" "") "both empty")
  (prove:ok (not (s:ends-with "" nil)) "nil target")
  (prove:ok (not (s:ends-with "" nil)) "nil string")
  (prove:is-error (s:ends-with 3 3) 'type-error "Not strings"))

(prove:subtest "starts-with"
  (prove:ok (s:starts-with "1" "1" :ignore-case t) "equal strings")
  (prove:ok (not (s:starts-with "1" "12" :ignore-case t)) "target lengthier than string")
  (prove:ok (s:starts-with "123" "12" :ignore-case t) "does start with target")
  (prove:ok (s:starts-with "123" "123" :ignore-case nil) "equal strings")
  (prove:ok (s:starts-with "a123A" "A123" :ignore-case t) "starts with if ignoring case")
  (prove:ok (not (s:starts-with "123A" "123a" :ignore-case nil)) "doesn't start with if not ignoring case")
  (prove:ok (s:starts-with "123A" "123A" :ignore-case nil) "equal strings")
  (prove:ok (not (s:starts-with "123A" "123a")) "default to not ignore case")
  (prove:ok (s:starts-with "abcd" "") "empty target")
  (prove:ok (not (s:starts-with "" "abcd")) "empty string")
  (prove:ok (s:starts-with "" "") "both empty")
  (prove:ok (not (s:starts-with "" nil)) "nil target")
  (prove:ok (not (s:starts-with "" nil)) "nil string")
  (prove:is-error (s:starts-with 3 3) 'type-error "Not strings"))

(prove:subtest "truncate"
  (prove:is (s:truncate "123456789" 5) "12345..." "truncate with default")
  (prove:is (s:truncate "123456789" 8) "12345678..." "truncate close to the end")
  (prove:is (s:truncate "123456789" 9) "123456789" "same length")
  (prove:is (s:truncate "123456789" 15) "123456789" "smaller length")
  (prove:is (s:truncate "123456789" 0) "..." "0 length")
  (prove:is (s:truncate "123456789" 5 :truncate-string ", etc.") "12345, etc." "different truncate-string")
  (prove:is (s:truncate "123456789" 5 :truncate-string nil) "12345" "truncate-string nil")
  (prove:is (s:truncate "123456789" 5 :truncate-string "") "12345" "truncate-string empty")
  (prove:is (s:truncate "123456789" 0 :truncate-string "") "" "truncate-string empty and no length"))

(prove:subtest "repeat"
  (prove:is (s:repeat "123" 2) "123123" "normal correct case")
  (prove:is (s:repeat "123" 2 :separator "") "123123" "using :separator")
  (prove:is (s:repeat "12345" 4 :separator ", ") "12345, 12345, 12345, 12345" "with custom :separator")
  (prove:is (s:repeat "123456789" 0) "" "0 repeats")
  (prove:is-error (s:repeat "123456789" nil) 'type-error "nil repeats")
  (prove:is (s:repeat "" 10) "" "empty string")
  (prove:is (s:repeat "" 5 :separator ".") "...." "empty string with separator"))

(prove:subtest "join"
  (prove:is (s:join (list "ab" "cd" "ef")) "abcdef" "normal case")
  (prove:is (s:join (list "ab" "cd" "ef") :separator "") "abcdef" "normal case with delimiter")
  (prove:is-error (s:join (list "ab" "cd" "ef") :separator nil) 'type-error "nil separator")
  (prove:is (s:join (list "ab") :separator ", ") "ab" "just one element")
  (prove:is (s:join nil :separator ", ") "" "null list")
  (prove:is (s:join (list "ab" "cd" "ef") :separator ", ") "ab, cd, ef" "normal case with delimiter"))

(prove:subtest "replace-all"
  (prove:is (s:replace-all "the man bit the dog" "man" "snake")
        "the snake bit the dog" "simple replace")
  (prove:is (s:replace-all "the man bit the dog" "MAN" "snake")
        "the man bit the dog" "does not ignore case by default")
  (prove:is (s:replace-all "the man bit the dog" "MAN" "snake" :ignore-case t)
        "the snake bit the dog" "simple replace")
  (prove:is (s:replace-all "the man bit the other man" "Man" "snake" :ignore-case t)
        "the snake bit the other snake" "multiple replaces")
  (prove:is (s:replace-all "the man bit the dog" "the " "")
        "man bit dog" "replace for nothing")
  (prove:is (s:replace-all "the man bit the dog" "" "snake")
        "the man bit the dog" "no replace"))

(prove:subtest "chars"
  (prove:is (s:chars "foo bar") (list #\f #\o #\o #\SPACE #\b #\a #\r) "standard chars")
  (prove:is (s:chars "") nil "empty string")
  (prove:is-error (s:chars 32) 'type-error))

(prove:subtest "split"
  (prove:is (s:split "crazy foo bar") (list "crazy" "foo" "bar")
    "simple split")
  (prove:is (s:split "crazy  foo bar") (list "crazy" "" "foo" "bar")
    "simple split with empty string")
  (prove:is (s:split "crazy foo bar" #\space) (list "crazy" "foo" "bar")
    "split by space character")
  (prove:is (s:split "crazy foo bar" " ") (list "crazy" "foo" "bar")
    "split by space string")
  (prove:is (s:split "crazy foo bar" #\f) (list "crazy " "oo bar")
    "split by character f")
  (prove:is (s:split "crazy foo bar" " foo") (list "crazy" " bar")
    "split by string \" foo\"")
  (prove:is (s:split "crazy foo bar" " Foo") (list "crazy foo bar")
    "split by string \" Foo\"")
  (prove:is (s:split "crazy foo bar" " Foo" :ignore-case t) (list "crazy" " bar")
    "split by string \" Foo\"")
  (prove:is (s:split "crazy bar" "")
    (list #\c #\r #\a #\z #\y #\space #\b #\a #\r) "split by the empty string")
  (prove:is (s:split "" "foo") (list "") "empty string")
  (prove:is (s:split "" "") nil "both-empty")
  (prove:is-error (s:split 34 "foo") 'type-error "Test type error"))

(prove:subtest "chop"
 (prove:is (s:chop "the man bit the dog" 4) (list "the " "man " "bit " "the " "dog")
  "Simple chop")
 (prove:is (s:chop "the man bit the dog" 12) (list "the man bit " "the dog")
  "Big chop")
 (prove:is (s:chop "the man bit the dog" 19) (list "the man bit the dog")
  "Chop with full length")
 (prove:is (s:chop "the man bit the dog" 30) (list "the man bit the dog")
  "Bigger chop than string")
 (prove:is (s:chop "foo bar" 1) (list "f" "o" "o" " " "b" "a" "r")
  "Chop by one")
 (prove:is (s:chop "foo bar" 0) (list "foo bar")
  "Chop by zero")
 (prove:is (s:chop "foo bar" -8) (list "foo bar")
  "Chop by negative number")
 (prove:is-error (s:chop 34 3) 'type-error))

(prove:subtest "toggle-case"
  (prove:is (s:toggle-case "This Stuff Is Crazy") "tHIS sTUFF iS cRAZY"
    "Simple case")
  (prove:is (s:toggle-case "mY aUNT hAS 3 dOGS!") "My Aunt Has 3 Dogs!"
    "Whith neutral characters")
  (prove:is (s:toggle-case "") ""
    "Empty string")
  (prove:is-error (s:toggle-case 34) 'type-error
    "Not a string"))

(prove:subtest "format-number"
  (prove:is (s:format-number 34) "34" "Simplest case")
  (prove:is (s:format-number 39.2) "39" "Round down")
  (prove:is (s:format-number 39.6) "40" "Round up")
  (prove:is (s:format-number -10.3) "-10" "Negative number")
  (prove:is (s:format-number 3.419 :precision 1) "3.4" "Precision 1")
  (prove:is (s:format-number 3.419 :precision 2) "3.42" "Precision 2")
  (prove:is (s:format-number 3.419 :precision 3) "3.419" "Precision 3")
  (prove:is (s:format-number 3.419 :precision 4) "3.4190" "Precision 4")
  (prove:is (s:format-number 3.419 :precision 5) "3.41900" "Precision 5")
  (prove:is (s:format-number -3.419 :precision 5) "-3.41900" "Precision 5, negative")
  (prove:is (s:format-number -3.419 :precision 5 :decimal-separator "!") "-3!41900"
    "Custom decimal separator")
  (prove:is (s:format-number 3419.25 :precision 2) "3,419.25" "Default delimiters")
  (prove:is (s:format-number 3419.25 :precision 1 :order-separator #\.) "3.419.3"
    "Custom order separator as char")
  (prove:is (s:format-number 3419.25 :precision 1 :order-separator ".") "3.419.3"
    "Custom order separator as string")
  (prove:is-error (s:format-number "3419") 'type-error "Number must be a number")
  (prove:is-error (s:format-number 3419.25 :precision -3) 'simple-error
    "Precision must be 0 or higher")
  (prove:is-error (s:format-number 3419.25 :precision 3 :order-separator 4) 'simple-error
    "Separators must be a char or a string"))

(prove:subtest "parse-number"
  (prove:ok (= (s:parse-number "0") 0) "Zero")
  (prove:ok (= (s:parse-number "-5") -5) "Negative")
  (prove:ok (= (s:parse-number "3.1") 3.1) "Decimal")
  (prove:ok (= (s:parse-number "-10.9") -10.9) "Negative and decimal")
  (prove:ok (= (s:parse-number "-10,9" :decimal-separator #\,) -10.9)
    "Custom char separator")
  (prove:ok (= (s:parse-number "-13,92" :decimal-separator ",") -13.92)
    "Custom string separator")
  (prove:ok (= (s:parse-number "1 234 456" :order-separator " ") 1234456)
    "With order separator")
  (prove:ok (= (s:parse-number "1,234,456.9" :order-separator #\,) 1234456.9)
    "Order separator and decimal")
  (prove:ok (= (s:parse-number "-1,234-22" :decimal-separator "-" :order-separator ",")
               -1234.22) "Order separator, custom decimal separator and negative")
  (prove:ok (= (s:parse-number "2e0") 2) "Scientific notation")
  (prove:ok (= (s:parse-number "-2.3e2") -230)
    "Scientific notation, decimal and negative")
  (prove:ok (= (s:parse-number "-20.1e-1") -2.01)
    "Scientific notation with negative exponent")
  (prove:ok (= (s:parse-number "-2,3e2" :decimal-separator ",") -230)
    "Scientific notation with custom decimal separator")
  (prove:ok (= (s:parse-number "10/2") 5) "Fractional number")
  (prove:ok (= (s:parse-number "-10/3") -10/3) "Fractional negative number")
  (prove:ok (= (s:parse-number "-.3") -0.3) "No integer part in decimal number")
  (prove:ok (= (s:parse-number "-3.") -3) "No decimal part in decimal number")
  (prove:is-error (s:parse-number "2ea") 'parse-error "Not a number")
  (prove:is-error (s:parse-number "2e3.4") 'parse-error
    "Decimal exponent not allowed")
  (prove:is-error (s:parse-number "10/3.4") 'parse-error
    "Decimal denominator not allowed")
  (prove:is-error (s:parse-number "3 123,123" :order-separator " ") 'parse-error
    "Two different order separators not allowed"))

(prove:subtest "clean-diacritics"
  (prove:is (s:clean-diacritics "") "" "Empty string")
  (prove:is (s:clean-diacritics "no diacritics") "no diacritics" "No diacritics")
  (prove:is (s:clean-diacritics "Déjà vu") "Deja vu" "Clean multiple diacritics")
  (prove:is (s:clean-diacritics "ÀÀÀÀÀÀÀhhh") "AAAAAAAhhh" "Upcase diacritics")
  (prove:is-error (s:clean-diacritics 3) 'type-error "Not a string")
  (prove:is (s:clean-diacritics nil) "" "Null argument"))

(prove:subtest "clean"
  (prove:is (s:clean "") "" "Empty string")
  (prove:is (s:clean "no double spaces") "no double spaces" "No double spaces")
  (prove:is (s:clean " foo bar") "foo bar" "Left trim")
  (prove:is (s:clean " foo bar ") "foo bar" "Left and right trim")
  (prove:is (s:clean " foo  bar ") "foo bar" "Left and right trim")
  (prove:is (s:clean "   the   man  bit  the    dog   ") "the man bit the dog"
    "Multiple spaces everywhere")
  (prove:is (s:clean "zzzthezzzzmanzzzzzbitzzzzzthezzzzzzdogz" :char #\z)
    "thezmanzbitzthezdog" "Multiple spaces everywhere")
  (prove:is-error (s:clean 3) 'type-error "Not a string")
  (prove:is-error (s:clean nil) 'type-error "Null argument"))

(prove:subtest "insert"
  (prove:is (s:insert "man ""the bit the dog" :position 4) "the man bit the dog"
    "Simple case")
  (prove:is (s:insert "the " "man bit the dog" :position 0) "the man bit the dog"
    "Insert at the start")
  (prove:is (s:insert "dog" "the man bit the ") "the man bit the dog"
    "Insert at the end by default")
  (prove:is (s:insert "" "foo" :position 1) "foo" "Insert empty string")
  (prove:is (s:insert "foo" "" :position 0) "foo" "Insert into empty string")
  (prove:is (s:insert "foo" "") "foo" "Into empty string with default pos")
  (prove:is-error (s:insert "foo" 3 :position 0) 'type-error "Original not a string")
  (prove:is-error (s:insert " bar" "foo" :position 10) 'simple-type-error "Out of bounds")
  (prove:is-error (s:insert nil "foo" :position 0) 'type-error "Insert nil"))

(prove:subtest "camel-upper-case"
  (prove:is (s:camel-upper-case "the man bit the dog") "The man bit the dog" "Simple case")
  (prove:is (s:camel-upper-case "") "" "Empty string")
  (prove:is (s:camel-upper-case "The man bit the dog") "The man bit the dog" "Already an uppercase letter")
  (prove:is (s:camel-upper-case "893") "893" "String starting with numericals char")
  (prove:is-error (s:camel-upper-case 893) 'simple-type-error "Not a string"))

(prove:subtest "camel-lower-case"
  (prove:is (s:camel-lower-case "The man bit the dog") "the man bit the dog" "Simple case")
  (prove:is (s:camel-lower-case "") "" "Empty string")
  (prove:is (s:camel-lower-case "the man bit the dog") "the man bit the dog" "Already an lowercase letter")
  (prove:is (s:camel-lower-case "893") "893" "String starting with numericals char")
  (prove:is-error (s:camel-lower-case 893) 'simple-type-error "Not a string"))

(prove:subtest "snake-case"
  (prove:is (s:snake-case "the man bit the dog") "the_man_bit_the_dog" "Simple case")
  (prove:is (s:snake-case "") "" "Empty string")
  (prove:is (s:snake-case "The man bit the dog" :part "e ") "Th_man bit th_dog" "Replacing more than one char")
  (prove:is-error (s:snake-case 893) 'simple-type-error "First arg not a string")
  (prove:is-error (s:snake-case "The man bit the dog" :part 21) 'type-error "Second arg not a string"))

(prove:subtest "kebab-case"
  (prove:is (s:kebab-case "the man bit the dog") "the-man-bit-the-dog" "Simple case")
  (prove:is (s:kebab-case "") "" "Empty string")
  (prove:is (s:kebab-case "The man bit the dog" :part "e ") "th-man bit th-dog" "Replacing more than one char")
  (prove:is (s:kebab-case "THE man BIT the DOG") "the-man-bit-the-dog" "Uppercase in the string")
  (prove:is-error (s:kebab-case 893) 'simple-type-error "First arg not a string")
  (prove:is-error (s:kebab-case "The man bit the dog" :part 21) 'type-error "Second arg not a string"))


(prove:finalize)
