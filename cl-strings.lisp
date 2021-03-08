;;; -*- mode: lisp; encoding: (utf-8) -*-

(in-package :cl-strings)

(defvar *blank-chars* '(#\Space #\Newline #\Backspace #\Tab
                        #\Linefeed #\Page #\Return #\Rubout))

(defun starts-with (string prefix &key (ignore-case nil))
  "Returns true if \"string\"'s first characters are equal to \"prefix\"."
  (let ((prefix-len (length prefix))
        (string-len (length string)))
    (when (>= string-len prefix-len)
      (funcall (if ignore-case #'string-equal #'string=)
               string prefix :start1 0 :end1 prefix-len))))

(defun ends-with (string suffix &key (ignore-case nil))
  "Returns true if \"string\"'s last characters are equal to \"suffix\"."
  (let ((suffix-len (length suffix))
        (string-len (length string)))
    (when (>= string-len suffix-len)
      (funcall (if ignore-case #'string-equal #'string=)
               string suffix :start1 (- string-len suffix-len)))))

(defun shorten (string len &key (truncate-string "..."))
  "If \"string\"'s length is bigger than \"length\", cut the last
  characters out. Also replaces the last characters of the shortened
  string for the omission string. It defaults to \"...\", but can be
  nil or the empty string."
  (let ((string-len (length string)))

    (if (<= string-len len)
      (return-from shorten string))

    (concatenate 'string (subseq string 0 len)
                 truncate-string)))

(defun repeat (string count &key (separator ""))
  "Repeats a given string \"count\" number of times"
  (check-type count integer)
  (if (> count 0)
      (with-output-to-string (stream)
        (write-string string stream)
        (dotimes (i (1- count))
          (write-string separator stream)
          (write-string string stream)))
      ""))

(defun join (lst &key (separator ""))
  "Joins a list of strings (or other objects) in a string,
  delimited by \"separator\""
  (check-type lst list)
  (check-type separator string)
  (if lst
    (with-output-to-string (stream)
      (princ (first lst) stream)
      (dolist (el (rest lst))
        (write-string separator stream)
        (princ el stream)))
    ""))

(defun replace-all (string part replacement &key (ignore-case nil))
  "Returns a new string in which all the occurences of \"part\" in \"string\"
  are replaced with replacement."
  (check-type string string)
  (check-type part string)
  (check-type replacement string)
  (if (string= part "")
      string
      (with-output-to-string (out)
        (loop with part-length = (length part)
              for old-pos = 0 then (+ pos part-length)
              for pos = (search part string
                                :start2 old-pos
                                :test (if ignore-case #'char-equal #'char=))
              do (write-string string out
                               :start old-pos
                               :end (or pos (length string)))
              when pos do (write-string replacement out)
              while pos))))

(defun chars (string)
  "Returns a list with the chars in \"string\""
  (loop for c across string
    collect c))

(defun split (string &optional (separator #\space) &key (ignore-case nil))
  "Returns a list of substrings of string
  divided by separator. Separator can be a string or
  a character.
  Note: Two consecutive separators will be seen as
  if there were an empty string between them."
  (labels ((%split-by-char (string separator)
            (loop for i = 0 then (1+ j)
                  as j = (position separator string :start i :test (if ignore-case
                                                                    #'char-equal
                                                                    #'char=))
                  collect (subseq string i j)
                  while j))
           (%split-by-str (string separator)
             (loop for i = 0 then (+ j (length separator))
                   as j = (search separator string :start2 i :test (if ignore-case
                                                                    #'string-equal
                                                                    #'string=))
                   collect (subseq string i j)
                   while j)))
      (check-type string string)
      (cond ((typep separator 'character)
             (%split-by-char string separator))
            ((string= separator "") (chars string))
            ((typep separator 'string)
             (%split-by-str string separator))
            (t (error 'type-error :datum separator :expected-type 'string)))))

(defun chop (string step)
  "Returns a list with parts of \"string\", each with
	length \"step\", except for the last one which might
	have a length small than \"step\"."
  (check-type string string)
  (check-type step integer)
  (if (> step 0)
    (let ((string-len (length string)))
      (loop for i = 0 then (+ i step)
            for j = step then (+ j step)
          collect (subseq string i (if (> j string-len)
                                    string-len
                                    j))
          while (< j string-len)))
    (list string)))

(defun toggle-case (string)
  "Changes the case of each character in \"string\""
  (check-type string string)
  (with-output-to-string (stream)
    (loop for c across string do
      (if (upper-case-p c)
          (write-char (char-downcase c) stream)
          (write-char (char-upcase c) stream)))))

(defun format-number (number &key (precision 0) (decimal-separator ".") (order-separator ","))
  "Converts a number to a string, with \"precision\" number of digits."
  (check-type number number)
  (if (< precision 0)
      (error 'simple-type-error :format-control "Precision should be 0 or higher."))
  (unless (and (or (stringp decimal-separator) (characterp decimal-separator))
               (or (stringp order-separator) (characterp order-separator) (null order-separator)))
      (error 'simple-type-error :format-control
        "decimal-separator and order-separator should both be characters or strings."))

  (let* ((float-formatted (format nil "~,vF" precision number))
         (decimal-part (subseq float-formatted (- (length float-formatted) precision)))
         (integer-formatted (parse-integer
                              (subseq float-formatted 0 (1- (- (length float-formatted) precision)))))
         (integer-part (if (or (null order-separator) (string= order-separator ""))
                           (format nil "~a" integer-formatted)
                           (format nil "~0,'0,v,3:D"
                                   (if (characterp order-separator)
                                       order-separator
                                       (char order-separator 0))
                                   integer-formatted))))
    (if (> precision 0)
        (concatenate 'string integer-part (if (characterp decimal-separator)
                                              (string decimal-separator)
                                              decimal-separator)
                                          decimal-part)
        integer-part)))

(defun parse-number (number-str &key (decimal-separator #\.) (order-separator nil))
  "Parses number-str without using the reader, returning the equivalent number"
  (check-type number-str string)
  (if (string= number-str "") (error 'parse-error))
  (labels ((%clean-order-separators (number-str &optional order-separator)
              (if (stringp order-separator)
                (setf order-separator (char order-separator 0)))
              (loop for c across number-str do
                (when (not (digit-char-p c))
                    (if order-separator
                        (if (or (char= order-separator c))
                            (return-from %clean-order-separators
                              (%clean-order-separators (remove c number-str) c))
                            (error 'parse-error))
                        (error 'parse-error))))
              number-str)
           (%parse-int (number-str)
            (if (string= number-str "")
                0
                (nth-value 0 (parse-integer (%clean-order-separators number-str order-separator)))))
           (%parse-float (int-part decimal-part)
            (let ((int-part-nr (%parse-int (%clean-order-separators int-part order-separator)))
                  (decimal-part-nr (%parse-int (%clean-order-separators decimal-part))))
              (float (+ int-part-nr (/ decimal-part-nr (expt 10 (length decimal-part)))))))
           (%parse-exp (coeff-str exponent-str coeff-separator)
            (when (or (string= coeff-str "") (string= exponent-str ""))
                  (error 'parse-error))
            (let ((exponent-part (if (char= (char exponent-str 0) #\-)
                                     (expt 10 (* -1 (%parse-int (subseq exponent-str 1))))
                                     (expt 10 (%parse-int exponent-str)))))
              (if coeff-separator
                  (* (%parse-float (subseq coeff-str 0 coeff-separator)
                                   (subseq coeff-str (1+ coeff-separator)))
                     exponent-part)
                  (* (%parse-int coeff-str) exponent-part))))
           (%parse-div (numerator denominator)
            (/ (%parse-int numerator) (%parse-int denominator)))
           (%parse-positive (number-str decimal-separator)
            (let* ((separator (if (stringp decimal-separator)
                                  (char decimal-separator 0)
                                  decimal-separator))
                   (separator-pos (position separator number-str))
                   (exponential-pos (position #\e number-str))
                   (divisor-pos (position #\/ number-str)))
              (cond ((and separator-pos (not exponential-pos) (not divisor-pos))
                     (%parse-float (subseq number-str 0 separator-pos)
                                   (subseq number-str (1+ separator-pos))))
                    ((and exponential-pos (not divisor-pos))
                     (%parse-exp (subseq number-str 0 exponential-pos)
                                 (subseq number-str (1+ exponential-pos))
                                 (if (and separator-pos (< separator-pos exponential-pos))
                                     separator-pos)))
                    ((and divisor-pos (not exponential-pos) (not separator-pos))
                     (%parse-div (subseq number-str 0 divisor-pos)
                                 (subseq number-str (1+ divisor-pos))))
                    ((and (not separator-pos) (not exponential-pos) (not divisor-pos))
                     (%parse-int number-str))
                    (t (error 'parse-error))))))

    (let ((first-char (char number-str 0)))
      (cond ((char= first-char #\-)
             (* -1 (%parse-positive (subseq number-str 1) decimal-separator)))
            ((char= first-char #\+)
             (%parse-positive (subseq number-str 1) decimal-separator))
            (t
              (%parse-positive number-str decimal-separator))))))

(defun clean-diacritics (string)
  "Returns a string with the diacritics replaced by their closest ASCII equivalents"
  (let ((from "ąàáäâãåæăćčĉęèéëêěĝĥìíïîĵłľńňñòóöőôõðøřśșşšŝťțţŭùúüűûůñÿýçżźžĄÀÁÄÂÃÅÆĂĆČĈĘÈÉËÊĚĜĤÌÍÏÎĴŁĽŃŇÑÒÓÖŐÔÕÐØŘŚȘŞŠŜŤȚŢŬÙÚÜŰÛŮÑŸÝÇŻŹŽ")
        (to "aaaaaaaaaccceeeeeeghiiiijllnnnoooooooorssssstttuuuuuuunyyczzzAAAAAAAAACCCEEEEEEGHIIIIJLLNNNOOOOOOOORSSSSSTTTUUUUUUUNYYCZZZ"))
    (map 'string #'(lambda (x)
                    (let ((pos (position x from)))
                      (if pos
                          (char to pos)
                          x)))
                string)))

(defun clean (string &key (char #\space))
  "Returns a trimmed string with multiple spaces replaced by one"
  (check-type string string)
  (check-type char character)
  (let ((trimmed (string-trim (string char) string))
        (char-found nil))
    (with-output-to-string (stream)
      (loop for c across trimmed do
        (if (char= c char)
            (when (not char-found)
                  (write-char c stream)
                  (setq char-found t))
            (progn
              (if char-found (setf char-found nil))
              (write-char c stream)))))))

(defun insert (string original &key (position (length original)))
  "Returns a string consisting of \"original\" with \"string\" inserted
  at \"position\"."
  (check-type original string)
  (check-type string string)
  (check-type position number)
  (when (not (<= 0 position (length original)))
    (error 'simple-type-error :format-control "Position out of bounds."))

  (concatenate 'string (subseq original 0 position)
                       string
                       (subseq original position)))

(defun camel-case (string &key (delimiter #\space))
  "Returns a string which concatenates every word separated by a space
  (or a specified delimiter), and upcases every first letter
  except for the first word of the string."
  (check-type string string)
  (unless (or (characterp delimiter) (stringp delimiter))
    (error 'simple-type-error
           :format-control "delimiter should be a character or a string."))
  (let ((words (split (string-trim *blank-chars* string) delimiter)))
    (with-output-to-string (stream)
      (unless (= (length (first words)) 0)
        (write-char (char (first words) 0) stream)
        (write-string (string-downcase (subseq (first words) 1)) stream))
      (loop for word in (rest words) do
        (unless (= (length (first words)) 0)
          (write-char (char-upcase (char word 0)) stream)
          (write-string (string-downcase (subseq word 1)) stream))))))

(defun snake-case (string &key (delimiter #\space))
  "Returns a string with every space (or a specified delimiter)
  replaced by an underscore, and downcased, except for the first letter."
  (check-type string string)
  (unless (or (characterp delimiter) (stringp delimiter))
    (error 'simple-type-error
           :format-control "delimiter should be a character or a string."))
  (let ((words (split (string-trim *blank-chars* string) delimiter)))
    (with-output-to-string (stream)
      (unless (= (length (first words)) 0)
        (write-char (char (first words) 0) stream)
        (write-string (string-downcase (subseq (first words) 1)) stream))
      (loop for word in (rest words) do
        (unless (= (length (first words)) 0)
          (write-char #\_ stream)
          (write-string (string-downcase word) stream))))))

(defun kebab-case (string &key (delimiter #\space))
  "Returns a string with every space (or a specified char)
  replaced by an hyphen, and every character lower cased."
  (check-type string string)
  (unless (or (characterp delimiter) (stringp delimiter))
    (error 'simple-type-error
           :format-control "delimiter should be a character or a string."))
  (string-downcase
    (replace-all string (if (stringp delimiter)
                            delimiter
                            (string delimiter))
                        "-")))

(defun title-case (string &key (remove-hyphens t))
  "Returns a string with the first letter of every word
  upcased, and the other ones downcased."
  (check-type string string)
  (let* ((clean (if remove-hyphens (replace-all string "-" " ") string))
         (words (split (string-trim *blank-chars* clean) #\space)))
    (with-output-to-string (stream)
      (unless (= (length (first words)) 0)
        (write-char (char-upcase (char (first words) 0)) stream)
        (write-string (string-downcase (subseq (first words) 1)) stream))
      (loop for word in (rest words) do
        (unless (= (length (first words)) 0)
          (write-char #\space stream)
          (write-char (char-upcase (char word 0)) stream)
          (write-string (string-downcase (subseq word 1)) stream))))))

(defun make-template-parser (start-delimiter end-delimiter &key (ignore-case nil))
  "Returns a closure than can substitute variables
  delimited by \"start-delimiter\" and \"end-delimiter\"
  in a string, by the provided values."
  (check-type start-delimiter string)
  (check-type end-delimiter string)
  (when (or (string= start-delimiter "")
            (string= end-delimiter ""))
      (error 'simple-type-error
              :format-control "The empty string is not a valid delimiter."))
  (let ((start-len (length start-delimiter))
        (end-len (length end-delimiter))
        (test (if ignore-case
                  #'string-equal
                  #'string=)))

    (lambda (string values)
      (check-type string string)
      (unless (listp values)
        (error 'simple-type-error
               :format-control "values should be an association list"))

      (with-output-to-string (stream)
        (loop for prev = 0 then (+ j end-len)
              for i = (search start-delimiter string)
                      then (search start-delimiter string :start2 j)
              for j = (if i (search end-delimiter string :start2 i))
                      then (if i (search end-delimiter string :start2 i))
              while (and i j)
          do (write-string (subseq string prev i) stream)
             (let ((instance (rest (assoc (subseq string (+ i start-len) j)
                                          values
                                          :test test))))
               (if instance
                (princ instance stream)
                (write-string (subseq string i (+ j end-len)) stream)))

          finally (write-string (subseq string prev) stream))))))
