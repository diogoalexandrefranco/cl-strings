(in-package :cl-strings)

(defun starts-with (string target &key (ignore-case nil))
  "Returns true if \"string\"'s first characters are equal to \"target\"."
  (let ((target-len (length target))
        (string-len (length string)))
    (when (>= string-len target-len)
      (funcall (if ignore-case #'string-equal #'string=)
               string target :start1 0 :end1 target-len))))

(defun ends-with (string target &key (ignore-case nil))
  "Returns true if \"string\"'s last characters are equal to \"target\"."
  (let ((target-len (length target))
        (string-len (length string)))
    (when (>= string-len target-len)
      (funcall (if ignore-case #'string-equal #'string=)
               string target :start1 (- string-len target-len)))))

(defun truncate (string len &key (truncate-string "..."))
  "If \"string\"'s length is bigger than \"length\", cut the last
  characters out. Also replaces the last characters of the truncated
  string for the omission string. It defaults to \"...\", but can be
  nil or the empty string."
  (let ((string-len (length string)))

    (if (<= string-len len)
      (return-from truncate string))

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

(defun split (string &optional separator &key (ignore-case nil))
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
      (cond ((null separator)
             (%split-by-char string #\space))
            ((typep separator 'character)
             (%split-by-char string separator))
            ((string= separator "") (chars string))
            ((typep separator 'string)
             (%split-by-str string separator))
            (t (error (make-condition 'type-error :datum separator :expected-type 'string))))))

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
  (if (< precision 0) (error "Precision should be 0 or higher."))
  (unless (and (or (stringp decimal-separator) (characterp decimal-separator))
               (or (stringp order-separator) (characterp order-separator)))
      (error "decimal-separator and order-separator should both be characters or strings."))

  (let* ((float-formatted (format nil "~,vF" precision number))
         (decimal-part (subseq float-formatted (- (length float-formatted) precision)))
         (integer-formatted (parse-integer
                              (subseq float-formatted 0 (1- (- (length float-formatted) precision)))))
         (integer-part (format nil "~0,'0,v,3:D"
                                   (if (characterp order-separator)
                                       order-separator
                                       (char order-separator 0))
                                   integer-formatted)))
    (if (> precision 0)
        (concatenate 'string integer-part (if (characterp decimal-separator)
                                              (string decimal-separator)
                                              decimal-separator)
                                          decimal-part)
        integer-part)))

(defun parse-number (number-str &key (decimal-separator #\.) (order-separator nil))
  "Parses number-str without using the reader, returning the equivalent number"
  (check-type number-str string)
  (if (string= number-str "") (error (make-condition 'parse-error)))
  (labels ((%clean-order-separators (number-str &optional order-separator)
              (if (stringp order-separator)
                (setf order-separator (char order-separator 0)))
              (loop for c across number-str do
                (when (not (digit-char-p c))
                    (if order-separator
                        (if (or (char= order-separator c))
                            (return-from %clean-order-separators
                              (%clean-order-separators (remove c number-str) c))
                            (error (make-condition 'parse-error)))
                        (error (make-condition 'parse-error)))))
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
                  (error (make-condition 'parse-error)))
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
                    (t (error (make-condition 'parse-error)))))))

    (if (char= (char number-str 0) #\-)
        (* -1 (%parse-positive (subseq number-str 1) decimal-separator))
        (%parse-positive number-str decimal-separator))))

(defun clean-diacritics (string)
  "Returns a string with the diacritics replaced by their closest ASCII equivalents"
  (let ((from "ąàáäâãåæăćčĉęèéëêĝĥìíïîĵłľńňòóöőôõðøśșşšŝťțţŭùúüűûñÿýçżźžĄÀÁÄÂÃÅÆĂĆČĈĘÈÉËÊĜĤÌÍÏÎĴŁĽŃŇÒÓÖŐÔÕÐØŚȘŞŠŜŤȚŢŬÙÚÜŰÛÑŸÝÇŻŹŽ")
        (to "aaaaaaaaaccceeeeeghiiiijllnnoooooooossssstttuuuuuunyyczzzAAAAAAAAACCCEEEEEGHIIIIJLLNNOOOOOOOOSSSSSTTTUUUUUUNYYCZZZ"))
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

(defun insert (string original &key (position nil))
  "Returns a string consisting of \"original\" with \"string\" inserted
  at \"position\"."
  (check-type original string)
  (check-type string string)
  (if (null position)
      (setq position (length original))
      (progn
        (check-type position number)
        (when (not (<= 0 position (length original)))
          (error (make-condition 'simple-type-error :format-control
                  "position out of bounds.")))))
  (with-output-to-string (stream)
    (write-string (subseq original 0 position) stream)
    (write-string string stream)
    (write-string (subseq original position) stream)))



(defun camel-case (string &key (part " "))
  "Returns a string wich concatenate every word separated by a space(or a specified delimiter), and uppercase every first letter of those words except for the first word of the string. "
  (check-type string string)
  (check-type part string)
  (if (eq (length string) 0)
    ""
    (let ((list-of-words (split string part)))
      (if (< (length list-of-words) 2)
	string
	(with-output-to-string (stream)
	  (write-string (elt list-of-words 0) stream)
	  (write-string (join  (map 
				'list 
				(lambda (e)
				  (if (< (length e) 1)
				      ""
				      (concatenate 
				       'string 
				       (string (char-upcase (char e 0))) 
				       (subseq e 1))))
				  (subseq list-of-words 1))) stream))))))


(defun snake-case (string &key (part " "))
  "Returns a string with every space (or a char specified) replaced by an underscore"
  (check-type string string)
  (check-type part string)
  (replace-all string part "_")) 
  
(defun kebab-case (string &key (part " "))
  "Returns a string with every space (or a char specified) replaced by an hyphen"
  (check-type string string)
  (check-type part string)
  (string-downcase (replace-all string part "-")))

