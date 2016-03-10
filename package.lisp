(in-package :cl-user)

(defpackage :cl-strings
  (:use :common-lisp)
  (:nicknames :s)
  (:shadow
    #:truncate)
  (:export
    #:starts-with
    #:ends-with
    #:truncate
    #:repeat
    #:join
    #:replace-all
    #:chars
    #:split
    #:chop
    #:toggle-case
    #:format-number
    #:parse-number
    #:clean-diacritics
    #:clean
    #:insert
    #:camel-upper-case
    #:snake-case
    #:kebab-case))
