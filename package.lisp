(in-package :cl-user)

(defpackage :cl-strings
  (:use :common-lisp)
  (:shadow)
  (:export
    #:starts-with
    #:ends-with
    #:shorten
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
    #:camel-case
    #:snake-case
    #:kebab-case
    #:title-case
    #:make-template-parser))
