;;
;;  XML-PList Emitter
;;
;;  Copyright 2011 Thomas de Grivel <billitch@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for
;;  any purpose with or without fee is hereby granted, provided that
;;  the above copyright notice and this permission notice appear in
;;  all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
;;  WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
;;  WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL
;;  THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
;;  CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
;;  LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
;;  NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
;;  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

(defpackage :xml-plist-emitter
  (:nicknames :xml-plist)
  (:use :cl :xml-emitter)
  (:export #:item
	   #:key
	   #:key-value
	   #:with-array
	   #:with-dict
	   #:with-xml-plist))

(in-package :xml-plist-emitter)

(defmacro with-xml-plist ((stream) &body body)
  `(with-xml-output (,stream :encoding "UTF-8")
     (xml-as-is "<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">")
     (with-tag ("plist" '(("version" "1.0")))
       ,@body)))

;;  PList elements

(defun key (name)
  (with-simple-tag ("key")
    (xml-out name)))

(defmacro with-array (&body body)
  `(with-tag ("array")
     ,@body))

(defmacro with-dict (&body body)
  `(with-tag ("dict")
     ,@body))

;;  Serializer


(defgeneric item (object))

(defmethod item (object)
  (error "Unspecialized method XML-PLIST:ITEM with argument ~S" object))

(defmethod item ((object string))
  (with-simple-tag ("string")
    (xml-out object)))

(defmethod item ((object integer))
  (with-simple-tag ("integer")
    (xml-out (format nil "~D" object))))

(defmethod item ((object sequence))
  (with-tag ("array")
    (map nil (lambda (o)
	       (item o))
	 object)))

(defmethod item ((object hash-table))
  (with-tag ("dict")
    (maphash (lambda (key value)
	       (key key)
	       (item value))
	     object)))

(defun key-value (key value)
  (key key)
  (item value))
