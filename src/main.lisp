(defpackage syllab
  (:use :arrow-macros :cl :cl-oju :cl-ppcre)
  (:export :make-name :full-name-as-str :full-name-as-list))
(in-package :syllab)

(defun getenv (name &optional default)
  ;; From http://cl-cookbook.sourceforge.net/os.html
  #+CMU
  (let ((x (assoc name ext:*environment-list*
                  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
   #+Allegro (sys:getenv name)
   #+CLISP (ext:getenv name)
   #+ECL (si:getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   default))

(defparameter +corpus+
  (cl-ppcre:split "\\s+"
                  (concatenate 'string
                               (slurp (concatenate 'string
                                                   (getenv "LISP_HOME")
                                                   "/syllab/female-names.txt"))
                               (slurp (concatenate 'string
                                                   (getenv "LISP_HOME")
                                                   "/syllab/male-names.txt")))))

(defun seq (x)
  "poor person's sequential abstraction"
  (cond ((listp x) x)
        ((stringp x) (loop for c across x collect c))))

(defun l->str (x)
  (concatenate 'string x))

(defun mappend (fn &rest lsts)
  "maps elements in list and finally appends all resulted lists."
  (apply #'append (apply #'mapcar fn lsts)))

(defun make-trigrams (n)
  (->> +corpus+
       (mappend #'(lambda (x)
                    (->> x
                         string-downcase
                         seq
                         (partition-all n 1)
                         (remove-if-not #'(lambda (l) (= n (length l)))))))
       (mapcar #'l->str)
       frequencies
       (lambda (l) (sort l #'>= :key #'cadr))
       (mapcar #'car)
       (take 100)))

(defparameter trigrams (mapcar #'make-trigrams (range 2 5)))

(defun trigram-name ()
  (loop repeat (1+ (rand-int (1+ (rand-int (1+ (rand-int 5))))))
        collect (rand-nth (rand-nth trigrams)) into ret
        finally (return (apply #'concatenate 'string ret))))

(defun corpus-name ()
  "Return a name at random directly from the corpus"
  (rand-nth +corpus+))

(defun make-name ()
  (if (= (random 2) 0)
      (trigram-name)
      (corpus-name)))

(defun full-name-as-list ()
  (loop repeat (rand-nth '(1 2 2 2 2 2 3 3 4 5 6))
        collect (string-capitalize (make-name))))

(defun string-join-space (coll)
  (format nil "~{~A~^ ~}" coll))

(defun full-name-as-str ()
  (string-join-space (full-name-as-list)))
