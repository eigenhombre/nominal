(defpackage nominal
  (:use :arrow-macros :cl :cl-oju :cl-ppcre)
  (:export
   :make-name
   :ngram-name
   :trad-name
   :single-name
   :full-name-as-str
   :full-name-as-list))

(in-package :nominal)

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

(defun read-resource (filename)
  (slurp (concatenate 'string
                      (getenv "LISP_HOME")
                      "/nominal/resources/"
                      filename)))

(defun split-lines (s)
  (cl-ppcre:split "\\s+" s))

(defparameter +corpus+
  (split-lines
   (concatenate 'string
                (read-resource "female-names.txt")
                (read-resource "male-names.txt"))))

(defun seq (x)
  "poor person's sequential abstraction"
  (cond ((listp x) x)
        ((stringp x) (loop for c across x collect c))))

(defun string-join-space (coll)
  (format nil "~{~A~^ ~}" coll))

(defun string-join (x)
  (concatenate 'string x))

(defun mappend (fn &rest lsts)
  "maps elements in list and finally appends all resulted lists."
  (apply #'append (apply #'mapcar fn lsts)))

(defun make-ngrams (n)
  (->> +corpus+
       (mappend #'(lambda (x)
                    (->> x
                         string-downcase
                         seq
                         (partition-all n 1)
                         (remove-if-not #'(lambda (l) (= n (length l)))))))
       (mapcar #'string-join)
       frequencies
       (lambda (l) (sort l #'>= :key #'cadr))
       (mapcar #'car)
       (take 200)))

(defparameter ngrams (mappend #'make-ngrams (range 2 5)))

(defun ngram-name ()
  (loop repeat (1+ (rand-int (1+ (rand-int (1+ (rand-int 5))))))
        collect (rand-nth ngrams) into ret
        finally (return (apply #'concatenate 'string ret))))

(defun trad-name ()
  "Return a name at random directly from the corpus"
  (rand-nth +corpus+))

(defun single-name ()
  (if (= (random 2) 0)
      (ngram-name)
      (trad-name)))

(defun make-name () (single-name))  ;; Backwards compat

(defun full-name-as-list ()
  (loop repeat (rand-nth '(1 2 2 2 2 2 3 3 4 5 6))
        collect (string-capitalize (single-name))))

(defun maybe-suffix ()
  (if (< (random 1.0) 0.2)
      (list (rand-nth '("I" "II" "III" "Jr." "Jr." "IV" "Esq.")))
      nil))

(defun maybe-honorific ()
  (let ((r (random 1.0)))
    (cond
      ((< r 0.01) (list (rand-nth '("先生" "女士" "小姐" "夫人"))))
      ((< r 0.5) (list (rand-nth '("Dr." "Mr." "Ms." "Fr." "Miss"
                                   "Mrs." "Herr" "M" "Sir" "Mx."
                                   "Lady" "Lord"))))
      (t ()))))

(defun full-name-as-list-with-honorific-and-suffix ()
  `(,@ (maybe-honorific)
       ,@ (full-name-as-list)
       ,@ (maybe-suffix)))

(defun full-name-as-str ()
  (string-join-space (full-name-as-list-with-honorific-and-suffix)))

(comment
 (time (loop repeat 100000 do (full-name-as-str)))
 ;; 1.8 seconds
 (* 1E6 (/ 1.8 100000))
 ;;=>
 17.999998  ;; Microseconds

 (loop repeat 10 collect (full-name-as-str))
 ;;=>
 '("Mx. Marvis Gerry" "Rleeio Et Zaida Madeline Shauna Than" "Ngmarc Ph Nna"
   "女士 Rosesta Ngrene Ll IV" "先生 Ry Ish Colby Urden Eabe"
   "James Abram Rodrigo II" "Mrs. Ant Vince Waean Ste II" "Ryfa Conchita"
   "Kamala Evon May Jr." "Herr Lannie"))
