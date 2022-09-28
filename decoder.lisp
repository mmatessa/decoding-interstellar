; Author: Mike Matessa mmatessa@gmail.com
; Paper: Algorithms for Decoding Interstellar Messages, Journal of the British Interplanetary Society 

; (load "d:/Mike/METI/Algorithms for Decoding/decoder.lisp")
; (check-all code)

; Decodes message in lis
(defun check-all (lis)
  (setf hyp-lis nil)
  (equal-check lis)
  (hyp-substitute lis)
  (count-check lis)
  (hyp-substitute lis)
  (count-check lis)
  (hyp-substitute lis)
  (fun-check lis)
  (hyp-substitute lis)
  (base-check lis)
  (hyp-substitute lis)
  (approx-check lis)
  (hyp-substitute lis)
  (constant-check lis)
  (hyp-substitute lis)
  (planck-check lis)
  (hyp-prune)
  (hyp-substitute lis)
;  (sort-hyp)
  (dolist (i code) (print i))
)

; Prints a sorted list of hypotheses
(defun sort-hyp ()
  (setf lis (copy-list hyp-lis))
  (setf lis (sort lis '< :key 's2num))
  (dolist (i lis)
    (format t "~a~&" i)))

; Substitute algorithm for replacing symbols with hypotheses
(defun hyp-substitute (lis)
  (dolist (i (copy-list hyp-lis))
    (cond ((equal (length (second i)) 1)
	   (nsubst (first (second i)) (first i) lis :test 'equal)))))

; Returns a symbol's number
(defun s2num (x)
  (let ((str (format nil "~a" (first x))))
    (read-from-string (subseq str 1 (length str)))))

; Equal-check algorithm maps a symbol to the equals sign
; ex. (s01 s10 s01)
(defun equal-check (lis)
  (dolist (ex lis)
    (cond ((and (equal (length ex) 3)
		(equal (first ex) (third ex)))
	   (add-hyp (second ex) '(=))))))

; Count-check algorithm maps symbols to numbers and brackets
; ex. (s02 = s11 s12 s12 s13) 
(defun count-check (lis)
  (let ((c-start nil) (c-end nil) (c-lis nil))
    (dolist (ex lis)
    (cond ((and (setf c-start (position '[ ex))
		  (setf c-end (position '] ex))
		  (equal (second ex) '=))
	     (cond ((equal c-end (1+ c-start))
		    (add-hyp (first ex) '(0)))
		   ((all-equal-p (setf c-lis (subseq ex (1+ (position '[ ex)) (position '] ex))))
		    (add-hyp (nth (1+ c-start) ex) '(o))
		    (add-hyp (first ex) (list (length c-lis))))))
	  ((and (equal (nth 1 ex) '=)
		(nth 5 ex)
		(equal (nth 3 ex) (nth 4 ex))
		(not (equal (nth 4 ex) (nth 5 ex))))
	   (add-hyp (nth 2 ex) '([))
	   (add-hyp (nth 5 ex) '(])))
	  ((and (equal (nth 1 ex) '=)
		(nth 6 ex)
		(equal (nth 3 ex) (nth 4 ex))
		(equal (nth 4 ex) (nth 5 ex))
		(not (equal (nth 5 ex) (nth 6 ex))))
	   (add-hyp (nth 2 ex) '([))
	   (add-hyp (nth 6 ex) '(])))
))))

; Checks if all members of the list are equal		  
(defun all-equal-p (list)
  (or (null (rest list)) ;; singleton/empty
      (and (equalp (first list)
                   (second list))
           (all-equal-p (rest list)))))

; Function-check algorithm maps symbols to logically consistent functions 
(defun fun-check (lis)
  (dolist (ex lis)
;(0 s14 0 = 0)
    (cond ((and (equal (length ex) 5)
		(numberp (first ex))
		(numberp (third ex))
		(equal (fourth ex) '=))
;	   (add-hyp 'notation '(infix))
	   (fun-hyp (second ex) (first ex) (third ex) (fifth ex) ex))
;( s1 ( 1 & 1 ) = 2 )
	  ((and (equal (length ex) 8)
		(numberp (nth 2 ex))
		(numberp (nth 4 ex))
		(equal (nth 6 ex) '=)
		(equal (nth 3 ex) '&))
;	   (add-hyp 'notation '(prefix))
	   (fun-hyp (first ex) (nth 2 ex) (nth 4 ex) (nth 7 ex) ex))
;(s19 [ 9 ] = 3) 
	  ((and (equal (length ex) 6)
		(numberp (nth 2 ex))
		(numberp (nth 5 ex))
		(equal (nth 4 ex) '=)
		(equal (sqrt (nth 2 ex)) (nth 5 ex)))
	   (add-hyp (first ex) '(root))) ;all symbols start with 's' so root instead
)))

; puts all logically consistent functions in the hypothesis list
(defun fun-hyp (fun-sym arg1 arg2 val ex)
  (cond ((not (setf fun-lis (second (first (member fun-sym hyp-lis :key 'first :test 'equal)))))
	 (add-hyp fun-sym '(+ - * / expt))
	 (setf fun-lis '(+ - * / expt))))
  (dolist (fun fun-lis)
    (cond ((or (and (equal fun '/) (zerop arg2))
	       (not (equal (eval `(,fun ,arg1 ,arg2)) val)))
	   (setf hyp-lis (substitute (list fun-sym (remove fun fun-lis)) 
				     (list fun-sym fun-lis)
				     hyp-lis :test 'equal))
	   (setf fun-lis (remove fun fun-lis))
	   )))
)

; Base-check algorithm determines base of message and assigns decimal points
(defun base-check (lis)
  (dolist (ex lis)
;(9 + 1 = 1 0)
    (cond ((and (equal (length ex) 6)
		(equal (fourth ex) '=)
		(equal (sixth ex) 0)
		(or (and (equal (first ex) 1)
			 (add-hyp 'base (list (+ 1 (third ex))))
			 )
		    (and (equal (third ex) 1)
			 (add-hyp 'base (list (+ 1 (first ex))))
			 ))))
;(1 s5 0 = 1)
	  ((and (equal (length ex) 5)
		(equal (first ex) 1) (equal (third ex) 0)
		(equal (fourth ex) '=) (equal (fifth ex) 1))
	   (add-hyp (second ex) '(".")))
;(0 s5 1 = 1 / 1 0)
	  ((and (equal (length ex) 8)
		(equal (first ex) 0) (equal (third ex) 1)
		(equal (fourth ex) '=) (equal (cddddr ex) '(1 / 1 0)))
	   (add-hyp (second ex) '(".")))
)))

; Approximation-check algorithm maps symbol to approximation
; ex. (1 / 3 S21 0 "." 3 3 3) 
(defun approx-check (lis)
  (dolist (ex lis)
    (cond ((and (equal (nth 0 ex) 1)
		(equal (nth 1 ex) '/)
		(not (equal (nth 3 ex) '=))
		(not (zerop (nth 2 ex)))
		(equal (nth 5 ex) ".")
		(approx-p (/ 1 (nth 2 ex)) (lis2dec (cddddr ex))))
	   (add-hyp (nth 3 ex) '(approx))))))

; returns true if x and y are appoximately the same
(defun approx-p (x y)
  (< (abs (- x y)) 0.1))

; returns a decimal number from a list
; ex. (3 "." 1 4 1)
(defun lis2dec (lis)
  (read-from-string (format nil "~a.~a" (first lis) (lis2num (cddr lis)))))

; returns a number from a list
; ex. (1 8 3 6)
(defun lis2num (lis)
  (read-from-string (remove #\space (remove #\( (remove #\) (format nil "~a" lis))))))

; Constant-check algorithm maps symbols to universal constants and values in ratios
(defun constant-check (lis)
  (dolist (ex lis)
    (cond ((and (equal (second ex) 'approx)
		(equal (nth 3 ex) ".")
		(approx-p 3.14 (lis2dec (nthcdr 2 ex))))
	   (add-hyp (first ex) '(pi)))
	  ((and (equal (second ex) 'approx)
		(equal (nth 3 ex) ".")
		(approx-p 2.17 (lis2dec (nthcdr 2 ex))))
	   (add-hyp (first ex) '(euler)))
;(s23 [ s24 ] / [ s23 [ s25 ] ] approx 1 8 3 6)
	  ((and (equal (lis2num (nthcdr 12 ex)) 1836)
		(equal (nth 4 ex) '/))
	   (add-hyp (first ex) '(mass proton))
	   (add-hyp (third ex) '(mass proton))
	   (add-hyp (nth 6 ex) '(mass electron))
	   (add-hyp (nth 8 ex) '(mass electron)))
;(s26 approx 1 / 1 3 7)
	  ((and (equal (lis2num (nthcdr 4 ex)) 137)
		(equal (nth 2 ex) 1) (equal (nth 3 ex) '/))
	   (add-hyp (first ex) '(alpha)))
;(S26 = [ S27 EXPT 2 ] / [ 4 * S22 * S28 * S29 * S30 ])
	  ((and (equal (nth 4 ex) 'expt) (equal (nth 5 ex) 2)
		(member 4 (nthcdr 8 ex)) (member 'pi (nthcdr 8 ex)))
	   (add-hyp (first ex) '(alpha))
	   (add-hyp (fourth ex) '(e))
	   (add-hyp (nth 13 ex) '(e0 h-bar c)) 
	   (add-hyp (nth 15 ex) '(e0 h-bar c)) 
	   (add-hyp (nth 17 ex) '(e0 h-bar c)))
	  )
(hyp-substitute lis)
))

; Planck-check algorithm maps symbols to universal constants
(defun planck-check (lis)
  (dolist (ex lis)
; (s16 [ s17 ] = sqrt [ [ s14 * s18 ] / [ s15 ^ 3 ] ])
    (cond ((and (equal (nth 4 ex) '=)
		(equal (nth 5 ex) 'root)
		(equal (nth 9 ex) '*)
		(equal (nth 12 ex) '/)
		(or (equal (nth 15 ex) 'expt)
		    (equal (nth 15 ex) '^))
		(equal (nth 16 ex) 3))
	   (add-hyp (nth 0 ex) '(planck length))
	   (add-hyp (nth 2 ex) '(planck length))
	   (add-hyp (nth 8 ex) '(h-bar G))
	   (add-hyp (nth 10 ex) '(h-bar G))
	   (add-hyp (nth 14 ex) '(c))
	   (hyp-substitute code))
;(S31 [ S23 ] = SQRT [ [ S29 * C ] / S33 ]) 
	  ((and (or (find 'planck (second (find (first ex) hyp-lis :key 'car)))
		    (find 'planck (second (find (third ex) hyp-lis :key 'car))))
		(equal (nth 4 ex) '=)
		(equal (nth 5 ex) 'root)
		(equal (nth 9 ex) '*)
		(equal (nth 12 ex) '/))
	   (add-hyp (nth 0 ex) '(planck mass))
	   (add-hyp (nth 2 ex) '(planck mass))
	   (add-hyp (nth 8 ex) '(h-bar c))
	   (add-hyp (nth 10 ex) '(h-bar c))
	   (add-hyp (nth 13 ex) '(g))))))

; returns position of 'c' in expression
(defun c-pos (ex)
  (setf c-lis nil)
  (dotimes (i (length ex))
    (cond ((member 'c (second (first (member (nth i ex) hyp-lis :key 'first :test 'equal))))
	   (push i c-lis))))
  (reverse c-lis))

; Adds hypothesis to hypothesis list
; assumes all symbols start with S, otherwise real values put in
; so sqrt is root
(defun add-hyp (sym hyps)
  (cond ((equal (subseq (format nil "~a" sym) 0 1) "S")
  (setf mapy nil)
  (setf inter nil)
  (cond ((and (setf mapy (member sym hyp-lis :key 'first :test 'equal))
	      (setf inter (intersection (second (first mapy)) hyps)))
	 (setf hyp-lis (substitute (list (first (first mapy)) inter) (first mapy) hyp-lis :test 'equal))
	 )
	(t
	 (push (list sym hyps) hyp-lis)
	 (setf hyp-lis (reverse hyp-lis)))))))

(setf planck-def '(
(planck [ length ] = sqrt [ [ h-bar * G ] / [ c ^ 3 ] ] )
(planck [ mass ] = sqrt [ [ h-bar * c ] / G ] )
(planck [ time ] = sqrt [ [ h-bar * G ] / [ c ^ 5 ] ] )
(planck [ charge ] = sqrt [ 4 * pi * e0 * h-bar * c ] ] )))

(setf planck-hid '(
(S1 [ S2 ] = SQRT [ [ S3 * S4 ] / [ C ^ 3 ] ]) 
(S1 [ S5 ] = SQRT [ [ S3 * C ] / S4 ]) 
(S1 [ S6 ] = SQRT [ [ S3 * S4 ] / [ C ^ 5 ] ]) 
(S1 [ S7 ] = SQRT [ 4 * S8 * S9 * S3 * C ] ]) ))

(defun hider (lis)
  (setf mapping nil)
  (setf hid-ex nil)
  (setf hid-lis nil)
  (setf hidey nil)
  (setf mapy nil)
  (setf count 0)
  (dolist (ex lis)
;    (format t "~a~&" ex)
    (setf hid-ex nil)
    (dolist (sym ex)
      (setf mapy (member sym mapping :key 'first :test 'equal))
;      (format t "mappy: ~a~&" mapy)
      (cond ((or (member sym '([ ] = / * sqrt ^ c))
		 (numberp sym))
	     (push sym hid-ex))
	    (mapy
	     (push (second (first mapy)) hid-ex))
	    (t
	     (incf count)
	     (setf hidey (read-from-string (format nil "s~a" count)))
	     (push hidey hid-ex)
	     (push (list sym hidey) mapping)))
      )
    (setf hid-ex (reverse hid-ex))
    (format t "hide-ex: ~A~&" hid-ex )
    (push hid-ex hid-lis)
    (setf hid-ex nil)
    )
  (setf hid-lis (reverse hid-lis))
)

; Prune algorithm removes single hypothesis from other lists
; delete is destructive
(defun hyp-prune ()
  (setf lis hyp-lis)
  (dolist (map lis)
    (cond ((equal 1 (length (second map)))
	   (setf lis2 hyp-lis)
	   (dolist (map2 lis2)
	     (cond ((> (length (second map2)) 1)
		    (member (first (second map)) (second map2) :test 'equal)
		    (add-hyp (first map2) (remove (first (second map)) (second map2) :test 'equal))
)))))))

(setf message 
" 1 = 1 
 2 = 2 
 3 = 3 
 1 = [ o ] 
 2 = [ o o ] 
 3 = [ o o o ] 
 4 = [ o o o o ] 
 5 = [ o o o o o ] 
 6 = [ o o o o o o ] 
 7 = [ o o o o o o o ] 
 8 = [ o o o o o o o o ] 
 9 = [ o o o o o o o o o ] 
 0 = [ ] 
 4 + 5 = 9 
 9 - 4 = 5 
 2 * 3 = 6 
 6 / 2 = 3 
 2 ^ 3 = 8 
 [ 1 + 3 ] = 4 
 2 * [ 1 + 3 ] = 8 
 9 / [ 1 + 2 ] = 3 
 sqrt [ 4 ] = 2 
 sqrt [ 9 ] = 3 
 8 + 1 = 9 
 9 + 1 = 1 0 
 9 + 2 = 1 1 
 1 0 * 1 0 = 1 0 0 
 1 . 0 = 1 
 0 . 1 = 1 / 1 0 
 0 . 0 1 = 1 / 1 0 0 
 3 . 1 = 3 + 1 / 1 0 
 1 / 3 approx 0 . 3 3 3 
 pi approx 3 . 1 4 1 5 9 
 mass [ proton ] / mass [ electron ] approx 1 8 3 6 
 alpha approx 1 / 1 3 7 
 alpha = [ e ^ 2 ] / [ 4 * pi * e0 * h-bar * c ] 
 planck [ length ] = sqrt [ [ h-bar * G ] / [ c ^ 3 ] ] 
 planck [ mass ] = sqrt [ [ h-bar * c ] / G ] 
")

(defun reset-code ()
(setf code (copy-list '(
(S01 S10 S01) 
(S02 S10 S02) 
(S03 S10 S03) 
(S01 S10 S11 S12 S13) 
(S02 S10 S11 S12 S12 S13) 
(S03 S10 S11 S12 S12 S12 S13) 
(S04 S10 S11 S12 S12 S12 S12 S13) 
(S05 S10 S11 S12 S12 S12 S12 S12 S13) 
(S06 S10 S11 S12 S12 S12 S12 S12 S12 S13) 
(S07 S10 S11 S12 S12 S12 S12 S12 S12 S12 S13) 
(S08 S10 S11 S12 S12 S12 S12 S12 S12 S12 S12 S13) 
(S09 S10 S11 S12 S12 S12 S12 S12 S12 S12 S12 S12 S13) 
(S00 S10 S11 S13) 
(S04 S14 S05 S10 S09) 
(S09 S15 S04 S10 S05) 
(S02 S16 S03 S10 S06) 
(S06 S17 S02 S10 S03) 
(S02 S18 S03 S10 S08) 
(S11 S01 S14 S03 S13 S10 S04) 
(S02 S16 S11 S01 S14 S03 S13 S10 S08) 
(S09 S17 S11 S01 S14 S02 S13 S10 S03) 
(S19 S11 S04 S13 S10 S02) 
(S19 S11 S09 S13 S10 S03) 
(S08 S14 S01 S10 S09) 
(S09 S14 S01 S10 S01 S00) 
(S09 S14 S02 S10 S01 S01) 
(S01 S00 S16 S01 S00 S10 S01 S00 S00) 
(S00 S20 S01 S10 S01 S17 S01 S00) 
(S00 S20 S00 S01 S10 S01 S17 S01 S00 S00) 
(S03 S20 S01 S10 S03 S14 S01 S17 S01 S00) 
(S01 S17 S03 S21 S00 S20 S03 S03 S03) 
(S22 S21 S03 S20 S01 S04 S01 S05 S09) 
(S23 S11 S24 S13 S17 S11 S23 S11 S25 S13 S13 S21 S01 S08 S03 S06) 
(S26 S21 S01 S17 S01 S03 S07) 
(S26 S10 S11 S27 S18 S02 S13 S17 S11 S04 S16 S22 S16 S28 S16 S29 S16 S30 S13) 
(S31 S11 S32 S13 S10 S19 S11 S11 S29 S16 S33 S13 S17 S11 S30 S18 S03 S13 S13) 
(S31 S11 S23 S13 S10 S19 S11 S11 S29 S16 S30 S13 S17 S33 S13) 
)))
)

(reset-code)
