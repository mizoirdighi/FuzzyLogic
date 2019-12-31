;code 6-1
; (fl-belongs? fset x)
; returns true if x belongs to fset, else returns nil
;
(define (fl-belongs? fset x)
    (if (and (>= x (nth 1 fset)) (<= x (nth 4 fset)))
        true
        nil
    )
)


;code 6-2
;the function (fl-set-membership?) returns the membership degree
;of an element x to a fuzzy set
(define (fl-set-membership? fset x, name x1 x2 x3 x4 membership-degree)
 
   (setq name (nth 0 fset))   ;fuzzy set name
   (setq x1 (nth 1 fset))    ;support starts
   (setq x2 (nth 2 fset))    ;nucleus starts
   (setq x3 (nth 3 fset))     ;nucleus finishes
   (setq x4 (nth 4 fset))    ;support ends

   ;x <= x1 | x >= x4 => membership-degree = 0.0
   (if (or (<= x x1) (>= x x4))
    (setq membership-degree 0.0)
   )

   ;x >x1 && x < x2 => membership-degree increasing
   (if (and (> x x1) (< x x2))
    (setq membership-degree (div (mul (sub x x1) 1.0) (sub x2 x1)))
   )

   ;nucleus membership degree is always 1.0
   (if (and (>= x x2) (<= x x3))
    (setq membership-degree 1.0)
   )

   ;x >x3 && x <x4 => membership-degree decreasing
   (if (and (> x x3) (< x x4))
    (setq membership-degree (div (mul (sub x4 x) 1.0) (sub x4 x3)))
   )

   (list name membership-degree) ;gives results as a list
)


;Code 6-3: Merging the predicate (fl-belongs?) with
;(fl-set-membership?)
(define (fl-belongs2? fset x)
   (if (and (>= x (nth 1 fset)) (<= x (nth 4 fset)))
    (fl-set-membership? fset x) ;return the membership degree if any
    nil ;else returns nil
    )
)


;code 6-4: A function for calculating alpha-cuts
(define (fl-alpha-cut fset alpha, name x1 x2 x3 x4 extrem_left extrem_right tan_phi1 tan_phi2 fraction numerator)

   (setq name (nth 0 fset)) ;fuzzy set name
   (setq x1 (nth 1 fset))  ;support starts
   (setq x2 (nth 2 fset))  ;nucleus starts
   (setq x3 (nth 3 fset))  ; nucleus finishes
   (setq x4 (nth 4 fset))  ;support ends

   ;left extrem of alpha-cut begins, vertical mf case:
   (if (= x1 x2)
    (setq extrem_left x1)
   )

   ;left extrem of alpha-cut begins, ascending mf case:
   (if (!= x1 x2)
    (begin
       (setq tan_phi1 (div 1.0 (sub x2 x1)))
       (setq fraction (mul tan_phi1 x2))
       (setq numerator (sub (add fraction alpha) 1.0))
       (setq extrem_left (div numerator tan_phi1))
     )
   )

   ;right extrem of alpha-cut begins, vertical mf case:
   (if (= x3 x4)
    (setq extrem_right x4)
   )

   ;right extrem of alpha-cut begins, descending mf case:
   (if (!= x3 x4)
      (begin
       (setq tan_phi2 (div 1.0 (sub x4 x3)))
       (setq fraction (mul tan_phi2 x3))
         (setq numerator (sub (add fraction 1.0) alpha))
       (setq extrem_right (div numerator tan_phi2))
      )
   )
   ;returns the alpha-cut as a list
   (list name extrem_left extrem_right)
)


;Code 6-5a
;(fl-def-set) generates a fuzzy set by means of two alpha-cuts
;it automatically recognizes is the resulting fuzzy set is
;represented by a triangular or trapezoidal fuzzy set.
;parameters:
;name: the name of the resulting fuzzy set
;a-cut1: a-cut interval and its alpha1 value, expressed as a list
;a-cut2: The same as a-cut1, with is alpha2 value
 
(define (fl-def-set name a-cut1 a-cut2,
triangle x1-a x1-b x2-a x2-b alpha-1 alpha-2 m1 m2
base-1 base-2 base-3 base-4 temp-x temp-y)

   ;initially we assume it is not a triangular mf:
   (setq triangle nil)
   (setq x1-a (nth 0 a-cut1))
   (setq x1-b (nth 1 a-cut1))
   (setq alpha-1 (nth 2 a-cut1))
   (setq x2-a (nth 0 a-cut2))
   (setq x2-b (nth 1 a-cut2))
   (setq alpha-2 (nth 2 a-cut2))
    
   (if (< (abs (sub x1-a x2-a)) 0.000001) ;in practice: x1-a = x2-a
      (setq m1 1e+300) ;slope’s tangent equals to infinity
      (setq m1 (fl-aux-calculate-m x1-a x2-a alpha-1 alpha-2))
   )

   (if (< (abs (sub x1-b x2-b)) 0.000001) ;in practice: x1-b = x2-b
      (setq m2 1e+300)
      (setq m2 (fl-aux-calculate-m x1-b x2-b alpha-1 alpha-2))
   )

   ;calculation of X axis intersections:
   ;base-1 and base_4 are the extremes of the set's support

   (setq base-1 (sub x1-a (div alpha-1 m1)))
   (if (< m2 0.0)
      (setq m2 (mul m2 -1.0)) ;absolute value
   )
   (setq base-4 (add (div alpha-1 m2) x1-b))

   ;base-2 and base-3 represent the extremes of the set's nucleus

   (setq base-2 (div (add 1 (mul m1 base-1)) m1))
   (setq base-3 (div (sub (mul m2 base-4) 1.0) m2))

   ;check if the set will have a triangular membership function
   (if (>= base-2 base-3)
   (begin
      (setq triangle true)
      (setq temp-x
(div (add (mul m1 base-1) (mul m2 base-4)) (add m1 m2)))
      (setq temp-y (mul m1 (sub temp-x base-1)))
   );end begin
   );end if
  
   ;and finally:

   (if (= triangle true)
      (list name base-1 temp-x temp-x base-4) ;it's a triangle
      (list name base-1 base-2 base-3 base-4) ;it's a trapezium
   );enf if
);end function


;Code 6-5b
;(fl-aux-calculate-m) calculates the slope of a line given
;two points p1(x1,y1) and p2(x2,y2)
(define (fl-aux-calculate-m x1 x2 y1 y2)
        (div (sub y2 y1) (sub x2 x1))
)


;Code 6-6
;(fl-discretize) takes a fuzzy set with triangular or trapezoidal
;characteristic function and discretizes it
;call example: (fl-discretize '(B1 7 10 12 15) 4)
(define (fl-discretize fset steps, name x1 x2 x3 x4
                              i resolution list-out x trapezium)
   (setq name (nth 0 fset))
   (setq x1 (nth 1 fset) x2 (nth 2 fset)
      x3 (nth 3 fset) x4 (nth 4 fset))
   (setq list-out (list name)); the first element is
                              ;the associated name
   (setq trapezium true)

   ;discretize from x1 to x2:
   (setq resolution (div (sub x2 x1) steps))
   (setq i 0)
   (setq x x1)
   (while (< i steps)
      (setq list-out
         (cons (list x (last (fl-set-membership? fset x))) list-out))
      (setq x (add x resolution))
      (++ i)
   ); end while

   ;discretize from x2 to x3
   (if (< (sub x3 x2) 0.0000001) ;testing if fset is a triangle
   (begin
      (setq list-out
         (cons (list x2 1.0) list-out)) ;if it is only one point
      (setq trapezium nil)
   )
   ;else it is a trapezium:
   (begin
      (setq resolution (div (sub x3 x2) steps))
      (setq i 0)
      (setq x x2)
      (while (< i steps)
         (setq list-out (cons (list x 1.0) list-out))
         (setq x (add x resolution))
         (++ i)
      );end while
   );end begin
   );end if

   ;finally, discretize from x3 to x4:
   (setq resolution (div (sub x4 x3) steps))
   (setq i 0)
   (if (= trapezium true)
      (setq x x3)
      (setq x (add x3 resolution)) ;it's a triangle
   );if end
   (while (< i steps)
      (setq list-out
         (cons (list x (last (fl-set-membership? fset x))) list-out))
      (setq x (add x resolution))
      (++ i)
   ); end while

   ;add the last element corresponding to x4 if fset is a trapezium:
   (if (= trapezium true)
      (setq list-out (cons (list x 0.0) list-out))
   )
   (reverse list-out)
)


;Code 6-7a
;(fl-dset-membership? dfset x) returns the interpolated membership
;degree of x from a fuzzy set with discrete characteristic function
(define (fl-dset-membership? dfset x, i n pair-a pair-b result)
   (setq result (list (first dfset) 0.0))
   (setq i 1) ;we are not interested in dfset's name anymore
   (setq n (length dfset))

   (while (< i (- n 1)) ;traverse list taking care at the end
    (setq pair-a (nth i dfset))
    (setq pair-b (nth (+ 1 i) dfset))

    ;if x is bounded:
    (if (and (<= (first pair-a) x) (>= (first pair-b) x))
      ;we pass the shift at right from the left value:
      (setq result (list (first dfset)
        (interpolation pair-a pair-b (sub x (first pair-a)))))
     ); end if
    (++ i)
   ); end while
   result
)


;code 6-7b
(define (interpolation pair-a pair-b p, a b y1 y2)
   ;extract values:
   (setq a (first pair-a) b (first pair-b))
   (setq y1 (last pair-a) y2 (last pair-b))
   ;calculate the interpolation using simple analytic geometry
   (add y1 (div (mul p (sub y2 y1))    (sub b a)))
)


;Code 6-8
;this function discretizes a continuous function y = f(x) between
;two points x = a and x = b, for a resolution given by the
;parameter steps. Two examples:
; (fl-discretize-fx 'A-name '(mul x x) 10 0 1)
;a bell-shaped membership function , Fig. 6-2, mu4:
;remember first to do (setq pi 3.141592)
; (fl-discretize-fx 'Bell '(div (add 1.0 (cos (mul 2.0 pi (sub x 2.0)))) 2.0) 10 1.5 2.5)

(define (fl-discretize-fx name fx steps a b, x resolution list-out)
   ; the first element is the associated name
   (setq list-out (list name))
   (setq resolution (div (sub b a) steps))
   (setq x a) ;start from left ro right
   (while (<= x (add b 0.00001))
      ;eval fx, make a list (x fx) and cons it to list-out
      (setq list-out (cons (list x (eval fx)) list-out))
      (setq x (add x resolution))
   ); while end
   (reverse list-out)
)


;code 6-9
;(fl-set-complement-membership?) returns the membership degree
;of x from the complementary of a fuzzy set, fset
(define (fl-set-complement-membership? fset x)
  (list
      (first (fl-set-membership? fset x))
      (sub 1.0 (last (fl-set-membership? fset x)))
  )
)


;code 6-10
;(fl-set-union-membership?) returns the membership degree of x from
;the union of two fuzzy sets fset1, fset2
;example call:
;(setq A '(Triangle 0 5 5 10))
;(setq B '(Trapezium 5 10 15 20))
;(fl-set-union-membership? 'AuB A B 7.5)

(define (fl-set-union-membership? name fset1 fset2 x, mu1 mu2)
  (setq mu1 (last (fl-set-membership? fset1 x)))
  (setq mu2 (last (fl-set-membership? fset2 x)))
  (list name (max mu1 mu2))
)


;code 6-11
;(fl-set-intersect-membership?) returns the membership degree of x from
;the intersection of two fuzzy sets fset1, fset2
(define (fl-set-intersect-membership? name fset1 fset2 x, mu1 mu2)
   (setq mu1 (last (fl-set-membership? fset1 x)))
   (setq mu2 (last (fl-set-membership? fset2 x)))
   (list name (min mu1 mu2))
)


;code 6-12a
;(fl-intv-add) this function adds two intervals
(define (fl-intv-add x1 x2 x3 x4)
   (list (add x1 x3) (add x2 x4))
)


;code 6-12b
;(fl-intv-sub) this function subtracts two intervals
(define (fl-intv-sub x1 x2 x3 x4)
   (list (sub x1 x4) (sub x2 x3))
)


;code 6-12c
;(fl-intv-mult) this function multiplies two intervals
(define (fl-intv-mult x1 x2 x3 x4, extrm-left extrm-right)
   (setq extrm-left (min
      (mul x1 x3) (mul x1 x4) (mul x2 x3) (mul x2 x4))
   )
   (setq extrm-right (max
      (mul x1 x3) (mul x1 x4) (mul x2 x3) (mul x2 x4))
   )
   (list extrm-left extrm-right)
)


;code 6-12d
;(fl-intv-div) this function divides two intervals
(define (fl-intv-div a b d e , extrm-left extrm-right)
   (setq extrm-left (min
      (div a d) (div a e) (div b d) (div b e))
   )
   (setq extrm-right (max
      (div a d) (div a e) (div b d) (div b e))
   )
   (list extrm-left extrm-right)
)


;code 6-13a
;(fl-fuzzy-add) returns the sum of two fuzzy numbers A B
(define (fl-fuzzy-add name A B, cut1A cut1B cut2A cut2B sum1 sum2)
   (setq cut1A (fl-alpha-cut A 0.25))
   (setq cut1B (fl-alpha-cut B 0.25))
   (setq cut2A (fl-alpha-cut A 0.75))
   (setq cut2B (fl-alpha-cut B 0.75))

   ;eliminate first element from every cut
   (pop cut1A) (pop cut1B)
   (pop cut2A) (pop cut2B)

   (setq sum1 (fl-intv-add
    (nth 0 cut1A) (nth 1 cut1A)
    (nth 0 cut1B) (nth 1 cut1B))
   )

   (setq sum2 (fl-intv-add
    (nth 0 cut2A) (nth 1 cut2A)
    (nth 0 cut2B) (nth 1 cut2B))
   )
        
   ;add alpha-cut value at the last position in the list

   (push '0.25 sum1 2)
   (push '0.75 sum2 2)
   (fl-def-set name sum1 sum2)
)


;code 6-13b
;(fl-fuzzy-sub) returns the subtraction of two fuzzy numbers A B
(define (fl-fuzzy-sub name A B , cut1A cut1B cut2A cut2B sum1 sum2)
   (setq cut1A (fl-alpha-cut A 0.25))
   (setq cut1B (fl-alpha-cut B 0.25))
   (setq cut2A (fl-alpha-cut A 0.75))
   (setq cut2B (fl-alpha-cut B 0.75))

   ;eliminate first element from every cut
   (pop cut1A) (pop cut1B)
   (pop cut2A) (pop cut2B)

   (setq sum1 (fl-intv-sub
    (nth 0 cut1A) (nth 1 cut1A)
    (nth 0 cut1B) (nth 1 cut1B))
   )

   (setq sum2 (fl-intv-sub
    (nth 0 cut2A) (nth 1 cut2A)
    (nth 0 cut2B) (nth 1 cut2B))
   )
        
   ;add alpha-cut value at the last position in the list

   (push '0.25 sum1 2)
   (push '0.75 sum2 2)
   (fl-def-set name sum1 sum2)
)


;code 6-14
;(fl-fuzzy-mult name A B n)
;this function returns the multiplication of two fuzzy numbers A B
(define (fl-fuzzy-mult name A B n,
i alpha cutA cutB mult head tail interval)
   (setq head '() tail '())
   (setq interval (div 1.0 n))
   (setq alpha 0.0 i 0)

   (while (<= i n)
      (setq cutA (rest (fl-alpha-cut A alpha)))
      (setq cutB (rest (fl-alpha-cut B alpha)))
      ;perform the multiplication of alpha-cuts (intervals):
      (setq mult
         (fl-intv-mult
            (first cutA) (last cutA) (first cutB) (last cutB)
         )
      );setq mult
        
      ;carefuly construct the head and tail of the
      ;discretized resulting set:
     (setq head (cons (append (list (first mult)) (list alpha)) head))
     (setq tail (cons (append (list (last mult)) (list alpha)) tail))

      (setq alpha (add interval alpha))
      (++ i)
   );while end
   (append (list name) (reverse head) (rest tail))
)


;Code 6-15
;(fl-fuzzy-div name A B n)
;this function returns the division of two fuzzy numbers A B
(define (fl-fuzzy-div name A B n,
 i alpha cutA cutB mult head tail interval)
   (setq head '() tail '())
   (setq interval (div 1.0 n))
   (setq alpha 0.0 i 0)

   (while (<= i n)
      (setq cutA (rest (fl-alpha-cut A alpha)))
      (setq cutB (rest (fl-alpha-cut B alpha)))
      ;perform the division of alpha-cuts (intervals):
      (setq mult
         (fl-intv-div
            (first cutA) (last cutA) (first cutB) (last cutB)
         )
      );setq mult
        
      ;carefuly construct the head and tail of the
      ;discretized resulting set:
     (setq head (cons (append (list (first mult)) (list alpha)) head))
     (setq tail (cons (append (list (last mult)) (list alpha)) tail))

      (setq alpha (add interval alpha))
      (++ i)
   );while end
   (append (list name) (reverse head) (rest tail))
)


;code 6-16
;(fl-fuzzy-factor fset k)
;this funtion takes a fuzzy set and then multiplies/divides
;it by a real number k
(define (fl-fuzzy-factor fset k, x1 x2 x3 x4 list-out)
   (setq x1 (mul k (nth 1 fset)))
   (setq x2 (mul k (nth 2 fset)))
   (setq x3 (mul k (nth 3 fset)))
   (setq x4 (mul k (nth 4 fset)))

   (if (>= x4 x1) ;normal case
      (setq list-out (list (nth 0 fset) x1 x2 x3 x4))
      ;else there is a negative number
      (setq list-out (list (nth 0 fset) x4 x2 x3 x1))
   ); if end
   list-out
)


;code 6-17
;(fl-fuzzy-shift fset x)
;this funtion shifts a fuzzy set towards left or right
;by a real number x
(define (fl-fuzzy-shift fset x)
    (list
        (nth 0 fset) ;the name of the set
        (add x (nth 1 fset))
        (add x (nth 2 fset))
        (add x (nth 3 fset))
        (add x (nth 4 fset))
    ); list end
)


;code 6-18
;(fl-expand-contract-set)
;this function expands/contracts a fuzzy set. The resulting
;fuzzy set is placed over its original position

(define (fl-expand-contract-set fset k, center1 center2 result)
   ;calculate support's centre:
   (setq center1 (div (add (nth 2 fset) (nth 3 fset)) 2.0))
   (setq result (fl-fuzzy-factor fset k)) ;expand/contract
   ;calculate new support's centre:
   (setq center2 (div (add (nth 2 result) (nth 3 result)) 2.0))
   ;shift it to its original position
   (fl-fuzzy-shift result (mul (sub center2 center1) -1.0))
)


;code 6-19
;(fl-fuzzy-add-sets)
;this function adds all the fuzzy sets contained into a set of
;fuzzy sets, fsets. It must  contain at least two fuzzy sets
(define (fl-fuzzy-add-sets fsets name, i n lst-out)
   (setq n (length fsets))
   (setq lst-out '())

   (setq lst-out
      (fl-fuzzy-add name (eval (nth 0 fsets)) (eval (nth 1 fsets))))
   (setq i 2); we have already added two fuzzy numbers
    ;now for the rest of fuzzy numbers:
   (while (< i n)
      (setq lst-out (fl-fuzzy-add name lst-out (eval (nth i fsets))))
      (++ i)
   ); while end
   lst-out
)


;code 6-20
;(fl-fuzzy-average fsets name)
;this function returns the fuzzy average of the fuzzy sets contained
;into a set of fuzzy sets
(define (fl-fuzzy-average fsets name)
   (fl-fuzzy-factor
      (fl-fuzzy-add-sets fsets name) (div 1.0 (length fsets))
   )
)


;code 6-21
;(fl-simple-defuzzification)
;this function obtains a crisp value from a fuzzy number
;the parameter "mode" gives increasing weight to its nucleus average;

(define (fl-simple-defuzzification fset mode, m)
   ;first, get nucleus average:
   (setq m (div (add (nth 2 fset) (nth 3 fset)) 2.0))

   ;for every mode, we give different weight to m:
   (case mode
      (1 (div (add (nth 1 fset) m (nth 4 fset)) 3.0))
      (2 (div (add (nth 1 fset) (mul m 2.0) (nth 4 fset)) 4.0))
      (3 (div (add (nth 1 fset) (mul m 4.0) (nth 4 fset)) 6.0))
    (4 (div (add (nth 1 fset) (mul m 6.0) (nth 4 fset)) 8.0))
   )
)


;code 6-22
;(fl-list-sets)
;the following function lists the fuzzy sets that belong to a
;linguistic variable
    
(define (fl-list-sets lv, i n fset)
   (setq i 0)
   (setq n (length lv))
   (while (< i n)
      (setq fset (eval (nth i lv)))
      (print fset)
      (++ i)
      (print "\n")
    )
   (println)
)


;code 6-23
;(fl-lv-membership?) returns all the membership degrees of an
;element x to a linguistic variable
(define (fl-lv-membership? lv x , i n fset answer)
   (setq i 0)
   (setq n (length lv))
   (while (< i n)
      ;first we obtain every fuzzy set
      (setq fset (eval (nth i lv)))
      ;now we call the function that performs the calculations
      (setq answer (fl-set-membership? fset x))
      (println answer)
      (++ i)
   );end while
   (println)
)


;code 6-24
;(fl-lv-membership2?)
;returns the membership degree of an element x to a linguistic ;variable as a list
(define (fl-lv-membership2? lv x, list_out i n fset)
   (setq list_out '())
   (setq i 0)
   (setq n (length lv))
   (while (< i n)
      ;here we obtain a fuzzy set:
      (setq fset (eval (nth i lv)))
      ;and now it's time to perform calculations:
    (setq list_out (append list_out
            (list (fl-set-membership? fset x))))
      (++ i)
   );while end
   list_out
)


;code 6-25
;(fl-dlv-membership2?) works similar to (fl-lv-membership2?), but it
;works on linguistic variables whose fuzzy sets are discrete
(define (fl-dlv-membership2? lv x, list_out i n fset)
   (setq list_out '())
   (setq i 0)
   (setq n (length lv))
   (while (< i n)
      ;here we obtain a fuzzy set:
      (setq fset (eval (nth i lv)))
      ;and now it's time to perform calculations:
      (setq list_out (append list_out
            (list (fl-dset-membership? fset x))))
      (++ i)
   );while end
   list_out
)


;code 6-26
;(fl-db-new-field lst sharp-field fz-field fset mode)
;a function for adding fuzzy capabilities to CSV databases
(define (fl-db-new-field lst sharp-field fz-field fset mode,
                         i l position fz-value lst-out)
   (setq lst-out '())
   (setq l (length lst)) ;number of records in lst
   ;get the field’s position reading the database header:
   (setq position (find sharp-field (nth 0 lst)))
   ;in the following line we append the new field's name:
   (setq lst-out (cons (append (nth 0 lst) (list fz-field)) lst-out))

   ;then we copy the entire database, calculating every fuzzy value
   (setq i 1)
   (while (< i l)
      ;read the value from the sharp-field and fuzzify it:
    (case mode
        (1 (setq fz-value
            (last (fl-set-membership? fset
                     (float (nth position (nth i lst)))))
             ))
        (2 (setq fz-value
            (last (fl-dset-membership? fset
                     (float (nth position (nth i lst)))))
             ))
      );end case

    ;then put it on the fz-field (last position):
      (setq lst-out
         (cons (append (nth i lst) (list (string fz-value))) lst-out)
      )
      (++ i)
   );while end

   (reverse lst-out)
);end function

