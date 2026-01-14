;; 20-point H-Pile section fix
(defun create-hpile-section (insert-pt h b tw tf layer-name / half-h half-b half-tw half-tf cx cy pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 pt9 pt10 pt11 pt12 pt13 pt14 pt15 pt16 pt17 pt18 pline-ent fillet-r)
  (setq half-h (/ h 2.0))
  (setq half-b (/ b 2.0))
  (setq half-tw (/ tw 2.0))
  (setq half-tf (/ tf 2.0))
  (setq fillet-r (* tw 2.0))
  
  (setq cx (car insert-pt))
  (setq cy (cadr insert-pt))
  
  ;; 20-point I-beam (CCW from top-right)
  (setq pt1  (list (+ cx half-b) (+ cy half-h)))                            ; 1. TR outer
  (setq pt2  (list (+ cx half-b) (+ cy (- half-h half-tf))))                ; 2. TR inner
  (setq pt3  (list (+ cx (+ half-tw fillet-r)) (+ cy (- half-h half-tf))))  ; 3. Fillet start (TR)
  (setq pt4  (list (+ cx half-tw) (+ cy (- half-h half-tf fillet-r))))      ; 4. Fillet end (TR)
  (setq pt5  (list (+ cx half-tw) (- cy (- half-h half-tf fillet-r))))      ; 5. Web right bottom
  (setq pt6  (list (+ cx half-tw) (- cy (- half-h half-tf fillet-r))))      ; 6. Fillet start (BR)
  (setq pt7  (list (+ cx (+ half-tw fillet-r)) (- cy (- half-h half-tf))))  ; 7. Fillet end (BR)
  (setq pt8  (list (+ cx half-b) (- cy (- half-h half-tf))))                ; 8. BR inner
  (setq pt9  (list (+ cx half-b) (- cy half-h)))                            ; 9. BR outer
  (setq pt10 (list (- cx half-b) (- cy half-h)))                            ; 10. BL outer
  (setq pt11 (list (- cx half-b) (- cy (- half-h half-tf))))                ; 11. BL inner
  (setq pt12 (list (- cx (+ half-tw fillet-r)) (- cy (- half-h half-tf))))  ; 12. Fillet start (BL)
  (setq pt13 (list (- cx half-tw) (- cy (- half-h half-tf fillet-r))))      ; 13. Fillet end (BL)
  (setq pt14 (list (- cx half-tw) (+ cy (- half-h half-tf fillet-r))))      ; 14. Web left top
  (setq pt15 (list (- cx half-tw) (+ cy (- half-h half-tf fillet-r))))      ; 15. Fillet start (TL)
  (setq pt16 (list (- cx (+ half-tw fillet-r)) (+ cy (- half-h half-tf))))  ; 16. Fillet end (TL)
  (setq pt17 (list (- cx half-b) (+ cy (- half-h half-tf))))                ; 17. TL inner
  (setq pt18 (list (- cx half-b) (+ cy half-h)))                            ; 18. TL outer
  
  (entmake
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbPolyline")
      (cons 8 layer-name)
      (cons 62 3)
      '(90 . 18)
      '(70 . 1)
      (cons 10 pt1)
      (cons 10 pt2)
      (cons 10 pt3)
      (cons 42 0.4142135623730951)
      (cons 10 pt4)
      (cons 10 pt5)
      (cons 10 pt6)
      (cons 42 0.4142135623730951)
      (cons 10 pt7)
      (cons 10 pt8)
      (cons 10 pt9)
      (cons 10 pt10)
      (cons 10 pt11)
      (cons 10 pt12)
      (cons 42 0.4142135623730951)
      (cons 10 pt13)
      (cons 10 pt14)
      (cons 10 pt15)
      (cons 42 0.4142135623730951)
      (cons 10 pt16)
      (cons 10 pt17)
      (cons 10 pt18)
    )
  )
  
  (entlast)
)

(princ "\nH-Pile 20-point fix loaded. Use (create-hpile-section ...) to test.\n")
(princ)
