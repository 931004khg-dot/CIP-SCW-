;;; Test coordinate calculation for H-Pile
(defun test-hpile-coords (/ h b tw tf cx cy half-h half-b half-tw half-tf pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 pt9 pt10 pt11 pt12)
  ;; Test values from H 298×201×9/14
  (setq h 298)
  (setq b 201)
  (setq tw 9)
  (setq tf 14)
  
  ;; Center point
  (setq cx 31106.58)
  (setq cy 57579.48)
  
  ;; Calculate half values
  (setq half-h (/ h 2.0))
  (setq half-b (/ b 2.0))
  (setq half-tw (/ tw 2.0))
  (setq half-tf (/ tf 2.0))
  
  (princ (strcat "\nTest H-Pile Coordinate Calculation"))
  (princ (strcat "\n=================================="))
  (princ (strcat "\nH=" (rtos h 2 0) " B=" (rtos b 2 0) " tw=" (rtos tw 2 0) " tf=" (rtos tf 2 0)))
  (princ (strcat "\nCenter: (" (rtos cx 2 2) ", " (rtos cy 2 2) ")"))
  (princ (strcat "\nhalf-h=" (rtos half-h 2 1) " half-b=" (rtos half-b 2 1) " half-tw=" (rtos half-tw 2 1) " half-tf=" (rtos half-tf 2 1)))
  
  ;; Calculate 12 points
  (setq pt1  (list (- cx half-b) (+ cy half-h)))
  (setq pt2  (list (+ cx half-b) (+ cy half-h)))
  (setq pt3  (list (+ cx half-b) (+ cy (- half-h half-tf))))
  (setq pt4  (list (+ cx half-tw) (+ cy (- half-h half-tf))))
  (setq pt5  (list (+ cx half-tw) (- cy (- half-h half-tf))))
  (setq pt6  (list (+ cx half-b) (- cy (- half-h half-tf))))
  (setq pt7  (list (+ cx half-b) (- cy half-h)))
  (setq pt8  (list (- cx half-b) (- cy half-h)))
  (setq pt9  (list (- cx half-b) (- cy (- half-h half-tf))))
  (setq pt10 (list (- cx half-tw) (- cy (- half-h half-tf))))
  (setq pt11 (list (- cx half-tw) (+ cy (- half-h half-tf))))
  (setq pt12 (list (- cx half-b) (+ cy (- half-h half-tf))))
  
  ;; Print all points
  (princ "\n\nCalculated Points:")
  (princ (strcat "\npt1  (top-left):         " (rtos (car pt1) 2 2) ", " (rtos (cadr pt1) 2 2)))
  (princ (strcat "\npt2  (top-right):        " (rtos (car pt2) 2 2) ", " (rtos (cadr pt2) 2 2)))
  (princ (strcat "\npt3  (top-right inner):  " (rtos (car pt3) 2 2) ", " (rtos (cadr pt3) 2 2)))
  (princ (strcat "\npt4  (web top-right):    " (rtos (car pt4) 2 2) ", " (rtos (cadr pt4) 2 2)))
  (princ (strcat "\npt5  (web bot-right):    " (rtos (car pt5) 2 2) ", " (rtos (cadr pt5) 2 2)))
  (princ (strcat "\npt6  (bot-right inner):  " (rtos (car pt6) 2 2) ", " (rtos (cadr pt6) 2 2)))
  (princ (strcat "\npt7  (bot-right):        " (rtos (car pt7) 2 2) ", " (rtos (cadr pt7) 2 2)))
  (princ (strcat "\npt8  (bot-left):         " (rtos (car pt8) 2 2) ", " (rtos (cadr pt8) 2 2)))
  (princ (strcat "\npt9  (bot-left inner):   " (rtos (car pt9) 2 2) ", " (rtos (cadr pt9) 2 2)))
  (princ (strcat "\npt10 (web bot-left):     " (rtos (car pt10) 2 2) ", " (rtos (cadr pt10) 2 2)))
  (princ (strcat "\npt11 (web top-left):     " (rtos (car pt11) 2 2) ", " (rtos (cadr pt11) 2 2)))
  (princ (strcat "\npt12 (top-left inner):   " (rtos (car pt12) 2 2) ", " (rtos (cadr pt12) 2 2)))
  
  ;; Verify points are different
  (princ "\n\nVerification:")
  (if (equal pt1 pt2) (princ "\nERROR: pt1 = pt2") (princ "\nOK: pt1 ≠ pt2"))
  (if (equal pt1 pt8) (princ "\nERROR: pt1 = pt8") (princ "\nOK: pt1 ≠ pt8"))
  (if (equal pt2 pt7) (princ "\nERROR: pt2 = pt7") (princ "\nOK: pt2 ≠ pt7"))
  
  ;; Calculate expected dimensions
  (princ "\n\nExpected Dimensions:")
  (princ (strcat "\nWidth (pt1 to pt2):  " (rtos (distance pt1 pt2) 2 2) " mm (expected: " (rtos b 2 0) ")"))
  (princ (strcat "\nHeight (pt1 to pt8): " (rtos (distance pt1 pt8) 2 2) " mm (expected: " (rtos h 2 0) ")"))
  
  (princ "\n\nTest Complete!\n")
  (princ)
)

(test-hpile-coords)
