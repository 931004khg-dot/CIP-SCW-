;;; ======================================================================
;;; retaining-utils.lsp
;;; 가시설 흙막이 벽체 공통 유틸리티 함수
;;; ======================================================================

;;; ----------------------------------------------------------------------
;;; 레이어 생성 및 설정 함수
;;; ----------------------------------------------------------------------

;; 레이어 생성 (레이어명, 색상, 선종류)
(defun create-layer (layer-name color linetype / )
  (if (not (tblsearch "LAYER" layer-name))
    (progn
      (command "._LAYER" "_M" layer-name "_C" color layer-name "_L" linetype layer-name "")
      (princ (strcat "\n레이어 생성: " layer-name))
    )
  )
  (setvar "CLAYER" layer-name)
)

;; 가시설 레이어 초기화
(defun init-retaining-layers (/ )
  (create-layer "PILE" "1" "Continuous")        ; 엄지말뚝/벽체 - Red
  (create-layer "WALE" "5" "Continuous")        ; 띠장 - Blue
  (create-layer "STRUT" "3" "Continuous")       ; 버팀보 - Green
  (create-layer "RETAINING-DIM" "2" "Continuous") ; 치수선 - Yellow
  (create-layer "RETAINING-TEXT" "7" "Continuous") ; 문자 - White
  (create-layer "EARTH-ANCHOR" "6" "Continuous") ; 앵커 - Magenta
  (princ "\n가시설 레이어 초기화 완료")
)

;;; ----------------------------------------------------------------------
;;; 기하학 계산 함수
;;; ----------------------------------------------------------------------

;; 두 점 사이의 거리
(defun dist-2p (p1 p2)
  (distance p1 p2)
)

;; 두 점 사이의 각도 (라디안)
(defun angle-2p (p1 p2)
  (angle p1 p2)
)

;; 점에서 각도와 거리로 새 점 계산
(defun polar-point (pt ang dist)
  (polar pt ang dist)
)

;; 두 점의 중점
(defun mid-point (p1 p2)
  (list
    (/ (+ (car p1) (car p2)) 2.0)
    (/ (+ (cadr p1) (cadr p2)) 2.0)
    0.0
  )
)

;; 선분의 수직 방향 각도
(defun perpendicular-angle (ang)
  (+ ang (/ pi 2.0))
)

;; 점을 오프셋 (점, 각도, 거리)
(defun offset-point (pt ang offset-dist)
  (polar pt ang offset-dist)
)

;;; ----------------------------------------------------------------------
;;; 선 및 폴리라인 처리 함수
;;; ----------------------------------------------------------------------

;; 폴리라인/라인의 좌표 리스트 추출
(defun get-line-points (ent / ent-data ent-type pt-list i pt)
  (setq ent-data (entget ent))
  (setq ent-type (cdr (assoc 0 ent-data)))
  
  (cond
    ;; LINE인 경우
    ((= ent-type "LINE")
     (setq pt-list (list
       (cdr (assoc 10 ent-data))
       (cdr (assoc 11 ent-data))
     ))
    )
    
    ;; LWPOLYLINE인 경우
    ((= ent-type "LWPOLYLINE")
     (setq i 0)
     (setq pt-list '())
     (foreach item ent-data
       (if (= (car item) 10)
         (setq pt-list (append pt-list (list (cdr item))))
       )
     )
    )
    
    ;; POLYLINE인 경우
    ((= ent-type "POLYLINE")
     (setq pt-list '())
     (setq ent (entnext ent))
     (while (not (equal (cdr (assoc 0 (entget ent))) "SEQEND"))
       (setq pt (cdr (assoc 10 (entget ent))))
       (setq pt-list (append pt-list (list pt)))
       (setq ent (entnext ent))
     )
    )
  )
  
  pt-list
)

;; 선분 리스트를 세그먼트로 분할
(defun divide-segments (pt-list / seg-list i)
  (setq seg-list '())
  (setq i 0)
  (while (< i (- (length pt-list) 1))
    (setq seg-list (append seg-list 
      (list (list (nth i pt-list) (nth (+ i 1) pt-list)))
    ))
    (setq i (+ i 1))
  )
  seg-list
)

;; 선분을 등간격으로 분할하는 점 리스트 생성
(defun divide-segment (p1 p2 spacing / total-dist num-divs div-list i ratio pt)
  (setq total-dist (distance p1 p2))
  (setq num-divs (fix (/ total-dist spacing)))
  (setq div-list (list p1))
  
  (setq i 1)
  (while (<= i num-divs)
    (setq ratio (/ (* i spacing) total-dist))
    (setq pt (list
      (+ (car p1) (* ratio (- (car p2) (car p1))))
      (+ (cadr p1) (* ratio (- (cadr p2) (cadr p1))))
      0.0
    ))
    (setq div-list (append div-list (list pt)))
    (setq i (+ i 1))
  )
  
  div-list
)

;;; ----------------------------------------------------------------------
;;; 도형 그리기 함수
;;; ----------------------------------------------------------------------

;; H-Pile 단면 그리기 (중심점, 각도, 크기)
(defun draw-hpile (center-pt ang size / half-size pt1 pt2 pt3 pt4 flange-w web-h flange-t web-t)
  ;; H-Pile 크기 파싱 (예: "H-300x300x10x15")
  ;; 간단화: 정사각형 H 형강으로 가정
  (setq half-size (/ size 2.0))
  (setq flange-w size)
  (setq web-h size)
  (setq flange-t (* size 0.05))
  (setq web-t (* size 0.04))
  
  ;; H 형강 외곽선 그리기 (단순화)
  (create-layer "PILE" "1" "Continuous")
  
  ;; 상부 플랜지
  (setq pt1 (polar center-pt (+ ang (* pi 0.5)) (* flange-w 0.5)))
  (setq pt2 (polar pt1 ang flange-t))
  (command "._LINE" pt1 pt2 "")
  
  ;; 웹
  (setq pt3 (polar center-pt (+ ang (* pi 0.5)) (* web-t 0.5)))
  (setq pt4 (polar center-pt (+ ang (* pi 1.5)) (* web-t 0.5)))
  (command "._LINE" pt3 pt4 "")
  
  ;; 하부 플랜지
  (setq pt1 (polar center-pt (+ ang (* pi 1.5)) (* flange-w 0.5)))
  (setq pt2 (polar pt1 ang flange-t))
  (command "._LINE" pt1 pt2 "")
)

;; CIP 원형 말뚝 그리기
(defun draw-cip-circle (center-pt diameter / radius)
  (create-layer "PILE" "1" "Continuous")
  (setq radius (/ diameter 2.0))
  (command "._CIRCLE" center-pt radius)
)

;; SCW 벽체 그리기 (시작점, 끝점, 두께)
(defun draw-scw-wall (p1 p2 thickness / ang offset-ang pt1 pt2 pt3 pt4)
  (create-layer "PILE" "1" "Continuous")
  (setq ang (angle p1 p2))
  (setq offset-ang (+ ang (/ pi 2.0)))
  
  (setq pt1 (polar p1 offset-ang (/ thickness 2.0)))
  (setq pt2 (polar p2 offset-ang (/ thickness 2.0)))
  (setq pt3 (polar p2 offset-ang (/ thickness -2.0)))
  (setq pt4 (polar p1 offset-ang (/ thickness -2.0)))
  
  (command "._PLINE" pt1 pt2 pt3 pt4 "_C")
)

;; 띠장 그리기 (시작점, 끝점, H-Beam 크기)
(defun draw-wale (p1 p2 size / )
  (create-layer "WALE" "5" "Continuous")
  (command "._LINE" p1 p2 "")
  
  ;; 중심점에 H-Beam 마크 표시
  (setq mid-pt (mid-point p1 p2))
  (command "._CIRCLE" mid-pt (/ size 4.0))
)

;; 버팀보 그리기 (시작점, 끝점, 단면 크기)
(defun draw-strut (p1 p2 size / )
  (create-layer "STRUT" "3" "Continuous")
  (command "._LINE" p1 p2 "")
  
  ;; 중심점에 마크 표시
  (setq mid-pt (mid-point p1 p2))
  (command "._CIRCLE" mid-pt (/ size 3.0))
)

;;; ----------------------------------------------------------------------
;;; 문자 및 치수 함수
;;; ----------------------------------------------------------------------

;; 문자 배치 (점, 문자열, 높이)
(defun place-text (pt text-str height / )
  (create-layer "RETAINING-TEXT" "7" "Continuous")
  (command "._TEXT" pt height "0" text-str)
)

;; 치수선 배치 (시작점, 끝점)
(defun place-dimension (p1 p2 / )
  (create-layer "RETAINING-DIM" "2" "Continuous")
  (command "._DIMLINEAR" p1 p2 "")
)

;;; ----------------------------------------------------------------------
;;; 사용자 입력 함수
;;; ----------------------------------------------------------------------

;; 실수 입력 (프롬프트, 기본값)
(defun get-real-input (prompt default / input)
  (setq input (getreal (strcat "\n" prompt " <" (rtos default 2 2) ">: ")))
  (if input input default)
)

;; 정수 입력 (프롬프트, 기본값)
(defun get-int-input (prompt default / input)
  (setq input (getint (strcat "\n" prompt " <" (itoa default) ">: ")))
  (if input input default)
)

;; 키워드 입력 (프롬프트, 기본값, 옵션 리스트)
(defun get-keyword-input (prompt default options / input)
  (initget options)
  (setq input (getkword (strcat "\n" prompt " [" options "] <" default ">: ")))
  (if input input default)
)

;;; ----------------------------------------------------------------------
;;; 초기화
;;; ----------------------------------------------------------------------

(princ "\n========================================")
(princ "\n가시설 유틸리티 함수 로드 완료")
(princ "\n========================================")
(princ)
