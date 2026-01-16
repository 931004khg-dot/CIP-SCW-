;; Test boundary detection for TSP
;; Test file: test-boundary-detection.lsp
;; Purpose: Verify is-closed-polyline detection and boundary orientation
;; Created: 2026-01-16
;; Version: 1.1.0

(defun c:test-boundary ( / test-ent ent-data closed-flag vertices orientation centroid)
  (princ "\n테스트: 경계선 선택...")
  (setq test-ent (car (entsel "\n경계선 폴리라인 선택: ")))
  
  (if test-ent
    (progn
      (princ "\n\n=== 경계선 분석 ===")
      
      ;; 1. 엔티티 데이터 읽기
      (setq ent-data (entget test-ent))
      (princ (strcat "\n엔티티 타입: " (cdr (assoc 0 ent-data))))
      
      ;; 2. 폐합 여부 확인 (DXF 코드 70)
      (setq closed-flag (cdr (assoc 70 ent-data)))
      (princ (strcat "\nDXF 70 코드 값: " (if closed-flag (itoa closed-flag) "없음")))
      
      (if closed-flag
        (progn
          (princ (strcat "\nBit 0 (폐합): " (if (= 1 (logand 1 closed-flag)) "1 (폐합)" "0 (열림)")))
          (princ (strcat "\nBit 1 (Plinegen): " (if (= 2 (logand 2 closed-flag)) "1" "0")))
        )
        (princ "\n폐합 플래그 없음")
      )
      
      ;; 3. 폐합 판별 함수 테스트
      (if (is-closed-polyline test-ent)
        (princ "\n\n결과: 폐합 다각형 (Closed)")
        (princ "\n\n결과: 열린 경계선 (Open)")
      )
      
      ;; 4. 꼭지점 추출
      (setq vertices (extract-vertices test-ent))
      (princ (strcat "\n꼭지점 개수: " (itoa (length vertices))))
      
      ;; 5. 방향 판단 (폐합선인 경우)
      (if (>= (length vertices) 3)
        (progn
          (setq orientation (get-polygon-orientation vertices))
          (princ (strcat "\n다각형 방향: " (if (= orientation 1) "CCW (반시계)" "CW (시계)")))
          
          ;; 6. 중심점 계산 및 내부 확인
          (setq centroid (get-polygon-centroid vertices))
          (princ (strcat "\n중심점: (" 
                         (rtos (car centroid) 2 2) ", " 
                         (rtos (cadr centroid) 2 2) ")"))
          
          (if (point-inside-polygon centroid vertices)
            (princ "\n중심점 위치: 다각형 내부 ✓")
            (princ "\n중심점 위치: 다각형 외부 ✗")
          )
          
          ;; 7. 폐합선인 경우 노란색 원 생성
          (if (is-closed-polyline test-ent)
            (progn
              (princ "\n\n[시각화] 노란색 원 생성 중...")
              (create-interior-circle test-ent vertices)
            )
          )
        )
        (princ "\n방향 판단 불가: 꼭지점 부족")
      )
      
      (princ "\n\n=== 테스트 완료 ===")
    )
    (princ "\n경계선 선택 취소됨")
  )
  (princ)
)

;; 필요한 유틸리티 함수들 (TSP-debug.lsp에서 복사)

;; 폴리라인 폐합 여부 판별 (개선된 버전)
(defun is-closed-polyline (ent / ent-data closed-flag vertices first-pt last-pt)
  (setq ent-data (entget ent))
  
  ;; 방법 1: DXF 코드 70의 bit 0 확인
  (setq closed-flag (cdr (assoc 70 ent-data)))
  (if (and closed-flag (= 1 (logand 1 closed-flag)))
    T  ; DXF 플래그로 폐합 확인됨
    ;; 방법 2: 첫/마지막 꼭지점 비교
    (progn
      (setq vertices '())
      (foreach item ent-data
        (if (= (car item) 10)
          (setq vertices (append vertices (list (cdr item))))
        )
      )
      (if (>= (length vertices) 2)
        (progn
          (setq first-pt (car vertices))
          (setq last-pt (last vertices))
          ;; 0.1mm 허용 오차로 일치 여부 확인
          (equal first-pt last-pt 0.1)
        )
        nil  ; 꼭지점 부족
      )
    )
  )
)

(defun extract-vertices (ent / ent-data vertices item first-pt last-pt)
  (setq ent-data (entget ent))
  (setq vertices '())
  
  (foreach item ent-data
    (if (= (car item) 10)
      (setq vertices (append vertices (list (cdr item))))
    )
  )
  
  ;; 닫힌 폴리라인: 첫 번째와 마지막이 같으면 마지막 제거
  (if (and (> (length vertices) 1)
           (setq first-pt (car vertices))
           (setq last-pt (last vertices))
           (equal first-pt last-pt 0.01))
    (setq vertices (reverse (cdr (reverse vertices))))
  )
  
  vertices
)

(defun get-polygon-orientation (vertices / signed-area i n pt1 pt2)
  (setq signed-area 0.0)
  (setq n (length vertices))
  
  ;; Shoelace 공식 변형: Σ(xi - xi+1)(yi + yi+1)
  (setq i 0)
  (while (< i n)
    (setq pt1 (nth i vertices))
    (setq pt2 (nth (rem (+ i 1) n) vertices))
    (setq signed-area 
      (+ signed-area 
         (* (- (car pt1) (car pt2))
            (+ (cadr pt1) (cadr pt2))
         )
      )
    )
    (setq i (+ i 1))
  )
  
  (princ (strcat "\nShoelace signed-area: " (rtos signed-area 2 2)))
  
  ;; 양수 = CCW, 음수 = CW
  (if (> signed-area 0)
    1    ; CCW
    -1   ; CW
  )
)

;; 다각형의 중심점(Centroid) 계산
(defun get-polygon-centroid (vertices / n i pt sum-x sum-y cx cy)
  (setq n (length vertices))
  (setq sum-x 0.0)
  (setq sum-y 0.0)
  
  (setq i 0)
  (while (< i n)
    (setq pt (nth i vertices))
    (setq sum-x (+ sum-x (car pt)))
    (setq sum-y (+ sum-y (cadr pt)))
    (setq i (+ i 1))
  )
  
  (setq cx (/ sum-x n))
  (setq cy (/ sum-y n))
  
  (list cx cy)
)

;; 점이 다각형 내부에 있는지 확인 (Ray Casting Algorithm)
(defun point-inside-polygon (pt vertices / n i j inside x y xi yi xj yj intersect)
  (setq n (length vertices))
  (setq inside nil)
  (setq x (car pt))
  (setq y (cadr pt))
  
  (setq i 0)
  (setq j (- n 1))
  
  (while (< i n)
    (setq xi (car (nth i vertices)))
    (setq yi (cadr (nth i vertices)))
    (setq xj (car (nth j vertices)))
    (setq yj (cadr (nth j vertices)))
    
    ;; Ray casting: 점에서 오른쪽으로 수평선을 그었을 때 교차 여부
    (setq intersect 
      (and 
        (or (and (> yi y) (<= yj y))
            (and (> yj y) (<= yi y)))
        (< x (+ (* (/ (- xj xi) (- yj yi)) (- y yi)) xi))
      )
    )
    
    (if intersect
      (setq inside (not inside))
    )
    
    (setq j i)
    (setq i (+ i 1))
  )
  
  inside
)

;; 폐합 경계선 내부에 노란색 원 생성 (시각화)
(defun create-interior-circle (boundary-ent vertices / centroid circle-ent)
  (setq centroid (get-polygon-centroid vertices))
  
  (princ (strcat "\n[시각화] 경계선 중심점: (" 
                 (rtos (car centroid) 2 2) ", " 
                 (rtos (cadr centroid) 2 2) ")"))
  
  ;; 중심점이 내부에 있는지 확인
  (if (point-inside-polygon centroid vertices)
    (progn
      (princ "\n[시각화] 중심점이 내부에 있음 - 노란색 원 생성")
      
      ;; 노란색 원 생성 (D=2000, R=1000)
      (entmake
        (list
          '(0 . "CIRCLE")
          '(8 . "0")              ; 레이어 0
          (cons 62 2)             ; 색상: 노란색(2)
          (cons 10 centroid)      ; 중심점
          '(40 . 1000.0)          ; 반지름 1000mm (D=2000)
        )
      )
      
      (setq circle-ent (entlast))
      (princ "\n노란색 원 생성 완료!")
      circle-ent
    )
    (progn
      (princ "\n[경고] 중심점이 외부에 있음 - 원 생성 실패")
      nil
    )
  )
)

(princ "\n테스트 명령어 로드됨: TEST-BOUNDARY")
(princ "\n사용법: 명령행에 TEST-BOUNDARY 입력 후 폴리라인 선택")
(princ)
