;; Test boundary detection for TSP
;; Test file: test-boundary-detection.lsp
;; Purpose: Verify is-closed-polyline detection and boundary orientation
;; Created: 2026-01-16
;; Version: 1.0.0

(defun c:test-boundary ( / test-ent ent-data closed-flag vertices orientation)
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

(defun is-closed-polyline (ent / ent-data closed-flag)
  (setq ent-data (entget ent))
  (setq closed-flag (cdr (assoc 70 ent-data)))
  (if closed-flag
    (= 1 (logand 1 closed-flag))
    nil
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

(princ "\n테스트 명령어 로드됨: TEST-BOUNDARY")
(princ "\n사용법: 명령행에 TEST-BOUNDARY 입력 후 폴리라인 선택")
(princ)
