;;; ======================================================================
;;; hpile-retaining.lsp
;;; H-Pile + 토류판 흙막이 벽체 자동 작도
;;; ======================================================================

;; 유틸리티 함수 로드 확인
(if (not init-retaining-layers)
  (load "retaining-utils.lsp")
)

;;; ----------------------------------------------------------------------
;;; H-Pile + 토류판 메인 함수
;;; ----------------------------------------------------------------------

(defun C:HPILE-RT (/ boundary-ent pt-list seg-list 
                      pile-spacing pile-size wale-levels wale-size
                      strut-spacing strut-size pile-points
                      seg p1 p2 div-pts pt wale-level)
  
  (princ "\n========================================")
  (princ "\nH-Pile + 토류판 공법 자동 작도")
  (princ "\n========================================")
  
  ;; 레이어 초기화
  (init-retaining-layers)
  
  ;; 1. 경계선 선택
  (princ "\n\n[1단계] 지하벽체 경계선 선택")
  (setq boundary-ent (car (entsel "\n경계선 선택 (Polyline 또는 Line): ")))
  
  (if (not boundary-ent)
    (progn
      (princ "\n경계선 선택 취소됨")
      (exit)
    )
  )
  
  ;; 경계선 좌표 추출
  (setq pt-list (get-line-points boundary-ent))
  (princ (strcat "\n좌표 개수: " (itoa (length pt-list))))
  
  ;; 세그먼트 분할
  (setq seg-list (divide-segments pt-list))
  (princ (strcat "\n세그먼트 개수: " (itoa (length seg-list))))
  
  ;; 2. 설계 파라미터 입력
  (princ "\n\n[2단계] H-Pile 설계 파라미터 입력")
  (setq pile-spacing (get-real-input "엄지말뚝(H-Pile) 간격(m)" 1.8))
  (setq pile-spacing (* pile-spacing 1000.0)) ;; m를 mm로 변환
  
  (setq pile-size (get-real-input "H-Pile 크기(mm)" 300.0))
  
  (princ "\n\n[3단계] 띠장(Wale) 설계 파라미터")
  (setq wale-levels (get-int-input "띠장 단수" 3))
  (setq wale-size (get-real-input "띠장 H-Beam 크기(mm)" 400.0))
  
  (princ "\n\n[4단계] 버팀보(Strut) 설계 파라미터")
  (setq strut-spacing (get-real-input "버팀보 간격(m)" 6.0))
  (setq strut-spacing (* strut-spacing 1000.0)) ;; m를 mm로 변환
  (setq strut-size (get-real-input "버팀보 H-Beam 크기(mm)" 500.0))
  
  ;; 3. H-Pile 배치
  (princ "\n\n[실행] H-Pile 배치 중...")
  (setq pile-points '())
  
  (foreach seg seg-list
    (setq p1 (car seg))
    (setq p2 (cadr seg))
    
    ;; 세그먼트를 등간격으로 분할
    (setq div-pts (divide-segment p1 p2 pile-spacing))
    
    ;; 각 분할점에 H-Pile 배치
    (foreach pt div-pts
      (setq pile-points (append pile-points (list pt)))
      ;; H-Pile 그리기
      (setq ang (angle p1 p2))
      (draw-hpile pt ang pile-size)
      
      ;; H-Pile 마커 (원으로 표시)
      (create-layer "PILE" "1" "Continuous")
      (command "._CIRCLE" pt (/ pile-size 2.0))
    )
  )
  
  (princ (strcat "\nH-Pile 배치 완료: " (itoa (length pile-points)) "개소"))
  
  ;; 4. 토류판 표시 (각 세그먼트에 해칭 또는 라인)
  (princ "\n토류판 표시 중...")
  (create-layer "PILE" "1" "Continuous")
  
  (foreach seg seg-list
    (setq p1 (car seg))
    (setq p2 (cadr seg))
    (setq ang (angle p1 p2))
    (setq perp-ang (perpendicular-angle ang))
    
    ;; 토류판 두께 (간단히 이중선으로 표시)
    (setq offset-dist 150.0) ;; 150mm 두께 가정
    (setq p1-offset (polar p1 perp-ang offset-dist))
    (setq p2-offset (polar p2 perp-ang offset-dist))
    
    (command "._LINE" p1 p2 "")
    (command "._LINE" p1-offset p2-offset "")
  )
  
  (princ "\n토류판 표시 완료")
  
  ;; 5. 띠장(Wale) 배치
  (princ "\n띠장(Wale) 배치 중...")
  
  (setq i 1)
  (while (<= i wale-levels)
    (princ (strcat "\n띠장 " (itoa i) "단 배치..."))
    
    ;; 각 세그먼트에 띠장 배치
    (foreach seg seg-list
      (setq p1 (car seg))
      (setq p2 (cadr seg))
      (setq ang (angle p1 p2))
      (setq perp-ang (perpendicular-angle ang))
      
      ;; 띠장 오프셋 (벽체에서 안쪽으로)
      (setq wale-offset (+ 200.0 (* pile-size 0.5)))
      (setq p1-wale (polar p1 perp-ang wale-offset))
      (setq p2-wale (polar p2 perp-ang wale-offset))
      
      (draw-wale p1-wale p2-wale wale-size)
      
      ;; 띠장 문자 표시
      (setq mid-pt (mid-point p1-wale p2-wale))
      (setq text-pt (polar mid-pt perp-ang 500.0))
      (place-text text-pt (strcat (itoa i) "단 띠장 H-" (rtos wale-size 2 0)) 100.0)
    )
    
    (setq i (+ i 1))
  )
  
  (princ "\n띠장 배치 완료")
  
  ;; 6. 버팀보(Strut) 배치
  (princ "\n버팀보(Strut) 배치 중...")
  
  ;; 경계선이 폐합된 경우, 대각선 또는 교차 버팀보 배치
  ;; 간단화: 첫 세그먼트와 대향 세그먼트 연결
  (if (>= (length seg-list) 2)
    (progn
      ;; 첫 번째와 세 번째 세그먼트 사이에 버팀보 배치 (사각형 가정)
      (setq seg1 (nth 0 seg-list))
      
      (if (>= (length seg-list) 3)
        (setq seg2 (nth 2 seg-list))
        (setq seg2 (nth 1 seg-list))
      )
      
      (setq p1-mid (mid-point (car seg1) (cadr seg1)))
      (setq p2-mid (mid-point (car seg2) (cadr seg2)))
      
      ;; 띠장 높이에 맞춰 버팀보 배치
      (setq i 1)
      (while (<= i wale-levels)
        ;; 버팀보 연결
        (draw-strut p1-mid p2-mid strut-size)
        
        ;; 버팀보 문자 표시
        (setq strut-mid (mid-point p1-mid p2-mid))
        (setq ang-strut (angle p1-mid p2-mid))
        (setq perp-strut (perpendicular-angle ang-strut))
        (setq text-pt (polar strut-mid perp-strut 600.0))
        (place-text text-pt (strcat (itoa i) "단 버팀보 H-" (rtos strut-size 2 0)) 120.0)
        
        (setq i (+ i 1))
      )
      
      ;; 추가 버팀보 (직각 방향)
      (if (>= (length seg-list) 4)
        (progn
          (setq seg3 (nth 1 seg-list))
          (setq seg4 (nth 3 seg-list))
          
          (setq p3-mid (mid-point (car seg3) (cadr seg3)))
          (setq p4-mid (mid-point (car seg4) (cadr seg4)))
          
          (setq i 1)
          (while (<= i wale-levels)
            (draw-strut p3-mid p4-mid strut-size)
            (setq i (+ i 1))
          )
        )
      )
    )
  )
  
  (princ "\n버팀보 배치 완료")
  
  ;; 완료 메시지
  (princ "\n\n========================================")
  (princ "\nH-Pile + 토류판 공법 작도 완료!")
  (princ "\n========================================")
  (princ (strcat "\n- H-Pile 개수: " (itoa (length pile-points)) "개소"))
  (princ (strcat "\n- H-Pile 간격: " (rtos (/ pile-spacing 1000.0) 2 2) "m"))
  (princ (strcat "\n- 띠장 단수: " (itoa wale-levels) "단"))
  (princ "\n========================================\n")
  
  (princ)
)

;;; ----------------------------------------------------------------------
;;; 명령어 등록
;;; ----------------------------------------------------------------------

(princ "\n========================================")
(princ "\nH-Pile + 토류판 공법 로드 완료")
(princ "\n명령어: HPILE-RT")
(princ "\n========================================")
(princ)
