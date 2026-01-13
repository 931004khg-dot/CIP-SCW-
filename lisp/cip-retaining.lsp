;;; ======================================================================
;;; cip-retaining.lsp
;;; CIP (Cast In Place Pile) 현장타설 말뚝 흙막이 벽체 자동 작도
;;; ======================================================================

;; 유틸리티 함수 로드 확인
(if (not init-retaining-layers)
  (load "retaining-utils.lsp")
)

;;; ----------------------------------------------------------------------
;;; CIP 공법 메인 함수
;;; ----------------------------------------------------------------------

(defun C:CIP-RT (/ boundary-ent pt-list seg-list 
                   pile-spacing pile-diameter pile-type
                   wale-levels wale-size strut-spacing strut-size
                   pile-points seg p1 p2 div-pts pt
                   ang perp-ang i wale-offset p1-wale p2-wale
                   mid-pt text-pt rebar-dia rebar-count)
  
  (princ "\n========================================")
  (princ "\nCIP (현장타설말뚝) 공법 자동 작도")
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
  (princ "\n\n[2단계] CIP 설계 파라미터 입력")
  
  ;; CIP 타입 선택
  (princ "\nCIP 타입 선택:")
  (princ "\n  1 = 원형 CIP")
  (princ "\n  2 = 사각형 CIP")
  (princ "\n  3 = 연속벽체 (Continuous)")
  (setq pile-type (get-int-input "CIP 타입 [1/2/3]" 1))
  
  (if (or (= pile-type 1) (= pile-type 2))
    (progn
      (setq pile-spacing (get-real-input "CIP 말뚝 간격(m)" 1.5))
      (setq pile-spacing (* pile-spacing 1000.0)) ;; m를 mm로 변환
    )
    (setq pile-spacing 0.0) ;; 연속벽체는 간격 없음
  )
  
  (setq pile-diameter (get-real-input "CIP 직경/폭(mm)" 800.0))
  
  ;; 철근 정보
  (setq rebar-dia (get-real-input "주철근 직경(mm)" 32.0))
  (setq rebar-count (get-int-input "주철근 개수" 12))
  
  (princ "\n\n[3단계] 띠장(Wale) 설계 파라미터")
  (setq wale-levels (get-int-input "띠장 단수" 3))
  (setq wale-size (get-real-input "띠장 H-Beam 크기(mm)" 400.0))
  
  (princ "\n\n[4단계] 버팀보(Strut) 설계 파라미터")
  (setq strut-spacing (get-real-input "버팀보 간격(m)" 6.0))
  (setq strut-spacing (* strut-spacing 1000.0)) ;; m를 mm로 변환
  (setq strut-size (get-real-input "버팀보 H-Beam 크기(mm)" 600.0))
  
  ;; 3. CIP 말뚝 배치
  (princ "\n\n[실행] CIP 말뚝 배치 중...")
  (setq pile-points '())
  
  (cond
    ;; 연속벽체 타입
    ((= pile-type 3)
     (princ "\n연속 CIP 벽체 배치...")
     (foreach seg seg-list
       (setq p1 (car seg))
       (setq p2 (cadr seg))
       (setq ang (angle p1 p2))
       (setq perp-ang (perpendicular-angle ang))
       
       ;; 연속벽체 그리기
       (draw-scw-wall p1 p2 pile-diameter)
       
       ;; 중심선에 철근 표시
       (setq mid-pt (mid-point p1 p2))
       (create-layer "PILE" "1" "Continuous")
       
       ;; 철근 마커 (작은 원들)
       (setq rebar-spacing (/ pile-diameter (+ rebar-count 1)))
       (setq j 1)
       (while (<= j rebar-count)
         (setq offset-dist (- (* j rebar-spacing) (/ pile-diameter 2.0)))
         (setq rebar-pt (polar mid-pt perp-ang offset-dist))
         (command "._CIRCLE" rebar-pt (/ rebar-dia 2.0))
         (setq j (+ j 1))
       )
     )
     (princ "\n연속 CIP 벽체 배치 완료")
    )
    
    ;; 원형 또는 사각형 CIP
    (t
     (foreach seg seg-list
       (setq p1 (car seg))
       (setq p2 (cadr seg))
       
       ;; 세그먼트를 등간격으로 분할
       (setq div-pts (divide-segment p1 p2 pile-spacing))
       
       ;; 각 분할점에 CIP 배치
       (foreach pt div-pts
         (setq pile-points (append pile-points (list pt)))
         
         ;; CIP 말뚝 그리기
         (cond
           ;; 원형 CIP
           ((= pile-type 1)
            (draw-cip-circle pt pile-diameter)
            
            ;; 철근 표시 (원주 상에 배치)
            (setq angle-step (/ (* 2.0 pi) rebar-count))
            (setq j 0)
            (while (< j rebar-count)
              (setq rebar-angle (* j angle-step))
              (setq rebar-radius (- (/ pile-diameter 2.0) 50.0)) ;; 피복 50mm
              (setq rebar-pt (polar pt rebar-angle rebar-radius))
              (create-layer "PILE" "1" "Continuous")
              (command "._CIRCLE" rebar-pt (/ rebar-dia 2.0))
              (setq j (+ j 1))
            )
           )
           
           ;; 사각형 CIP
           ((= pile-type 2)
            (setq ang (angle p1 p2))
            (setq half-size (/ pile-diameter 2.0))
            
            ;; 사각형 그리기
            (create-layer "PILE" "1" "Continuous")
            (setq perp-ang (perpendicular-angle ang))
            (setq pt1 (polar (polar pt ang half-size) perp-ang half-size))
            (setq pt2 (polar (polar pt ang half-size) perp-ang (- half-size)))
            (setq pt3 (polar (polar pt ang (- half-size)) perp-ang (- half-size)))
            (setq pt4 (polar (polar pt ang (- half-size)) perp-ang half-size))
            (command "._PLINE" pt1 pt2 pt3 pt4 "_C")
            
            ;; 철근 표시 (모서리와 중앙)
            (setq rebar-inset 50.0) ;; 피복
            (setq rebar-dist (- half-size rebar-inset))
            
            ;; 4개 모서리
            (setq rebar-pt1 (polar (polar pt ang rebar-dist) perp-ang rebar-dist))
            (setq rebar-pt2 (polar (polar pt ang rebar-dist) perp-ang (- rebar-dist)))
            (setq rebar-pt3 (polar (polar pt ang (- rebar-dist)) perp-ang (- rebar-dist)))
            (setq rebar-pt4 (polar (polar pt ang (- rebar-dist)) perp-ang rebar-dist))
            
            (command "._CIRCLE" rebar-pt1 (/ rebar-dia 2.0))
            (command "._CIRCLE" rebar-pt2 (/ rebar-dia 2.0))
            (command "._CIRCLE" rebar-pt3 (/ rebar-dia 2.0))
            (command "._CIRCLE" rebar-pt4 (/ rebar-dia 2.0))
           )
         )
       )
     )
     (princ (strcat "\nCIP 말뚝 배치 완료: " (itoa (length pile-points)) "개소"))
    )
  )
  
  ;; 4. 띠장(Wale) 배치
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
      (setq wale-offset (+ 250.0 (/ pile-diameter 2.0)))
      (setq p1-wale (polar p1 perp-ang wale-offset))
      (setq p2-wale (polar p2 perp-ang wale-offset))
      
      (draw-wale p1-wale p2-wale wale-size)
      
      ;; 띠장 문자 표시
      (setq mid-pt (mid-point p1-wale p2-wale))
      (setq text-pt (polar mid-pt perp-ang 600.0))
      (place-text text-pt (strcat (itoa i) "단 띠장 H-" (rtos wale-size 2 0)) 120.0)
    )
    
    (setq i (+ i 1))
  )
  
  (princ "\n띠장 배치 완료")
  
  ;; 5. 버팀보(Strut) 배치
  (princ "\n버팀보(Strut) 배치 중...")
  
  (if (>= (length seg-list) 2)
    (progn
      (setq seg1 (nth 0 seg-list))
      
      (if (>= (length seg-list) 3)
        (setq seg2 (nth 2 seg-list))
        (setq seg2 (nth 1 seg-list))
      )
      
      (setq p1-mid (mid-point (car seg1) (cadr seg1)))
      (setq p2-mid (mid-point (car seg2) (cadr seg2)))
      
      (setq i 1)
      (while (<= i wale-levels)
        (draw-strut p1-mid p2-mid strut-size)
        
        (setq strut-mid (mid-point p1-mid p2-mid))
        (setq ang-strut (angle p1-mid p2-mid))
        (setq perp-strut (perpendicular-angle ang-strut))
        (setq text-pt (polar strut-mid perp-strut 700.0))
        (place-text text-pt (strcat (itoa i) "단 버팀보 H-" (rtos strut-size 2 0)) 140.0)
        
        (setq i (+ i 1))
      )
      
      ;; 추가 버팀보
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
  (princ "\nCIP 공법 작도 완료!")
  (princ "\n========================================")
  (cond
    ((= pile-type 3)
     (princ "\n- CIP 타입: 연속벽체")
     (princ (strcat "\n- 벽체 두께: " (rtos pile-diameter 2 0) "mm"))
    )
    (t
     (princ (strcat "\n- CIP 개수: " (itoa (length pile-points)) "개소"))
     (princ (strcat "\n- CIP 간격: " (rtos (/ pile-spacing 1000.0) 2 2) "m"))
     (princ (strcat "\n- CIP 직경: " (rtos pile-diameter 2 0) "mm"))
    )
  )
  (princ (strcat "\n- 주철근: D" (rtos rebar-dia 2 0) " x " (itoa rebar-count) "개"))
  (princ (strcat "\n- 띠장 단수: " (itoa wale-levels) "단"))
  (princ "\n========================================\n")
  
  (princ)
)

;;; ----------------------------------------------------------------------
;;; 명령어 등록
;;; ----------------------------------------------------------------------

(princ "\n========================================")
(princ "\nCIP 공법 로드 완료")
(princ "\n명령어: CIP-RT")
(princ "\n========================================")
(princ)
