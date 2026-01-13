;;; ======================================================================
;;; scw-retaining.lsp
;;; S.C.W (Soil Cement Wall) 소일시멘트 벽체 흙막이 자동 작도
;;; ======================================================================

;; 유틸리티 함수 로드 확인
(if (not init-retaining-layers)
  (load "retaining-utils.lsp")
)

;;; ----------------------------------------------------------------------
;;; S.C.W 공법 메인 함수
;;; ----------------------------------------------------------------------

(defun C:SCW-RT (/ boundary-ent pt-list seg-list 
                   wall-thickness hbeam-spacing hbeam-size
                   wale-levels wale-size strut-spacing strut-size
                   hbeam-points seg p1 p2 div-pts pt
                   ang perp-ang i wale-offset p1-wale p2-wale
                   mid-pt text-pt cement-strength overlap-length)
  
  (princ "\n========================================")
  (princ "\nS.C.W (소일시멘트벽) 공법 자동 작도")
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
  (princ "\n\n[2단계] S.C.W 설계 파라미터 입력")
  
  (setq wall-thickness (get-real-input "소일시멘트 벽체 두께(mm)" 800.0))
  (setq overlap-length (get-real-input "벽체 중첩 길이(mm)" 200.0))
  (setq cement-strength (get-real-input "시멘트 설계기준강도(MPa)" 1.0))
  
  (princ "\n\n[3단계] H-Beam 보강재 파라미터")
  (setq hbeam-spacing (get-real-input "H-Beam 간격(m)" 2.0))
  (setq hbeam-spacing (* hbeam-spacing 1000.0)) ;; m를 mm로 변환
  (setq hbeam-size (get-real-input "H-Beam 크기(mm)" 400.0))
  
  (princ "\n\n[4단계] 띠장(Wale) 설계 파라미터")
  (setq wale-levels (get-int-input "띠장 단수" 3))
  (setq wale-size (get-real-input "띠장 H-Beam 크기(mm)" 500.0))
  
  (princ "\n\n[5단계] 버팀보(Strut) 설계 파라미터")
  (setq strut-spacing (get-real-input "버팀보 간격(m)" 6.0))
  (setq strut-spacing (* strut-spacing 1000.0)) ;; m를 mm로 변환
  (setq strut-size (get-real-input "버팀보 H-Beam 크기(mm)" 600.0))
  
  ;; 3. 소일시멘트 연속벽체 그리기
  (princ "\n\n[실행] 소일시멘트 연속벽체 배치 중...")
  
  (foreach seg seg-list
    (setq p1 (car seg))
    (setq p2 (cadr seg))
    (setq ang (angle p1 p2))
    (setq perp-ang (perpendicular-angle ang))
    
    ;; 연속벽체 그리기 (이중선으로 표시)
    (draw-scw-wall p1 p2 wall-thickness)
    
    ;; 벽체 중심선 표시 (점선)
    (create-layer "PILE" "1" "Continuous")
    (command "._LINE" p1 p2 "")
    
    ;; 중첩부 표시 (해칭 또는 패턴)
    ;; 벽체 시작과 끝에 중첩 표시
    (if (> overlap-length 0)
      (progn
        ;; 시작 중첩
        (setq overlap-p1 p1)
        (setq overlap-p2 (polar p1 ang overlap-length))
        (setq overlap-pt1 (polar overlap-p1 perp-ang (/ wall-thickness 2.0)))
        (setq overlap-pt2 (polar overlap-p2 perp-ang (/ wall-thickness 2.0)))
        (setq overlap-pt3 (polar overlap-p2 perp-ang (/ wall-thickness -2.0)))
        (setq overlap-pt4 (polar overlap-p1 perp-ang (/ wall-thickness -2.0)))
        
        ;; 중첩부 해칭
        (command "._HATCH" "_P" "ANSI31" "100" "45" "_N" "")
        (command overlap-pt1 overlap-pt2 overlap-pt3 overlap-pt4 overlap-pt1 "" "")
      )
    )
    
    ;; 벽체 규격 문자 표시
    (setq mid-pt (mid-point p1 p2))
    (setq text-pt (polar mid-pt perp-ang (+ (/ wall-thickness 2.0) 500.0)))
    (place-text text-pt (strcat "SCW t=" (rtos wall-thickness 2 0)) 150.0)
    (setq text-pt2 (polar text-pt (/ pi 1.5) 200.0))
    (place-text text-pt2 (strcat "Fc=" (rtos cement-strength 2 1) "MPa") 100.0)
  )
  
  (princ "\n소일시멘트 연속벽체 배치 완료")
  
  ;; 4. H-Beam 보강재 배치
  (princ "\nH-Beam 보강재 배치 중...")
  (setq hbeam-points '())
  
  (foreach seg seg-list
    (setq p1 (car seg))
    (setq p2 (cadr seg))
    (setq ang (angle p1 p2))
    
    ;; 세그먼트를 등간격으로 분할
    (setq div-pts (divide-segment p1 p2 hbeam-spacing))
    
    ;; 각 분할점에 H-Beam 배치
    (foreach pt div-pts
      (setq hbeam-points (append hbeam-points (list pt)))
      
      ;; H-Beam 마커 그리기
      (create-layer "PILE" "1" "Continuous")
      
      ;; H 형강 단면 표시 (십자가 형태)
      (setq half-size (/ hbeam-size 2.0))
      (setq perp-ang (perpendicular-angle ang))
      
      ;; 수평선
      (setq h-pt1 (polar pt perp-ang half-size))
      (setq h-pt2 (polar pt perp-ang (- half-size)))
      (command "._LINE" h-pt1 h-pt2 "")
      
      ;; 수직선
      (setq v-pt1 (polar pt ang half-size))
      (setq v-pt2 (polar pt ang (- half-size)))
      (command "._LINE" v-pt1 v-pt2 "")
      
      ;; 외곽 원
      (command "._CIRCLE" pt (* half-size 0.7))
      
      ;; H-Beam 문자 표시
      (setq text-pt (polar pt perp-ang (+ half-size 300.0)))
      (place-text text-pt (strcat "H-" (rtos hbeam-size 2 0)) 80.0)
    )
  )
  
  (princ (strcat "\nH-Beam 보강재 배치 완료: " (itoa (length hbeam-points)) "개소"))
  
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
      (setq wale-offset (+ 300.0 (/ wall-thickness 2.0)))
      (setq p1-wale (polar p1 perp-ang wale-offset))
      (setq p2-wale (polar p2 perp-ang wale-offset))
      
      (draw-wale p1-wale p2-wale wale-size)
      
      ;; 띠장과 H-Beam 연결선 표시
      ;; H-Beam 위치에서 띠장으로 연결
      (foreach hbeam-pt hbeam-points
        ;; 점이 현재 세그먼트 근처에 있는지 확인
        (setq dist-to-seg (distance hbeam-pt (mid-point p1 p2)))
        (if (< dist-to-seg (* (distance p1 p2) 0.6))
          (progn
            (setq wale-pt (polar hbeam-pt perp-ang wale-offset))
            (create-layer "WALE" "5" "Continuous")
            (command "._LINE" hbeam-pt wale-pt "")
          )
        )
      )
      
      ;; 띠장 문자 표시
      (setq mid-pt (mid-point p1-wale p2-wale))
      (setq text-pt (polar mid-pt perp-ang 700.0))
      (place-text text-pt (strcat (itoa i) "단 띠장 H-" (rtos wale-size 2 0)) 130.0)
    )
    
    (setq i (+ i 1))
  )
  
  (princ "\n띠장 배치 완료")
  
  ;; 6. 버팀보(Strut) 배치
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
        (setq text-pt (polar strut-mid perp-strut 800.0))
        (place-text text-pt (strcat (itoa i) "단 버팀보 H-" (rtos strut-size 2 0)) 150.0)
        
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
  (princ "\nS.C.W 공법 작도 완료!")
  (princ "\n========================================")
  (princ (strcat "\n- 벽체 두께: " (rtos wall-thickness 2 0) "mm"))
  (princ (strcat "\n- 벽체 중첩: " (rtos overlap-length 2 0) "mm"))
  (princ (strcat "\n- 시멘트 강도: " (rtos cement-strength 2 1) "MPa"))
  (princ (strcat "\n- H-Beam 개수: " (itoa (length hbeam-points)) "개소"))
  (princ (strcat "\n- H-Beam 간격: " (rtos (/ hbeam-spacing 1000.0) 2 2) "m"))
  (princ (strcat "\n- H-Beam 크기: H-" (rtos hbeam-size 2 0)))
  (princ (strcat "\n- 띠장 단수: " (itoa wale-levels) "단"))
  (princ "\n========================================\n")
  
  (princ)
)

;;; ----------------------------------------------------------------------
;;; 명령어 등록
;;; ----------------------------------------------------------------------

(princ "\n========================================")
(princ "\nS.C.W 공법 로드 완료")
(princ "\n명령어: SCW-RT")
(princ "\n========================================")
(princ)
