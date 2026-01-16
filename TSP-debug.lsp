;;; ======================================================================
;;; TSP-debug.lsp - Debugging Infrastructure for TSP
;;; TSP.lsp 디버깅 기능 확장 모듈
;;; ======================================================================
;;; 
;;; 사용법: TSP.lsp 로드 후 이 파일의 내용을 복사하여 TSP.lsp 맨 뒤에 붙여넣으세요
;;; 
;;; 작성일: 2026-01-14
;;; 버전: 1.0.0
;;; 
;;; ======================================================================

;;; ----------------------------------------------------------------------
;;; 전역 변수
;;; ----------------------------------------------------------------------

(setq *tsp-hpile-spec* "H 298×201×9/14")     ; H-Pile 규격
(setq *tsp-hpile-custom* '(298 201 9 14))    ; User-defined H-Pile
(setq *tsp-wale-spec* "H 300×300×10/15")      ; 띠장 규격
(setq *tsp-wale-custom* '(300 300 10 15))     ; User-defined 띠장
(setq *tsp-ctc* 2.0)                           ; C.T.C 값
(setq *tsp-timber-thickness* 60)               ; 토류판 두께 (mm)

;;; ----------------------------------------------------------------------
;;; 디버깅 전역 변수
;;; ----------------------------------------------------------------------

(setq *tsp-debug* T)                           ; 디버그 모드 (T=활성화, nil=비활성화)
(setq *tsp-debug-log* '())                     ; 디버그 로그 리스트

;;; ----------------------------------------------------------------------
;;; 디버그 로그 함수
;;; ----------------------------------------------------------------------

(defun debug-log (msg / timestamp)
  (if *tsp-debug*
    (progn
      (setq timestamp (rtos (getvar "MILLISECS") 2 0))
      (setq *tsp-debug-log* (append *tsp-debug-log* (list (strcat "[" timestamp "ms] " msg))))
      (princ (strcat "\n[DEBUG] " msg))
    )
  )
)

(defun debug-clear ()
  (setq *tsp-debug-log* '())
  (if *tsp-debug*
    (princ "\n[DEBUG] 로그 초기화 완료")
  )
)

(defun debug-show-log ()
  (if *tsp-debug*
    (progn
      (princ "\n========== DEBUG LOG ==========")
      (foreach item *tsp-debug-log*
        (princ (strcat "\n" item))
      )
      (princ "\n==============================\n")
    )
  )
)

;;; ----------------------------------------------------------------------
;;; 유틸리티 함수
;;; ----------------------------------------------------------------------

;; 레이어 생성 (없으면 생성)
(defun create-layer-if-not-exists (layer-name color / )
  (if (not (tblsearch "LAYER" layer-name))
    (command "._LAYER" "_M" layer-name "_C" color layer-name "")
  )
)

;; 숫자 검증
(defun is-numeric (str / result)
  (setq result T)
  (if (or (null str) (= str ""))
    (setq result nil)
    (if (not (numberp (read str)))
      (setq result nil)
    )
  )
  result
)

;;; ----------------------------------------------------------------------
;;; 기하학 유틸리티 함수 (다각형 방향 판단)
;;; ----------------------------------------------------------------------

;; 엔티티에서 꼭지점 추출
(defun extract-vertices (ent / ent-data vertices item first-pt last-pt)
  (debug-log "=== extract-vertices 시작 ===")
  (setq ent-data (entget ent))
  (setq vertices '())
  
  ;; DXF 코드 10 = 꼭지점 좌표
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
  
  (debug-log (strcat "추출된 꼭지점 개수: " (itoa (length vertices))))
  vertices
)

;; 다각형 방향 판단 (Shoelace 공식 - Signed Area)
;; 반환값: 1 = CCW (반시계방향), -1 = CW (시계방향)
(defun get-polygon-orientation (vertices / signed-area i n x1 y1 x2 y2)
  (debug-log "=== get-polygon-orientation 시작 ===")
  (setq signed-area 0.0)
  (setq n (length vertices))
  (setq i 0)
  
  ;; Shoelace 공식: Σ (x[i] - x[i+1]) * (y[i] + y[i+1])
  (while (< i n)
    (setq x1 (car (nth i vertices)))
    (setq y1 (cadr (nth i vertices)))
    (setq x2 (car (nth (if (= i (1- n)) 0 (1+ i)) vertices)))
    (setq y2 (cadr (nth (if (= i (1- n)) 0 (1+ i)) vertices)))
    
    (setq signed-area (+ signed-area (* (- x1 x2) (+ y1 y2))))
    (setq i (1+ i))
  )
  
  (debug-log (strcat "Signed Area: " (rtos signed-area 2 2)))
  
  ;; 양수 = CCW, 음수 = CW
  (if (> signed-area 0)
    (progn
      (debug-log "다각형 방향: CCW (반시계방향)")
      (princ "\n[기하] 다각형 방향: CCW (반시계방향) - 진행방향의 왼쪽이 내부")
      1
    )
    (progn
      (debug-log "다각형 방향: CW (시계방향)")
      (princ "\n[기하] 다각형 방향: CW (시계방향) - 진행방향의 오른쪽이 내부")
      -1
    )
  )
)

;; 폴리라인 폐합 여부 판별
;; 반환값: T = 폐합선 (Closed), nil = 열린선 (Open)
(defun is-closed-polyline (ent / ent-data closed-flag)
  (setq ent-data (entget ent))
  ;; DXF 코드 70의 bit 0 = closed flag
  (setq closed-flag (cdr (assoc 70 ent-data)))
  (if closed-flag
    (= 1 (logand 1 closed-flag))
    nil
  )
)

;; 벡터 외적으로 모서리 볼록/오목 판단
;; 반환값: T = 볼록 (Convex), nil = 오목 (Concave)
(defun is-corner-convex (prev-pt curr-pt next-pt orientation / 
  v1x v1y v2x v2y cross-product)
  
  ;; 벡터 1: prev → curr
  (setq v1x (- (car curr-pt) (car prev-pt)))
  (setq v1y (- (cadr curr-pt) (cadr prev-pt)))
  
  ;; 벡터 2: curr → next
  (setq v2x (- (car next-pt) (car curr-pt)))
  (setq v2y (- (cadr next-pt) (cadr curr-pt)))
  
  ;; 외적 (cross product): v1 × v2 = v1x*v2y - v1y*v2x
  (setq cross-product (- (* v1x v2y) (* v1y v2x)))
  
  (debug-log (strcat "외적 값: " (rtos cross-product 2 2)))
  
  ;; 다각형 방향에 따라 판단
  ;; CCW (orientation=1): cross > 0 → 왼쪽으로 꺾임 → 볼록
  ;; CW (orientation=-1): cross < 0 → 오른쪽으로 꺾임 → 볼록
  (if (= orientation 1)
    (> cross-product 0)  ; CCW
    (< cross-product 0)  ; CW
  )
)

;;; ----------------------------------------------------------------------
;;; 기존 함수 재정의 (디버깅 기능 추가)
;;; ----------------------------------------------------------------------

;; create-tsp-dcl 재정의
(defun create-tsp-dcl (/ dcl-file dcl-path lisp-path)
  (debug-log "DIAG_START: DCL 생성 시작")
  (debug-log (strcat "TSP 버전: 1.0.0, 날짜: 2026-01-13"))
  (debug-log (strcat "*tsp-debug* = " (if *tsp-debug* "true" "false")))
  
  ;; 현재 LISP 파일 경로 가져오기
  (setq lisp-path (findfile "TSP.lsp"))
  (debug-log (strcat "LISP 파일 경로: " (if lisp-path lisp-path "not found")))
  
  (if lisp-path
    (progn
      ;; LISP 파일이 있는 디렉토리에 DCL 파일 생성
      (setq dcl-path (strcat (vl-filename-directory lisp-path) "\\tsp.dcl"))
      (debug-log (strcat "DCL 경로 (LISP 디렉토리): " dcl-path))
    )
    (progn
      ;; LISP 파일을 찾을 수 없으면 TEMP 디렉토리 사용
      (setq dcl-path (strcat (getvar "TEMPPREFIX") "tsp.dcl"))
      (debug-log (strcat "DCL 경로 (TEMP 디렉토리): " dcl-path))
    )
  )
  
  ;; DCL 파일 생성
  (setq dcl-file (open dcl-path "w"))
  
  (if dcl-file
    (progn
      (debug-log "DCL 파일 열기 성공")
      ;; 메인 Dialog
      (write-line "tsp_main : dialog {" dcl-file)
      (write-line "  label = \"TSP - Temporary Structure Plan\";" dcl-file)
      (write-line "  : column {" dcl-file)
      (write-line "    alignment = centered;" dcl-file)
      (write-line "    : text {" dcl-file)
      (write-line "      label = \"공법 선택\";" dcl-file)
      (write-line "      alignment = centered;" dcl-file)
      (write-line "    }" dcl-file)
      (write-line "    : spacer { height = 0.5; }" dcl-file)
      (write-line "    : button {" dcl-file)
      (write-line "      key = \"btn_hpile\";" dcl-file)
      (write-line "      label = \"H-Pile + 토류판\";" dcl-file)
      (write-line "      width = 30;" dcl-file)
      (write-line "      fixed_width = true;" dcl-file)
      (write-line "    }" dcl-file)
      (write-line "    : button {" dcl-file)
      (write-line "      key = \"btn_cip\";" dcl-file)
      (write-line "      label = \"CIP (현장타설말뚝)\";" dcl-file)
      (write-line "      width = 30;" dcl-file)
      (write-line "      fixed_width = true;" dcl-file)
      (write-line "      is_enabled = false;" dcl-file)
      (write-line "    }" dcl-file)
      (write-line "    : button {" dcl-file)
      (write-line "      key = \"btn_scw\";" dcl-file)
      (write-line "      label = \"SCW (소일시멘트벽)\";" dcl-file)
      (write-line "      width = 30;" dcl-file)
      (write-line "      fixed_width = true;" dcl-file)
      (write-line "      is_enabled = false;" dcl-file)
      (write-line "    }" dcl-file)
      (write-line "    : spacer { height = 0.5; }" dcl-file)
      (write-line "    : button {" dcl-file)
      (write-line "      key = \"cancel\";" dcl-file)
      (write-line "      label = \"취소\";" dcl-file)
      (write-line "      is_cancel = true;" dcl-file)
      (write-line "      fixed_width = true;" dcl-file)
      (write-line "      alignment = centered;" dcl-file)
      (write-line "    }" dcl-file)
      (write-line "  }" dcl-file)
      (write-line "}" dcl-file)
      (write-line "" dcl-file)
      
      ;; H-Pile 설정 Dialog
      (write-line "tsp_hpile : dialog {" dcl-file)
      (write-line "  label = \"H-Pile + 토류판 공법 설정\";" dcl-file)
      (write-line "  : column {" dcl-file)
      (write-line "    : text {" dcl-file)
      (write-line "      label = \"H-Pile 규격\";" dcl-file)
      (write-line "    }" dcl-file)
      (write-line "    : popup_list {" dcl-file)
      (write-line "      key = \"hpile_spec\";" dcl-file)
      (write-line "      width = 30;" dcl-file)
      (write-line "      fixed_width = true;" dcl-file)
      (write-line "    }" dcl-file)
    (write-line "    : row {" dcl-file)
    (write-line "      : edit_box {" dcl-file)
    (write-line "        key = \"hpile_h\";" dcl-file)
    (write-line "        label = \"\";" dcl-file)
    (write-line "        width = 5;" dcl-file)
    (write-line "        fixed_width = true;" dcl-file)
    (write-line "        edit_width = 5;" dcl-file)
    (write-line "        is_enabled = false;" dcl-file)
    (write-line "      }" dcl-file)
    (write-line "      : text { label = \"×\"; }" dcl-file)
    (write-line "      : edit_box {" dcl-file)
    (write-line "        key = \"hpile_b\";" dcl-file)
    (write-line "        label = \"\";" dcl-file)
    (write-line "        width = 5;" dcl-file)
    (write-line "        fixed_width = true;" dcl-file)
    (write-line "        edit_width = 5;" dcl-file)
    (write-line "        is_enabled = false;" dcl-file)
    (write-line "      }" dcl-file)
    (write-line "      : text { label = \"×\"; }" dcl-file)
    (write-line "      : edit_box {" dcl-file)
    (write-line "        key = \"hpile_tw\";" dcl-file)
    (write-line "        label = \"\";" dcl-file)
    (write-line "        width = 5;" dcl-file)
    (write-line "        fixed_width = true;" dcl-file)
    (write-line "        edit_width = 5;" dcl-file)
    (write-line "        is_enabled = false;" dcl-file)
    (write-line "      }" dcl-file)
    (write-line "      : text { label = \"/\"; }" dcl-file)
    (write-line "      : edit_box {" dcl-file)
    (write-line "        key = \"hpile_tf\";" dcl-file)
    (write-line "        label = \"\";" dcl-file)
    (write-line "        width = 5;" dcl-file)
    (write-line "        fixed_width = true;" dcl-file)
    (write-line "        edit_width = 5;" dcl-file)
    (write-line "        is_enabled = false;" dcl-file)
    (write-line "      }" dcl-file)
    (write-line "    }" dcl-file)
    (write-line "    : spacer { height = 0.5; }" dcl-file)
    (write-line "    : text {" dcl-file)
    (write-line "      label = \"띠장 규격\";" dcl-file)
    (write-line "    }" dcl-file)
    (write-line "    : popup_list {" dcl-file)
    (write-line "      key = \"wale_spec\";" dcl-file)
    (write-line "      width = 30;" dcl-file)
    (write-line "      fixed_width = true;" dcl-file)
    (write-line "    }" dcl-file)
    (write-line "    : row {" dcl-file)
    (write-line "      : edit_box {" dcl-file)
    (write-line "        key = \"wale_h\";" dcl-file)
    (write-line "        label = \"\";" dcl-file)
    (write-line "        width = 5;" dcl-file)
    (write-line "        fixed_width = true;" dcl-file)
    (write-line "        edit_width = 5;" dcl-file)
    (write-line "        is_enabled = false;" dcl-file)
    (write-line "      }" dcl-file)
    (write-line "      : text { label = \"×\"; }" dcl-file)
    (write-line "      : edit_box {" dcl-file)
    (write-line "        key = \"wale_b\";" dcl-file)
    (write-line "        label = \"\";" dcl-file)
    (write-line "        width = 5;" dcl-file)
    (write-line "        fixed_width = true;" dcl-file)
    (write-line "        edit_width = 5;" dcl-file)
    (write-line "        is_enabled = false;" dcl-file)
    (write-line "      }" dcl-file)
    (write-line "      : text { label = \"×\"; }" dcl-file)
    (write-line "      : edit_box {" dcl-file)
    (write-line "        key = \"wale_tw\";" dcl-file)
    (write-line "        label = \"\";" dcl-file)
    (write-line "        width = 5;" dcl-file)
    (write-line "        fixed_width = true;" dcl-file)
    (write-line "        edit_width = 5;" dcl-file)
    (write-line "        is_enabled = false;" dcl-file)
    (write-line "      }" dcl-file)
    (write-line "      : text { label = \"/\"; }" dcl-file)
    (write-line "      : edit_box {" dcl-file)
    (write-line "        key = \"wale_tf\";" dcl-file)
    (write-line "        label = \"\";" dcl-file)
    (write-line "        width = 5;" dcl-file)
    (write-line "        fixed_width = true;" dcl-file)
    (write-line "        edit_width = 5;" dcl-file)
    (write-line "        is_enabled = false;" dcl-file)
    (write-line "      }" dcl-file)
    (write-line "    }" dcl-file)
    (write-line "    : spacer { height = 0.5; }" dcl-file)
    (write-line "    : row {" dcl-file)
    (write-line "      : text {" dcl-file)
    (write-line "        label = \"C.T.C (m)\";" dcl-file)
    (write-line "        width = 10;" dcl-file)
    (write-line "      }" dcl-file)
    (write-line "      : edit_box {" dcl-file)
    (write-line "        key = \"ctc\";" dcl-file)
    (write-line "        label = \"\";" dcl-file)
    (write-line "        width = 5;" dcl-file)
    (write-line "        fixed_width = true;" dcl-file)
    (write-line "        edit_width = 5;" dcl-file)
    (write-line "        value = \"2\";" dcl-file)
    (write-line "      }" dcl-file)
    (write-line "    }" dcl-file)
    (write-line "    : spacer { height = 0.5; }" dcl-file)
    (write-line "    : row {" dcl-file)
    (write-line "      : text {" dcl-file)
    (write-line "        label = \"토류판 두께 (mm)\";" dcl-file)
    (write-line "        width = 10;" dcl-file)
    (write-line "      }" dcl-file)
    (write-line "      : edit_box {" dcl-file)
    (write-line "        key = \"timber_thickness\";" dcl-file)
    (write-line "        label = \"\";" dcl-file)
    (write-line "        width = 5;" dcl-file)
    (write-line "        fixed_width = true;" dcl-file)
    (write-line "        edit_width = 5;" dcl-file)
    (write-line "        value = \"60\";" dcl-file)
    (write-line "      }" dcl-file)
    (write-line "    }" dcl-file)
    (write-line "  }" dcl-file)
      (write-line "  : spacer { height = 1.0; }" dcl-file)
      (write-line "  : row {" dcl-file)
      (write-line "    alignment = centered;" dcl-file)
      (write-line "    fixed_width = true;" dcl-file)
      (write-line "    : button {" dcl-file)
      (write-line "      key = \"accept\";" dcl-file)
      (write-line "      label = \"OK\";" dcl-file)
      (write-line "      is_default = true;" dcl-file)
      (write-line "      fixed_width = true;" dcl-file)
      (write-line "      width = 12;" dcl-file)
      (write-line "    }" dcl-file)
      (write-line "    : button {" dcl-file)
      (write-line "      key = \"cancel\";" dcl-file)
      (write-line "      label = \"Cancel\";" dcl-file)
      (write-line "      is_cancel = true;" dcl-file)
      (write-line "      fixed_width = true;" dcl-file)
      (write-line "      width = 12;" dcl-file)
      (write-line "    }" dcl-file)
      (write-line "  }" dcl-file)
      (write-line "}" dcl-file)
      
      (close dcl-file)
      (debug-log (strcat "DCL 파일 생성 완료: " dcl-path))
      dcl-path
    )
    (progn
      (princ "\nDCL 파일 생성 실패!")
      (debug-log "ERROR: DCL 파일 열기 실패")
      nil
    )
  )
)

;; parse-h-spec 재정의
(defun parse-h-spec (spec-str / clean-str char-list i ch result-str parsed-result)
  ;; "H 298×201×9/14" -> (298 201 9 14)
  ;; 모든 문자를 하나씩 검사하여 숫자와 공백만 남김
  (debug-log (strcat "parse-h-spec 입력: \"" (if spec-str spec-str "nil") "\""))
  
  (if (and spec-str (wcmatch spec-str "H *"))
    (progn
      ;; "H " 제거
      (setq clean-str (substr spec-str 3))
      (debug-log (strcat "'H ' 제거 후: \"" clean-str "\""))
      (setq result-str "")
      (setq i 1)
      
      ;; 문자열의 각 문자를 검사
      (while (<= i (strlen clean-str))
        (setq ch (substr clean-str i 1))
        (cond
          ;; 숫자인 경우
          ((and (>= (ascii ch) 48) (<= (ascii ch) 57))
           (setq result-str (strcat result-str ch)))
          ;; 공백 추가 (구분자)
          (t
           (if (> (strlen result-str) 0)
             (if (/= (substr result-str (strlen result-str) 1) " ")
               (setq result-str (strcat result-str " "))
             )
           ))
        )
        (setq i (1+ i))
      )
      
      ;; "298 201 9 14" 형태의 문자열을 리스트로 변환
      (debug-log (strcat "숫자 추출 결과: \"" result-str "\""))
      (setq parsed-result (read (strcat "(" result-str ")")))
      (debug-log (strcat "파싱 완료: " (vl-princ-to-string parsed-result)))
      parsed-result
    )
    (progn
      (debug-log "ERROR: parse-h-spec 실패 (형식 불일치)")
      nil
    )
  )
)

;; create-wale-offsets 재정의
(defun create-wale-offsets (boundary-ent wale-spec / h b tw tf offset-list obj1 obj2 obj3 obj4 wale-values boundary-vla obj2-vla obj3-vla obj4-vla original-area offset-area)
  (debug-log "=== create-wale-offsets 시작 ===")
  (debug-log (strcat "wale-spec: " wale-spec))
  
  ;; 레이어 생성
  (create-layer-if-not-exists "_띠장(wale)" "3")
  (debug-log "레이어 '_띠장(wale)' 확인/생성 완료")
  
  ;; 규격 파싱
  (if (= wale-spec "User-defined")
    (progn
      (setq wale-values *tsp-wale-custom*)
      (debug-log (strcat "User-defined 띠장: " (vl-princ-to-string wale-values)))
    )
    (progn
      (setq wale-values (parse-h-spec wale-spec))
      (debug-log (strcat "파싱된 띠장 규격: " (vl-princ-to-string wale-values)))
    )
  )
  
  (setq h (nth 0 wale-values))
  (setq b (nth 1 wale-values))
  (setq tw (nth 2 wale-values))
  (setq tf (nth 3 wale-values))
  
  (princ (strcat "\n띠장 규격: H=" (rtos h 2 0) " B=" (rtos b 2 0) " tw=" (rtos tw 2 0) " tf=" (rtos tf 2 0)))
  
  ;; 옵셋 거리 리스트: (0, tf, H-tf, H)
  (setq offset-list (list 0 tf (- h tf) h))
  
  (princ (strcat "\n옵셋 거리: 0, " (rtos tf 2 0) ", " (rtos (- h tf) 2 0) ", " (rtos h 2 0)))
  
  ;; 객체 1: 원본 복사 (0mm) - 초록색
  (command "._COPY" boundary-ent "" "0,0,0" "0,0,0")
  (setq obj1 (entlast))
  (command "._CHPROP" obj1 "" "_LA" "_띠장(wale)" "_C" "3" "")
  
  ;; VLA-OFFSET을 사용한 자동 오프셋 (사용자 입력 불필요)
  (setq boundary-vla (vlax-ename->vla-object boundary-ent))
  
  ;; 경계선의 면적 계산 (비교용)
  (setq original-area (vla-get-area boundary-vla))
  (princ (strcat "\n원본 경계선 면적: " (rtos original-area 2 2)))
  (debug-log (strcat "원본 경계선 면적: " (rtos original-area 2 2)))
  
  ;; 객체 2: tf 옵셋 - 빨간색 (안쪽)
  ;; 음수 오프셋 = 안쪽, 양수 오프셋 = 바깥쪽
  (setq obj2-vla (vl-catch-all-apply 'vla-offset (list boundary-vla (- tf))))
  
  (if (vl-catch-all-error-p obj2-vla)
    (progn
      ;; 음수 오프셋 실패 시 양수로 시도
      (princ "\n[Warning] 음수 오프셋 실패, 양수 오프셋 시도...")
      (debug-log "WARNING: 음수 오프셋 실패, 양수 오프셋 시도")
      (setq obj2-vla (vl-catch-all-apply 'vla-offset (list boundary-vla tf)))
    )
  )
  
  (if (not (vl-catch-all-error-p obj2-vla))
    (progn
      ;; VLA-OFFSET은 variant를 반환하므로 vlax-variant-value로 추출
      (if (= (type obj2-vla) 'variant)
        (setq obj2-vla (vlax-variant-value obj2-vla))
      )
      ;; safearray인 경우 리스트로 변환
      (if (= (type obj2-vla) 'safearray)
        (setq obj2-vla (vlax-safearray->list obj2-vla))
      )
      ;; 리스트인 경우 첫 번째 요소 추출
      (if (= (type obj2-vla) 'list)
        (setq obj2-vla (car obj2-vla))
      )
      
      (setq obj2 (vlax-vla-object->ename obj2-vla))
      (setq offset-area (vla-get-area obj2-vla))
      (princ (strcat "\ntf 옵셋 후 면적: " (rtos offset-area 2 2)))
      (debug-log (strcat "tf 옵셋 후 면적: " (rtos offset-area 2 2)))
      
      ;; 면적이 작아지지 않았으면 바깥쪽으로 옵셋된 것 → 삭제 후 반대 방향으로 재시도
      (if (>= offset-area original-area)
        (progn
          (princ "\n[Warning] tf 옵셋이 바깥쪽으로 생성됨, 반대 방향으로 재시도...")
          (debug-log "WARNING: tf 옵셋이 바깥쪽, 반대 방향으로 재시도")
          (vla-delete obj2-vla)
          ;; 반대 방향으로 오프셋
          (setq obj2-vla (vla-offset boundary-vla tf))
          (if (= (type obj2-vla) 'variant)
            (setq obj2-vla (vlax-variant-value obj2-vla))
          )
          (if (= (type obj2-vla) 'safearray)
            (setq obj2-vla (vlax-safearray->list obj2-vla))
          )
          (if (= (type obj2-vla) 'list)
            (setq obj2-vla (car obj2-vla))
          )
          (setq obj2 (vlax-vla-object->ename obj2-vla))
          (setq offset-area (vla-get-area obj2-vla))
          (princ (strcat "\ntf 옵셋 재시도 후 면적: " (rtos offset-area 2 2)))
          (debug-log (strcat "tf 옵셋 재시도 후 면적: " (rtos offset-area 2 2)))
        )
      )
      
      (if obj2
        (command "._CHPROP" obj2 "" "_LA" "_띠장(wale)" "_C" "1" "")
      )
    )
    (progn
      (princ "\n[Error] tf 옵셋 생성 실패")
      (debug-log "ERROR: tf 옵셋 생성 실패")
      (setq obj2 nil)
    )
  )
  
  ;; 객체 3: (H - tf) 옵셋 - 빨간색 (안쪽)
  (setq obj3-vla (vl-catch-all-apply 'vla-offset (list boundary-vla (- (- h tf)))))
  
  (if (vl-catch-all-error-p obj3-vla)
    (progn
      (princ "\n[Warning] 음수 오프셋 실패, 양수 오프셋 시도...")
      (debug-log "WARNING: 음수 오프셋 실패, 양수 오프셋 시도")
      (setq obj3-vla (vl-catch-all-apply 'vla-offset (list boundary-vla (- h tf))))
    )
  )
  
  (if (not (vl-catch-all-error-p obj3-vla))
    (progn
      (if (= (type obj3-vla) 'variant)
        (setq obj3-vla (vlax-variant-value obj3-vla))
      )
      (if (= (type obj3-vla) 'safearray)
        (setq obj3-vla (vlax-safearray->list obj3-vla))
      )
      (if (= (type obj3-vla) 'list)
        (setq obj3-vla (car obj3-vla))
      )
      
      (setq obj3 (vlax-vla-object->ename obj3-vla))
      (setq offset-area (vla-get-area obj3-vla))
      (princ (strcat "\n(H-tf) 옵셋 후 면적: " (rtos offset-area 2 2)))
      (debug-log (strcat "(H-tf) 옵셋 후 면적: " (rtos offset-area 2 2)))
      
      (if (>= offset-area original-area)
        (progn
          (princ "\n[Warning] (H-tf) 옵셋이 바깥쪽으로 생성됨, 반대 방향으로 재시도...")
          (debug-log "WARNING: (H-tf) 옵셋이 바깥쪽, 반대 방향으로 재시도")
          (vla-delete obj3-vla)
          (setq obj3-vla (vla-offset boundary-vla (- h tf)))
          (if (= (type obj3-vla) 'variant)
            (setq obj3-vla (vlax-variant-value obj3-vla))
          )
          (if (= (type obj3-vla) 'safearray)
            (setq obj3-vla (vlax-safearray->list obj3-vla))
          )
          (if (= (type obj3-vla) 'list)
            (setq obj3-vla (car obj3-vla))
          )
          (setq obj3 (vlax-vla-object->ename obj3-vla))
          (setq offset-area (vla-get-area obj3-vla))
          (princ (strcat "\n(H-tf) 옵셋 재시도 후 면적: " (rtos offset-area 2 2)))
          (debug-log (strcat "(H-tf) 옵셋 재시도 후 면적: " (rtos offset-area 2 2)))
        )
      )
      
      (if obj3
        (command "._CHPROP" obj3 "" "_LA" "_띠장(wale)" "_C" "1" "")
      )
    )
    (progn
      (princ "\n[Error] (H-tf) 옵셋 생성 실패")
      (debug-log "ERROR: (H-tf) 옵셋 생성 실패")
      (setq obj3 nil)
    )
  )
  
  ;; 객체 4: H 옵셋 - 초록색 (안쪽)
  (setq obj4-vla (vl-catch-all-apply 'vla-offset (list boundary-vla (- h))))
  
  (if (vl-catch-all-error-p obj4-vla)
    (progn
      (princ "\n[Warning] 음수 오프셋 실패, 양수 오프셋 시도...")
      (debug-log "WARNING: 음수 오프셋 실패, 양수 오프셋 시도")
      (setq obj4-vla (vl-catch-all-apply 'vla-offset (list boundary-vla h)))
    )
  )
  
  (if (not (vl-catch-all-error-p obj4-vla))
    (progn
      (if (= (type obj4-vla) 'variant)
        (setq obj4-vla (vlax-variant-value obj4-vla))
      )
      (if (= (type obj4-vla) 'safearray)
        (setq obj4-vla (vlax-safearray->list obj4-vla))
      )
      (if (= (type obj4-vla) 'list)
        (setq obj4-vla (car obj4-vla))
      )
      
      (setq obj4 (vlax-vla-object->ename obj4-vla))
      (setq offset-area (vla-get-area obj4-vla))
      (princ (strcat "\nH 옵셋 후 면적: " (rtos offset-area 2 2)))
      (debug-log (strcat "H 옵셋 후 면적: " (rtos offset-area 2 2)))
      
      (if (>= offset-area original-area)
        (progn
          (princ "\n[Warning] H 옵셋이 바깥쪽으로 생성됨, 반대 방향으로 재시도...")
          (debug-log "WARNING: H 옵셋이 바깥쪽, 반대 방향으로 재시도")
          (vla-delete obj4-vla)
          (setq obj4-vla (vla-offset boundary-vla h))
          (if (= (type obj4-vla) 'variant)
            (setq obj4-vla (vlax-variant-value obj4-vla))
          )
          (if (= (type obj4-vla) 'safearray)
            (setq obj4-vla (vlax-safearray->list obj4-vla))
          )
          (if (= (type obj4-vla) 'list)
            (setq obj4-vla (car obj4-vla))
          )
          (setq obj4 (vlax-vla-object->ename obj4-vla))
          (setq offset-area (vla-get-area obj4-vla))
          (princ (strcat "\nH 옵셋 재시도 후 면적: " (rtos offset-area 2 2)))
          (debug-log (strcat "H 옵셋 재시도 후 면적: " (rtos offset-area 2 2)))
        )
      )
      
      (if obj4
        (command "._CHPROP" obj4 "" "_LA" "_띠장(wale)" "_C" "3" "")
      )
    )
    (progn
      (princ "\n[Error] H 옵셋 생성 실패")
      (debug-log "ERROR: H 옵셋 생성 실패")
      (setq obj4 nil)
    )
  )
  
  (debug-log "=== create-wale-offsets 완료 ===")
  (princ "\n띠장 옵셋 생성 완료!")
)

;; hpile-dialog-callback 재정의
(defun hpile-dialog-callback (dcl-path / dcl-id result hpile-idx wale-idx)
  (debug-log "=== hpile-dialog-callback 시작 ===")
  (debug-log (strcat "DCL 경로: " dcl-path))
  
  (setq dcl-id (load_dialog dcl-path))
  (debug-log (strcat "DCL 로드 ID: " (if dcl-id (itoa dcl-id) "nil")))
  
  (if (not (new_dialog "tsp_hpile" dcl-id))
    (progn
      (princ "\nDialog 로드 실패!")
      (debug-log "ERROR: tsp_hpile 다이얼로그 로드 실패")
      (unload_dialog dcl-id)
      nil
    )
    (progn
      (debug-log "tsp_hpile 다이얼로그 표시 준비 완료")
      ;; 드롭다운 리스트 초기화
      (start_list "hpile_spec")
      (mapcar 'add_list '("H 298×201×9/14" "H 300×300×10/15" "H 350×350×12/19" "H 400×400×13/21" "User-defined"))
      (end_list)
      (set_tile "hpile_spec" "0")  ; 기본값: H 298×201×9/14
      
      (start_list "wale_spec")
      (mapcar 'add_list '("H 298×201×9/14" "H 300×300×10/15" "H 350×350×12/19" "H 400×400×13/21" "User-defined"))
      (end_list)
      (set_tile "wale_spec" "1")  ; 기본값: H 300×300×10/15
      
      ;; C.T.C 기본값
      (set_tile "ctc" "2")
      
      ;; H-Pile 드롭다운 변경 이벤트
      (action_tile "hpile_spec" 
        "(progn
           (setq hpile-idx (atoi $value))
           (if (= hpile-idx 4)
             (progn
               (mode_tile \"hpile_h\" 0)
               (mode_tile \"hpile_b\" 0)
               (mode_tile \"hpile_tw\" 0)
               (mode_tile \"hpile_tf\" 0)
             )
             (progn
               (mode_tile \"hpile_h\" 1)
               (mode_tile \"hpile_b\" 1)
               (mode_tile \"hpile_tw\" 1)
               (mode_tile \"hpile_tf\" 1)
             )
           )
         )"
      )
      
      ;; 띠장 드롭다운 변경 이벤트
      (action_tile "wale_spec" 
        "(progn
           (setq wale-idx (atoi $value))
           (if (= wale-idx 4)
             (progn
               (mode_tile \"wale_h\" 0)
               (mode_tile \"wale_b\" 0)
               (mode_tile \"wale_tw\" 0)
               (mode_tile \"wale_tf\" 0)
             )
             (progn
               (mode_tile \"wale_h\" 1)
               (mode_tile \"wale_b\" 1)
               (mode_tile \"wale_tw\" 1)
               (mode_tile \"wale_tf\" 1)
             )
           )
         )"
      )
      
      ;; 숫자 검증 - C.T.C
      (action_tile "ctc" 
        "(if (not (is-numeric $value))
           (progn
             (set_tile \"ctc\" (rtos *tsp-ctc* 2 2))
             (alert \"숫자만 입력 가능합니다!\")
           )
         )"
      )
      
      ;; OK 버튼
      (action_tile "accept"
        "(progn
           (setq hpile-idx (atoi (get_tile \"hpile_spec\")))
           (setq wale-idx (atoi (get_tile \"wale_spec\")))
           
           ;; H-Pile 규격 저장
           (cond
             ((= hpile-idx 0) (setq *tsp-hpile-spec* \"H 298×201×9/14\"))
             ((= hpile-idx 1) (setq *tsp-hpile-spec* \"H 300×300×10/15\"))
             ((= hpile-idx 2) (setq *tsp-hpile-spec* \"H 350×350×12/19\"))
             ((= hpile-idx 3) (setq *tsp-hpile-spec* \"H 400×400×13/21\"))
             ((= hpile-idx 4) 
              (progn
                (setq *tsp-hpile-spec* \"User-defined\")
                (setq *tsp-hpile-custom* (list
                  (atoi (get_tile \"hpile_h\"))
                  (atoi (get_tile \"hpile_b\"))
                  (atoi (get_tile \"hpile_tw\"))
                  (atoi (get_tile \"hpile_tf\"))
                ))
              )
             )
           )
           
           ;; 띠장 규격 저장
           (cond
             ((= wale-idx 0) (setq *tsp-wale-spec* \"H 298×201×9/14\"))
             ((= wale-idx 1) (setq *tsp-wale-spec* \"H 300×300×10/15\"))
             ((= wale-idx 2) (setq *tsp-wale-spec* \"H 350×350×12/19\"))
             ((= wale-idx 3) (setq *tsp-wale-spec* \"H 400×400×13/21\"))
             ((= wale-idx 4) 
              (progn
                (setq *tsp-wale-spec* \"User-defined\")
                (setq *tsp-wale-custom* (list
                  (atoi (get_tile \"wale_h\"))
                  (atoi (get_tile \"wale_b\"))
                  (atoi (get_tile \"wale_tw\"))
                  (atoi (get_tile \"wale_tf\"))
                ))
              )
             )
           )
           
           ;; C.T.C 저장
           (setq *tsp-ctc* (atof (get_tile \"ctc\")))
           
           ;; 토류판 두께 저장
           (setq *tsp-timber-thickness* (atoi (get_tile \"timber_thickness\")))
           
           (done_dialog 1)
         )"
      )
      
      ;; Cancel 버튼
      (action_tile "cancel" "(done_dialog 0)")
      
      ;; Dialog 표시
      (setq result (start_dialog))
      (debug-log (strcat "Dialog 결과: " (itoa result)))
      (debug-log (strcat "선택된 H-Pile: " *tsp-hpile-spec*))
      (debug-log (strcat "선택된 띠장: " *tsp-wale-spec*))
      (debug-log (strcat "C.T.C: " (rtos *tsp-ctc* 2 2)))
      (unload_dialog dcl-id)
      (debug-log "=== hpile-dialog-callback 완료 ===")
      
      result
    )
  )
)

;; main-dialog-callback 재정의
(defun main-dialog-callback (dcl-path / dcl-id result)
  (debug-log "=== main-dialog-callback 시작 ===")
  (setq dcl-id (load_dialog dcl-path))
  (debug-log (strcat "DCL 로드 ID: " (if dcl-id (itoa dcl-id) "nil")))
  
  (if (not (new_dialog "tsp_main" dcl-id))
    (progn
      (princ "\nDialog 로드 실패!")
      (debug-log "ERROR: tsp_main 다이얼로그 로드 실패")
      (unload_dialog dcl-id)
      nil
    )
    (progn
      (debug-log "tsp_main 다이얼로그 표시 준비 완료")
      ;; H-Pile 버튼
      (action_tile "btn_hpile"
        "(progn
           (done_dialog 1)
         )"
      )
      
      ;; CIP 버튼 (비활성화)
      (action_tile "btn_cip"
        "(alert \"CIP 공법은 아직 구현되지 않았습니다.\")"
      )
      
      ;; SCW 버튼 (비활성화)
      (action_tile "btn_scw"
        "(alert \"SCW 공법은 아직 구현되지 않았습니다.\")"
      )
      
      ;; Cancel 버튼
      (action_tile "cancel" "(done_dialog 0)")
      
      ;; Dialog 표시
      (setq result (start_dialog))
      (debug-log (strcat "메인 Dialog 결과: " (itoa result)))
      (unload_dialog dcl-id)
      (debug-log "=== main-dialog-callback 완료 ===")
      
      result
    )
  )
)

;; C:TSP 재정의
(defun C:TSP (/ dcl-path dcl-id main-result hpile-result boundary-ent)
  ;; 디버그 로그 초기화
  (debug-clear)
  
  (debug-log "========================================")
  (debug-log "TSP 명령 시작")
  (debug-log "버전: 1.0.0")
  (debug-log "날짜: 2026-01-13")
  (debug-log "========================================")
  
  (princ "\n========================================")
  (princ "\nTSP - Temporary Structure Plan")
  (princ "\n========================================\n")
  
  ;; DCL 파일 생성
  (setq dcl-path (create-tsp-dcl))
  (debug-log (strcat "DCL 파일 경로: " (if dcl-path dcl-path "nil")))
  
  (if (not dcl-path)
    (progn
      (princ "\nDCL 파일 생성 실패!")
      (exit)
    )
  )
  
  ;; DCL 파일 로드
  (setq dcl-id (load_dialog dcl-path))
  
  (if (not dcl-id)
    (progn
      (princ "\nDCL 로드 실패!")
      (exit)
    )
  )
  
  (unload_dialog dcl-id)
  
  ;; 메인 Dialog 표시
  (setq main-result (main-dialog-callback dcl-path))
  
  (cond
    ;; H-Pile + 토류판 선택
    ((= main-result 1)
     (princ "\n>>> H-Pile + 토류판 공법 선택\n")
     
     ;; H-Pile 설정 Dialog 표시
     (setq hpile-result (hpile-dialog-callback dcl-path))
     
     (if (= hpile-result 1)
       (progn
         (princ "\n설정 완료!")
         (princ (strcat "\n- H-Pile 규격: " *tsp-hpile-spec*))
         (princ (strcat "\n- 띠장 규격: " *tsp-wale-spec*))
         (princ (strcat "\n- C.T.C: " (rtos *tsp-ctc* 2 2) "m"))
         (princ (strcat "\n- 토류판 두께: " (itoa *tsp-timber-thickness*) "mm\n"))
         
         ;; 경계선 선택
         (princ "\n경계선을 선택하세요 (Polyline 또는 Line): ")
         (setq boundary-ent (car (entsel)))
         
         (if boundary-ent
           (progn
             (princ "\n경계선 선택 완료!")
             (debug-log (strcat "경계선 엔티티: " (vl-princ-to-string boundary-ent)))
             (debug-log (strcat "엔티티 타입: " (cdr (assoc 0 (entget boundary-ent)))))
             
             ;; 띠장 옵셋 생성
             (create-wale-offsets boundary-ent *tsp-wale-spec*)
             
             ;; H-Pile 세트 생성 (경계선의 가장 긴 세그먼트 중간에 생성)
             (princ "\n\n>>> H-Pile 세트 생성 시작...")
             (debug-log "=== H-Pile 세트 생성 시작 ===")
             (create-hpile-set-on-boundary boundary-ent *tsp-hpile-spec* *tsp-ctc*)
             
             (princ "\n========================================")
             (princ "\n작업 완료!")
             (princ "\n========================================\n")
             (debug-log "========================================")
             (debug-log "TSP 작업 완료")
             (debug-log "========================================")
             
             ;; 디버그 로그 출력 (옵션)
             ;; (debug-show-log)
           )
           (progn
             (princ "\n경계선 선택 취소됨\n")
             (debug-log "경계선 선택 취소됨")
           )
         )
       )
       (progn
         (princ "\n설정 취소됨\n")
         (debug-log "설정 취소됨")
       )
     )
    )
    
    ;; 취소
    (t
     (princ "\n취소됨\n")
     (debug-log "TSP 명령 취소됨")
    )
  )
  
  (princ)
)

;; get-longest-segment 재정의
(defun get-longest-segment (ent / ent-data ent-type vertices num-vertices i pt1 pt2 dist max-dist longest-seg)
  (debug-log "=== get-longest-segment 시작 ===")
  (setq ent-data (entget ent))
  (setq ent-type (cdr (assoc 0 ent-data)))
  (debug-log (strcat "엔티티 타입: " ent-type))
  
  (cond
    ;; LWPOLYLINE 처리
    ((= ent-type "LWPOLYLINE")
     (setq vertices '())
     (foreach item ent-data
       (if (= (car item) 10)
         (setq vertices (append vertices (list (cdr item))))
       )
     )
     
     (setq num-vertices (length vertices))
     (debug-log (strcat "정점 개수: " (itoa num-vertices)))
     (setq max-dist 0)
     (setq longest-seg nil)
     
     ;; 각 세그먼트의 길이 계산
     (setq i 0)
     (while (< i (- num-vertices 1))
       (setq pt1 (nth i vertices))
       (setq pt2 (nth (+ i 1) vertices))
       (setq dist (distance pt1 pt2))
       
       (if (> dist max-dist)
         (progn
           (setq max-dist dist)
           (setq longest-seg (list pt1 pt2))
         )
       )
       
       (setq i (+ i 1))
     )
     
     (debug-log (strcat "가장 긴 세그먼트 길이: " (rtos max-dist 2 2)))
     (debug-log "=== get-longest-segment 완료 ===")
     longest-seg
    )
    
    ;; LINE 처리
    ((= ent-type "LINE")
     (setq pt1 (cdr (assoc 10 ent-data)))
     (setq pt2 (cdr (assoc 11 ent-data)))
     (list pt1 pt2)
    )
    
    ;; 기타
    (t
     nil
    )
  )
)

;; create-hpile-set-on-boundary 재정의
(defun create-hpile-set-on-boundary (boundary-ent hpile-spec ctc / longest-seg pt1 pt2 mid-pt hpile-values h offset-pt)
  (debug-log "=== create-hpile-set-on-boundary 시작 ===")
  (debug-log (strcat "H-Pile 규격: " hpile-spec))
  (debug-log (strcat "C.T.C: " (rtos ctc 2 2) "m"))
  
  ;; 가장 긴 세그먼트 찾기
  (setq longest-seg (get-longest-segment boundary-ent))
  
  (if longest-seg
    (progn
      (setq pt1 (car longest-seg))
      (setq pt2 (cadr longest-seg))
      
      ;; 중점 계산
      (setq mid-pt (list
        (/ (+ (car pt1) (car pt2)) 2.0)
        (/ (+ (cadr pt1) (cadr pt2)) 2.0)
      ))
      
      (princ (strcat "\n가장 긴 세그먼트 길이: " (rtos (distance pt1 pt2) 2 2) "mm"))
      (princ (strcat "\n경계선 중점: (" (rtos (car mid-pt) 2 2) ", " (rtos (cadr mid-pt) 2 2) ")"))
      (debug-log (strcat "경계선 중점: (" (rtos (car mid-pt) 2 2) ", " (rtos (cadr mid-pt) 2 2) ")"))
      
      ;; H-Pile 높이 파싱 (하단을 경계선에 맞추기 위한 오프셋 계산)
      (if (= hpile-spec "User-defined")
        (setq hpile-values *tsp-hpile-custom*)
        (setq hpile-values (parse-h-spec hpile-spec))
      )
      
      (if hpile-values
        (progn
          (setq h (nth 0 hpile-values))  ; H-Pile 높이 (mm)
          
          ;; H-Pile 중심을 위로 올려서 하단이 경계선에 오도록 조정
          ;; 하단 플랜지 외부 모서리가 경계선에 오려면: cy = boundary_y + half-h
          (setq offset-pt (list
            (car mid-pt)
            (+ (cadr mid-pt) (/ h 2.0))  ; Y 좌표를 half-h만큼 위로
          ))
          
          (princ (strcat "\nH-Pile 높이: " (rtos h 2 0) "mm"))
          (princ (strcat "\nH-Pile 중심 (조정 후): (" (rtos (car offset-pt) 2 2) ", " (rtos (cadr offset-pt) 2 2) ")"))
          (princ (strcat "\n→ 하단 플랜지 Y 좌표: " (rtos (cadr mid-pt) 2 2) " (경계선)"))
          (debug-log (strcat "H-Pile 높이: " (rtos h 2 0) "mm"))
          (debug-log (strcat "H-Pile 중심 (조정 후): (" (rtos (car offset-pt) 2 2) ", " (rtos (cadr offset-pt) 2 2) ")"))
          (debug-log (strcat "하단 플랜지 Y 좌표: " (rtos (cadr mid-pt) 2 2) " (경계선)"))
          
          ;; 경계선 전체에 H-Pile+토류판 배치
          (place-hpile-timber-along-boundary boundary-ent hpile-spec ctc *tsp-timber-thickness*)
          (debug-log "=== place-hpile-timber-along-boundary 완료 ===")
        )
        (progn
          (princ "\n[ERROR] H-Pile 규격 파싱 실패!")
          (debug-log "ERROR: H-Pile 규격 파싱 실패")
          nil
        )
      )
    )
    (progn
      (princ "\n경계선에서 세그먼트를 찾을 수 없습니다!")
      (debug-log "ERROR: 세그먼트를 찾을 수 없음")
      nil
    )
  )
)

;; create-hpile-section 재정의
(defun create-hpile-section (insert-pt h b tw tf layer-name / half-h half-b half-tw half-tf cx cy pt1 pt2 pt3 pt3a pt3b pt4 pt4a pt4b pt5 pt6 pt7 pt8 pt9 pt9a pt9b pt10 pt10a pt10b pt11 pt12 pline-ent fillet-r)
  ;; insert-pt: 삽입 기준점 (중심)
  ;; h: 높이 (mm) = hh
  ;; b: 폭 (mm) = hb
  ;; tw: 웹 두께 (mm) = wb
  ;; tf: 플랜지 두께 (mm) = fb
  ;; layer-name: 레이어명
  
  (debug-log (strcat "create-hpile-section: H=" (rtos h 2 0) " B=" (rtos b 2 0) " tw=" (rtos tw 2 0) " tf=" (rtos tf 2 0)))
  
  ;; 반값 계산
  (setq half-h (/ h 2.0))
  (setq half-b (/ b 2.0))
  (setq half-tw (/ tw 2.0))
  (setq fillet-r (* tw 2.0))  ; 필렛 반지름 = 웹 두께 x 2
  
  (debug-log (strcat "half-h=" (rtos half-h 2 2) " half-b=" (rtos half-b 2 2) " half-tw=" (rtos half-tw 2 2) " fillet-r=" (rtos fillet-r 2 2)))
  
  ;; 중심 좌표 추출
  (setq cx (car insert-pt))
  (setq cy (cadr insert-pt))
  
  (debug-log (strcat "중심점: cx=" (rtos cx 2 2) " cy=" (rtos cy 2 2)))
  
  ;; ============================================================
  ;; 12개 기본 점 (필렛 전) - 반시계방향 CCW
  ;; ============================================================
  
  ;; 상단 플랜지 - 우측
  (setq pt1  (list (+ cx half-b) (+ cy half-h)))           ; ① 우상단 외부
  (setq pt2  (list (+ cx half-b) (+ cy (- half-h tf))))    ; ② 우상단 내부
  (setq pt3  (list (+ cx half-tw) (+ cy (- half-h tf))))   ; ③ 웹 우상단 (필렛 모서리)
  
  ;; 웹 우측 + 하단 플랜지 우측
  (setq pt9  (list (+ cx half-tw) (- cy (- half-h tf))))   ; ⑨ 웹 우하단 (필렛 모서리)
  (setq pt8  (list (+ cx half-b) (- cy (- half-h tf))))    ; ⑧ 우하단 내부
  (setq pt7  (list (+ cx half-b) (- cy half-h)))           ; ⑦ 우하단 외부
  
  ;; 하단 플랜지 - 좌측
  (setq pt12 (list (- cx half-b) (- cy half-h)))           ; ⑫ 좌하단 외부
  (setq pt11 (list (- cx half-b) (- cy (- half-h tf))))    ; ⑪ 좌하단 내부
  (setq pt10 (list (- cx half-tw) (- cy (- half-h tf))))   ; ⑩ 웹 좌하단 (필렛 모서리)
  
  ;; 웹 좌측 + 상단 플랜지 좌측
  (setq pt4  (list (- cx half-tw) (+ cy (- half-h tf))))   ; ④ 웹 좌상단 (필렛 모서리)
  (setq pt5  (list (- cx half-b) (+ cy (- half-h tf))))    ; ⑤ 좌상단 내부
  (setq pt6  (list (- cx half-b) (+ cy half-h)))           ; ⑥ 좌상단 외부
  
  (debug-log "12개 기본 점 계산 완료")
  
  ;; ============================================================
  ;; 필렛 적용: 4개 모서리에 반지름 18mm 필렛
  ;; ============================================================
  
  ;; 모서리 ③ (우상단): ②→③ (수평) + ③→⑨ (수직)
  (setq pt3a (list (+ cx half-tw fillet-r) (+ cy (- half-h tf))))      ; ③a 필렛 시작
  (setq pt3b (list (+ cx half-tw) (+ cy (- half-h tf fillet-r))))      ; ③b 필렛 끝
  
  ;; 모서리 ⑨ (우하단): ③→⑨ (수직) + ⑨→⑧ (수평)
  (setq pt9a (list (+ cx half-tw) (- cy (- half-h tf fillet-r))))      ; ⑨a 필렛 시작
  (setq pt9b (list (+ cx half-tw fillet-r) (- cy (- half-h tf))))      ; ⑨b 필렛 끝
  
  ;; 모서리 ⑩ (좌하단): ⑪→⑩ (수평) + ⑩→④ (수직)
  (setq pt10a (list (- cx half-tw fillet-r) (- cy (- half-h tf))))     ; ⑩a 필렛 시작
  (setq pt10b (list (- cx half-tw) (- cy (- half-h tf fillet-r))))     ; ⑩b 필렛 끝
  
  ;; 모서리 ④ (좌상단): ⑩→④ (수직) + ④→⑤ (수평)
  (setq pt4a (list (- cx half-tw) (+ cy (- half-h tf fillet-r))))      ; ④a 필렛 시작
  (setq pt4b (list (- cx half-tw fillet-r) (+ cy (- half-h tf))))      ; ④b 필렛 끝
  
  (debug-log "필렛 점 계산 완료")
  (debug-log (strcat "필렛 반지름: " (rtos fillet-r 2 2) "mm"))
  
  ;; ============================================================
  ;; 폴리라인 생성 (20개 점, CCW)
  ;; ============================================================
  (entmake
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbPolyline")
      (cons 8 layer-name)
      (cons 62 3)  ; 색상: 초록(3)
      '(90 . 20)   ; 정점 개수: 20개
      '(70 . 1)    ; 닫힘 플래그
      (cons 10 pt1)    ; ① 우상 외부
      (cons 10 pt2)    ; ② 우상 내부
      (cons 10 pt3a)   ; ③a 필렛 시작 (우상)
      (cons 42 0.4142135623730951)  ; 필렛 1
      (cons 10 pt3b)   ; ③b 필렛 끝
      (cons 10 pt9a)   ; ⑨a 필렛 시작 (우하)
      (cons 42 0.4142135623730951)  ; 필렛 2
      (cons 10 pt9b)   ; ⑨b 필렛 끝
      (cons 10 pt8)    ; ⑧ 우하 내부
      (cons 10 pt7)    ; ⑦ 우하 외부
      (cons 10 pt12)   ; ⑫ 좌하 외부
      (cons 10 pt11)   ; ⑪ 좌하 내부
      (cons 10 pt10a)  ; ⑩a 필렛 시작 (좌하)
      (cons 42 0.4142135623730951)  ; 필렛 3
      (cons 10 pt10b)  ; ⑩b 필렛 끝
      (cons 10 pt4a)   ; ④a 필렛 시작 (좌상)
      (cons 42 0.4142135623730951)  ; 필렛 4
      (cons 10 pt4b)   ; ④b 필렛 끝
      (cons 10 pt5)    ; ⑤ 좌상 내부
      (cons 10 pt6)    ; ⑥ 좌상 외부
    )
  )
  
  (setq pline-ent (entlast))
  
  (if pline-ent
    (debug-log (strcat "H-Pile 단면 생성 완료, 엔티티: " (vl-princ-to-string pline-ent)))
    (debug-log "ERROR: H-Pile 폴리라인 생성 실패")
  )
  
  pline-ent
)

;; create-timber-panel 재정의
(defun create-timber-panel (pt1 pt2 width h tf / mid-pt dx dy length panel-length panel-pt1 panel-pt2 panel-pt3 panel-pt4 pline-ent perp-dx perp-dy half-h flange-top-y panel-height timber-obj doc mspace hatch-obj sa)
  ;; pt1, pt2: H-Pile 중심점
  ;; width: 토류판 두께 (70mm)
  ;; h: H-Pile 높이 (mm)
  ;; tf: 플랜지 두께 (mm)
  
  (debug-log "=== create-timber-panel 시작 ===")
  (debug-log (strcat "토류판 두께: " (rtos width 2 0) "mm"))
  
  ;; H-Pile 하단 플랜지 상단 Y 좌표 계산
  (setq half-h (/ h 2.0))
  (setq flange-top-y (- (cadr pt1) (- half-h tf)))  ; cy - (half-h - tf)
  
  ;; 토류판 높이 = 토류판 두께 = width (70mm)
  (setq panel-height width)
  
  (princ (strcat "\n[토류판] H-Pile 하단 플랜지 상단 Y: " (rtos flange-top-y 2 2)))
  (princ (strcat "\n[토류판] 토류판 하단 Y: " (rtos flange-top-y 2 2)))
  (princ (strcat "\n[토류판] 토류판 상단 Y: " (rtos (+ flange-top-y panel-height) 2 2)))
  (debug-log (strcat "H-Pile 하단 플랜지 상단 Y: " (rtos flange-top-y 2 2)))
  (debug-log (strcat "토류판 하단 Y: " (rtos flange-top-y 2 2)))
  (debug-log (strcat "토류판 상단 Y: " (rtos (+ flange-top-y panel-height) 2 2)))
  
  ;; 두 점 사이의 중점 계산 (X 방향만)
  (setq mid-pt (list
    (/ (+ (car pt1) (car pt2)) 2.0)
    (cadr pt1)  ; Y는 중심점 그대로 사용 (나중에 flange-top-y로 조정)
  ))
  
  ;; 방향 벡터
  (setq dx (- (car pt2) (car pt1)))
  (setq dy (- (cadr pt2) (cadr pt1)))
  (setq length (sqrt (+ (* dx dx) (* dy dy))))
  
  ;; 정규화
  (setq dx (/ dx length))
  (setq dy (/ dy length))
  
  ;; 토류판 길이 = C.T.C - 50 (양쪽 25mm씩 여유)
  (setq panel-length (- length 50))
  
  ;; 토류판 4개 점 계산
  ;; 토류판 수평 길이 = C.T.C - 50 (양쪽 25mm씩 여유)
  ;; 토류판 수직 높이 = 1000mm
  
  (debug-log "토류판 4개 꼭지점 계산 시작")
  
  ;; 토류판의 4개 꼭지점 (직사각형)
  ;; 하단 왼쪽: 왼쪽 H-Pile에서 25mm 오른쪽
  (setq panel-pt1 (list
    (+ (car pt1) 25.0)
    flange-top-y
  ))
  
  ;; 하단 오른쪽: 오른쪽 H-Pile에서 25mm 왼쪽
  (setq panel-pt2 (list
    (- (car pt2) 25.0)
    flange-top-y
  ))
  
  ;; 상단 오른쪽
  (setq panel-pt3 (list
    (- (car pt2) 25.0)
    (+ flange-top-y panel-height)
  ))
  
  ;; 상단 왼쪽
  (setq panel-pt4 (list
    (+ (car pt1) 25.0)
    (+ flange-top-y panel-height)
  ))
  
  (debug-log (strcat "panel-pt1 X: " (rtos (car panel-pt1) 2 2)))
  (debug-log (strcat "panel-pt2 X: " (rtos (car panel-pt2) 2 2)))
  (debug-log (strcat "토류판 X 방향 길이: " (rtos (- (car panel-pt2) (car panel-pt1)) 2 2) "mm"))
  
  ;; 폴리라인 생성 (entmake 사용)
  ;; 순서: 하단 왼쪽 → 하단 오른쪽 → 상단 오른쪽 → 상단 왼쪽
  (entmake
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbPolyline")
      '(8 . "_토류판(timber)")
      '(62 . 1)  ; 색상: 빨강(1)
      '(90 . 4)  ; 정점 개수
      '(70 . 1)  ; 닫힘 플래그
      (cons 10 panel-pt1)  ; 하단 왼쪽
      (cons 10 panel-pt2)  ; 하단 오른쪽
      (cons 10 panel-pt3)  ; 상단 오른쪽
      (cons 10 panel-pt4)  ; 상단 왼쪽
    )
  )
  
  (setq pline-ent (entlast))
  
  ;; 해치 생성
  (if pline-ent
    (progn
      (debug-log "해치 생성 시작 (ANSI36, 축척 30, 레이어 _토류판(timber), 색상 9)")
      (command "._-BHATCH"
        "_P" "ANSI36"
        "30"
        "0"
        "_LA" "_토류판(timber)"  ; 레이어 설정
        "_C" "9"                  ; 색상 9번 (회색)
        "_SEL" pline-ent ""
        ""
      )
      (debug-log "토류판 및 해치 생성 완료")
    )
    (debug-log "ERROR: 토류판 폴리라인 생성 실패")
  )
  
  (debug-log "=== create-timber-panel 완료 ===")
  pline-ent
)

;; create-hpile-set 재정의
(defun create-hpile-set (pt1 hpile-spec ctc / h b tw tf layer-name pt2 hpile1 hpile2 timber hpile-values)
  ;; pt1: 첫 번째 H-Pile 중심점
  ;; hpile-spec: H-Pile 규격 문자열
  ;; ctc: C.T.C 거리 (m → mm 변환 필요)
  
  (debug-log "=== create-hpile-set 시작 ===")
  (debug-log (strcat "중심점: (" (rtos (car pt1) 2 2) ", " (rtos (cadr pt1) 2 2) ")"))
  (debug-log (strcat "H-Pile 규격: " hpile-spec))
  (debug-log (strcat "C.T.C: " (rtos ctc 2 2) "m"))
  
  ;; 규격 파싱
  (princ (strcat "\n[DEBUG] H-Pile 규격 문자열: \"" hpile-spec "\""))
  
  (if (= hpile-spec "User-defined")
    (setq hpile-values *tsp-hpile-custom*)
    (setq hpile-values (parse-h-spec hpile-spec))
  )
  
  (princ (strcat "\n[DEBUG] 파싱 결과: " (if hpile-values (vl-princ-to-string hpile-values) "nil")))
  
  (if (not hpile-values)
    (progn
      (princ "\n[ERROR] 규격 파싱 실패!")
      (exit)
    )
  )
  
  (setq h (nth 0 hpile-values))
  (setq b (nth 1 hpile-values))
  (setq tw (nth 2 hpile-values))
  (setq tf (nth 3 hpile-values))
  
  ;; 레이어명 생성 (간단한 형식으로 변경)
  (setq layer-name (strcat "_H-Pile_" 
                           (itoa h) "x" 
                           (itoa b) "x" 
                           (itoa tw) "-" 
                           (itoa tf)))
  
  ;; 레이어 생성
  (create-layer-if-not-exists layer-name "3")
  (create-layer-if-not-exists "_토류판(timber)" "1")
  
  (princ (strcat "\nH-Pile 규격: H=" (rtos h 2 0) " B=" (rtos b 2 0) " tw=" (rtos tw 2 0) " tf=" (rtos tf 2 0)))
  (princ (strcat "\nC.T.C: " (rtos (* ctc 1000) 2 0) "mm"))
  
  ;; 두 번째 H-Pile 중심점 계산 (수평 방향으로 C.T.C 거리)
  (setq pt2 (list (+ (car pt1) (* ctc 1000)) (cadr pt1)))
  
  ;; 토류판 생성 (먼저 생성하여 H-Pile 아래에 배치)
  (princ "\n토류판 생성...")
  (setq timber (create-timber-panel pt1 pt2 *tsp-timber-thickness* h tf))
  
  ;; 첫 번째 H-Pile 생성
  (princ "\n첫 번째 H-Pile 생성...")
  (setq hpile1 (create-hpile-section pt1 h b tw tf layer-name))
  
  ;; 두 번째 H-Pile 생성
  (princ "\n두 번째 H-Pile 생성...")
  (setq hpile2 (create-hpile-section pt2 h b tw tf layer-name))
  
  (princ "\nH-Pile 세트 생성 완료!")
  (debug-log "=== create-hpile-set 완료 ===")
  
  (list hpile1 hpile2 timber)
)

;;; ----------------------------------------------------------------------
;;; 블록 생성 함수
;;; ----------------------------------------------------------------------

;; 토류판 블록 생성 (해치 포함)
(defun create-timber-panel-block (width height / timber-pline hatch-ent block-name half-width half-height timber-obj doc mspace hatch-obj sa old-ucs old-osnap)
  ;; width: 토류판 수평 길이 (C.T.C - 50mm)
  ;; height: 토류판 높이 (thickness, 예: 60mm)
  
  (debug-log "=== create-timber-panel-block 시작 ===")
  
  ;; 블록명에 규격 포함 (간단한 형식)
  (setq block-name (strcat "_TIMBER(" 
                           (itoa (fix width)) "x" 
                           (itoa (fix height)) ")"))
  
  (debug-log (strcat "블록명: " block-name))
  
  ;; 레이어 생성
  (create-layer-if-not-exists "_토류판(timber)" "1")
  
  ;; 블록이 이미 존재하는지 확인
  (if (tblsearch "BLOCK" block-name)
    (progn
      (debug-log "블록이 이미 존재함, 재사용")
      block-name  ; 블록명 반환
    )
    (progn
      ;; 블록이 없으면 생성
      
      ;; ===== UCS/OSNAP 저장 및 WCS로 변경 =====
      (setq old-ucs (getvar "UCSNAME"))
      (setq old-osnap (getvar "OSMODE"))
      (debug-log (strcat "기존 UCS: " (if old-ucs old-ucs "WORLD") ", OSNAP: " (itoa old-osnap)))
      
      ;; UCS를 World로 변경
      (command "._UCS" "_W")
      ;; OSNAP 끄기
      (setvar "OSMODE" 0)
      (debug-log "UCS → WORLD, OSNAP → OFF")
  
  ;; 토류판 폴리라인 생성 (원점 기준, 중심이 원점)
  ;; 토류판 중심을 (0,0)으로 하기 위해 좌표 조정
  (setq half-width (/ width 2.0))
  (setq half-height (/ height 2.0))
  
  (entmake
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(8 . "_토류판(timber)")  ; 레이어
      '(62 . 1)  ; 색상: 빨강
      '(100 . "AcDbPolyline")
      '(90 . 4)  ; 정점 개수
      '(70 . 1)  ; 닫힘 플래그
      (cons 10 (list (- half-width) (- half-height)))  ; 좌하
      (cons 10 (list half-width (- half-height)))      ; 우하
      (cons 10 (list half-width half-height))          ; 우상
      (cons 10 (list (- half-width) half-height))      ; 좌상
    )
  )
  
  (setq timber-pline (entlast))
  (debug-log "토류판 폴리라인 생성 완료")
  
  ;; 해치 생성 (VLA 방식)
  (setq timber-obj (vlax-ename->vla-object timber-pline))
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (setq mspace (vla-get-modelspace doc))
  
  ;; 해치 객체 생성 (PatternType: 1 = Predefined)
  (setq hatch-obj (vla-addhatch mspace 1 "ANSI36" :vlax-true))
  
  ;; 해치 패턴 속성 설정
  (vla-put-patternscale hatch-obj 30.0)
  (vla-put-patternangle hatch-obj 0.0)
  (vla-put-layer hatch-obj "_토류판(timber)")
  (vla-put-color hatch-obj 9)
  
  ;; SafeArray 생성 및 채우기 (vlax-vbObject = 9)
  (setq sa (vlax-make-safearray 9 (cons 0 0)))
  (vlax-safearray-fill sa (list timber-obj))
  
  ;; 외부 경계 추가
  (vla-appendouterloop hatch-obj sa)
  
  ;; 해치 평가
  (vla-evaluate hatch-obj)
  
      (setq hatch-ent (vlax-vla-object->ename hatch-obj))
      (debug-log "해치 생성 완료 (VLA)")
      
      ;; 블록 생성
      (command "._-BLOCK"
        block-name    ; 블록명
        '(0 0 0)      ; 기준점 (원점) - 리스트로 전달
        timber-pline  ; 토류판 폴리라인
        hatch-ent     ; 해치
        ""
      )
      
      (debug-log (strcat "토류판 블록 생성 완료: " block-name))
      
      ;; ===== UCS/OSNAP 복원 =====
      (if old-ucs
        (command "._UCS" "_R" old-ucs)
        (command "._UCS" "_W")
      )
      (setvar "OSMODE" old-osnap)
      (debug-log "UCS/OSNAP 복원 완료")
      
      block-name
    )
  )
)

;; H-Pile 블록 생성 (아래 플랜지 중심에 POINT 포함)
(defun create-hpile-section-block (h b tw tf / hpile-ent point-ent block-name layer-name old-ucs old-osnap half-h flange-center)
  ;; h: 높이, b: 폭, tw: 웹 두께, tf: 플랜지 두께
  
  (debug-log "=== create-hpile-section-block 시작 ===")
  
  ;; 블록명에 규격 포함 (간단한 형식)
  (setq block-name (strcat "_HPILE(" 
                           (itoa h) "x" 
                           (itoa b) "x" 
                           (itoa tw) "x" 
                           (itoa tf) ")"))
  
  (debug-log (strcat "블록명: " block-name))
  
  ;; H-Pile 객체용 레이어명 생성
  (setq layer-name (strcat "_H-Pile_" 
                           (itoa h) "x" 
                           (itoa b) "x" 
                           (itoa tw) "-" 
                           (itoa tf)))
  
  ;; 레이어 생성
  (create-layer-if-not-exists layer-name "3")
  (create-layer-if-not-exists "_측면말뚝" "3")
  
  ;; 블록이 이미 존재하는지 확인
  (if (tblsearch "BLOCK" block-name)
    (progn
      (debug-log "블록이 이미 존재함, 재사용")
      block-name  ; 블록명 반환
    )
    (progn
      ;; 블록이 없으면 생성
      
      ;; ===== UCS/OSNAP 저장 및 WCS로 변경 =====
      (setq old-ucs (getvar "UCSNAME"))
      (setq old-osnap (getvar "OSMODE"))
      (debug-log (strcat "기존 UCS: " (if old-ucs old-ucs "WORLD") ", OSNAP: " (itoa old-osnap)))
      
      ;; UCS를 World로 변경
      (command "._UCS" "_W")
      ;; OSNAP 끄기
      (setvar "OSMODE" 0)
      (debug-log "UCS → WORLD, OSNAP → OFF")
  
      ;; H-Pile 단면 생성 (중심이 원점)
      (setq hpile-ent (create-hpile-section '(0 0) h b tw tf layer-name))
      (debug-log "H-Pile 단면 생성 완료")
      
      ;; 아래 플랜지 중심 계산 (바로 서있는 H-Pile)
      ;; H-Pile 중심이 (0,0)이고, 아래 플랜지 중심 = (0, -half-h)
      (setq half-h (/ h 2.0))
      (setq flange-center (list 0.0 (- half-h) 0.0))
      
      ;; POINT 생성 (아래 플랜지 중심)
      (entmake
        (list
          '(0 . "POINT")
          '(8 . "_측면말뚝")
          (cons 10 flange-center)
        )
      )
      (setq point-ent (entlast))
      (debug-log (strcat "아래 플랜지 중심 POINT 생성: (0, " (rtos (- half-h) 2 2) ")"))
      
      ;; 블록 생성 (기준점 = 아래 플랜지 중심)
      (command "._-BLOCK"
        block-name      ; 블록명
        flange-center   ; 기준점 = 아래 플랜지 중심 POINT
        hpile-ent       ; H-Pile 단면
        point-ent       ; POINT
        ""
      )
      
      (debug-log (strcat "H-Pile 블록 생성 완료 (기준점: 위 플랜지 중심): " block-name))
      
      ;; ===== UCS/OSNAP 복원 =====
      (if old-ucs
        (command "._UCS" "_R" old-ucs)
        (command "._UCS" "_W")
      )
      (setvar "OSMODE" old-osnap)
      (debug-log "UCS/OSNAP 복원 완료")
      
      block-name
    )
  )
)

;; 토류판 개별 객체 생성 (블록 대신 직접 생성)
(defun create-timber-panel-object (center-pt width height angle / half-width half-height cos-a sin-a pt1 pt2 pt3 pt4 timber-pline timber-obj doc mspace hatch-obj sa)
  ;; center-pt: 중심점 (삽입점)
  ;; width: 토류판 수평 길이 (C.T.C - 50mm)
  ;; height: 토류판 높이 (thickness, 예: 60mm)
  ;; angle: 회전 각도 (라디안)
  
  (setq half-width (/ width 2.0))
  (setq half-height (/ height 2.0))
  
  ;; 회전 행렬 계산
  (setq cos-a (cos angle))
  (setq sin-a (sin angle))
  
  ;; 중심점 기준 4개 꼭지점 계산 (회전 적용)
  ;; 좌하: (-half-width, -half-height)
  (setq pt1 (list
    (+ (car center-pt) (- (* (- half-width) cos-a) (* (- half-height) sin-a)))
    (+ (cadr center-pt) (+ (* (- half-width) sin-a) (* (- half-height) cos-a)))
    0.0
  ))
  
  ;; 우하: (half-width, -half-height)
  (setq pt2 (list
    (+ (car center-pt) (- (* half-width cos-a) (* (- half-height) sin-a)))
    (+ (cadr center-pt) (+ (* half-width sin-a) (* (- half-height) cos-a)))
    0.0
  ))
  
  ;; 우상: (half-width, half-height)
  (setq pt3 (list
    (+ (car center-pt) (- (* half-width cos-a) (* half-height sin-a)))
    (+ (cadr center-pt) (+ (* half-width sin-a) (* half-height cos-a)))
    0.0
  ))
  
  ;; 좌상: (-half-width, half-height)
  (setq pt4 (list
    (+ (car center-pt) (- (* (- half-width) cos-a) (* half-height sin-a)))
    (+ (cadr center-pt) (+ (* (- half-width) sin-a) (* half-height cos-a)))
    0.0
  ))
  
  ;; 폴리라인 생성
  (entmake
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(8 . "_토류판(timber)")
      '(62 . 1)  ; 색상: 빨강
      '(100 . "AcDbPolyline")
      '(90 . 4)  ; 정점 개수
      '(70 . 1)  ; 닫힘 플래그
      (cons 10 (list (car pt1) (cadr pt1)))  ; 좌하
      (cons 10 (list (car pt2) (cadr pt2)))  ; 우하
      (cons 10 (list (car pt3) (cadr pt3)))  ; 우상
      (cons 10 (list (car pt4) (cadr pt4)))  ; 좌상
    )
  )
  
  (setq timber-pline (entlast))
  
  ;; 해치 생성 (VLA 방식)
  (if timber-pline
    (progn
      (setq timber-obj (vlax-ename->vla-object timber-pline))
      (setq doc (vla-get-activedocument (vlax-get-acad-object)))
      (setq mspace (vla-get-modelspace doc))
      
      ;; 해치 객체 생성
      (setq hatch-obj (vla-addhatch mspace 1 "ANSI36" :vlax-true))
      
      ;; 해치 속성 설정
      (vla-put-patternscale hatch-obj 30.0)
      (vla-put-patternangle hatch-obj 0.0)
      (vla-put-layer hatch-obj "_토류판(timber)")
      (vla-put-color hatch-obj 9)
      
      ;; SafeArray 생성
      (setq sa (vlax-make-safearray 9 (cons 0 0)))
      (vlax-safearray-fill sa (list timber-obj))
      
      ;; 외부 경계 추가
      (vla-appendouterloop hatch-obj sa)
      
      ;; 해치 평가
      (vla-evaluate hatch-obj)
    )
  )
  
  timber-pline
)

;; 모서리(꼭지점)에 H-Pile 배치 (블록 기준점 사용 - 아래 플랜지 중심)
;; timber-offset: 토류판 오프셋 거리 (띠장까지의 거리)
;; boundary-orient: 경계선 방향 (1=CCW, -1=CW)
(defun place-hpile-at-corner-simple (vertex angle1 angle2 h b tw tf hpile-block timber-offset boundary-orient prev-vertex next-vertex / 
  interior-angle exterior-angle bisector-angle wale-corner-angle is-convex half-h half-b 
  hpile-rotation insert-point offset-dir bisector-end wale-corner prev-dir next-dir
  prev-wale-pt next-wale-pt wale-dir-angle)
  
  ;; H-Pile 크기
  (setq half-h (/ h 2.0))  ; 149 mm
  (setq half-b (/ b 2.0))  ; 100.5 mm
  
  ;; 디버그: 꼭지점 및 각도 출력
  (princ (strcat "\n[모서리] 꼭지점: (" (rtos (car vertex) 2 2) ", " (rtos (cadr vertex) 2 2) ")"))
  (princ (strcat "\n  angle1=" (rtos (* angle1 (/ 180.0 pi)) 2 1) "도, angle2=" (rtos (* angle2 (/ 180.0 pi)) 2 1) "도"))
  
  ;; ★★★ 벡터 외적으로 볼록/오목 판단 ★★★
  (setq is-convex (is-corner-convex prev-vertex vertex next-vertex boundary-orient))
  (princ (if is-convex "\n  타입: 볼록(Convex)" "\n  타입: 오목(Concave)"))
  
  ;; 각의 이등분선 계산 (참고용)
  (setq exterior-angle (- angle2 angle1))
  (if (< exterior-angle 0)
    (setq exterior-angle (+ exterior-angle (* 2 pi)))
  )
  (setq interior-angle (- (* 2 pi) exterior-angle))
  (setq bisector-angle (+ angle1 (/ interior-angle 2.0)))
  (princ (strcat "\n  기하학 이등분선=" (rtos (* bisector-angle (/ 180.0 pi)) 2 1) "도"))
  
  ;; ★★★ 띠장 모서리 방향 계산 (경계선 방향 고려) ★★★
  ;; 띠장 = 경계선에서 수직으로 timber-offset만큼 떨어진 선
  ;; CCW: 진행방향의 왼쪽(+90°)이 바깥쪽
  ;; CW: 진행방향의 오른쪽(-90°)이 바깥쪽
  (setq prev-dir (+ angle1 (* boundary-orient (/ pi 2.0))))
  (setq prev-wale-pt (polar vertex prev-dir timber-offset))
  
  (setq next-dir (+ angle2 (* boundary-orient (/ pi 2.0))))
  (setq next-wale-pt (polar vertex next-dir timber-offset))
  
  ;; 띠장 모서리 = 두 오프셋 점의 중점 (근사값)
  (setq wale-corner (list
    (/ (+ (car prev-wale-pt) (car next-wale-pt)) 2.0)
    (/ (+ (cadr prev-wale-pt) (cadr next-wale-pt)) 2.0)
    0.0
  ))
  
  ;; 경계선 꼭지점 → 띠장 모서리 방향
  (setq wale-corner-angle (angle vertex wale-corner))
  (princ (strcat "\n  띠장 모서리 방향=" (rtos (* wale-corner-angle (/ 180.0 pi)) 2 1) "도"))
  
  ;; H-Pile 회전 각도 및 삽입점 계산
  ;; 블록 기준점 = 아래 플랜지 중심
  ;; 웹 방향이 경계선 바깥쪽(띠장 모서리 반대 방향)을 향해야 함
  (if is-convex
    (progn
      ;; 볼록 모서리: 회전 = 띠장방향 + 90도 (반대 방향)
      (setq insert-point vertex)
      (setq hpile-rotation (+ wale-corner-angle (/ pi 2.0)))
    )
    (progn
      ;; 오목 모서리: B/2 바깥쪽 오프셋, 회전 = 띠장방향 + 90도 (볼록과 동일)
      (setq offset-dir (+ wale-corner-angle pi))
      (setq insert-point (polar vertex offset-dir half-b))
      (setq hpile-rotation (+ wale-corner-angle (/ pi 2.0)))
    )
  )
  
  ;; 회전 각도 정규화 (0 ~ 2*pi)
  (while (< hpile-rotation 0)
    (setq hpile-rotation (+ hpile-rotation (* 2 pi)))
  )
  (while (>= hpile-rotation (* 2 pi))
    (setq hpile-rotation (- hpile-rotation (* 2 pi)))
  )
  
  ;; 로그 출력
  (if is-convex
    (princ (strcat "\n  H-Pile 회전=" (rtos (* hpile-rotation (/ 180.0 pi)) 2 1) "도 (띠장방향+90)"))
    (princ (strcat "\n  오목: B/2 오프셋=" (rtos half-b 2 2) "mm, 회전=" (rtos (* hpile-rotation (/ 180.0 pi)) 2 1) "도 (띠장방향+90)"))
  )
  
  ;; (시각화 제거됨)
  
  ;; H-Pile 블록 INSERT
  (entmake
    (list
      '(0 . "INSERT")
      (cons 2 hpile-block)
      '(8 . "_측면말뚝")
      (cons 10 insert-point)
      '(41 . 1.0)
      '(42 . 1.0)
      '(43 . 1.0)
      (cons 50 hpile-rotation)
    )
  )
)

;;; ----------------------------------------------------------------------
;;; 경계선 따라 배치 함수
;;; ----------------------------------------------------------------------

;; 경계선을 따라 H-Pile+토류판 배치 (새로운 로직)
(defun place-hpile-timber-along-boundary (boundary-ent hpile-spec ctc timber-thickness / 
  h b tw tf hpile-values hpile-block timber-width half-h 
  timber-offset hpile-offset boundary-vla offset-obj offset-vla exploded-lines
  line-ent line-data pt1 pt2 mid-pt seg-angle angle-deg original-area offset-area
  last-before first-new current-ent ent-data delete-count
  seg-length ctc-mm half-length num-left num-right point-list i dist new-pt
  dist-from-pt1 dist-from-pt2 pt vertices vertex boundary-data item
  boundary-copy last-before-boundary first-new-boundary boundary-lines
  boundary-orient offset-sign user-pt left-test-pt v1 v2 cross-z
  outward-normal insert-pt hpile-positions hpile-pt hpile-rotation)
  
  (debug-log "=== place-hpile-timber-along-boundary 시작 (새로운 로직) ===")
  
  ;; ===== 경계선 타입 판별 및 방향 결정 =====
  (princ "\n\n[0단계] 경계선 타입 및 방향 판단...")
  (setq vertices (extract-vertices boundary-ent))
  
  (if (is-closed-polyline boundary-ent)
    ;; 폐합선: 자동으로 방향 계산
    (progn
      (princ "\n→ 폐합 다각형 감지")
      (setq boundary-orient (get-polygon-orientation vertices))
      (if (= boundary-orient 1)
        (princ "\n→ CCW(반시계): 외부 방향 자동 계산 (왼쪽 = 바깥)")
        (princ "\n→ CW(시계): 외부 방향 자동 계산 (오른쪽 = 바깥)")
      )
      (debug-log (strcat "폐합선 - 자동 방향: " (if (= boundary-orient 1) "CCW" "CW")))
    )
    
    ;; 열린선: 사용자에게 클릭 요청
    (progn
      (princ "\n→ 열린 경계선 감지")
      (princ "\n경계선 바깥쪽(파일 설치 위치)을 클릭하세요: ")
      (setq user-pt (getpoint))
      
      ;; 첫 번째 선분 기준으로 좌/우 판별
      (setq pt1 (car vertices))
      (setq pt2 (cadr vertices))
      (setq seg-angle (angle pt1 pt2))
      (setq mid-pt (polar pt1 seg-angle (/ (distance pt1 pt2) 2.0)))
      
      ;; 왼쪽 테스트 포인트 (진행방향의 왼쪽)
      (setq left-test-pt (polar mid-pt (+ seg-angle (/ pi 2.0)) 1000.0))
      
      ;; 외적으로 좌/우 판별
      (setq v1 (list (- (car user-pt) (car mid-pt)) 
                     (- (cadr user-pt) (cadr mid-pt))))
      (setq v2 (list (- (car left-test-pt) (car mid-pt)) 
                     (- (cadr left-test-pt) (cadr mid-pt))))
      (setq cross-z (- (* (car v1) (cadr v2)) 
                       (* (cadr v1) (car v2))))
      
      (if (> cross-z 0)
        (progn
          (setq boundary-orient 1)   ; 왼쪽 클릭 = CCW
          (princ "\n→ 사용자 선택: 진행방향의 왼쪽 (CCW 방향)")
        )
        (progn
          (setq boundary-orient -1)  ; 오른쪽 클릭 = CW
          (princ "\n→ 사용자 선택: 진행방향의 오른쪽 (CW 방향)")
        )
      )
      (debug-log (strcat "열린선 - 사용자 선택: " (if (= boundary-orient 1) "왼쪽(CCW)" "오른쪽(CW)")))
    )
  )
  
  ;; 오프셋 부호 결정
  ;; boundary-orient: 1 = CCW (왼쪽=바깥), -1 = CW (오른쪽=바깥)
  ;; AutoCAD의 양수 오프셋은 진행방향의 왼쪽
  (setq offset-sign boundary-orient)
  (debug-log (strcat "오프셋 부호: " (if (= offset-sign 1) "+" "-")))
  
  ;; H-Pile 규격 파싱
  (if (= hpile-spec "User-defined")
    (setq hpile-values *tsp-hpile-custom*)
    (setq hpile-values (parse-h-spec hpile-spec))
  )
  
  (setq h (nth 0 hpile-values))
  (setq b (nth 1 hpile-values))
  (setq tw (nth 2 hpile-values))
  (setq tf (nth 3 hpile-values))
  
  (princ (strcat "\n경계선 따라 배치 시작 (새로운 방식)..."))
  (princ (strcat "\nH-Pile: H=" (rtos h 2 0) " B=" (rtos b 2 0) " tw=" (rtos tw 2 0) " tf=" (rtos tf 2 0)))
  (princ (strcat "\nC.T.C: " (rtos (* ctc 1000) 2 0) "mm"))
  (princ (strcat "\n토류판 두께: " (itoa timber-thickness) "mm"))
  
  ;; 반값 계산
  (setq half-h (/ h 2.0))
  
  ;; 토류판 오프셋 거리 = tf + timber-thickness/2
  (setq timber-offset (+ tf (/ timber-thickness 2.0)))
  (princ (strcat "\n토류판 오프셋: " (rtos timber-offset 2 2) "mm (바깥쪽)"))
  (debug-log (strcat "토류판 오프셋: " (rtos timber-offset 2 2) "mm"))
  
  ;; H-Pile 오프셋 거리
  (setq hpile-offset half-h)
  (princ (strcat "\nH-Pile 오프셋: " (rtos hpile-offset 2 2) "mm (바깥쪽)"))
  (debug-log (strcat "H-Pile 오프셋: " (rtos hpile-offset 2 2) "mm"))
  
  ;; 토류판 너비 계산
  (setq timber-width (- (* ctc 1000) 50))  ; C.T.C - 50mm (양쪽 25mm 여유)
  (debug-log (strcat "토류판 너비: " (rtos timber-width 2 2) "mm"))
  
  ;; H-Pile 블록 생성
  (setq hpile-block (create-hpile-section-block h b tw tf))
  (debug-log (strcat "H-Pile 블록: " hpile-block))
  
  ;; ===== 1단계: 경계선을 timber-offset만큼 바깥쪽으로 오프셋 =====
  (princ (strcat "\n\n[1단계] 경계선 오프셋 (" (rtos timber-offset 2 2) "mm 바깥쪽)..."))
  (setq boundary-vla (vlax-ename->vla-object boundary-ent))
  
  ;; 원본 경계선 면적 계산
  (setq original-area (vla-get-area boundary-vla))
  (debug-log (strcat "원본 면적: " (rtos original-area 2 2)))
  
  ;; 경계선 방향에 따라 확정된 오프셋 부호 사용
  ;; CCW (offset-sign=1): +timber-offset = 바깥쪽
  ;; CW (offset-sign=-1): -timber-offset = 바깥쪽
  (setq offset-vla (vl-catch-all-apply 'vla-offset 
    (list boundary-vla (* timber-offset offset-sign))))
  
  ;; 오류 처리 (복잡한 다각형에서 자기 교차 발생 시)
  (if (vl-catch-all-error-p offset-vla)
    (progn
      (princ "\n[Error] 오프셋 생성 실패 (자기 교차 가능성)")
      (debug-log "ERROR: 오프셋 생성 실패")
      (princ "\n작업 중단.")
      (exit)
    )
  )
  
  ;; VLA variant 처리
  (if (= (type offset-vla) 'variant)
    (setq offset-vla (vlax-variant-value offset-vla))
  )
  (if (= (type offset-vla) 'safearray)
    (setq offset-vla (vlax-safearray->list offset-vla))
  )
  (if (= (type offset-vla) 'list)
    (setq offset-vla (car offset-vla))
  )
  
  (setq offset-obj (vlax-vla-object->ename offset-vla))
  (setq offset-area (vla-get-area offset-vla))
  
  ;; 오프셋 결과 검증
  (princ (strcat "\n오프셋 후 면적: " (rtos offset-area 2 2)))
  (debug-log (strcat "오프셋 면적: " (rtos offset-area 2 2)))
  
  ;; 면적 검증 (바깥쪽 오프셋은 면적이 커야 함)
  (if (<= offset-area original-area)
    (progn
      (princ "\n[Warning] 오프셋이 안쪽으로 생성됨 - 방향 판단 오류 가능성")
      (debug-log "WARNING: 오프셋 면적이 작아짐")
    )
    (princ "\n[OK] 바깥쪽 오프셋 확인")
  )
  
  (princ (strcat "\n오프셋 객체 생성 완료: " (vl-princ-to-string offset-obj)))
  (debug-log (strcat "오프셋 객체: " (vl-princ-to-string offset-obj)))
  
  ;; ===== 2단계: 오프셋 객체를 EXPLODE =====
  (princ "\n\n[2단계] 오프셋 객체 EXPLODE...")
  (debug-log "EXPLODE 시작")
  
  ;; EXPLODE 전 마지막 엔티티 저장
  (setq last-before (entlast))
  (debug-log (strcat "EXPLODE 전 entlast: " (if last-before (vl-princ-to-string last-before) "nil")))
  
  (command "._EXPLODE" offset-obj)
  (command)  ;; 명령 완료 확인
  
  ;; EXPLODE 후 첫 번째 새 엔티티부터 수집
  (setq first-new (if last-before (entnext last-before) (entnext)))
  (debug-log (strcat "EXPLODE 후 첫 번째 엔티티: " (if first-new (vl-princ-to-string first-new) "nil")))
  
  ;; LINE 수집
  (setq exploded-lines '())
  (setq current-ent first-new)
  (while current-ent
    (setq ent-data (entget current-ent))
    (if (= (cdr (assoc 0 ent-data)) "LINE")
      (progn
        (setq exploded-lines (append exploded-lines (list current-ent)))
        (debug-log (strcat "LINE 수집: " (vl-princ-to-string current-ent)))
      )
    )
    (setq current-ent (entnext current-ent))
  )
  
  (princ (strcat "\nEXPLODE 완료: " (itoa (length exploded-lines)) "개 선분 생성"))
  (debug-log (strcat "EXPLODE 후 LINE 개수: " (itoa (length exploded-lines))))
  
  ;; ===== 3단계: 각 LINE을 따라 C.T.C 간격으로 POINT 생성 + 토류판 배치 =====
  (princ "\n\n[3단계] 각 선분을 따라 C.T.C 간격으로 토류판 배치...")
  
  (foreach line-ent exploded-lines
    (setq line-data (entget line-ent))
    (setq pt1 (cdr (assoc 10 line-data)))
    (setq pt2 (cdr (assoc 11 line-data)))
    
    ;; 선분 길이와 각도 계산
    (setq seg-length (distance pt1 pt2))
    (setq seg-angle (angle pt1 pt2))
    
    ;; 중점 계산
    (setq mid-pt (list
      (/ (+ (car pt1) (car pt2)) 2.0)
      (/ (+ (cadr pt1) (cadr pt2)) 2.0)
      0.0
    ))
    
    ;; C.T.C 간격 (mm 단위)
    (setq ctc-mm (* ctc 1000.0))
    
    ;; 중점에서 양쪽으로 몇 개씩 배치 가능한지 계산
    (setq half-length (/ seg-length 2.0))
    (setq num-left (fix (/ half-length ctc-mm)))   ; 왼쪽 개수
    (setq num-right (fix (/ half-length ctc-mm)))  ; 오른쪽 개수
    
    ;; 포인트 리스트 생성
    (setq point-list '())
    
    ;; 왼쪽 방향 (음수 거리)
    (setq i num-left)
    (while (>= i 1)
      (setq dist (* i ctc-mm -1.0))
      (setq new-pt (polar mid-pt seg-angle dist))
      
      ;; 세그먼트 범위 체크
      (setq dist-from-pt1 (distance pt1 new-pt))
      (setq dist-from-pt2 (distance pt2 new-pt))
      
      ;; pt1과 pt2 사이에 있는지 확인
      (if (<= (+ dist-from-pt1 dist-from-pt2) (+ seg-length 0.1))
        (setq point-list (append point-list (list new-pt)))
      )
      
      (setq i (1- i))
    )
    
    ;; 중점 추가
    (setq point-list (append point-list (list mid-pt)))
    
    ;; 오른쪽 방향 (양수 거리)
    (setq i 1)
    (while (<= i num-right)
      (setq dist (* i ctc-mm))
      (setq new-pt (polar mid-pt seg-angle dist))
      
      ;; 세그먼트 범위 체크
      (setq dist-from-pt1 (distance pt1 new-pt))
      (setq dist-from-pt2 (distance pt2 new-pt))
      
      ;; pt1과 pt2 사이에 있는지 확인
      (if (<= (+ dist-from-pt1 dist-from-pt2) (+ seg-length 0.1))
        (setq point-list (append point-list (list new-pt)))
      )
      
      (setq i (1+ i))
    )
    
    (if (> (length point-list) 0)
      (princ (strcat "\n  토류판: " (itoa (length point-list)) "개"))
    )
    
    ;; 각 포인트에 POINT 생성 + 토류판 객체 배치
    (foreach pt point-list
      ;; POINT 생성
      (entmake
        (list
          '(0 . "POINT")
          (cons 10 pt)
          '(8 . "_토류판(timber)")
        )
      )
      
      ;; 토류판 개별 객체 생성
      (create-timber-panel-object pt timber-width timber-thickness seg-angle)
    )
  )
  
  (princ "\n토류판 배치 완료!")
  
  ;; ===== 3.5단계: 직선 구간에 H-Pile 배치 (토류판 사이 중앙) =====
  (princ "\n\n[3.5단계] 직선 구간에 H-Pile 배치 (토류판 사이 중앙)...")
  (debug-log "=== 3.5단계: 직선 구간 H-Pile 배치 시작 ===")
  
  (foreach line-ent exploded-lines
    (setq line-data (entget line-ent))
    (setq pt1 (cdr (assoc 10 line-data)))
    (setq pt2 (cdr (assoc 11 line-data)))
    
    ;; 선분 길이와 각도 계산
    (setq seg-length (distance pt1 pt2))
    (setq seg-angle (angle pt1 pt2))
    
    ;; 중점 계산
    (setq mid-pt (list
      (/ (+ (car pt1) (car pt2)) 2.0)
      (/ (+ (cadr pt1) (cadr pt2)) 2.0)
      0.0
    ))
    
    ;; C.T.C 간격
    (setq ctc-mm (* ctc 1000.0))
    
    ;; 중점에서 양쪽으로 몇 개씩
    (setq half-length (/ seg-length 2.0))
    (setq num-left (fix (/ half-length ctc-mm)))
    (setq num-right (fix (/ half-length ctc-mm)))
    
    ;; H-Pile 배치 위치 리스트 생성 (토류판 사이 = C.T.C/2 오프셋)
    (setq hpile-positions '())
    
    ;; 왼쪽 방향 (음수)
    (setq i num-left)
    (while (>= i 1)
      (setq dist (+ (* i ctc-mm -1.0) (/ ctc-mm 2.0)))  ; C.T.C/2 만큼 오른쪽으로
      (setq new-pt (polar mid-pt seg-angle dist))
      
      ;; 범위 체크
      (setq dist-from-pt1 (distance pt1 new-pt))
      (setq dist-from-pt2 (distance pt2 new-pt))
      
      (if (<= (+ dist-from-pt1 dist-from-pt2) (+ seg-length 0.1))
        (setq hpile-positions (append hpile-positions (list new-pt)))
      )
      
      (setq i (1- i))
    )
    
    ;; 오른쪽 방향 (양수)
    (setq i 0)
    (while (< i num-right)
      (setq dist (+ (* i ctc-mm) (/ ctc-mm 2.0)))  ; C.T.C/2 만큼 오른쪽으로
      (setq new-pt (polar mid-pt seg-angle dist))
      
      ;; 범위 체크
      (setq dist-from-pt1 (distance pt1 new-pt))
      (setq dist-from-pt2 (distance pt2 new-pt))
      
      (if (<= (+ dist-from-pt1 dist-from-pt2) (+ seg-length 0.1))
        (setq hpile-positions (append hpile-positions (list new-pt)))
      )
      
      (setq i (1+ i))
    )
    
    (if (> (length hpile-positions) 0)
      (princ (strcat "\n  직선 H-Pile: " (itoa (length hpile-positions)) "개"))
    )
    
    ;; ✅ 수정: 외부 법선 방향 계산
    ;; 경계선의 외부 방향 = seg-angle + (boundary-orient × 90°)
    (setq outward-normal (+ seg-angle (* boundary-orient (/ pi 2.0))))
    
    ;; ✅ 수정: 웹이 바깥을 향하도록 회전
    (setq hpile-rotation outward-normal)
    
    (foreach hpile-pt hpile-positions
      ;; ✅ 수정: 하부 플랜지가 경계선에 닿도록 tf만큼 바깥으로 오프셋
      (setq insert-pt (polar hpile-pt outward-normal tf))
      
      (entmake
        (list
          '(0 . "INSERT")
          (cons 2 hpile-block)
          '(8 . "_측면말뚝")
          (cons 10 insert-pt)
          '(41 . 1.0)
          '(42 . 1.0)
          '(43 . 1.0)
          (cons 50 hpile-rotation)
        )
      )
    )
  )
  
  (princ "\n직선 H-Pile 배치 완료!")
  
  ;; ===== 4단계: 모서리(꼭지점)에 H-Pile 배치 =====
  (princ "\n\n[4단계] 모서리(꼭지점)에 H-Pile 배치...")
  (debug-log "=== 4단계: 모서리 H-Pile 배치 시작 ===")
  
  ;; LWPOLYLINE에서 직접 꼭지점 추출
  (setq boundary-data (entget boundary-ent))
  (setq vertices '())
  (foreach item boundary-data
    (if (= (car item) 10)
      (setq vertices (append vertices (list (cdr item))))
    )
  )
  
  ;; 닫힌 폴리라인: 첫 번째와 마지막 꼭지점이 같으면 마지막 제거
  (if (and (> (length vertices) 1)
           (equal (car vertices) (last vertices) 0.01))
    (setq vertices (reverse (cdr (reverse vertices))))
  )
  
  (princ (strcat "\n  경계선 꼭지점 개수: " (itoa (length vertices))))
  
  ;; 각 꼭지점에 H-Pile 배치 (이전/현재/다음 꼭지점 사용)
  (setq i 0)
  (setq num-vertices (length vertices))
  (while (< i num-vertices)
    (setq prev-vertex (nth (if (= i 0) (- num-vertices 1) (- i 1)) vertices))
    (setq curr-vertex (nth i vertices))
    (setq next-vertex (nth (if (= i (- num-vertices 1)) 0 (+ i 1)) vertices))
    
    ;; 각도 계산
    (setq angle1 (angle prev-vertex curr-vertex))
    (setq angle2 (angle curr-vertex next-vertex))
    
    ;; H-Pile 배치 (boundary-orient, prev-vertex, next-vertex 전달)
    (place-hpile-at-corner-simple curr-vertex angle1 angle2 h b tw tf hpile-block timber-offset boundary-orient prev-vertex next-vertex)
    
    (setq i (+ i 1))
  )
  
  (princ "\n모서리 H-Pile 배치 완료!")
  
  
  ;; ===== 5단계: EXPLODE된 LINE 객체 삭제 =====
  (setq delete-count 0)
  (foreach line-ent exploded-lines
    (if (and line-ent (entget line-ent))
      (progn
        (entdel line-ent)
        (setq delete-count (1+ delete-count))
      )
    )
  )
  
  (princ "\n\n작업 완료!")
)

;; 경계선 방향 판단 (CW/CCW)
;; 반환값: 1 = CCW (반시계), -1 = CW (시계)
(defun get-boundary-direction (vertices / signed-area i x1 y1 x2 y2 n)
  (setq signed-area 0.0)
  (setq n (length vertices))
  (setq i 0)
  
  ;; Shoelace 공식으로 부호 있는 면적 계산
  (while (< i n)
    (setq x1 (car (nth i vertices)))
    (setq y1 (cadr (nth i vertices)))
    (setq x2 (car (nth (if (= i (1- n)) 0 (1+ i)) vertices)))
    (setq y2 (cadr (nth (if (= i (1- n)) 0 (1+ i)) vertices)))
    (setq signed-area (+ signed-area (* (- x1 x2) (+ y1 y2))))
    (setq i (1+ i))
  )
  
  (if (> signed-area 0)
    1   ; CCW (반시계방향)
    -1  ; CW (시계방향)
  )
)

;; 경계선 세그먼트 추출
(defun get-boundary-segments (ent / ent-data vertices seg-list i pt1 pt2)
  (debug-log "=== get-boundary-segments 시작 ===")
  
  (setq ent-data (entget ent))
  (setq vertices '())
  
  ;; 정점 추출
  (foreach item ent-data
    (if (= (car item) 10)
      (setq vertices (append vertices (list (cdr item))))
    )
  )
  
  ;; 세그먼트 생성 (폐합 처리)
  (setq seg-list '())
  (setq i 0)
  (while (< i (length vertices))
    (setq pt1 (nth i vertices))
    (setq pt2 (if (= i (1- (length vertices)))
                (nth 0 vertices)  ; 마지막 → 첫 번째 (폐합)
                (nth (1+ i) vertices)
              ))
    (setq seg-list (append seg-list (list (list pt1 pt2))))
    (setq i (1+ i))
  )
  
  (debug-log (strcat "추출된 세그먼트 개수: " (itoa (length seg-list))))
  seg-list
)

;; 세그먼트 위에 C.T.C 간격으로 위치 계산
(defun calculate-positions-on-segment (pt1 pt2 ctc / seg-length seg-mid seg-angle positions 
  num-left num-right total-num i dist pos)
  
  (setq seg-length (distance pt1 pt2))
  (setq seg-mid (list 
    (/ (+ (car pt1) (car pt2)) 2.0)
    (/ (+ (cadr pt1) (cadr pt2)) 2.0)
  ))
  (setq seg-angle (angle pt1 pt2))
  
  ;; 중심에서 양쪽으로 몇 개씩 배치 가능한지 계산
  (setq num-left (fix (/ (/ seg-length 2.0) ctc)))
  (setq num-right num-left)
  (setq total-num (+ num-left num-right 1))  ; 중심 포함
  
  ;; 위치 리스트 생성
  (setq positions '())
  
  ;; 왼쪽 방향 (음수)
  (setq i 1)
  (while (<= i num-left)
    (setq dist (* i ctc -1.0))
    (setq pos (polar seg-mid seg-angle dist))
    (setq positions (append positions (list pos)))
    (setq i (1+ i))
  )
  
  ;; 중심
  (setq positions (append positions (list seg-mid)))
  
  ;; 오른쪽 방향 (양수)
  (setq i 1)
  (while (<= i num-right)
    (setq dist (* i ctc))
    (setq pos (polar seg-mid seg-angle dist))
    (setq positions (append positions (list pos)))
    (setq i (1+ i))
  )
  
  positions
)

;;; ----------------------------------------------------------------------
;;; 로드 메시지
;;; ----------------------------------------------------------------------

(princ "\nTSP-debug.lsp loaded. Debugging features enabled.")
(princ "\n\n디버그 명령:")
(princ "\n- (debug-show-log): 디버그 로그 출력")
(princ "\n- (debug-clear): 디버그 로그 초기화")
(princ "\n- (setq *tsp-debug* nil): 디버그 모드 비활성화")
(princ "\n- (setq *tsp-debug* T): 디버그 모드 활성화\n")
(princ)
