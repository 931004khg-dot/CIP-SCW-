;;; ======================================================================
;;; TSP.lsp - Temporary Structure Plan
;;; 가시설 흙막이 벽체 자동 작도 시스템
;;; ======================================================================
;;; 
;;; 명령어: TSP
;;; 
;;; 작성일: 2026-01-13
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

;;; ----------------------------------------------------------------------
;;; DCL 파일 생성
;;; ----------------------------------------------------------------------

(defun create-tsp-dcl (/ dcl-file dcl-path lisp-path)
  ;; 현재 LISP 파일 경로 가져오기
  (setq lisp-path (findfile "TSP.lsp"))
  
  (if lisp-path
    (progn
      ;; LISP 파일이 있는 디렉토리에 DCL 파일 생성
      (setq dcl-path (strcat (vl-filename-directory lisp-path) "\\tsp.dcl"))
    )
    (progn
      ;; LISP 파일을 찾을 수 없으면 TEMP 디렉토리 사용
      (setq dcl-path (strcat (getvar "TEMPPREFIX") "tsp.dcl"))
    )
  )
  
  ;; DCL 파일 생성
  (setq dcl-file (open dcl-path "w"))
  
  (if dcl-file
    (progn
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
      dcl-path
    )
    (progn
      (princ "\nDCL 파일 생성 실패!")
      nil
    )
  )
)

;;; ----------------------------------------------------------------------
;;; 레이어 생성 함수
;;; ----------------------------------------------------------------------

(defun create-layer-if-not-exists (layer-name color / )
  (if (not (tblsearch "LAYER" layer-name))
    (command "._LAYER" "_M" layer-name "_C" color layer-name "")
  )
)

;;; ----------------------------------------------------------------------
;;; 숫자만 입력 가능하도록 검증
;;; ----------------------------------------------------------------------

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
;;; H형강 규격 파싱
;;; ----------------------------------------------------------------------

(defun parse-h-spec (spec-str / clean-str char-list i ch result-str)
  ;; "H 298×201×9/14" -> (298 201 9 14)
  ;; 모든 문자를 하나씩 검사하여 숫자와 공백만 남김
  (if (and spec-str (wcmatch spec-str "H *"))
    (progn
      ;; "H " 제거
      (setq clean-str (substr spec-str 3))
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
      (read (strcat "(" result-str ")"))
    )
    nil
  )
)

;;; ----------------------------------------------------------------------
;;; 띠장 옵셋 생성
;;; ----------------------------------------------------------------------

(defun create-wale-offsets (boundary-ent wale-spec / h b tw tf offset-list obj1 obj2 obj3 obj4 ent-data ent-type first-vertex inside-pt wale-values)
  ;; 레이어 생성
  (create-layer-if-not-exists "_띠장(wale)" "3")
  
  ;; 규격 파싱
  (if (= wale-spec "User-defined")
    (setq wale-values *tsp-wale-custom*)
    (setq wale-values (parse-h-spec wale-spec))
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
  
  ;; 경계선의 내부 점 찾기 (중심점 방향으로 옵셋하기 위함)
  (setq ent-data (entget boundary-ent))
  (setq ent-type (cdr (assoc 0 ent-data)))
  
  ;; LWPOLYLINE인 경우 첫 번째 정점 가져오기
  (if (= ent-type "LWPOLYLINE")
    (progn
      (setq first-vertex (cdr (assoc 10 ent-data)))
      ;; 내부 방향 점 (안쪽으로 조금 이동)
      (setq inside-pt (list (- (car first-vertex) 1) (- (cadr first-vertex) 1)))
    )
    ;; LINE인 경우
    (progn
      (setq first-vertex (cdr (assoc 10 ent-data)))
      (setq inside-pt (list (- (car first-vertex) 1) (- (cadr first-vertex) 1)))
    )
  )
  
  ;; 객체 2: tf 옵셋 - 빨간색 (안쪽)
  (command "._OFFSET" tf boundary-ent inside-pt "")
  (setq obj2 (entlast))
  (if obj2
    (command "._CHPROP" obj2 "" "_LA" "_띠장(wale)" "_C" "1" "")
  )
  
  ;; 객체 3: (H - tf) 옵셋 - 빨간색 (안쪽)
  (command "._OFFSET" (- h tf) boundary-ent inside-pt "")
  (setq obj3 (entlast))
  (if obj3
    (command "._CHPROP" obj3 "" "_LA" "_띠장(wale)" "_C" "1" "")
  )
  
  ;; 객체 4: H 옵셋 - 초록색 (안쪽)
  (command "._OFFSET" h boundary-ent inside-pt "")
  (setq obj4 (entlast))
  (if obj4
    (command "._CHPROP" obj4 "" "_LA" "_띠장(wale)" "_C" "3" "")
  )
  
  (princ "\n띠장 옵셋 생성 완료!")
)

;;; ----------------------------------------------------------------------
;;; H-Pile Dialog 콜백 함수
;;; ----------------------------------------------------------------------

(defun hpile-dialog-callback (dcl-path / dcl-id result hpile-idx wale-idx)
  (setq dcl-id (load_dialog dcl-path))
  
  (if (not (new_dialog "tsp_hpile" dcl-id))
    (progn
      (princ "\nDialog 로드 실패!")
      (unload_dialog dcl-id)
      nil
    )
    (progn
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
           
           (done_dialog 1)
         )"
      )
      
      ;; Cancel 버튼
      (action_tile "cancel" "(done_dialog 0)")
      
      ;; Dialog 표시
      (setq result (start_dialog))
      (unload_dialog dcl-id)
      
      result
    )
  )
)

;;; ----------------------------------------------------------------------
;;; 메인 Dialog 콜백 함수
;;; ----------------------------------------------------------------------

(defun main-dialog-callback (dcl-path / dcl-id result)
  (setq dcl-id (load_dialog dcl-path))
  
  (if (not (new_dialog "tsp_main" dcl-id))
    (progn
      (princ "\nDialog 로드 실패!")
      (unload_dialog dcl-id)
      nil
    )
    (progn
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
      (unload_dialog dcl-id)
      
      result
    )
  )
)

;;; ----------------------------------------------------------------------
;;; TSP 메인 함수
;;; ----------------------------------------------------------------------

(defun C:TSP (/ dcl-path dcl-id main-result hpile-result boundary-ent)
  (princ "\n========================================")
  (princ "\nTSP - Temporary Structure Plan")
  (princ "\n========================================\n")
  
  ;; DCL 파일 생성
  (setq dcl-path (create-tsp-dcl))
  
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
         (princ (strcat "\n- C.T.C: " (rtos *tsp-ctc* 2 2) "m\n"))
         
         ;; 경계선 선택
         (princ "\n경계선을 선택하세요 (Polyline 또는 Line): ")
         (setq boundary-ent (car (entsel)))
         
         (if boundary-ent
           (progn
             (princ "\n경계선 선택 완료!")
             
             ;; 띠장 옵셋 생성
             (create-wale-offsets boundary-ent *tsp-wale-spec*)
             
             ;; H-Pile 세트 생성 (경계선의 가장 긴 세그먼트 중간에 생성)
             (princ "\n\n>>> H-Pile 세트 생성 시작...")
             (create-hpile-set-on-boundary boundary-ent *tsp-hpile-spec* *tsp-ctc*)
             
             (princ "\n========================================")
             (princ "\n작업 완료!")
             (princ "\n========================================\n")
           )
           (princ "\n경계선 선택 취소됨\n")
         )
       )
       (princ "\n설정 취소됨\n")
     )
    )
    
    ;; 취소
    (t
     (princ "\n취소됨\n")
    )
  )
  
  (princ)
)

;;; ----------------------------------------------------------------------
;;; 경계선에서 가장 긴 세그먼트 찾기
;;; ----------------------------------------------------------------------

(defun get-longest-segment (ent / ent-data ent-type vertices num-vertices i pt1 pt2 dist max-dist longest-seg)
  (setq ent-data (entget ent))
  (setq ent-type (cdr (assoc 0 ent-data)))
  
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

;;; ----------------------------------------------------------------------
;;; 경계선에 H-Pile 세트 생성
;;; ----------------------------------------------------------------------

(defun create-hpile-set-on-boundary (boundary-ent hpile-spec ctc / longest-seg pt1 pt2 mid-pt)
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
      (princ (strcat "\n중점 위치: (" (rtos (car mid-pt) 2 2) ", " (rtos (cadr mid-pt) 2 2) ")"))
      
      ;; H-Pile 세트 생성
      (create-hpile-set mid-pt hpile-spec ctc)
    )
    (progn
      (princ "\n경계선에서 세그먼트를 찾을 수 없습니다!")
      nil
    )
  )
)

;;; ----------------------------------------------------------------------
;;; H-Pile 단면 생성 (I자 형태)
;;; ----------------------------------------------------------------------

(defun create-hpile-section (insert-pt h b tw tf layer-name / half-h half-b half-tw half-tf cx cy pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 pt9 pt10 pt11 pt12 pt13 pt14 pt15 pt16 pline-ent fillet-r)
  ;; insert-pt: 삽입 기준점 (중심)
  ;; h: 높이 (mm)
  ;; b: 폭 (mm)
  ;; tw: 웹 두께 (mm)
  ;; tf: 플랜지 두께 (mm)
  ;; layer-name: 레이어명
  
  ;; 반값 계산
  (setq half-h (/ h 2.0))
  (setq half-b (/ b 2.0))
  (setq half-tw (/ tw 2.0))
  (setq half-tf (/ tf 2.0))
  (setq fillet-r (* tw 2.0))  ; 필렛 반지름 = 웹 두께 × 2
  
  ;; 중심 좌표 추출
  (setq cx (car insert-pt))
  (setq cy (cadr insert-pt))
  
  ;; I형강 좌표 계산 (중심 기준, 16개 점 - 반시계방향 CCW)
  ;; 구조: 플랜지 끝 → 필렛 시작 (웹 근처) → 필렛 끝 (웹 시작) → 웹
  
  ;; 상단 플랜지 - 우측 (반시계방향 시작)
  (setq pt1  (list (+ cx half-b) (+ cy half-h)))                           ; 1. 우상단 외부 모서리
  (setq pt2  (list (+ cx half-b) (+ cy (- half-h half-tf))))               ; 2. 우측 플랜지 하단 (끝)
  (setq pt3  (list (+ cx (+ half-tw fillet-r)) (+ cy (- half-h half-tf)))) ; 3. 필렛 시작 (웹 근처, 우상)
  
  ;; 우측 상단 필렛 + 웹
  (setq pt4  (list (+ cx half-tw) (+ cy (- half-h half-tf fillet-r))))     ; 4. 필렛 끝 (웹 시작, 우상)
  (setq pt5  (list (+ cx half-tw) (- cy (- half-h half-tf fillet-r))))     ; 5. 웹 우하단 (필렛 시작 전)
  
  ;; 우측 하단 필렛
  (setq pt6  (list (+ cx (+ half-tw fillet-r)) (- cy (- half-h half-tf)))) ; 6. 필렛 시작 (웹 근처, 우하)
  
  ;; 하단 플랜지 - 우측
  (setq pt7  (list (+ cx half-b) (- cy (- half-h half-tf))))               ; 7. 우하 플랜지 상단
  (setq pt8  (list (+ cx half-b) (- cy half-h)))                           ; 8. 우하단 외부 모서리
  
  ;; 하단 플랜지 - 좌측
  (setq pt9  (list (- cx half-b) (- cy half-h)))                           ; 9. 좌하단 외부 모서리
  (setq pt10 (list (- cx half-b) (- cy (- half-h half-tf))))               ; 10. 좌하 플랜지 상단
  (setq pt11 (list (- cx (+ half-tw fillet-r)) (- cy (- half-h half-tf)))) ; 11. 필렛 시작 (웹 근처, 좌하)
  
  ;; 좌측 하단 필렛 + 웹
  (setq pt12 (list (- cx half-tw) (- cy (- half-h half-tf fillet-r))))     ; 12. 필렛 끝 (웹 시작, 좌하)
  (setq pt13 (list (- cx half-tw) (+ cy (- half-h half-tf fillet-r))))     ; 13. 웹 좌상단 (필렛 시작 전)
  
  ;; 좌측 상단 필렛
  (setq pt14 (list (- cx (+ half-tw fillet-r)) (+ cy (- half-h half-tf)))) ; 14. 필렛 시작 (웹 근처, 좌상)
  
  ;; 상단 플랜지 - 좌측
  (setq pt15 (list (- cx half-b) (+ cy (- half-h half-tf))))               ; 15. 좌상 플랜지 하단
  (setq pt16 (list (- cx half-b) (+ cy half-h)))                           ; 16. 좌상단 외부 모서리
  
  ;; 폴리라인 생성 (entmake 사용, 필렛 포함)
  ;; 필렛은 웹-플랜지 모서리 4곳에만 적용 (bulge = 0.4142)
  (entmake
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbPolyline")
      (cons 8 layer-name)
      (cons 62 3)  ; 색상: 초록(3)
      '(90 . 16)   ; 정점 개수: 16개
      '(70 . 1)    ; 닫힘 플래그
      (cons 10 pt1)   ; 1. 우상단 외부
      (cons 10 pt2)   ; 2. 우측 플랜지 하단 (끝)
      (cons 10 pt3)   ; 3. 필렛 시작 (웹 근처, 우상)
      (cons 42 0.4142135623730951)  ; 필렛 1: pt3→pt4 (우상단)
      (cons 10 pt4)   ; 4. 필렛 끝 (웹 시작, 우상)
      (cons 10 pt5)   ; 5. 웹 우하단
      (cons 10 pt6)   ; 6. 필렛 시작 (웹 근처, 우하)
      (cons 42 0.4142135623730951)  ; 필렛 2: pt6→pt7 (우하단)
      (cons 10 pt7)   ; 7. 필렛 끝 (우하 플랜지 상단)
      (cons 10 pt8)   ; 8. 우하단 외부
      (cons 10 pt9)   ; 9. 좌하단 외부
      (cons 10 pt10)  ; 10. 좌하 플랜지 상단
      (cons 10 pt11)  ; 11. 필렛 시작 (웹 근처, 좌하)
      (cons 42 0.4142135623730951)  ; 필렛 3: pt11→pt12 (좌하단)
      (cons 10 pt12)  ; 12. 필렛 끝 (웹 시작, 좌하)
      (cons 10 pt13)  ; 13. 웹 좌상단
      (cons 10 pt14)  ; 14. 필렛 시작 (웹 근처, 좌상)
      (cons 42 0.4142135623730951)  ; 필렛 4: pt14→pt15 (좌상단)
      (cons 10 pt15)  ; 15. 필렛 끝 (좌상 플랜지 하단)
      (cons 10 pt16)  ; 16. 좌상단 외부
    )
  )
  
  (setq pline-ent (entlast))
  pline-ent
)

;;; ----------------------------------------------------------------------
;;; 토류판 생성
;;; ----------------------------------------------------------------------

(defun create-timber-panel (pt1 pt2 width / mid-pt dx dy length panel-length panel-pt1 panel-pt2 panel-pt3 panel-pt4 pline-ent perp-dx perp-dy)
  ;; pt1, pt2: H-Pile 중심점
  ;; width: 토류판 두께 (70mm)
  
  ;; 두 점 사이의 중점 계산
  (setq mid-pt (list
    (/ (+ (car pt1) (car pt2)) 2.0)
    (/ (+ (cadr pt1) (cadr pt2)) 2.0)
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
  ;; 방향에 수직인 벡터 계산
  (setq perp-dx (- dy))
  (setq perp-dy dx)
  
  ;; 토류판의 4개 꼭지점 (직사각형)
  ;; pt1에서 25mm 떨어진 점 + 수직 방향으로 width/2
  (setq panel-pt1 (list
    (+ (car pt1) (* dx 25.0) (* perp-dx (/ width 2.0)))
    (+ (cadr pt1) (* dy 25.0) (* perp-dy (/ width 2.0)))
  ))
  
  ;; pt2에서 25mm 떨어진 점 + 수직 방향으로 width/2
  (setq panel-pt2 (list
    (- (car pt2) (* dx 25.0) (* perp-dx (/ width 2.0)))
    (- (cadr pt2) (* dy 25.0) (* perp-dy (/ width 2.0)))
  ))
  
  ;; pt2에서 25mm 떨어진 점 - 수직 방향으로 width/2
  (setq panel-pt3 (list
    (+ (- (car pt2) (* dx 25.0)) (* perp-dx (/ width 2.0)))
    (+ (- (cadr pt2) (* dy 25.0)) (* perp-dy (/ width 2.0)))
  ))
  
  ;; pt1에서 25mm 떨어진 점 - 수직 방향으로 width/2
  (setq panel-pt4 (list
    (- (+ (car pt1) (* dx 25.0)) (* perp-dx (/ width 2.0)))
    (- (+ (cadr pt1) (* dy 25.0)) (* perp-dy (/ width 2.0)))
  ))
  
  ;; 폴리라인 생성 (entmake 사용)
  ;; 순서 수정: 교차 방지를 위해 직사각형 순서로 연결
  ;; panel-pt1: pt1 + 25dx + (width/2)perp
  ;; panel-pt2: pt2 - 25dx - (width/2)perp
  ;; panel-pt3: pt2 - 25dx + (width/2)perp
  ;; panel-pt4: pt1 + 25dx - (width/2)perp
  ;; 올바른 순서: pt4 → pt2 → pt3 → pt1 (닫힘으로 pt4로 복귀)
  (entmake
    (list
      '(0 . "LWPOLYLINE")
      '(100 . "AcDbEntity")
      '(100 . "AcDbPolyline")
      '(8 . "_토류판(timber)")
      '(62 . 1)  ; 색상: 빨강(1)
      '(90 . 4)  ; 정점 개수
      '(70 . 1)  ; 닫힘 플래그
      (cons 10 panel-pt4)  ; pt1 - (width/2)perp
      (cons 10 panel-pt2)  ; pt2 - (width/2)perp
      (cons 10 panel-pt3)  ; pt2 + (width/2)perp
      (cons 10 panel-pt1)  ; pt1 + (width/2)perp
    )
  )
  
  (setq pline-ent (entlast))
  
  ;; 해치 생성
  (if pline-ent
    (command "._-BHATCH"
      "_P" "ANSI36"
      "30"
      "0"
      "_SEL" pline-ent ""
      ""
    )
  )
  
  pline-ent
)

;;; ----------------------------------------------------------------------
;;; H-Pile 세트 생성 (H-Pile 2개 + 토류판 1개)
;;; ----------------------------------------------------------------------

(defun create-hpile-set (pt1 hpile-spec ctc / h b tw tf layer-name pt2 hpile1 hpile2 timber)
  ;; pt1: 첫 번째 H-Pile 중심점
  ;; hpile-spec: H-Pile 규격 문자열
  ;; ctc: C.T.C 거리 (m → mm 변환 필요)
  
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
  
  ;; 첫 번째 H-Pile 생성
  (princ "\n첫 번째 H-Pile 생성...")
  (setq hpile1 (create-hpile-section pt1 h b tw tf layer-name))
  
  ;; 두 번째 H-Pile 생성
  (princ "\n두 번째 H-Pile 생성...")
  (setq hpile2 (create-hpile-section pt2 h b tw tf layer-name))
  
  ;; 토류판 생성
  (princ "\n토류판 생성...")
  (setq timber (create-timber-panel pt1 pt2 70))
  
  (princ "\nH-Pile 세트 생성 완료!")
  (debug-log "=== create-hpile-set 완료 ===")
  
  (list hpile1 hpile2 timber)
)

;;; ----------------------------------------------------------------------
;;; 시작 메시지
;;; ----------------------------------------------------------------------

(princ "\nTSP.lsp loaded. Command: TSP")
(princ)
