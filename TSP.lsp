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

(setq *tsp-hpile-spec* "H 298x201x9/14")     ; H-Pile 규격
(setq *tsp-hpile-custom* '(298 201 9 14))    ; User-defined H-Pile
(setq *tsp-wale-spec* "H 300x300x10/15")      ; 띠장 규격
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
    (write-line "      : text { label = \" x \"; }" dcl-file)
    (write-line "      : edit_box {" dcl-file)
    (write-line "        key = \"hpile_b\";" dcl-file)
    (write-line "        label = \"\";" dcl-file)
    (write-line "        width = 5;" dcl-file)
    (write-line "        fixed_width = true;" dcl-file)
    (write-line "        edit_width = 5;" dcl-file)
    (write-line "        is_enabled = false;" dcl-file)
    (write-line "      }" dcl-file)
    (write-line "      : text { label = \" x \"; }" dcl-file)
    (write-line "      : edit_box {" dcl-file)
    (write-line "        key = \"hpile_tw\";" dcl-file)
    (write-line "        label = \"\";" dcl-file)
    (write-line "        width = 5;" dcl-file)
    (write-line "        fixed_width = true;" dcl-file)
    (write-line "        edit_width = 5;" dcl-file)
    (write-line "        is_enabled = false;" dcl-file)
    (write-line "      }" dcl-file)
    (write-line "      : text { label = \" / \"; }" dcl-file)
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
    (write-line "      : text { label = \" x \"; }" dcl-file)
    (write-line "      : edit_box {" dcl-file)
    (write-line "        key = \"wale_b\";" dcl-file)
    (write-line "        label = \"\";" dcl-file)
    (write-line "        width = 5;" dcl-file)
    (write-line "        fixed_width = true;" dcl-file)
    (write-line "        edit_width = 5;" dcl-file)
    (write-line "        is_enabled = false;" dcl-file)
    (write-line "      }" dcl-file)
    (write-line "      : text { label = \" x \"; }" dcl-file)
    (write-line "      : edit_box {" dcl-file)
    (write-line "        key = \"wale_tw\";" dcl-file)
    (write-line "        label = \"\";" dcl-file)
    (write-line "        width = 5;" dcl-file)
    (write-line "        fixed_width = true;" dcl-file)
    (write-line "        edit_width = 5;" dcl-file)
    (write-line "        is_enabled = false;" dcl-file)
    (write-line "      }" dcl-file)
    (write-line "      : text { label = \" / \"; }" dcl-file)
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
    (write-line "    : text {" dcl-file)
    (write-line "      label = \"C.T.C (m)\";" dcl-file)
    (write-line "    }" dcl-file)
    (write-line "    : edit_box {" dcl-file)
    (write-line "      key = \"ctc\";" dcl-file)
    (write-line "      width = 30;" dcl-file)
    (write-line "      fixed_width = true;" dcl-file)
    (write-line "      edit_width = 10;" dcl-file)
    (write-line "      value = \"2\";" dcl-file)
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

(defun parse-h-spec (spec-str / parts h b tw tf)
  ;; "H 300x300x10/15" -> (300 300 10 15)
  (if (wcmatch spec-str "H *x*x*/*")
    (progn
      ;; "H " 제거
      (setq spec-str (substr spec-str 3))
      
      ;; "x"와 "/"로 분할
      (setq parts '())
      (setq pos 1)
      
      ;; 간단한 파싱 (숫자 추출)
      (setq h (atoi spec-str))
      
      (setq pos (vl-string-search "x" spec-str))
      (setq spec-str (substr spec-str (+ pos 2)))
      (setq b (atoi spec-str))
      
      (setq pos (vl-string-search "x" spec-str))
      (setq spec-str (substr spec-str (+ pos 2)))
      (setq tw (atoi spec-str))
      
      (setq pos (vl-string-search "/" spec-str))
      (setq spec-str (substr spec-str (+ pos 2)))
      (setq tf (atoi spec-str))
      
      (list h b tw tf)
    )
    nil
  )
)

;;; ----------------------------------------------------------------------
;;; 띠장 옵셋 생성
;;; ----------------------------------------------------------------------

(defun create-wale-offsets (boundary-ent wale-spec / h b tw tf offset-list obj1 obj2 obj3 obj4)
  ;; 레이어 생성
  (create-layer-if-not-exists "_띠장" "3")
  
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
  (command "._CHPROP" obj1 "" "_LA" "_띠장" "_C" "3" "")
  
  ;; 객체 2: tf 옵셋 - 빨간색
  (command "._OFFSET" tf boundary-ent "_non" '(0 0 0) "")
  (setq obj2 (entlast))
  (if obj2
    (command "._CHPROP" obj2 "" "_LA" "_띠장" "_C" "1" "")
  )
  
  ;; 객체 3: (H - tf) 옵셋 - 빨간색
  (command "._OFFSET" (- h tf) boundary-ent "_non" '(0 0 0) "")
  (setq obj3 (entlast))
  (if obj3
    (command "._CHPROP" obj3 "" "_LA" "_띠장" "_C" "1" "")
  )
  
  ;; 객체 4: H 옵셋 - 초록색
  (command "._OFFSET" h boundary-ent "_non" '(0 0 0) "")
  (setq obj4 (entlast))
  (if obj4
    (command "._CHPROP" obj4 "" "_LA" "_띠장" "_C" "3" "")
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
      (mapcar 'add_list '("H 298x201x9/14" "H 300x300x10/15" "H 350x350x12/19" "H 400x400x13/21" "User-defined"))
      (end_list)
      (set_tile "hpile_spec" "0")  ; 기본값: H 298x201x9/14
      
      (start_list "wale_spec")
      (mapcar 'add_list '("H 298x201x9/14" "H 300x300x10/15" "H 350x350x12/19" "H 400x400x13/21" "User-defined"))
      (end_list)
      (set_tile "wale_spec" "1")  ; 기본값: H 300x300x10/15
      
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
             ((= hpile-idx 0) (setq *tsp-hpile-spec* \"H 298x201x9/14\"))
             ((= hpile-idx 1) (setq *tsp-hpile-spec* \"H 300x300x10/15\"))
             ((= hpile-idx 2) (setq *tsp-hpile-spec* \"H 350x350x12/19\"))
             ((= hpile-idx 3) (setq *tsp-hpile-spec* \"H 400x400x13/21\"))
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
             ((= wale-idx 0) (setq *tsp-wale-spec* \"H 298x201x9/14\"))
             ((= wale-idx 1) (setq *tsp-wale-spec* \"H 300x300x10/15\"))
             ((= wale-idx 2) (setq *tsp-wale-spec* \"H 350x350x12/19\"))
             ((= wale-idx 3) (setq *tsp-wale-spec* \"H 400x400x13/21\"))
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
;;; 시작 메시지
;;; ----------------------------------------------------------------------

(princ "\nTSP.lsp loaded. Command: TSP")
(princ)
