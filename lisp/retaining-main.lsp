;;; ======================================================================
;;; retaining-main.lsp
;;; 가시설 흙막이 벽체 통합 메인 프로그램
;;; ======================================================================

;;; ----------------------------------------------------------------------
;;; 필수 파일 로드
;;; ----------------------------------------------------------------------

(defun load-retaining-files (/ file-list file-path)
  (princ "\n가시설 프로그램 로드 중...\n")
  
  (setq file-list '(
    "retaining-utils.lsp"
    "hpile-retaining.lsp"
    "cip-retaining.lsp"
    "scw-retaining.lsp"
  ))
  
  (foreach file file-list
    ;; 상대 경로로 파일 로드 시도
    (setq file-path file)
    (if (findfile file-path)
      (progn
        (load file-path)
        (princ (strcat "  ✓ " file " 로드 완료\n"))
      )
      (progn
        (princ (strcat "  ✗ " file " 로드 실패\n"))
        (princ "     파일이 같은 폴더에 있는지 확인하세요.\n")
      )
    )
  )
  
  (princ "\n")
)

;; 초기 로드
(load-retaining-files)

;;; ----------------------------------------------------------------------
;;; 메인 메뉴 함수
;;; ----------------------------------------------------------------------

(defun C:RETAINING (/ choice)
  (princ "\n")
  (princ "\n╔════════════════════════════════════════════╗")
  (princ "\n║                                            ║")
  (princ "\n║    가시설 흙막이 벽체 자동 작도 시스템    ║")
  (princ "\n║                                            ║")
  (princ "\n╠════════════════════════════════════════════╣")
  (princ "\n║                                            ║")
  (princ "\n║  [1] H-Pile + 토류판 공법                  ║")
  (princ "\n║      - 엄지말뚝(H-Pile) 항타               ║")
  (princ "\n║      - 토류판(목재/강재) 삽입              ║")
  (princ "\n║      - 중소규모 굴착                       ║")
  (princ "\n║                                            ║")
  (princ "\n║  [2] CIP 공법 (현장타설말뚝)               ║")
  (princ "\n║      - 천공 후 철근 + 콘크리트 타설       ║")
  (princ "\n║      - 원형/사각형/연속벽체                ║")
  (princ "\n║      - 차수성 우수, 지하수위 높은 현장    ║")
  (princ "\n║                                            ║")
  (princ "\n║  [3] S.C.W 공법 (소일시멘트벽)             ║")
  (princ "\n║      - 지반 + 시멘트 밀크 혼합            ║")
  (princ "\n║      - H-Beam 보강재 삽입                  ║")
  (princ "\n║      - 연속벽체, 차수 효과 탁월           ║")
  (princ "\n║                                            ║")
  (princ "\n║  [0] 종료                                  ║")
  (princ "\n║                                            ║")
  (princ "\n╚════════════════════════════════════════════╝")
  (princ "\n")
  
  (setq choice (getint "\n공법 선택 [1/2/3/0]: "))
  
  (cond
    ((= choice 1)
     (princ "\n\n>>> H-Pile + 토류판 공법 실행\n")
     (C:HPILE-RT)
    )
    
    ((= choice 2)
     (princ "\n\n>>> CIP 공법 실행\n")
     (C:CIP-RT)
    )
    
    ((= choice 3)
     (princ "\n\n>>> S.C.W 공법 실행\n")
     (C:SCW-RT)
    )
    
    ((= choice 0)
     (princ "\n프로그램을 종료합니다.\n")
    )
    
    (t
     (princ "\n잘못된 선택입니다. 다시 시도하세요.\n")
     (C:RETAINING)
    )
  )
  
  (princ)
)

;;; ----------------------------------------------------------------------
;;; 단축 명령어
;;; ----------------------------------------------------------------------

;; 단축 명령어 정의
(defun C:RT () (C:RETAINING))

;;; ----------------------------------------------------------------------
;;; 유틸리티 명령어
;;; ----------------------------------------------------------------------

;; 레이어 초기화 단독 명령어
(defun C:RETAINING-INIT (/ )
  (princ "\n가시설 레이어 초기화 중...")
  (init-retaining-layers)
  (princ "\n레이어 초기화 완료!")
  (princ)
)

;; 도움말 표시
(defun C:RETAINING-HELP (/ )
  (princ "\n")
  (princ "\n╔════════════════════════════════════════════╗")
  (princ "\n║                                            ║")
  (princ "\n║         가시설 프로그램 도움말             ║")
  (princ "\n║                                            ║")
  (princ "\n╠════════════════════════════════════════════╣")
  (princ "\n║                                            ║")
  (princ "\n║  ■ 주요 명령어                            ║")
  (princ "\n║                                            ║")
  (princ "\n║  RETAINING 또는 RT                         ║")
  (princ "\n║    → 메인 메뉴 실행                        ║")
  (princ "\n║                                            ║")
  (princ "\n║  HPILE-RT                                  ║")
  (princ "\n║    → H-Pile+토류판 공법 직접 실행          ║")
  (princ "\n║                                            ║")
  (princ "\n║  CIP-RT                                    ║")
  (princ "\n║    → CIP 공법 직접 실행                    ║")
  (princ "\n║                                            ║")
  (princ "\n║  SCW-RT                                    ║")
  (princ "\n║    → S.C.W 공법 직접 실행                  ║")
  (princ "\n║                                            ║")
  (princ "\n║  RETAINING-INIT                            ║")
  (princ "\n║    → 레이어 초기화                         ║")
  (princ "\n║                                            ║")
  (princ "\n║  RETAINING-HELP                            ║")
  (princ "\n║    → 이 도움말 표시                        ║")
  (princ "\n║                                            ║")
  (princ "\n╠════════════════════════════════════════════╣")
  (princ "\n║                                            ║")
  (princ "\n║  ■ 사용 방법                              ║")
  (princ "\n║                                            ║")
  (princ "\n║  1. RETAINING 명령어 실행                  ║")
  (princ "\n║  2. 공법 선택 (1~3)                        ║")
  (princ "\n║  3. 지하벽체 경계선 선택                   ║")
  (princ "\n║     (Polyline 또는 Line)                   ║")
  (princ "\n║  4. 설계 파라미터 입력                     ║")
  (princ "\n║     - 말뚝/벽체 간격 및 크기               ║")
  (princ "\n║     - 띠장 단수 및 크기                    ║")
  (princ "\n║     - 버팀보 간격 및 크기                  ║")
  (princ "\n║  5. 자동 작도 실행                         ║")
  (princ "\n║                                            ║")
  (princ "\n╠════════════════════════════════════════════╣")
  (princ "\n║                                            ║")
  (princ "\n║  ■ 주요 레이어                            ║")
  (princ "\n║                                            ║")
  (princ "\n║  PILE           : 엄지말뚝/벽체 (Red)      ║")
  (princ "\n║  WALE           : 띠장 (Blue)              ║")
  (princ "\n║  STRUT          : 버팀보 (Green)           ║")
  (princ "\n║  RETAINING-DIM  : 치수선 (Yellow)          ║")
  (princ "\n║  RETAINING-TEXT : 문자 (White)             ║")
  (princ "\n║                                            ║")
  (princ "\n╠════════════════════════════════════════════╣")
  (princ "\n║                                            ║")
  (princ "\n║  ■ 권장 설정                              ║")
  (princ "\n║                                            ║")
  (princ "\n║  H-Pile+토류판:                            ║")
  (princ "\n║    - H-Pile 간격: 1.5~2.5m                 ║")
  (princ "\n║    - H-Pile 크기: H-250~400                ║")
  (princ "\n║    - 띠장: H-300~500                       ║")
  (princ "\n║                                            ║")
  (princ "\n║  CIP:                                      ║")
  (princ "\n║    - 말뚝 간격: 1.2~2.0m                   ║")
  (princ "\n║    - 말뚝 직경: Φ600~1200                 ║")
  (princ "\n║    - 띠장: H-400~600                       ║")
  (princ "\n║                                            ║")
  (princ "\n║  S.C.W:                                    ║")
  (princ "\n║    - 벽체 두께: 600~1000mm                 ║")
  (princ "\n║    - H-Beam 간격: 1.5~2.5m                 ║")
  (princ "\n║    - 띠장: H-400~600                       ║")
  (princ "\n║                                            ║")
  (princ "\n╠════════════════════════════════════════════╣")
  (princ "\n║                                            ║")
  (princ "\n║  ■ 주의사항                               ║")
  (princ "\n║                                            ║")
  (princ "\n║  - 경계선은 반드시 Polyline 또는           ║")
  (princ "\n║    연결된 Line으로 작성                    ║")
  (princ "\n║  - 도면 단위는 mm 기준                     ║")
  (princ "\n║  - 입력 파라미터는 <기본값> 제공          ║")
  (princ "\n║    (Enter 키로 기본값 사용 가능)           ║")
  (princ "\n║                                            ║")
  (princ "\n╚════════════════════════════════════════════╝")
  (princ "\n")
  (princ)
)

;; 버전 정보
(defun C:RETAINING-VERSION (/ )
  (princ "\n")
  (princ "\n╔════════════════════════════════════════════╗")
  (princ "\n║                                            ║")
  (princ "\n║    가시설 흙막이 벽체 자동 작도 시스템    ║")
  (princ "\n║                                            ║")
  (princ "\n║    Version: 1.0.0                          ║")
  (princ "\n║    Date: 2026-01-13                        ║")
  (princ "\n║                                            ║")
  (princ "\n║    지반사업부 설계팀                       ║")
  (princ "\n║                                            ║")
  (princ "\n╚════════════════════════════════════════════╝")
  (princ "\n")
  (princ)
)

;;; ----------------------------------------------------------------------
;;; 시작 메시지
;;; ----------------------------------------------------------------------

(princ "\n")
(princ "\n╔════════════════════════════════════════════╗")
(princ "\n║                                            ║")
(princ "\n║    가시설 흙막이 벽체 자동 작도 시스템    ║")
(princ "\n║                                            ║")
(princ "\n║    Version: 1.0.0                          ║")
(princ "\n║                                            ║")
(princ "\n╠════════════════════════════════════════════╣")
(princ "\n║                                            ║")
(princ "\n║  명령어: RETAINING 또는 RT                 ║")
(princ "\n║                                            ║")
(princ "\n║  도움말: RETAINING-HELP                    ║")
(princ "\n║                                            ║")
(princ "\n╚════════════════════════════════════════════╝")
(princ "\n")
(princ)
