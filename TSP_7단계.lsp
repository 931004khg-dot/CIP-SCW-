;;; ==========================================================================

;;; TSP.lsp - Temporary Structure Plan (Integrated Version)

;;; ==========================================================================



(vl-load-com)



;;; ==========================================================================

;;; [SECTION 1] 전역 변수 및 설정 (Global Variables)

;;; ==========================================================================



;;; ==========================================================================

;;; [교체 변수] 사용자 정의 지층 해치 패턴(PAT) 데이터베이스 (총 16종)

;;; ==========================================================================

(setq *tsp-custom-pat-data*

  '(

    ("CH_"

     "*CH_, 압축성 높은 점토"

     "45,0,0,0,3.53553391"

     "45,0.4,0,0,3.53553391"

    )

    ("CL_"

     "*CL_, 압축성 낮은 점토"

     "45,0,0,0,3.53553391"

    )

    ("GC_"

     "*GC_, 점토질 자갈"

     "45,0,0,0,3.53553391"

     "0,-0.21,0.72,2.5,2.5,0.42,-4.58"

     "0,-0.45,0.6,2.5,2.5,0.9,-4.1"

     "0,-0.5763,0.48,2.5,2.5,1.1526,-3.8474"

     "0,-0.658,0.36,2.5,2.5,1.3159,-3.6841"

     "0,-0.7106,0.24,2.5,2.5,1.4211,-3.5789"

     "0,-0.7403,0.12,2.5,2.5,1.4807,-3.5193"

     "0,-0.75,0,2.5,2.5,1.5,-3.5"

     "0,-0.7403,-0.12,2.5,2.5,1.4807,-3.5193"

     "0,-0.7106,-0.24,2.5,2.5,1.4211,-3.5789"

     "0,-0.658,-0.36,2.5,2.5,1.3159,-3.6841"

     "0,-0.5763,-0.48,2.5,2.5,1.1526,-3.8474"

     "0,-0.45,-0.6,2.5,2.5,0.9,-4.1"

     "0,-0.21,-0.72,2.5,2.5,0.42,-4.58"

    )

    ("GM_"

     "*GM_, 실트질 자갈"

     "90,0,0,0,2.5"

     "0,-0.21,0.72,2.5,2.5,0.42,-4.58"

     "0,-0.45,0.6,2.5,2.5,0.9,-4.1"

     "0,-0.5763,0.48,2.5,2.5,1.1526,-3.8474"

     "0,-0.658,0.36,2.5,2.5,1.3159,-3.6841"

     "0,-0.7106,0.24,2.5,2.5,1.4211,-3.5789"

     "0,-0.7403,0.12,2.5,2.5,1.4807,-3.5193"

     "0,-0.75,0,2.5,2.5,1.5,-3.5"

     "0,-0.7403,-0.12,2.5,2.5,1.4807,-3.5193"

     "0,-0.7106,-0.24,2.5,2.5,1.4211,-3.5789"

     "0,-0.658,-0.36,2.5,2.5,1.3159,-3.6841"

     "0,-0.5763,-0.48,2.5,2.5,1.1526,-3.8474"

     "0,-0.45,-0.6,2.5,2.5,0.9,-4.1"

     "0,-0.21,-0.72,2.5,2.5,0.42,-4.58"

    )

    ("GP_"

     "*GP_, 입도 불량 자갈"

     "0,0,0,2.5,2.5,0.42,-4.58"

     "0,-0.24,-0.12,2.5,2.5,0.9,-4.1"

     "0,-0.3663,-0.24,2.5,2.5,1.1526,-3.8474"

     "0,-0.448,-0.36,2.5,2.5,1.3159,-3.6841"

     "0,-0.5006,-0.48,2.5,2.5,1.4211,-3.5789"

     "0,-0.5303,-0.6,2.5,2.5,1.4807,-3.5193"

     "0,-0.54,-0.72,2.5,2.5,1.5,-3.5"

     "0,-0.5303,-0.84,2.5,2.5,1.4807,-3.5193"

     "0,-0.5006,-0.96,2.5,2.5,1.4211,-3.5789"

     "0,-0.448,-1.08,2.5,2.5,1.3159,-3.6841"

     "0,-0.3663,-1.2,2.5,2.5,1.1526,-3.8474"

     "0,-0.24,-1.32,2.5,2.5,0.9,-4.1"

     "0,0,-1.44,2.5,2.5,0.42,-4.58"

     "63.43494882,2.91,4.08,4.47213596,2.23606798,0.2236068,-10.95673309"

     "26.56505118,2.71,3.98,6.70820393,2.23606798,0.2236068,-10.95673309"

     "333.43494882,2.51,4.08,4.47213596,2.23606798,0.2236068,-10.95673309"

     "296.56505118,2.41,4.28,6.70820393,2.23606797,0.2236068,-10.95673309"

     "153.43494882,0.41,1.98,4.47213595,2.23606797,0.2236068,-10.95673309"

     "116.56505118,0.51,1.78,6.70820393,2.23606798,0.2236068,-10.95673309"

     "63.43494882,0.41,1.58,4.47213596,2.23606798,0.2236068,-10.95673309"

     "26.56505118,0.21,1.48,6.70820393,2.23606798,0.2236068,-10.95673309"

     "243.43494882,2.51,-0.52,4.47213596,2.23606798,0.2236068,-10.95673309"

     "206.56505118,2.71,-0.42,6.70820393,2.23606797,0.2236068,-10.95673309"

     "153.43494882,2.91,-0.52,4.47213595,2.23606797,0.2236068,-10.95673309"

     "116.56505118,3.01,-0.72,6.70820393,2.23606798,0.2236068,-10.95673309"

     "333.43494882,5.01,1.58,4.47213596,2.23606798,0.2236068,-10.95673309"

     "296.56505118,4.91,1.78,6.70820393,2.23606797,0.2236068,-10.95673309"

     "243.43494882,5.01,1.98,4.47213596,2.23606798,0.2236068,-10.95673309"

     "206.56505118,5.21,2.08,6.70820393,2.23606797,0.2236068,-10.95673309"

    )

    ("GW_"

     "*GW_, 입도 양호 자갈"

     "189.46232221,0,0,25.48184303,0.82199494,0.30413812,-30.10967452"

     "210.96375653,-0.3,-0.05,18.00735144,0.85749293,0.2915476,-28.86321188"

     "239.03624347,-0.55,-0.2,11.14740804,0.85749292,0.2915476,-28.86321188"

     "260.53767779,-0.7,-0.45,4.93196962,0.82199494,0.30413813,-30.10967453"

     "279.46232221,-0.75,-0.75,25.48184303,0.82199494,0.30413812,-30.10967452"

     "300.96375653,-0.7,-1.05,18.00735144,0.85749292,0.2915476,-28.86321188"

     "329.03624347,-0.55,-1.3,11.14740804,0.85749292,0.2915476,-28.86321188"

     "350.53767779,-0.3,-1.45,4.93196962,0.82199493,0.30413813,-30.10967453"

     "9.46232221,0,-1.5,25.48184303,0.82199494,0.30413812,-30.10967452"

     "30.96375653,0.3,-1.45,18.00735144,0.85749293,0.29154759,-28.86321188"

     "59.03624347,0.55,-1.3,11.14740804,0.85749292,0.2915476,-28.86321188"

     "80.53767779,0.7,-1.05,4.93196962,0.82199494,0.30413812,-30.10967452"

     "99.46232221,0.75,-0.75,25.48184303,0.82199494,0.30413812,-30.10967452"

     "120.96375653,0.7,-0.45,18.00735144,0.85749292,0.2915476,-28.86321188"

     "149.03624347,0.55,-0.2,11.14740804,0.85749292,0.2915476,-28.86321188"

     "170.53767779,0.3,-0.05,4.93196962,0.82199493,0.30413812,-30.10967452"

     "99.46232221,-1.75,-3.25,25.48184303,0.82199494,0.30413812,-30.10967452"

     "120.96375653,-1.8,-2.95,18.00735144,0.85749292,0.2915476,-28.86321188"

     "149.03624347,-1.95,-2.7,11.14740804,0.85749292,0.2915476,-28.86321188"

     "170.53767779,-2.2,-2.55,4.93196962,0.82199493,0.30413812,-30.10967452"

     "9.46232221,-2.5,1,25.48184303,0.82199494,0.30413812,-30.10967452"

     "30.96375653,-2.2,1.05,18.00735144,0.85749293,0.29154759,-28.86321188"

     "59.03624347,-1.95,1.2,11.14740804,0.85749292,0.2915476,-28.86321188"

     "80.53767779,-1.8,1.45,4.93196962,0.82199494,0.30413812,-30.10967452"

     "279.46232221,1.75,1.75,25.48184303,0.82199494,0.30413812,-30.10967452"

     "300.96375653,1.8,1.45,18.00735144,0.85749292,0.2915476,-28.86321188"

     "329.03624347,1.95,1.2,11.14740804,0.85749292,0.2915476,-28.86321188"

     "350.53767779,2.2,1.05,4.93196962,0.82199493,0.30413813,-30.10967453"

     "189.46232221,2.5,-2.5,25.48184303,0.82199494,0.30413812,-30.10967452"

     "210.96375653,2.2,-2.55,18.00735144,0.85749293,0.2915476,-28.86321188"

     "239.03624347,1.95,-2.7,11.14740804,0.85749292,0.2915476,-28.86321188"

     "260.53767779,1.8,-2.95,4.93196962,0.82199494,0.30413813,-30.10967453"

     "63.43494882,0.2,1.55,4.47213596,2.23606798,0.2236068,-10.95673309"

     "26.56505118,0,1.45,6.70820393,2.23606798,0.2236068,-10.95673309"

     "333.43494882,-0.2,1.55,4.47213596,2.23606798,0.2236068,-10.95673309"

     "296.56505118,-0.3,1.75,6.70820393,2.23606797,0.2236068,-10.95673309"

     "153.43494882,-2.3,-0.55,4.47213595,2.23606797,0.2236068,-10.95673309"

     "116.56505118,-2.2,-0.75,6.70820393,2.23606798,0.2236068,-10.95673309"

     "63.43494882,-2.3,-0.95,4.47213596,2.23606798,0.2236068,-10.95673309"

     "26.56505118,-2.5,-1.05,6.70820393,2.23606798,0.2236068,-10.95673309"

     "243.43494882,-0.2,-3.05,4.47213596,2.23606798,0.2236068,-10.95673309"

     "206.56505118,0,-2.95,6.70820393,2.23606797,0.2236068,-10.95673309"

     "153.43494882,0.2,-3.05,4.47213595,2.23606797,0.2236068,-10.95673309"

     "116.56505118,0.3,-3.25,6.70820393,2.23606798,0.2236068,-10.95673309"

     "333.43494882,2.3,-0.95,4.47213596,2.23606798,0.2236068,-10.95673309"

     "296.56505118,2.2,-0.75,6.70820393,2.23606797,0.2236068,-10.95673309"

     "243.43494882,2.3,-0.55,4.47213596,2.23606798,0.2236068,-10.95673309"

     "206.56505118,2.5,-0.45,6.70820393,2.23606797,0.2236068,-10.95673309"

    )

    ("HR_"

     "*HR_, 경암"

     "0,0,0,2.5,2.5,1.5,-3.5"

     "90,0.75,-0.75,2.5,2.5,1.5,-3.5"

    )

    ("MH_"

     "*MH_, 압축성 높은 실트"

     "90,0,0,0,2.5"

     "90,0.4,0,0,2.5"

    )

    ("ML_"

     "*ML_, 압축성 낮은 실트"

     "90,0,0,0,2.5"

    )

    ("RS_"

     "*RS_, 풍화토"

     "45,0,0,0.1767767,0.1767767"

     "45,0.01414,-0.01414,0.1767767,0.1767767"

     "0,0.0175,0.1725,0.125,0.125,0.06,-0.19"

     "90,0.0475,0.1425,0.125,0.125,0.06,-0.19"

     "45,-0.01415,0.01414,0.1767767,0.1767767"

    )

    ("SC_"

     "*SC_, 점토질 모래"

     "45,0,0,0,3.53553391"

     "0,-0.2062,0.4,2.5,2.5,0.4123,-4.5877"

     "0,-0.3354,0.3,2.5,2.5,0.6708,-4.3292"

     "0,-0.4031,0.2,2.5,2.5,0.8062,-4.1938"

     "0,-0.4387,0.1,2.5,2.5,0.8775,-4.1225"

     "0,-0.45,0,2.5,2.5,0.9,-4.1"

     "0,-0.4387,-0.1,2.5,2.5,0.8775,-4.1225"

     "0,-0.4031,-0.2,2.5,2.5,0.8062,-4.1938"

     "0,-0.3354,-0.3,2.5,2.5,0.6708,-4.3292"

     "0,-0.2062,-0.4,2.5,2.5,0.4123,-4.5877"

    )

    ("SM_"

     "*SM_, 실트질 모래"

     "90,0,0,0,2.5"

     "0,-0.2062,0.4,2.5,2.5,0.4123,-4.5877"

     "0,-0.3354,0.3,2.5,2.5,0.6708,-4.3292"

     "0,-0.4031,0.2,2.5,2.5,0.8062,-4.1938"

     "0,-0.4387,0.1,2.5,2.5,0.8775,-4.1225"

     "0,-0.45,0,2.5,2.5,0.9,-4.1"

     "0,-0.4387,-0.1,2.5,2.5,0.8775,-4.1225"

     "0,-0.4031,-0.2,2.5,2.5,0.8062,-4.1938"

     "0,-0.3354,-0.3,2.5,2.5,0.6708,-4.3292"

     "0,-0.2062,-0.4,2.5,2.5,0.4123,-4.5877"

    )

    ("SP_"

     "*SP_, 입도 불량 모래"

     "0,0,0,2.5,2.5,0.4123,-4.5877"

     "0,-0.1292,-0.1,2.5,2.5,0.6708,-4.3292"

     "0,-0.1969,-0.2,2.5,2.5,0.8062,-4.1938"

     "0,-0.2325,-0.3,2.5,2.5,0.8775,-4.1225"

     "0,-0.2438,-0.4,2.5,2.5,0.9,-4.1"

     "0,-0.2325,-0.5,2.5,2.5,0.8775,-4.1225"

     "0,-0.1969,-0.6,2.5,2.5,0.8062,-4.1938"

     "0,-0.1292,-0.7,2.5,2.5,0.6708,-4.3292"

     "0,0,-0.8,2.5,2.5,0.4123,-4.5877"

    )

    ("SR_"

     "*SR_, 연암"

     "0,0,0,2.5,2.5,1.5,-3.5"

     "90,0.75,-0.75,2.5,2.5,1.5,-3.5"

     "45,-1.75,0,0,3.53553391"

    )

    ("SW_"

     "*SW_, 입도 양호 모래"

     "206.56505118,0,0,6.70820393,2.23606797,0.3354102,-10.84492969"

     "153.43494882,0.3,-0.15,4.47213595,2.23606797,0.3354102,-10.84492969"

     "116.56505118,0.45,-0.45,6.70820393,2.23606798,0.3354102,-10.84492969"

     "63.43494882,0.3,-0.75,4.47213596,2.23606798,0.33541019,-10.84492969"

     "26.56505118,0,-0.9,6.70820393,2.23606798,0.3354102,-10.84492969"

     "333.43494882,-0.3,-0.75,4.47213596,2.23606798,0.3354102,-10.84492969"

     "296.56505118,-0.45,-0.45,6.70820393,2.23606797,0.3354102,-10.84492969"

     "243.43494882,-0.3,-0.15,4.47213596,2.23606798,0.3354102,-10.84492969"

     "153.43494882,-2.2,-2.65,4.47213595,2.23606797,0.3354102,-10.84492969"

     "116.56505118,-2.05,-2.95,6.70820393,2.23606798,0.3354102,-10.84492969"

     "63.43494882,-2.2,1.75,4.47213596,2.23606798,0.33541019,-10.84492969"

     "26.56505118,-2.5,1.6,6.70820393,2.23606798,0.3354102,-10.84492969"

     "333.43494882,2.2,1.75,4.47213596,2.23606798,0.3354102,-10.84492969"

     "296.56505118,2.05,2.05,6.70820393,2.23606797,0.3354102,-10.84492969"

     "243.43494882,2.2,-2.65,4.47213596,2.23606798,0.3354102,-10.84492969"

     "206.56505118,2.5,-2.5,6.70820393,2.23606797,0.3354102,-10.84492969"

    )

    ("WR_"

     "*WR_, 풍화암"

     "0,0,0,2.5,2.5,1.5,-3.5"

     "90,0.75,-0.75,2.5,2.5,1.5,-3.5"

     "45,-1.95,0,0,3.53553391"

     "45,-1.55,0,0,3.53553391"

    )

  )

)



;;; 표준 규격 리스트 중앙 관리

(setq *tsp-std-wall-list* '(

  "H 250×250×9/14"      ; 0

  "H 298×201×9/14"      ; 1

  "H 300×300×10/15"    ; 2

  "H 350×350×12/19"    

  "H 400×400×13/21"    

))



(setq *tsp-std-strut-list* '(

  "H 250×250×9/14"    

  "H 298×201×9/14"    

  "H 300×300×10/15"   

  "H 350×350×12/19"   

  "H 400×400×13/21"   

))



;;; 1.1 H-Pile 및 띠장(Wale) 규격 변수

;;; (기본값 설정: 리스트의 인덱스를 사용하여 안전하게 가져옴)

(setq *tsp-hpile-spec* (nth 2 *tsp-std-wall-list*))     ; 기본값: H 300

(setq *tsp-hpile-custom* '(300 300 10 15))

(setq *tsp-wale-spec* (nth 2 *tsp-std-wall-list*))      ; 기본값: H 300

(setq *tsp-wale-custom* '(300 300 10 15))



;; 1.14 벽체 공법 타입 변수
;; [1단계 고정] 기본값은 항상 "HPILE", C.I.P는 사용자가 최종 확정 시에만 "CIP"로 변경됨

(setq *tsp-wall-type* "HPILE")      ; 벽체 공법 타입 기본값 ("HPILE" or "CIP")



;; 1.15 C.I.P 흙막이벽 전용 설정 변수

(setq *tsp-cip-max-depth* "10.0")   ; 최대 굴착 깊이

(setq *tsp-cip-embed-depth* "3.0")  ; 근입 깊이

(setq *tsp-cip-hpile-idx* "0")      ; H-Pile 규격 인덱스

(setq *tsp-cip-wale-idx* "0")       ; 띠장(Wale) 규격 인덱스

(setq *tsp-cip-dia* "450")          ; C.I.P 직경 기본값 (D450)

(setq *tsp-cip-mode-idx* "0")       ; 배열 모드 (0: 맞닿음/Tangent, 1: 겹침/Secant)

(setq *tsp-cip-overlap* "0")        ; 겹침량 기본값

(setq *tsp-cip-interval-idx* "0")   ; H-Pile 삽입 간격 (0: 1/1, 1: 1/2, 2: 1/3)



;;; 1.2 배치 및 굴착 설정 변수

(setq *tsp-ctc* 2.0)                                ; C.T.C 간격 (m)

(setq *tsp-timber-thickness* 60)                ; 토류판 두께 (mm)

(setq *tsp-max-excavation-depth* 10.0)       ; 최대 굴착 깊이 (m)

(setq *tsp-embedment-depth* 3.0)             ; 근입 깊이 (m)



;;; 1.3 지반 및 수위 변수

;;; 저장 형식: (이름 깊이 해치패턴 해치축척 해치각도 종류명 내부마찰각)

(setq *tsp-soil-layers* '())

(setq *tsp-water-chk* "0")                   ; 지하수위 체크 여부 ("0"=Off, "1"=On)

(setq *tsp-water-depth* 0.0)                 ; 지하수위 깊이 (m)

(setq *tsp-shotcrete-enable* "1")            ; 숏크리트 생성 마스터 스위치 ("0"=Off, "1"=On)

(setq *tsp-shotcrete-chk* "0")               ; 숏크리트 암반 선택 여부 ("0"=Off, "1"=On)

(setq *tsp-shotcrete-layer* "")              ; 숏크리트 시작 지정 암반층 이름

(setq *tsp-shotcrete-thick* 0.0)             ; 숏크리트 두께 T(mm)



;;; 1.4 지보재 변수

(setq *tsp-support-list* '())



;;; 1.5 도면 스케일 변수

(setq *tsp-scale-section* 200)               ; 단면도 스케일 (기본 1/200)

(setq *tsp-scale-plan* 600)                  ; 평면도 스케일 (기본 1/600)



;;; 1.6 프로젝트 관리 변수

(setq *tsp-current-project-name* "")         ; 현재 프로젝트 이름

(setq *tsp-data-dict-name* "TSP_DATA_DICT")  ; 딕셔너리 저장소 이름



;;; 1.7 변경 사항 감지 플래그

(setq *tsp-data-dirty* nil)                  ; 데이터 변경 여부 (T=변경됨, nil=변경없음)



;;; ;;; ==========================================================================

;;; [SECTION 2] 공통 유틸리티 (Common Utilities)

;;; ==========================================================================



;;; --------------------------------------------------------------------------

;;; Function: get-hatch-scale-factor

;;; Description: 도면의 MEASUREMENT 변수(0=Imperial, 1=Metric)에 따라 해치 스케일 보정값 반환

;;; --------------------------------------------------------------------------

(defun get-hatch-scale-factor ()

  (if (= (getvar "MEASUREMENT") 0)

    25.4  ;; Imperial 도면이면 25.4배 확대 (mm 단위처럼 보이게 보정)

    1.0   ;; Metric 도면이면 그대로 사용

  )

)



;;; --------------------------------------------------------------------------

;;; Function: create-layer-if-not-exists

;;; Description: 레이어가 존재하지 않으면 지정된 색상으로 생성

;;; --------------------------------------------------------------------------

(defun create-layer-if-not-exists (layer-name color / )

  (if (not (tblsearch "LAYER" layer-name))

    (command "._LAYER" "_M" layer-name "_C" color layer-name "")

  )

)



;;; --------------------------------------------------------------------------

;;; Function: is-numeric

;;; Description: 문자열이 숫자로 변환 가능한지 확인 (True/False)

;;; --------------------------------------------------------------------------

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



;;; --------------------------------------------------------------------------

;;; Function: parse-h-spec

;;; Description: "H 298x201..." 형식의 문자열을 파싱하여 리스트 (298 201...)로 변환

;;; --------------------------------------------------------------------------

(defun parse-h-spec (spec-str / clean-str i ch result-str)

  (if (and spec-str (wcmatch spec-str "H *"))

    (progn

      (setq clean-str (substr spec-str 3))

      (setq result-str "")

      (setq i 1)

      (while (<= i (strlen clean-str))

        (setq ch (substr clean-str i 1))

        (cond

          ((and (>= (ascii ch) 48) (<= (ascii ch) 57))

           (setq result-str (strcat result-str ch)))

          (t

           (if (> (strlen result-str) 0)

             (if (/= (substr result-str (strlen result-str) 1) " ")

               (setq result-str (strcat result-str " "))

             )

           ))

        )

        (setq i (1+ i))

      )

      (read (strcat "(" result-str ")"))

    )

    nil

  )

)



;;; --------------------------------------------------------------------------

;;; Function: format-depth-text

;;; Description: 깊이 값을 소수점 1자리 문자열로 포맷팅 (ex: "5.0")

;;; --------------------------------------------------------------------------

(defun format-depth-text (depth / str)

  (setq str (rtos depth 2 1))

  (if (not (vl-string-search "." str))

    (setq str (strcat str ".0"))

  )

  str

)



;;; --------------------------------------------------------------------------

;;; Function: tsp-get-project-list

;;; Description: 저장된 프로젝트 목록 반환 (직접 순회 방식)

;;; --------------------------------------------------------------------------

(defun tsp-get-project-list (/ proj-list dict-name raw-data item key)

  (setq dict-name "TSP_DATA_DICT")

  (setq proj-list '())

  

  ;; 1. 딕셔너리의 전체 데이터를 리스트로 가져옴

  (setq raw-data (vlax-ldata-list dict-name))

  

  ;; 2. 데이터가 존재하면 키(Project Name)만 추출

  (if raw-data

    (foreach item raw-data

      (setq key (car item)) ; dotted pair의 앞부분(key) 추출

      (if (= (type key) 'STR) ; 키가 문자열인 경우만 추가

        (setq proj-list (append proj-list (list key)))

      )

    )

  )

  proj-list

)



;;; --------------------------------------------------------------------------

;;; Function: tsp-generate-default-project-name

;;; Description: 중복되지 않는 다음 프로젝트 이름 생성 (Project_N+1)

;;; --------------------------------------------------------------------------

(defun tsp-generate-default-project-name (/ proj-list max-num curr-num name upper-name)

  (setq proj-list (tsp-get-project-list))

  (setq max-num 0)

  

  ;; 기존 목록에서 번호 파싱

  (foreach name proj-list

    (setq upper-name (strcase name))

    ;; "PROJECT_" 로 시작하는지 확인 (와일드카드 매칭)

    (if (wcmatch upper-name "PROJECT_*")

      (progn

        ;; "PROJECT_" 글자수(8) 다음부터 숫자 추출

        ;; 문자열이 "PROJECT_10" 이면 substr 9부터가 "10"

        (setq curr-num (atoi (substr upper-name 9)))

        (if (> curr-num max-num) (setq max-num curr-num))

      )

    )

  )

  

  ;; 가장 큰 번호 + 1 반환

  (strcat "Project_" (itoa (1+ max-num)))

)



;;; --------------------------------------------------------------------------

;;; Function: tsp-save-project-data

;;; Description: 데이터 저장

;;; --------------------------------------------------------------------------

(defun tsp-save-project-data (proj-name / data-pack dict-name)

  (setq dict-name "TSP_DATA_DICT") 

  

  ;; 입력된 이름이 없으면 자동 생성

  (if (or (= proj-name "") (= proj-name nil))

    (setq proj-name (tsp-generate-default-project-name))

  )

  

  ;; 저장할 데이터 패키징

  (setq data-pack (list

    *segment-list*

    *tsp-hpile-spec*

    *tsp-hpile-custom*

    *tsp-wale-spec*

    *tsp-wale-custom*

    *tsp-ctc*

    *tsp-timber-thickness*

    *tsp-max-excavation-depth*

    *tsp-embedment-depth*

    *tsp-soil-layers*

    *tsp-water-chk*

    *tsp-water-depth*

    *tsp-support-list*

    *tsp-scale-section*

    *tsp-scale-plan*

  ))

  

  ;; ldata에 저장 수행

  (vlax-ldata-put dict-name proj-name data-pack)

  

  (setq *tsp-current-project-name* proj-name)

  (princ (strcat "\n[시스템] 프로젝트 '" proj-name "' 이(가) 도면에 저장되었습니다."))

  proj-name

)



;;; --------------------------------------------------------------------------

;;; Function: tsp-load-project-data

;;; Description: 데이터 불러오기 (형상일치 검증 및 좌표 보호)

;;; --------------------------------------------------------------------------

(defun tsp-load-project-data (proj-name / data-pack dict-name saved-segments curr-len saved-len i curr-seg saved-seg shape-match curr-len-val saved-len-val curr-ang-val saved-ang-val merged-seg merged-list)

  (setq dict-name "TSP_DATA_DICT") 

  (setq data-pack (vlax-ldata-get dict-name proj-name))

  

  (if data-pack

    (progn

      (setq saved-segments (nth 0 data-pack))

      (setq curr-len (length *segment-list*))

      (setq saved-len (length saved-segments))

      

      (if (/= curr-len saved-len)

        (progn

          (alert (strcat "현재 경계선과 저장된 프로젝트의 세그먼트 개수가 달라 불러올 수 없습니다.\n\n- 현재 선분 수: " (itoa curr-len) "개\n- 저장된 선분 수: " (itoa saved-len) "개"))

          nil 

        )

        (progn

          (setq shape-match T)

          (setq i 0)

          (while (and (< i curr-len) shape-match)

            (setq curr-seg (nth i *segment-list*))

            (setq saved-seg (nth i saved-segments))

            

            (setq curr-len-val (cdr (assoc 'LENGTH curr-seg)))

            (setq saved-len-val (cdr (assoc 'LENGTH saved-seg)))

            (setq curr-ang-val (cdr (assoc 'ANGLE curr-seg)))

            (setq saved-ang-val (cdr (assoc 'ANGLE saved-seg)))

            

            (if (or (not (equal curr-len-val saved-len-val 0.1))

                    (not (equal curr-ang-val saved-ang-val 0.1)))

              (setq shape-match nil)

            )

            (setq i (1+ i))

          )

          

          (if (not shape-match)

            (progn

              (alert "경계선의 형상(각 선분의 길이 또는 꺾임 각도)이 일치하지 않아 불러올 수 없습니다.\n\n정확히 동일한 형태의 경계선에서만 불러오기가 가능합니다.")

              nil

            )

            (progn

              (setq merged-list '())

              (setq i 0)

              (while (< i curr-len)

                (setq curr-seg (nth i *segment-list*))

                (setq saved-seg (nth i saved-segments))

                

                (setq merged-seg (list

                  (cons 'ID (cdr (assoc 'ID curr-seg)))

                  (cons 'NAME (cdr (assoc 'NAME saved-seg)))

                  (cons 'IS-DEFINED (cdr (assoc 'IS-DEFINED saved-seg)))

                  (cons 'SECTION-DRAW (cdr (assoc 'SECTION-DRAW saved-seg)))

                  (cons 'SOIL-DATA (cdr (assoc 'SOIL-DATA saved-seg)))

                  (cons 'WALL-SPEC (cdr (assoc 'WALL-SPEC saved-seg)))

                  (cons 'WALL-CUSTOM (cdr (assoc 'WALL-CUSTOM saved-seg)))

                  (cons 'WALE-SPEC (cdr (assoc 'WALE-SPEC saved-seg)))

                  (cons 'WALE-CUSTOM (cdr (assoc 'WALE-CUSTOM saved-seg)))

                  (cons 'CTC (cdr (assoc 'CTC saved-seg)))

                  (cons 'TIMBER-THICKNESS (cdr (assoc 'TIMBER-THICKNESS saved-seg)))

                  (cons 'MAX-DEPTH (cdr (assoc 'MAX-DEPTH saved-seg)))

                  (cons 'EMBED-DEPTH (cdr (assoc 'EMBED-DEPTH saved-seg)))

                  (cons 'WATER-CHK (cdr (assoc 'WATER-CHK saved-seg)))

                  (cons 'WATER-DEPTH (cdr (assoc 'WATER-DEPTH saved-seg)))

                  (cons 'SHOTCRETE-ENABLE (cdr (assoc 'SHOTCRETE-ENABLE saved-seg)))

                  (cons 'SHOTCRETE-CHK (cdr (assoc 'SHOTCRETE-CHK saved-seg)))

                  (cons 'SHOTCRETE-LAYER (cdr (assoc 'SHOTCRETE-LAYER saved-seg)))

                  (cons 'SHOTCRETE-THICK (cdr (assoc 'SHOTCRETE-THICK saved-seg)))

                  (cons 'SUPPORT-LIST (cdr (assoc 'SUPPORT-LIST saved-seg)))

                  (cons 'CORNER-BRACE (cdr (assoc 'CORNER-BRACE saved-seg)))

                  (cons 'UPGRADE-WALE (cdr (assoc 'UPGRADE-WALE saved-seg)))

                  (cons 'V-START (cdr (assoc 'V-START curr-seg)))

                  (cons 'V-END (cdr (assoc 'V-END curr-seg)))

                  (cons 'ANGLE (cdr (assoc 'ANGLE curr-seg)))

                  (cons 'LENGTH (cdr (assoc 'LENGTH curr-seg)))

                ))

                (setq merged-list (append merged-list (list merged-seg)))

                (setq i (1+ i))

              )

              

              (setq *segment-list* merged-list)

              (setq *tsp-hpile-spec* (nth 1 data-pack))

              (setq *tsp-hpile-custom* (nth 2 data-pack))

              (setq *tsp-wale-spec* (nth 3 data-pack))

              (setq *tsp-wale-custom* (nth 4 data-pack))

              (setq *tsp-ctc* (nth 5 data-pack))

              (setq *tsp-timber-thickness* (nth 6 data-pack))

              (setq *tsp-max-excavation-depth* (nth 7 data-pack))

              (setq *tsp-embedment-depth* (nth 8 data-pack))

              (setq *tsp-soil-layers* (nth 9 data-pack))

              (setq *tsp-water-chk* (nth 10 data-pack))

              (setq *tsp-water-depth* (nth 11 data-pack))

              (setq *tsp-support-list* (nth 12 data-pack))

              (setq *tsp-scale-section* (nth 13 data-pack))

              (setq *tsp-scale-plan* (nth 14 data-pack))

              

              (setq *tsp-current-project-name* proj-name)

              (princ (strcat "\n[시스템] 프로젝트 '" proj-name "' 의 설정값이 완벽하게 불러와졌습니다."))

              T

            )

          )

        )

      )

    )

    (progn (alert "데이터를 불러올 수 없습니다.") nil)

  )

)



;;; --------------------------------------------------------------------------

;;; Function: tsp-delete-project-data

;;; Description: 데이터 딕셔너리에서 프로젝트 삭제

;;; --------------------------------------------------------------------------

(defun tsp-delete-project-data (proj-name / dict-name)

  (setq dict-name "TSP_DATA_DICT")

  (vlax-ldata-delete dict-name proj-name)

  (princ (strcat "\n[시스템] 프로젝트 '" proj-name "' 이(가) 삭제되었습니다."))

  T

)



;;; --------------------------------------------------------------------------

;;; Function: save-prompt-dialog-callback

;;; Description: 저장 버튼 클릭 시 팝업창 띄우기 (예:1, 아니오:2, 취소:0 반환)

;;; --------------------------------------------------------------------------

(defun save-prompt-dialog-callback (dcl-path / dcl-id result)

  (setq dcl-id (load_dialog dcl-path))

  (if (new_dialog "tsp_save_prompt" dcl-id)

    (progn

      (action_tile "btn_yes" "(done_dialog 1)")  ;; 다른 이름으로 저장

      (action_tile "btn_no" "(done_dialog 2)")   ;; 덮어쓰기

      (action_tile "cancel" "(done_dialog 0)")   ;; 취소

      (setq result (start_dialog))

      (unload_dialog dcl-id)

      result

    )

    0

  )

)



;;; ==========================================================================

;;; [SECTION 2-1] 그룹 및 객체 관리 유틸리티 (Group & Entity Utils)

;;; ==========================================================================



;;; --------------------------------------------------------------------------

;;; Function: tsp-get-entity-handle

;;; Description: 엔티티 고유 핸들(Handle) ID를 추출

;;; --------------------------------------------------------------------------

(defun tsp-get-entity-handle (ent)

  (cdr (assoc 5 (entget ent)))

)



;;; --------------------------------------------------------------------------

;;; Function: tsp-get-intersection-point

;;; Description: 두 직선(무한)의 교차점을 계산

;;; --------------------------------------------------------------------------

(defun tsp-get-intersection-point (p1 p2 p3 p4)

  ;; nil 옵션은 선분이 아닌 무한 직선으로 간주하여 교차점을 찾음을 의미

  (inters p1 p2 p3 p4 nil) 

)



;;; --------------------------------------------------------------------------

;;; Function: tsp-clear-drawing-group

;;; Description: 특정 경계선(Boundary)에 종속된 기존 작도 그룹을 삭제

;;; --------------------------------------------------------------------------

(defun tsp-clear-drawing-group (boundary-ent / handle group-name dict group pair ent-to-del)

  ;; 1. 경계선 핸들을 기반으로 그룹명 생성 (예: TSP_GRP_12A4)

  (setq handle (tsp-get-entity-handle boundary-ent))

  (setq group-name (strcat "TSP_GRP_" handle))

  

  ;; 2. ACAD_GROUP 딕셔너리 검색

  (setq dict (dictsearch (namedobjdict) "ACAD_GROUP"))

  

  ;; 3. 해당 그룹이 존재할 경우 삭제 절차 진행

  (if (and dict (dictsearch (cdr (assoc -1 dict)) group-name))

    (progn

      (setq group (dictsearch (cdr (assoc -1 dict)) group-name))

      (princ (strcat "\n[시스템] 기존 객체 그룹 '" group-name "' 정리 중..."))

      

      (foreach pair group

        ;; DXF 코드 340은 그룹에 포함된 엔티티의 핸들 포인터

        (if (= (car pair) 340)

          (progn

             (setq ent-to-del (cdr pair))

             ;; [안전장치] 도면에 실제로 존재하는 객체인지 확인 후 삭제

             (if (entget ent-to-del) (entdel ent-to-del))

          )

        )

      )

      ;; 4. 빈 그룹 정의 제거

      (command "._-GROUP" "_E" group-name "")

    )

  )

)



;;; --------------------------------------------------------------------------

;;; Function: tsp-group-last-entities

;;; Description: 생성된 객체고유 그룹화

;;; --------------------------------------------------------------------------

(defun tsp-group-last-entities (start-ent boundary-ent / sset ent handle group-name)

  (setq handle (tsp-get-entity-handle boundary-ent))

  (setq group-name (strcat "TSP_GRP_" handle))

  

  ;; 1. 선택 세트(Selection Set) 생성

  (setq sset (ssadd))

  

  ;; 2. start-ent 이후에 생성된 모든 객체를 수집

  (if start-ent

    (setq ent (entnext start-ent))

    (setq ent (entnext)) ;; 도면이 비어있었을 경우

  )

  

  (while ent

    (ssadd ent sset)

    (setq ent (entnext ent))

  )

  

  ;; 3. 수집된 객체가 있다면 그룹 생성

  (if (> (sslength sset) 0)

    (progn

      ;; (혹시 모를 잔여 그룹명 중복 방지)

      (if (dictsearch (cdr (assoc -1 (dictsearch (namedobjdict) "ACAD_GROUP"))) group-name)

        (command "._-GROUP" "_E" group-name "")

      )

      ;; 그룹 생성 명령 실행

      (command "._-GROUP" "_C" group-name "TSP Auto-Gen" sset "")

      (princ (strcat "\n[시스템] " (itoa (sslength sset)) "개의 객체가 '" group-name "' 그룹으로 생성되었습니다."))

    )

    (princ "\n[경고] 생성된 객체가 없습니다.")

  )

)



;;; --------------------------------------------------------------------------

;;; Function: tsp-apply-custom-hatch

;;; Description: 커스텀 해치 패턴 PAT 생성 및 원점(Origin)/스케일 강제 지정

;;; --------------------------------------------------------------------------

(defun tsp-apply-custom-hatch (ent pat-name scale angle layer color origin-pt / support-dir pat-file f data old-echo old-hporiginmode old-hporigin old-hpannotative old-meas last-ent new-hatch)

  (setq old-echo (getvar "CMDECHO"))

  (setvar "CMDECHO" 0)

  

  ;; 도면마다 스케일이 달라지는 현상 방지용 환경 변수 강제 통일

  (setq old-hpannotative (getvar "HPANNOTATIVE"))

  (setq old-meas (getvar "MEASUREMENT"))

  (if old-hpannotative (setvar "HPANNOTATIVE" 0)) ; 주석 축척 해제

  (setvar "MEASUREMENT" 1) ; 미터법 환경 강제 고정

  

  ;; 해치 원점 강제 지정 모드 설정

  (setq old-hporiginmode (getvar "HPORIGINMODE"))

  (setq old-hporigin (getvar "HPORIGIN"))

  (if origin-pt

    (progn

      (setvar "HPORIGINMODE" 0)

      (setvar "HPORIGIN" (list (car origin-pt) (cadr origin-pt)))

    )

  )

  

  (setq data (cdr (assoc pat-name *tsp-custom-pat-data*)))

  

  ;; Custom 패턴인 경우 .pat 파일 생성

  (if data

    (progn

      (setq support-dir (strcat (getvar "ROAMABLEROOTPREFIX") "Support"))

      (setq pat-file (strcat support-dir "\\" pat-name ".pat"))

      (setq f (open pat-file "w"))

      (foreach line data

        (write-line line f)

      )

      (write-line "" f)

      (close f)

    )

  )

  

  ;; 기본패턴(매립토 등)과 커스텀 구분 없이 모두 Command 방식으로 일괄 생성

  (setq last-ent (entlast))

  (command "_.-HATCH" "_P" pat-name scale angle "_S" ent "" "")

  

  ;; 방금 생성된 해치 객체의 레이어 및 색상 변경

  (setq new-hatch (entnext last-ent))

  (if new-hatch

    (command "_.CHPROP" new-hatch "" "_LA" layer "_C" color "")

  )

  

  ;; 시스템 변수 원상 복구 (기존 사용자 환경 보호)

  (if old-hporiginmode (setvar "HPORIGINMODE" old-hporiginmode))

  (if old-hporigin (setvar "HPORIGIN" old-hporigin))

  (if old-hpannotative (setvar "HPANNOTATIVE" old-hpannotative))

  (setvar "MEASUREMENT" old-meas)

  

  (setvar "CMDECHO" old-echo)

  (princ)

)



;;; ==========================================================================

;;; [SECTION 3] 기하학 및 좌표 계산 (Geometry Utils)

;;; ==========================================================================



;;; --------------------------------------------------------------------------

;;; Function: extract-vertices

;;; Description: 엔티티(LWPOLYLINE 등)에서 정점 좌표(Vertex) 리스트 추출

;;; --------------------------------------------------------------------------

(defun extract-vertices (ent / ent-data vertices item elev pt-ocs pt-wcs first-pt last-pt)

  (setq ent-data (entget ent))

  (setq elev (cdr (assoc 38 ent-data)))

  (if (null elev) (setq elev 0.0))

  (setq vertices '())

  (foreach item ent-data

    (if (= (car item) 10)

      (progn

        ;; LWPOLYLINE의 DXF 10은 OCS 좌표이므로, 이를 WCS 절대좌표로 안전하게 변환

        (setq pt-ocs (list (nth 1 item) (nth 2 item) elev))

        (setq pt-wcs (trans pt-ocs ent 0))

        ;; Z축을 0으로 평탄화하여 2D 평면 연산 시 오차 및 안팎 뒤집힘 완벽 방지

        (setq vertices (append vertices (list (list (car pt-wcs) (cadr pt-wcs) 0.0))))

      )

    )

  )

  ;; 폐합 여부 확인 후 중복 정점 제거

  (if (and (> (length vertices) 1)

           (setq first-pt (car vertices))

           (setq last-pt (last vertices))

           (equal first-pt last-pt 0.01))

    (setq vertices (reverse (cdr (reverse vertices))))

  )

  vertices

)



;;; --------------------------------------------------------------------------

;;; Function: get-polygon-orientation

;;; Description: 폴리곤의 방향(CW/CCW) 판별 (1: CCW, -1: CW)

;;; --------------------------------------------------------------------------

(defun get-polygon-orientation (vertices / signed-area i n x1 y1 x2 y2)

  (setq signed-area 0.0)

  (setq n (length vertices))

  (setq i 0)

  (while (< i n)

    (setq x1 (car (nth i vertices)))

    (setq y1 (cadr (nth i vertices)))

    (setq x2 (car (nth (if (= i (1- n)) 0 (1+ i)) vertices)))

    (setq y2 (cadr (nth (if (= i (1- n)) 0 (1+ i)) vertices)))

    (setq signed-area (+ signed-area (* (- x1 x2) (+ y1 y2))))

    (setq i (1+ i))

  )

  (if (> signed-area 0) 1 -1)

)



;;; --------------------------------------------------------------------------

;;; Function: is-closed-polyline

;;; Description: 폴리라인이 폐합되었는지 확인 (Flag 70 또는 시작점=끝점)

;;; --------------------------------------------------------------------------

(defun is-closed-polyline (ent / ent-data closed-flag vertices first-pt last-pt)

  (setq ent-data (entget ent))

  (setq closed-flag (cdr (assoc 70 ent-data)))

  (if (and closed-flag (= 1 (logand 1 closed-flag)))

    T

    (progn

      (setq vertices '())

      (foreach item ent-data

        (if (= (car item) 10) (setq vertices (append vertices (list (cdr item)))))

      )

      (if (>= (length vertices) 2)

        (equal (car vertices) (last vertices) 0.1)

        nil

      )

    )

  )

)



;;; --------------------------------------------------------------------------

;;; Function: is-corner-convex

;;; Description: 코너가 볼록한지 오목한지 판별 (외적 활용)

;;; --------------------------------------------------------------------------

(defun is-corner-convex (prev-pt curr-pt next-pt orientation / v1x v1y v2x v2y cross-product)

  (setq v1x (- (car curr-pt) (car prev-pt)))

  (setq v1y (- (cadr curr-pt) (cadr prev-pt)))

  (setq v2x (- (car next-pt) (car curr-pt)))

  (setq v2y (- (cadr next-pt) (cadr curr-pt)))

  (setq cross-product (- (* v1x v2y) (* v1y v2x)))

  (if (= orientation 1)

    (> cross-product 0)

    (< cross-product 0)

  )

)



;;; --------------------------------------------------------------------------

;;; [신규 보조 함수] 점과 선분 사이의 최단 거리 계산

;;; --------------------------------------------------------------------------

(defun get-dist-point-to-segment (pt p1 p2 / x0 y0 x1 y1 x2 y2 dx dy t-param closest-x closest-y)

  (setq x0 (car pt) y0 (cadr pt))

  (setq x1 (car p1) y1 (cadr p1))

  (setq x2 (car p2) y2 (cadr p2))

  (setq dx (- x2 x1) dy (- y2 y1))

  

  (if (and (= dx 0) (= dy 0))

    (distance pt p1)

    (progn

      ;; 투영 계수 t 계산 (내적 활용)

      (setq t-param (/ (+ (* (- x0 x1) dx) (* (- y0 y1) dy)) (+ (* dx dx) (* dy dy))))

      

      ;; 선분 범위 내로 클램핑 (0 <= t <= 1)

      (if (< t-param 0) (setq t-param 0))

      (if (> t-param 1) (setq t-param 1))

      

      ;; 가장 가까운 점 좌표

      (setq closest-x (+ x1 (* t-param dx)))

      (setq closest-y (+ y1 (* t-param dy)))

      (distance pt (list closest-x closest-y))

    )

  )

)



;;; --------------------------------------------------------------------------

;;; Function: calc-orientation-by-closest-segment

;;; Description: [보조함수] 가상 연장선을 포함한 최적 방향 판별

;;; --------------------------------------------------------------------------

(defun calc-orientation-by-closest-segment (vertices user-pt / min-dist best-segment i p1 p2 p-prev p-next ext-len virt-start virt-end ext-segments dist cross-z)

  (setq ext-len 100000.0) ;; 충분히 긴 연장 길이

  

  ;; 1. 가상 연장점 생성

  (setq p1 (nth 0 vertices))

  (setq p2 (nth 1 vertices))

  (setq virt-start (polar p1 (angle p2 p1) ext-len)) ;; 시작점 뒤로 연장

  

  (setq p-prev (nth (- (length vertices) 2) vertices))

  (setq p-next (last vertices))

  (setq virt-end (polar p-next (angle p-prev p-next) ext-len)) ;; 끝점 앞으로 연장

  

  ;; 2. 전체 세그먼트 리스트 구성 (가상 시작 -> 본체 -> 가상 끝)

  (setq ext-segments (list (list virt-start (car vertices)))) ;; 가상 시작 세그먼트

  (setq i 0)

  (while (< i (1- (length vertices)))

    (setq ext-segments (append ext-segments (list (list (nth i vertices) (nth (1+ i) vertices)))))

    (setq i (1+ i))

  )

  (setq ext-segments (append ext-segments (list (list (last vertices) virt-end)))) ;; 가상 끝 세그먼트

  

  ;; 3. 가장 가까운 세그먼트 찾기

  (setq min-dist 1e99)

  (setq best-segment nil)

  

  (foreach seg ext-segments

    (setq dist (get-dist-point-to-segment user-pt (car seg) (cadr seg)))

    (if (< dist min-dist)

      (progn

        (setq min-dist dist)

        (setq best-segment seg)

      )

    )

  )

  

  ;; 4. 선택된 세그먼트 기준으로 외적(방향) 계산

  (if best-segment

    (progn

      (setq p1 (car best-segment))

      (setq p2 (cadr best-segment))

      (setq cross-z (- (* (- (car p2) (car p1)) (- (cadr user-pt) (cadr p1)))

                       (* (- (cadr p2) (cadr p1)) (- (car user-pt) (car p1)))))

      ;; [최종 수정] 토류판(바깥쪽)과 띠장(안쪽)이 클릭한 방향에 맞게 정확히 생성되도록 부호 정상화

      (if (> cross-z 0) -1 1) 

    )

    1 ;; 예외 시 기본값

  )

)



;;; --------------------------------------------------------------------------

;;; Function: determine-boundary-orientation

;;; Description: 경계선의 내부/외부 방향을 사용자 입력(가상 연장선 적용) 또는 자동 판별

;;; --------------------------------------------------------------------------

(defun determine-boundary-orientation (boundary-ent / vertices pt1 pt2 user-pt boundary-orient old-osmode)

  (princ "\n[방향 판별] 경계선 분석 중...")

  (setq vertices (extract-vertices boundary-ent))

  

  (if (is-closed-polyline boundary-ent)

    ;; [폐합된 경우] - 기존 로직 유지 (면적 부호로 판별)

    (progn

      (setq boundary-orient (get-polygon-orientation vertices))

      (princ (if (= boundary-orient 1) "\n- 폐합선: CCW (내부가 왼쪽)" "\n- 폐합선: CW (내부가 오른쪽)"))

      boundary-orient

    )

    ;; [열린 경계선] - 사용자 아이디어 (가상 연장선 분할) 적용

    (progn

      ;; 사용자가 무의식적으로 선분 위를 스냅(클릭)하여 외적이 0이 되는 오류를 방지하기 위해 임시로 스냅 끔

      (setq old-osmode (getvar "OSMODE"))

      (setvar "OSMODE" 0)

      (princ "\n파일설치 위치(Outside) 선택 (객체스냅 임시해제) : ")

      (setq user-pt (getpoint))

      (setvar "OSMODE" old-osmode)

      

      (if user-pt

        (progn

          ;; 사용자가 클릭한 UCS 좌표를 WCS 좌표로 변환 보정

          (setq user-pt (trans user-pt 1 0))

          

          ;; 사용자가 제안한 '연장선 분할' 논리를 구현한 함수 호출

          (setq boundary-orient (calc-orientation-by-closest-segment vertices user-pt))

          

          ;; 결과 반환

          boundary-orient

        )

        (progn (princ "\n선택 취소됨.") nil)

      )

    )

  )

)



;;; --------------------------------------------------------------------------

;;; Function: calc-hpile-vertices

;;; Description: [공통] 중심점 기준으로 H-Pile 형상 점 좌표 계산 (12개 정점 + 4개 Fillet)

;;; Returns: (pt1 pt2 pt3a pt3b pt9a pt9b pt8 pt7 pt12 pt11 pt10a pt10b pt4a pt4b pt5 pt6)

;;; --------------------------------------------------------------------------

(defun calc-hpile-vertices (center-pt h b tw tf / cx cy half-h half-b half-tw fillet-r)

  (setq cx (car center-pt))

  (setq cy (cadr center-pt))

  (setq half-h (/ h 2.0) half-b (/ b 2.0) half-tw (/ tw 2.0) fillet-r (* tw 2.0))

  

  (list

    (list (+ cx half-b) (+ cy half-h))                  ; pt1

    (list (+ cx half-b) (+ cy (- half-h tf)))           ; pt2

    (list (+ cx half-tw fillet-r) (+ cy (- half-h tf))) ; pt3a

    (list (+ cx half-tw) (+ cy (- half-h tf fillet-r))) ; pt3b

    (list (+ cx half-tw) (- cy (- half-h tf fillet-r))) ; pt9a

    (list (+ cx half-tw fillet-r) (- cy (- half-h tf))) ; pt9b

    (list (+ cx half-b) (- cy (- half-h tf)))           ; pt8

    (list (+ cx half-b) (- cy half-h))                  ; pt7

    (list (- cx half-b) (- cy half-h))                  ; pt12

    (list (- cx half-b) (- cy (- half-h tf)))           ; pt11

    (list (- cx half-tw fillet-r) (- cy (- half-h tf))) ; pt10a

    (list (- cx half-tw) (- cy (- half-h tf fillet-r))) ; pt10b

    (list (- cx half-tw) (+ cy (- half-h tf fillet-r))) ; pt4a

    (list (- cx half-tw fillet-r) (+ cy (- half-h tf))) ; pt4b

    (list (- cx half-b) (+ cy (- half-h tf)))           ; pt5

    (list (- cx half-b) (+ cy half-h))                  ; pt6

  )

)



;;; --------------------------------------------------------------------------

;;; 정점 리스트 재배열을 위한 보조 함수

;;; --------------------------------------------------------------------------



(defun tsp-get-closest-vertex-index (vertices pt / i min-dist best-idx dist p)

  (setq i 0 min-dist 1e99 best-idx 0)

  (foreach p vertices

    (setq dist (distance p pt))

    (if (< dist min-dist)

      (progn (setq min-dist dist) (setq best-idx i))

    )

    (setq i (1+ i))

  )

  best-idx

)



(defun tsp-shift-vertex-list (vertices idx / i len new-list)

  (setq len (length vertices))

  (setq new-list '())

  ;; 1. idx부터 끝까지 추가

  (setq i idx)

  (while (< i len)

    (setq new-list (append new-list (list (nth i vertices))))

    (setq i (1+ i))

  )

  ;; 2. 처음부터 idx 전까지 추가

  (setq i 0)

  (while (< i idx)

    (setq new-list (append new-list (list (nth i vertices))))

    (setq i (1+ i))

  )

  new-list

)



;;; ==========================================================================

;;; [SECTION 3-1] 세그먼트 데이터 초기화 

;;; ==========================================================================



;;; --------------------------------------------------------------------------

;;; Function: init-segment-data

;;; Description: 상위에서 정렬된 vertices를 받아 세그먼트 초기화

;;; --------------------------------------------------------------------------

(defun init-segment-data (boundary-ent boundary-orient override-vertices / vertices is-closed num-segs i pt1 pt2 pt0 seg-angle seg-len seg-data new-list old-data use-existing is-convex default-brace-data ang-v1 ang-v2 diff prev-seg-len min-len old-brace-entry old-brace-val old-upg-entry)

  (if (not (boundp '*segment-list*)) (setq *segment-list* nil))



  (if override-vertices

    (setq vertices override-vertices)

    (setq vertices (extract-vertices boundary-ent))

  )

  

  (setq is-closed (is-closed-polyline boundary-ent))

  (setq num-segs (if is-closed (length vertices) (1- (length vertices))))

  

  (setq use-existing nil)

  (if (and *segment-list* (= (length *segment-list*) num-segs))

    (setq use-existing T)

  )



  (if use-existing

    (princ "\n[알림] 이전 작업 데이터를 불러옵니다. (설정값 유지)")

    (princ (strcat "\n[STEP 1] " (itoa num-segs) "개의 세그먼트 데이터 초기화 중..."))

  )

  

  (setq new-list '())

  (setq i 0)

  

  (while (< i num-segs)

    (setq pt1 (nth i vertices))

    (setq pt2 (nth (if is-closed (rem (+ i 1) num-segs) (+ i 1)) vertices))

    

    (setq pt0 (if is-closed 

                (nth (if (= i 0) (1- num-segs) (1- i)) vertices)

                (if (> i 0) (nth (1- i) vertices) nil)))

                

    (setq is-convex nil)

    (if (and pt0 pt2)

      (progn

        (if (is-corner-convex pt0 pt1 pt2 boundary-orient) (setq is-convex T))

        

        (setq ang-v1 (angle pt1 pt0))

        (setq ang-v2 (angle pt1 pt2))

        (setq diff (abs (- ang-v1 ang-v2)))

        (if (> diff pi) (setq diff (- (* 2.0 pi) diff)))

        

        (if (>= diff (* 160.0 (/ pi 180.0)))

          (setq is-convex nil)

        )

      )

    )

    

    (setq seg-len (distance pt1 pt2))

    (setq seg-angle (angle pt1 pt2))



    (setq prev-seg-len (if pt0 (distance pt0 pt1) seg-len))

    (setq min-len (min seg-len prev-seg-len))

    

    (if (< min-len 1000.0)

      (setq is-convex nil)

    )



    (setq default-brace-data 

      (if is-convex 

        (list "Y" nil) 

        (list "N" nil)

      )

    )

    

    (if use-existing

      (progn

        (setq old-data (nth i *segment-list*))

        (setq seg-data (subst (cons 'V-START pt1) (assoc 'V-START old-data) old-data))

        (setq seg-data (subst (cons 'V-END pt2) (assoc 'V-END seg-data) seg-data))

        (setq seg-data (subst (cons 'ANGLE seg-angle) (assoc 'ANGLE seg-data) seg-data))

        (setq seg-data (subst (cons 'LENGTH seg-len) (assoc 'LENGTH seg-data) seg-data))

        (setq seg-data (subst (cons 'ID (1+ i)) (assoc 'ID seg-data) seg-data))

        

        (setq old-brace-entry (assoc 'CORNER-BRACE seg-data))

        (if old-brace-entry

          (progn

            (setq old-brace-val (cdr old-brace-entry))

            (if is-convex

              (if (= (car old-brace-val) "N")

                (setq seg-data (subst (cons 'CORNER-BRACE default-brace-data) old-brace-entry seg-data))

              )

              (setq seg-data (subst (cons 'CORNER-BRACE default-brace-data) old-brace-entry seg-data))

            )

          )

          (setq seg-data (append seg-data (list (cons 'CORNER-BRACE default-brace-data))))

        )

        

        (setq old-upg-entry (assoc 'UPGRADE-WALE seg-data))

        (if (not old-upg-entry)

          (setq seg-data (append seg-data (list (cons 'UPGRADE-WALE nil))))

        )

        ;; [5단계] 기존 세그먼트에 WALL-TYPE 키가 없으면 "HPILE"로 보완
        ;; (과거 저장 데이터 호환성 - 이전 버전에서 저장된 세그먼트에 WALL-TYPE 없을 때)
        (if (not (assoc 'WALL-TYPE seg-data))
          (setq seg-data (append seg-data (list (cons 'WALL-TYPE "HPILE"))))
        )

        ;; [5단계] 기존 세그먼트에 CIP 파라미터 키가 없으면 기본값으로 보완
        (if (not (assoc 'CIP-MAX-DEPTH seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-MAX-DEPTH "10.0"))))
        )

        (if (not (assoc 'CIP-EMBED-DEPTH seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-EMBED-DEPTH "3.0"))))
        )

        (if (not (assoc 'CIP-HPILE-IDX seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-HPILE-IDX "0"))))
        )

        (if (not (assoc 'CIP-WALE-IDX seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-WALE-IDX "0"))))
        )

        (if (not (assoc 'CIP-DIA seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-DIA "450"))))
        )

        (if (not (assoc 'CIP-MODE-IDX seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-MODE-IDX "0"))))
        )

        (if (not (assoc 'CIP-OVERLAP seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-OVERLAP "0"))))
        )

        (if (not (assoc 'CIP-INTERVAL-IDX seg-data))
          (setq seg-data (append seg-data (list (cons 'CIP-INTERVAL-IDX "0"))))
        )

      )

      (progn

        (setq seg-data (list

          (cons 'ID (1+ i))

          (cons 'NAME (strcat "Seg-" (itoa (1+ i))))

          (cons 'IS-DEFINED nil)              

          (cons 'SECTION-DRAW nil)

          (cons 'SOIL-DATA nil)

          (cons 'WALL-SPEC *tsp-hpile-spec*)    

          (cons 'WALL-CUSTOM '(300 300 10 15))

          (cons 'WALE-SPEC *tsp-wale-spec*)      

          (cons 'WALE-CUSTOM '(300 300 10 15))

          (cons 'CTC 2.0)

          (cons 'TIMBER-THICKNESS 60)         

          (cons 'MAX-DEPTH 10.0)              

          (cons 'EMBED-DEPTH 3.0)             

          (cons 'WATER-CHK "0")               

          (cons 'WATER-DEPTH 0.0)             

          (cons 'SHOTCRETE-ENABLE "1")

          (cons 'SHOTCRETE-CHK "0")

          (cons 'SHOTCRETE-LAYER "")

          (cons 'SHOTCRETE-THICK 0.0)

          (cons 'SUPPORT-LIST '())

          (cons 'CORNER-BRACE default-brace-data)

          (cons 'UPGRADE-WALE nil)

          ;; [5단계] 신규 세그먼트 기본값 - nil 방지용 안전장치
          ;; WALL-TYPE은 항상 "HPILE"로 시작, CIP는 다이얼로그 OK 후에만 변경됨
          (cons 'WALL-TYPE "HPILE")

          (cons 'CIP-MAX-DEPTH "10.0")

          (cons 'CIP-EMBED-DEPTH "3.0")

          (cons 'CIP-HPILE-IDX "0")

          (cons 'CIP-WALE-IDX "0")

          (cons 'CIP-DIA "450")

          (cons 'CIP-MODE-IDX "0")

          (cons 'CIP-OVERLAP "0")

          (cons 'CIP-INTERVAL-IDX "0")

          (cons 'V-START pt1)

          (cons 'V-END pt2)

          (cons 'ANGLE seg-angle)

          (cons 'LENGTH seg-len)

        ))

      )

    )

    (setq new-list (append new-list (list seg-data)))

    (setq i (1+ i))

  )

  

  (setq *segment-list* new-list)

  (princ "\n[완료] 세그먼트 리스트 준비됨.")

  (princ (strcat "\n - 총 세그먼트 수: " (itoa (length *segment-list*))))

)



;;; ==========================================================================

;;; [SECTION 4] 단면도 및 주상도 작도 (Section & Soil Profile)

;;; ==========================================================================



;;; --------------------------------------------------------------------------

;;; Function: create-wale-section-block

;;; Description: 단면 띠장(Wale) 블록 정의 (직사각형 외곽선 포함)

;;; --------------------------------------------------------------------------

(defun create-wale-section-block (h b tw tf / block-name pts layer-name)

    ;; 블록 이름과 레이어 이름 생성

    (setq block-name (strcat "_WALE(" (itoa h) "x" (itoa b) "x" (itoa tw) "x" (itoa tf) ")"))

    (setq layer-name (strcat "_H-Pile_" (itoa h) "x" (itoa b) "x" (itoa tw) "-" (itoa tf)))

    (if (or (null layer-name) (= layer-name "")) (setq layer-name "0"))

    

    ;; 블록이 없으면 생성

    (if (not (tblsearch "BLOCK" block-name))

      (progn

        ;; 필요한 레이어 생성

        (create-layer-if-not-exists layer-name "3")

        (create-layer-if-not-exists "_띠장 point" "1")

        (create-layer-if-not-exists "_띠장(wale)" "3") 

        

        (entmake (list '(0 . "BLOCK") (cons 2 block-name) '(70 . 0) '(10 . (0.0 0.0 0.0))))

        (setq pts (calc-hpile-vertices (list 0.0 (/ h 2.0) 0.0) h b tw tf))

        

        ;; 1. H-Pile 형상 작도

        (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") 

          (cons 8 layer-name) (cons 62 256) '(90 . 20) '(70 . 1)

          (cons 10 (nth 0 pts))  (cons 10 (nth 1 pts)) 

          (cons 10 (nth 2 pts))  (cons 42 0.4142135623730951) (cons 10 (nth 3 pts))

          (cons 10 (nth 4 pts))  (cons 42 0.4142135623730951) (cons 10 (nth 5 pts))

          (cons 10 (nth 6 pts))  (cons 10 (nth 7 pts)) 

          (cons 10 (nth 8 pts))  (cons 10 (nth 9 pts))

          (cons 10 (nth 10 pts)) (cons 42 0.4142135623730951) (cons 10 (nth 11 pts))

          (cons 10 (nth 12 pts)) (cons 42 0.4142135623730951) (cons 10 (nth 13 pts))

          (cons 10 (nth 14 pts)) (cons 10 (nth 15 pts))))

        

        ;; 2. STIFFNER_외곽 모서리 직사각형 (pt1-pt7-pt12-pt6)

        (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") 

          (cons 8 "_띠장(wale)") (cons 62 256) '(90 . 4) '(70 . 1)

          (cons 10 (nth 0 pts))   ;; pt1 (우상단)

          (cons 10 (nth 7 pts))   ;; pt7 (우하단)

          (cons 10 (nth 8 pts))   ;; pt12 (좌하단)

          (cons 10 (nth 15 pts))  ;; pt6 (좌상단)

        ))



        (entmake (list '(0 . "POINT") (cons 8 "_띠장 point") (cons 62 256) '(10 0.0 0.0 0.0)))

        (entmake (list '(0 . "ENDBLK")))

      )

    )

    block-name

  )



;;; --------------------------------------------------------------------------

;;; Function: draw-water-level-symbol

;;; Description: 지하수위 기호(삼각형 역상) 및 수위 텍스트 작도

;;; --------------------------------------------------------------------------

(defun draw-water-level-symbol (base-pt depth-val col-x-right / layer-name line1-mid line1-start line1-end line2-start line2-end line3-start line3-end tri-p1 tri-p2 tri-p3 text-pt text-str s-factor tri-h tri-w-half line1-len l2-half off-y1 line2-y l3-half off-y2 line3-y tri-p_top_center text-gap text-h)

  (setq layer-name "_지반선")

  (setq s-factor (/ *tsp-scale-section* 200.0))

  

  (setq tri-h (* 800.0 s-factor))

  (setq tri-w-half (/ tri-h (sqrt 3.0)))

  

  (setq line1-len (* 3000.0 s-factor))

  (setq line1-start (list col-x-right (cadr base-pt) 0.0))

  (setq line1-end (list (+ col-x-right line1-len) (cadr base-pt) 0.0))

  (setq line1-mid (list (/ (+ (car line1-start) (car line1-end)) 2.0) (cadr base-pt) 0.0))

  

  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 2) '(70 . 0) (cons 10 line1-start) (cons 10 line1-end)))

  

  (setq l2-half (* 750.0 s-factor)) (setq off-y1 (* 100.0 s-factor)) (setq line2-y (- (cadr line1-mid) off-y1))

  (setq line2-start (list (- (car line1-mid) l2-half) line2-y 0.0)) (setq line2-end (list (+ (car line1-mid) l2-half) line2-y 0.0))

  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 2) '(70 . 0) (cons 10 line2-start) (cons 10 line2-end)))

  

  (setq l3-half (* 500.0 s-factor)) (setq off-y2 (* 200.0 s-factor)) (setq line3-y (- (cadr line1-mid) off-y2))

  (setq line3-start (list (- (car line1-mid) l3-half) line3-y 0.0)) (setq line3-end (list (+ (car line1-mid) l3-half) line3-y 0.0))

  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 2) '(70 . 0) (cons 10 line3-start) (cons 10 line3-end)))

  

  (setq tri-p3 line1-mid)

  (setq tri-p1 (list (- (car line1-mid) tri-w-half) (+ (cadr line1-mid) tri-h) 0.0))

  (setq tri-p2 (list (+ (car line1-mid) tri-w-half) (+ (cadr line1-mid) tri-h) 0.0))

  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 3) '(70 . 1) (cons 10 tri-p1) (cons 10 tri-p2) (cons 10 tri-p3)))

  

  (setq tri-p_top_center (list (car line1-mid) (+ (cadr line1-mid) tri-h) 0.0))

  (entmake (list '(0 . "SOLID") '(100 . "AcDbEntity") '(100 . "AcDbTrace") (cons 8 layer-name) (cons 62 8) (cons 10 tri-p1) (cons 11 tri-p_top_center) (cons 12 tri-p3) (cons 13 tri-p3)))

  

  (setq text-gap (* 800.0 s-factor))

  (setq text-str (strcat "-" (format-depth-text depth-val) "m"))

  (setq text-pt (list (car line1-mid) (+ (cadr line1-mid) tri-h text-gap) 0.0))

  (setq text-h (* 600.0 s-factor))

  (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText") (cons 8 layer-name) (cons 62 8) (cons 10 text-pt) (cons 40 text-h) (cons 1 text-str) (cons 72 1) (cons 11 text-pt)))

)



;;; --------------------------------------------------------------------------

;;; Function: draw-soil-column

;;; Description: 지반 주상도(Soil Column) 생성 및 지층별 해치/텍스트 표기

;;; --------------------------------------------------------------------------

(defun draw-soil-column (top-left-pt side / prev-depth curr-depth layer-name layer-depth layer-hatch layer-scale layer-angle col-width y-curr y-prev rect-pl start-x s-factor txt-h leader-len txt-off-l txt-off-c col-x-right pt1 pt2 pt3 pt4 wl-y wl-pt l-name l-depth l-pattern l-scale l-angle text-x text-y-name center-pt base-scale final-scale final-angle)

  (setq layer-name "_지반선")

  (setq s-factor (/ *tsp-scale-section* 200.0))

  (setq col-width (* 1000.0 s-factor)) (setq txt-h (* 600.0 s-factor)) (setq leader-len (* 1000.0 s-factor)) (setq txt-off-l (* 1200.0 s-factor)) (setq txt-off-c (* 3500.0 s-factor))

  (setq prev-depth 0.0)

  

  (if (= side "L") (setq start-x (- (car top-left-pt) col-width)) (setq start-x (car top-left-pt)))

  (setq top-left-pt (list start-x (cadr top-left-pt) 0.0))

  (setq col-x-right (+ start-x col-width))



  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 2) '(70 . 0) (cons 10 (list (car top-left-pt) (cadr top-left-pt))) (cons 10 (list (- (car top-left-pt) leader-len) (cadr top-left-pt)))))

  (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText") (cons 8 layer-name) (cons 62 8) (cons 10 (list (- (car top-left-pt) txt-off-l) (cadr top-left-pt) 0.0)) (cons 40 txt-h) (cons 1 (format-depth-text 0.0)) (cons 72 2) (cons 11 (list (- (car top-left-pt) txt-off-l) (cadr top-left-pt) 0.0))))

  

  (if (= *tsp-water-chk* "1")

    (progn 

      (setq wl-y (- (cadr top-left-pt) (* *tsp-water-depth* 1000.0))) 

      (setq wl-x (+ col-x-right (* 3000.0 s-factor)))

      (setq wl-pt (list wl-x wl-y 0.0)) 

      (draw-water-level-symbol wl-pt *tsp-water-depth* wl-x)

    )

  )

  

  (foreach layer *tsp-soil-layers*

    (setq l-name (car layer)) (setq l-depth (cadr layer)) (setq l-pattern (nth 2 layer)) (setq l-scale (nth 3 layer)) (setq l-angle (nth 4 layer))

    (setq curr-depth (* l-depth 1000.0)) (setq y-prev (- (cadr top-left-pt) prev-depth)) (setq y-curr (- (cadr top-left-pt) curr-depth))

    (setq pt1 (list (car top-left-pt) y-prev)) (setq pt2 (list (+ (car top-left-pt) col-width) y-prev)) (setq pt3 (list (+ (car top-left-pt) col-width) y-curr)) (setq pt4 (list (car top-left-pt) y-curr))

    

    (setq center-pt (list (/ (+ (car pt1) (car pt3)) 2.0) (/ (+ (cadr pt1) (cadr pt3)) 2.0) 0.0))



    (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 4) '(70 . 1) (cons 10 pt1) (cons 10 pt2) (cons 10 pt3) (cons 10 pt4)))

    (setq rect-pl (entlast))

    

    ;; [수정됨] 이름 체크 하드코딩 삭제, 배열에 저장된 l-scale과 l-angle 적용

    (setq base-scale (if (and l-scale (> l-scale 0)) l-scale 100.0))

    (setq final-scale (* base-scale (/ *tsp-scale-section* 200.0)))

    (setq final-angle (if l-angle l-angle 0.0))

    

    (if rect-pl 

      (tsp-apply-custom-hatch rect-pl l-pattern final-scale (* final-angle (/ pi 180.0)) layer-name 8 center-pt)

    )

    

    (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 2) '(70 . 0) (cons 10 pt4) (cons 10 (list (- (car pt4) leader-len) (cadr pt4)))))

    (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText") (cons 8 layer-name) (cons 62 8) (cons 10 (list (- (car pt4) txt-off-l) (cadr pt4) 0.0)) (cons 40 txt-h) (cons 1 (format-depth-text l-depth)) (cons 72 2) (cons 11 (list (- (car pt4) txt-off-l) (cadr pt4) 0.0))))

    

    (setq text-x (- (+ (car pt1) (/ col-width 2.0)) txt-off-c)) 

    (setq text-y-name (- (/ (+ y-prev y-curr) 2.0) (/ txt-h 2.0)))

    (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText") (cons 8 layer-name) (cons 62 8) (cons 10 (list text-x text-y-name 0.0)) (cons 40 txt-h) (cons 1 l-name) (cons 72 2) (cons 11 (list text-x text-y-name 0.0))))

    

    (setq prev-depth curr-depth)

  )

  (princ "\n주상도 생성 완료.")

)



;;; --------------------------------------------------------------------------

;;; Function: draw-section-hpile

;;; Description: 단면 H-Pile, 토류판, 띠장, 지보재(버팀보/잭/앵커), 숏크리트 등 통합 작도

;;; --------------------------------------------------------------------------

(defun draw-section-hpile (start-pt side h b tw tf max-depth embed-depth / layer-name total-len top-len inner-layer-name p-center-top p-center-bot p-out1-top p-out1-bot p-in-top p-in-bot p-out2-top p-out2-bot dir-x x-out1 x-in x-out2 cx cy y-top y-bot x-tf p-tf1-top p-tf1-bot x-tf2 p-tf2-top p-tf2-bot exc-y pt-exc-start rock-depth found-rock timber-depth pt-timber-start pt-timber-end pt-timber-bottom-start pt-timber-bottom-end timber-pl hatch-obj sa doc mspace timber-bot-y col-x-pos rock-y pt-debug-start pt-debug-end pt-exc-end s-item s-type s-depth ins-y pt-ins s-ang s-free s-bond ang-rad draw-ang pt-free-end pt-bond-end wale-data wale-h wale-b wale-tw wale-tf wale-dir wale-offset wale-upper-y wale-lower-y wale-upper-insert-pt wale-lower-insert-pt draw-wale-section shotcrete-bot-y pt-sc-start pt-sc-end pt-sc-bot-start pt-sc-bot-end sc-pl l-type wale-layer-name pt-ins-wale-base wale-rot-angle jack-block-name jack-insert-x pt-jack-ins jack-rot strut-start-x pt-strut-start strut-end-x pt-strut-end strut-h half-h strut-tf s-jack-type s-sec-str strut-data ep-thick ep-end-x stiff-x1 stiff-x2 brk-p1 brk-p2 brk-p3 brk-p4 p1-x p1-y pt-p1 p2-x p2-y pt-p2 pt-p1-h pt-p2-h face-ang pt-meet p-head-center p-head-1 p-head-2 p-head-3 p-head-4 out-ang pt-anchor-start)



  ;; [내부 함수] 띠장 삽입 및 보걸이 상세 작도

  (defun draw-wale-section (insert-pt rotation layer-name w-h w-b w-tw w-tf extra-margin / block-name br-layer cx cy wale-dir y-base y-L1 y-L2 y-L3 x-wall x-tip x-L4-start pt-L1-s pt-L1-e pt-L2-s pt-L2-e pt-L3-s pt-L3-e pt-L4-s pt-L4-e pt-L5-s pt-L5-e pt-L6-s pt-L6-e pt-L7-s pt-L7-e pt-L8-s pt-L8-e k4 k6 k7 is-left trim-x-6 trim-x-7 intersect-x8)

    (setq block-name (create-wale-section-block w-h w-b w-tw w-tf))

    (entmake (list '(0 . "INSERT") (cons 2 block-name) (cons 8 layer-name) (cons 10 insert-pt) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) (cons 50 rotation)))      



    (setq br-layer "_보걸이")

    (create-layer-if-not-exists br-layer "7")

    

    (setq cx (car insert-pt) cy (cadr insert-pt))

    (if (< (sin rotation) -0.1) (setq wale-dir 1.0) (setq wale-dir -1.0))

    (setq is-left (> wale-dir 0))



    ;; 좌표 기준 설정

    (setq y-base (- cy (/ w-b 2.0)))

    (setq y-L1 y-base)

    (setq y-L2 (- y-base 10.0))

    (setq y-L3 (- y-base 100.0))

    (setq x-wall cx)

    

    ;; extra-margin 적용 (앵커 0, 버팀보 150)

    (setq x-tip (+ cx (* (+ w-h extra-margin) wale-dir))) 



    ;; 선 좌표 계산

    (setq pt-L1-s (list x-wall y-L1 0.0)) (setq pt-L1-e (list x-tip y-L1 0.0))

    (setq pt-L2-s (list x-wall y-L2 0.0)) (setq pt-L2-e (list x-tip y-L2 0.0))

    (setq pt-L3-s pt-L1-e) (setq pt-L3-e (list (car pt-L1-e) y-L3 0.0))



    (setq x-L4-start (- x-tip (* 30.0 wale-dir)))

    (setq pt-L4-s (list x-L4-start y-L2 0.0))

    

    (if is-left (setq k4 (- y-L2 x-L4-start)) (setq k4 (+ y-L2 x-L4-start)))

    (if is-left (setq pt-L4-e (list x-wall (+ x-wall k4) 0.0)) (setq pt-L4-e (list x-wall (- k4 x-wall) 0.0)))



    (setq pt-L5-s pt-L3-e)

    (if is-left (setq pt-L5-e (list (- y-L3 k4) y-L3 0.0)) (setq pt-L5-e (list (- k4 y-L3) y-L3 0.0)))



    (setq k6 (+ k4 (* 90.0 (sqrt 2.0))))

    (if is-left (setq trim-x-6 (- y-L2 k6)) (setq trim-x-6 (- k6 y-L2)))

    (setq pt-L6-s (list trim-x-6 y-L2 0.0)) (setq pt-L6-e (list x-wall (if is-left (+ x-wall k6) (- k6 x-wall)) 0.0))



    (setq k7 (+ k6 (* 10.0 (sqrt 2.0))))

    (if is-left (setq trim-x-7 (- y-L2 k7)) (setq trim-x-7 (- k7 y-L2)))

    (setq pt-L7-s (list trim-x-7 y-L2 0.0)) (setq pt-L7-e (list x-wall (if is-left (+ x-wall k7) (- k7 x-wall)) 0.0))



    (if is-left (setq intersect-x8 (- y-L3 k7)) (setq intersect-x8 (- k7 y-L3)))

    (setq pt-L8-s (list x-wall y-L3 0.0)) (setq pt-L8-e (list intersect-x8 y-L3 0.0))



    ;; [그리기 실행]

    (foreach pts (list (list pt-L1-s pt-L1-e) (list pt-L3-s pt-L3-e) (list pt-L4-s pt-L4-e) 

                       (list pt-L5-s pt-L5-e) (list pt-L7-s pt-L7-e) (list pt-L8-s pt-L8-e))

      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")

        (cons 8 br-layer) (cons 62 256) '(90 . 2) '(70 . 0) (cons 10 (car pts)) (cons 10 (cadr pts)))))

    

    (foreach pts (list (list pt-L2-s pt-L2-e) (list pt-L6-s pt-L6-e))

      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")

        (cons 8 br-layer) (cons 62 1) '(90 . 2) '(70 . 0) (cons 10 (car pts)) (cons 10 (cadr pts)))))

  )



  ;; ==========================================================

  ;; H-Pile 및 기타 요소 작도 로직 (지보재 상세 적용)

  ;; ==========================================================

  

  (setq inner-layer-name (strcat "_H-Pile_" (itoa h) "x" (itoa b) "x" (itoa tw) "-" (itoa tf)))

  (create-layer-if-not-exists inner-layer-name "3")

  (create-layer-if-not-exists "_토류판(timber)" "1") 

  (create-layer-if-not-exists "_앵커(Anchor)" "6") 

  (create-layer-if-not-exists "_지반선" "8")

  (create-layer-if-not-exists "_숏크리트(shotcrete)" "7")

  (create-layer-if-not-exists "DEBUG_POINT_ANCHOR" "6")

  (create-layer-if-not-exists "DEBUG_POINT_STRUT" "2")

  (create-layer-if-not-exists "DEBUG_POINT_USER" "7")

  (create-layer-if-not-exists "DEBUG_POINT_HPILE" "4")



  (setq total-len (* (+ max-depth embed-depth) 1000.0))

  (setq top-len 500.0)

  (setq cx (car start-pt)) (setq cy (cadr start-pt))



  (entmake (list '(0 . "POINT") '(8 . "DEBUG_POINT_USER") '(62 . 7) (cons 10 start-pt)))



  (setq exc-y (- cy (* max-depth 1000.0)))

  (setq pt-exc-start (list cx exc-y 0.0))

  (entmake (list '(0 . "POINT") '(8 . "DEBUG_POINT_HPILE") '(62 . 4) (cons 10 pt-exc-start)))



  (if (= side "L")

    (setq pt-exc-end (polar pt-exc-start 0.0 30000.0))

    (setq pt-exc-end (polar pt-exc-start pi 30000.0))

  )

  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 "_지반선") (cons 62 8) '(90 . 2) '(70 . 0) (cons 10 pt-exc-start) (cons 10 pt-exc-end)))



  (setq dir-x -1.0) (setq wale-dir 1.0)

  (if (= side "R") (progn (setq dir-x 1.0) (setq wale-dir -1.0)))



  (setq y-top (+ cy top-len)) (setq y-bot (- cy total-len))

  

  ;; H-Pile 본체 작도

  (setq p-out1-top (list cx y-top 0.0)) (setq p-out1-bot (list cx y-bot 0.0))

  (entmake (list '(0 . "LINE") '(100 . "AcDbEntity") '(100 . "AcDbLine") (cons 8 inner-layer-name) (cons 62 3) (cons 10 p-out1-top) (cons 11 p-out1-bot)))

  (setq x-tf (+ cx (* tf dir-x)))

  (setq p-tf1-top (list x-tf y-top 0.0)) (setq p-tf1-bot (list x-tf y-bot 0.0))

  (entmake (list '(0 . "LINE") '(100 . "AcDbEntity") '(100 . "AcDbLine") (cons 8 inner-layer-name) (cons 62 1) (cons 10 p-tf1-top) (cons 11 p-tf1-bot)))

  (setq x-tf2 (+ cx (* (- h tf) dir-x)))

  (setq p-tf2-top (list x-tf2 y-top 0.0)) (setq p-tf2-bot (list x-tf2 y-bot 0.0))

  (entmake (list '(0 . "LINE") '(100 . "AcDbEntity") '(100 . "AcDbLine") (cons 8 inner-layer-name) (cons 62 1) (cons 10 p-tf2-top) (cons 11 p-tf2-bot)))

  (setq x-out2 (+ cx (* h dir-x)))

  (setq p-out2-top (list x-out2 y-top 0.0)) (setq p-out2-bot (list x-out2 y-bot 0.0))

  (entmake (list '(0 . "LINE") '(100 . "AcDbEntity") '(100 . "AcDbLine") (cons 8 inner-layer-name) (cons 62 3) (cons 10 p-out2-top) (cons 11 p-out2-bot)))

  (entmake (list '(0 . "LINE") (cons 8 inner-layer-name) (cons 62 3) (cons 10 p-out1-top) (cons 11 p-out2-top)))

  (entmake (list '(0 . "LINE") (cons 8 inner-layer-name) (cons 62 3) (cons 10 p-out1-bot) (cons 11 p-out2-bot)))



  ;; 지보재 배치 로직

  (setq wale-data nil)

  (if (and *tsp-wale-spec* (= *tsp-wale-spec* "User-defined") *tsp-wale-custom*)

    (setq wale-data *tsp-wale-custom*)

    (setq wale-data (parse-h-spec *tsp-wale-spec*))

  )

  (if (null wale-data) (setq wale-data '(300 300 10 15))) 

  (setq wale-h (nth 0 wale-data)) (setq wale-b (nth 1 wale-data)) (setq wale-tw (nth 2 wale-data)) (setq wale-tf (nth 3 wale-data))



  (setq wale-layer-name "_띠장(Wale)")

  (create-layer-if-not-exists wale-layer-name "3")

  

  ;; 블록 준비 (없으면 생성)

  (create-strut-jack-block)       

  (create-strut-screw-jack-block) 



  (if *tsp-support-list*

    (foreach s-item *tsp-support-list*

      (setq s-type (car s-item)) (setq s-depth (cadddr s-item))

      (setq ins-y (- cy (* s-depth 1000.0)))

      (setq pt-ins (list cx ins-y 0.0)) 

      

      (cond

        ((wcmatch s-type "앵커*")

          (setq wale-rot-angle (* -0.5 pi wale-dir))

          (setq wale-offset (+ 150.0 (/ wale-b 2.0)))

          (setq wale-upper-insert-pt (list cx (+ ins-y wale-offset) 0.0))

          (draw-wale-section wale-upper-insert-pt wale-rot-angle wale-layer-name wale-h wale-b wale-tw wale-tf 0.0)

          

          (setq wale-lower-insert-pt (list cx (- ins-y wale-offset) 0.0))

          (draw-wale-section wale-lower-insert-pt wale-rot-angle wale-layer-name wale-h wale-b wale-tw wale-tf 0.0)

          

          ;; 앵커 정보 파싱 및 각도 계산

          (entmake (list '(0 . "POINT") '(8 . "DEBUG_POINT_ANCHOR") '(62 . 6) (cons 10 pt-ins)))

          (setq s-ang (nth 4 s-item)) (setq s-free (nth 5 s-item)) (setq s-bond (nth 6 s-item))

          (setq ang-rad (* s-ang (/ pi 180.0)))

          (if (= side "L") (setq draw-ang (+ pi ang-rad)) (setq draw-ang (- (* 2.0 pi) ang-rad)))

          (setq pt-free-end (polar pt-ins draw-ang (* s-free 1000.0)))

          (setq pt-bond-end (polar pt-free-end draw-ang (* s-bond 1000.0)))



          ;; 바깥쪽(굴착면 측)을 향하는 반대 각도 계산

          (setq out-ang (+ draw-ang pi))



          ;; 대좌(Pedestal) 및 정착구(Anchor Head) 계산 및 생성

          (setq p1-x (+ cx (* wale-h wale-dir)))

          (setq p1-y (+ ins-y wale-offset (/ wale-b 2.0)))

          (setq pt-p1 (list p1-x p1-y 0.0))



          (setq p2-x p1-x)

          (setq p2-y (- ins-y wale-offset (/ wale-b 2.0)))

          (setq pt-p2 (list p2-x p2-y 0.0))



          (setq pt-p1-h (list (+ p1-x (* 100.0 wale-dir)) p1-y 0.0))

          (setq pt-p2-h (list (+ p2-x (* 100.0 wale-dir)) p2-y 0.0))



          ;; 앵커선과 직교하는 대좌면 각도 산출

          (setq face-ang (+ draw-ang (* wale-dir (/ pi 2.0))))

          (setq pt-meet (polar pt-p1-h face-ang 400.0))



          ;; 1. 대좌 생성

          (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")

                         (cons 8 "_앵커(Anchor)") (cons 62 6) '(90 . 5) '(70 . 1)

                         (cons 10 pt-p1) (cons 10 pt-p1-h) (cons 10 pt-meet) (cons 10 pt-p2-h) (cons 10 pt-p2)))



          ;; 2. 정착구 중심점 계산

          (setq p-head-center (inters pt-ins pt-free-end pt-p1-h pt-meet nil))

          (if (null p-head-center)

            (setq p-head-center (polar pt-p1-h (angle pt-p1-h pt-meet) 200.0))

          )

          

          ;; 정착구가 대좌 바깥쪽(out-ang 방향)으로 생성되도록 변경

          (setq p-head-1 (polar p-head-center face-ang 100.0))

          (setq p-head-2 (polar p-head-center (+ face-ang pi) 100.0))

          (setq p-head-3 (polar p-head-2 out-ang 60.0))

          (setq p-head-4 (polar p-head-1 out-ang 60.0))



          ;; 3. 정착구 생성

          (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")

                         (cons 8 "_앵커(Anchor)") (cons 62 6) '(90 . 4) '(70 . 1)

                         (cons 10 p-head-1) (cons 10 p-head-2) (cons 10 p-head-3) (cons 10 p-head-4)))



          ;; 앵커선이 정착구를 통과하여 150 튀어나오도록 시작점 변경

          (setq pt-anchor-start (polar p-head-center out-ang 210.0))



          ;; 4. 앵커 본체 라인 작도 (DEBUG_POINT_ANCHOR 즉, pt-ins 기준으로 두 선으로 분할)

          (entmake (list '(0 . "LINE") (cons 8 "_앵커(Anchor)") (cons 62 6) (cons 10 pt-anchor-start) (cons 11 pt-ins)))

          (entmake (list '(0 . "LINE") (cons 8 "_앵커(Anchor)") (cons 62 6) (cons 10 pt-ins) (cons 11 pt-free-end)))

          

          (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 "_앵커(Anchor)") (cons 62 6) '(90 . 2) '(70 . 0) '(43 . 400.0) (cons 10 (list (car pt-free-end) (cadr pt-free-end))) (cons 10 (list (car pt-bond-end) (cadr pt-bond-end)))))

        )

        

        ((wcmatch s-type "버팀보*")

          ;; [1] 띠장 + 보걸이 그리기

          (setq wale-rot-angle (* -0.5 pi wale-dir))

          (draw-wale-section pt-ins wale-rot-angle wale-layer-name wale-h wale-b wale-tw wale-tf 150.0)

          

          ;; [2] 잭 종류 확인 및 블록 이름 결정

          (setq s-jack-type (nth 7 s-item)) 

          (if (or (null s-jack-type) (= s-jack-type "")) (setq s-jack-type "스크류잭"))

          

          (if (equal s-jack-type "스크류잭")

            (setq jack-block-name "스크류잭")

            (setq jack-block-name "유압잭")

          )



          ;; [3] 잭(Jack) 블록 삽입

          (setq jack-insert-x (+ cx (* wale-h wale-dir)))

          (setq pt-jack-ins (list jack-insert-x ins-y 0.0))

          (setq jack-rot (if (> wale-dir 0) 0.0 pi))

          

          (entmake (list '(0 . "INSERT") 

                         (cons 2 jack-block-name) 

                         (cons 8 (if (equal jack-block-name "스크류잭") "_스크류잭" "_유압잭")) 

                         (cons 62 2)

                         (cons 10 pt-jack-ins) 

                         '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) 

                         (cons 50 jack-rot)))

          

          ;; [4] 버팀보(Strut) 본체 그리기 세팅

          (setq s-sec-str (nth 6 s-item))

          (setq strut-data (parse-h-spec s-sec-str))

          (if (null strut-data) (setq strut-data '(300 300 10 15)))

          

          (setq strut-h (nth 0 strut-data))

          (setq strut-tf (nth 3 strut-data))

          (setq half-h (/ strut-h 2.0))

          

          ;; Y 좌표 (상단/하단 외부선 및 내부선)

          (setq y-top (+ ins-y half-h))

          (setq y-top-in (- y-top strut-tf))

          (setq y-bot (- ins-y half-h))

          (setq y-bot-in (+ y-bot strut-tf))



          ;; X 좌표 (잭 끝 -> 엔드플레이트 -> 스티프너 -> 4m 파단선)

          (setq x-jack-end (+ jack-insert-x (* 583.0 wale-dir)))

          (setq ep-thick 14.0)

          (setq x-ep-end (+ x-jack-end (* ep-thick wale-dir)))

          (setq stiff-len 200.0)

          (setq stiff-thick 14.0)

          (setq x-stiff-end (+ x-ep-end (* stiff-len wale-dir)))

          (setq x-strut-end (+ x-ep-end (* 4000.0 wale-dir)))



          ;; [4-1] 엔드플레이트 (두께 14) - 초록색(3)

          ;; 겹침 방지: 닫힌 사각형 폴리라인으로 작도하여 스트럿과 분리

          (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")

                         (cons 8 "_버팀보(Strut)") (cons 62 3) '(90 . 4) '(70 . 1)

                         (cons 10 (list x-jack-end y-top 0.0))

                         (cons 10 (list x-ep-end y-top 0.0))

                         (cons 10 (list x-ep-end y-bot 0.0))

                         (cons 10 (list x-jack-end y-bot 0.0))))



          ;; [4-2] 가로 스티프너 (중앙에 가로 배치, 길이 200, 두께 14) - 초록색(3)

          (setq stiff-y-top (+ ins-y (/ stiff-thick 2.0)))

          (setq stiff-y-bot (- ins-y (/ stiff-thick 2.0)))

          

          ;; 겹침 방지: 왼쪽면은 엔드플레이트와 동일하므로 그리지 않고 상, 하, 우 3면만 그림

          (entmake (list '(0 . "LINE") (cons 8 "_버팀보(Strut)") (cons 62 3) 

                         (cons 10 (list x-ep-end stiff-y-top 0.0)) (cons 11 (list x-stiff-end stiff-y-top 0.0))))

          (entmake (list '(0 . "LINE") (cons 8 "_버팀보(Strut)") (cons 62 3) 

                         (cons 10 (list x-ep-end stiff-y-bot 0.0)) (cons 11 (list x-stiff-end stiff-y-bot 0.0))))

          (entmake (list '(0 . "LINE") (cons 8 "_버팀보(Strut)") (cons 62 3) 

                         (cons 10 (list x-stiff-end stiff-y-top 0.0)) (cons 11 (list x-stiff-end stiff-y-bot 0.0))))



          ;; [4-3] 스트럿 본체 선 (겹치지 않게 엔드플레이트 끝 x-ep-end 부터 시작)

          ;; 플랜지 외부 선 (Top/Bottom) - 초록색(3)

          (entmake (list '(0 . "LINE") (cons 8 "_버팀보(Strut)") (cons 62 3) 

                         (cons 10 (list x-ep-end y-top 0.0)) (cons 11 (list x-strut-end y-top 0.0))))

          (entmake (list '(0 . "LINE") (cons 8 "_버팀보(Strut)") (cons 62 3) 

                         (cons 10 (list x-ep-end y-bot 0.0)) (cons 11 (list x-strut-end y-bot 0.0))))

          

          ;; 플랜지 내부 선 (Top/Bottom 두께 tf) - 빨간색(1)

          (entmake (list '(0 . "LINE") (cons 8 "_버팀보(Strut)") (cons 62 1) 

                         (cons 10 (list x-ep-end y-top-in 0.0)) (cons 11 (list x-strut-end y-top-in 0.0))))

          (entmake (list '(0 . "LINE") (cons 8 "_버팀보(Strut)") (cons 62 1) 

                         (cons 10 (list x-ep-end y-bot-in 0.0)) (cons 11 (list x-strut-end y-bot-in 0.0))))



          ;; [4-4] 파단선(Break Line) 작도 - 빨간색(1)

          ;; 해결책: 플랜지 내부와 외부 사이 구간은 '수직선'으로 두어 플랜지 선들이 딱 맞물리게 하고, 빈 웨브 부분만 곡선 처리

          (setq bulge-val (if (> wale-dir 0) 0.5 -0.5))

          (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")

                         (cons 8 "_버팀보(Strut)") (cons 62 1) '(90 . 5) '(70 . 0)

                         (cons 10 (list x-strut-end y-top 0.0))    (cons 42 0.0)         ;; P1: 상단 수직 구간 시작

                         (cons 10 (list x-strut-end y-top-in 0.0)) (cons 42 bulge-val)   ;; P2: 곡선 시작점

                         (cons 10 (list x-strut-end ins-y 0.0))    (cons 42 (- bulge-val)) ;; P3: 변곡점

                         (cons 10 (list x-strut-end y-bot-in 0.0)) (cons 42 0.0)         ;; P4: 곡선 끝, 하단 수직 시작

                         (cons 10 (list x-strut-end y-bot 0.0))))                        ;; P5: 하단 수직 끝점

        )

      )

    )

  )



  (setq rock-depth nil) (setq found-rock nil) (setq prev-depth 0.0)

  (if (and *tsp-shotcrete-enable* (= *tsp-shotcrete-enable* "1"))

    (foreach layer *tsp-soil-layers*

      (if (not found-rock)

      (progn

        (setq l-name (car layer)) (setq l-depth (cadr layer)) (setq l-type (nth 5 layer))

        

        (if (and *tsp-shotcrete-chk* (= *tsp-shotcrete-chk* "1") *tsp-shotcrete-layer* (/= *tsp-shotcrete-layer* ""))

          (if (= l-name *tsp-shotcrete-layer*)

            (progn (setq rock-depth prev-depth) (setq found-rock T))

          )

          (if (or (and l-type (or (wcmatch l-type "*풍화암*") (wcmatch l-type "*연암*") (wcmatch l-type "*경암*")))

                  (wcmatch l-name "*풍화암*") (wcmatch l-name "*연암*") (wcmatch l-name "*경암*"))

            (progn (setq rock-depth prev-depth) (setq found-rock T))

          )

        )

        (setq prev-depth l-depth)

        )

      )

    )

  )



  (if found-rock

    (if (>= rock-depth max-depth) 

        (progn (setq timber-bot-y exc-y) (setq shotcrete-bot-y nil)) 

        (progn (setq timber-bot-y (- cy (* rock-depth 1000.0))) (setq shotcrete-bot-y exc-y))

    )

    (progn (setq timber-bot-y exc-y) (setq shotcrete-bot-y nil))

  )

  

  (setq pt-timber-start (list x-tf cy 0.0))

  (setq pt-timber-end (list (+ x-tf (* *tsp-timber-thickness* dir-x)) cy 0.0))

  (setq pt-timber-bottom-end (list (car pt-timber-end) timber-bot-y 0.0))

  (setq pt-timber-bottom-start (list (car pt-timber-start) timber-bot-y 0.0))

  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 "_토류판(timber)") (cons 62 1) '(90 . 4) '(70 . 1) (cons 10 pt-timber-start) (cons 10 pt-timber-end) (cons 10 pt-timber-bottom-end) (cons 10 pt-timber-bottom-start)))

  (setq timber-pl (entlast))

  

  (setq doc (vla-get-activedocument (vlax-get-acad-object)))

  (if (= (getvar "CVPORT") 1)

    (setq mspace (vla-get-paperspace doc))

    (setq mspace (vla-get-modelspace doc))

  )

  (if timber-pl (progn 

      (setq hatch-obj (vla-addhatch mspace 1 "ANSI31" :vlax-true)) 

      (vla-put-patternscale hatch-obj (* 100.0 (get-hatch-scale-factor))) 

      (vla-put-patternangle hatch-obj (* 135.0 (/ pi 180.0))) 

      (vla-put-layer hatch-obj "_토류판(timber)") 

      (vla-put-color hatch-obj 1) 

      (setq sa (vlax-make-safearray 9 (cons 0 0))) 

      (vlax-safearray-fill sa (list (vlax-ename->vla-object timber-pl))) 

      (vla-appendouterloop hatch-obj sa) 

      (vla-evaluate hatch-obj)))



  (if shotcrete-bot-y

    (progn

      (setq pt-sc-start (list cx timber-bot-y 0.0))

      (setq pt-sc-end (list (+ cx (* *tsp-shotcrete-thick* dir-x)) timber-bot-y 0.0))

      (setq pt-sc-bot-end (list (car pt-sc-end) shotcrete-bot-y 0.0))

      (setq pt-sc-bot-start (list (car pt-sc-start) shotcrete-bot-y 0.0))

      

      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 "_숏크리트(shotcrete)") (cons 62 7) '(90 . 4) '(70 . 1) (cons 10 pt-sc-start) (cons 10 pt-sc-end) (cons 10 pt-sc-bot-end) (cons 10 pt-sc-bot-start)))

      (setq sc-pl (entlast))

      (if sc-pl (progn 

          (setq hatch-obj (vla-addhatch mspace 1 "AR-CONC" :vlax-true))

          (vla-put-patternscale hatch-obj (* 2.0 (get-hatch-scale-factor))) 

          (vla-put-patternangle hatch-obj 0.0) 

          (vla-put-layer hatch-obj "_숏크리트(shotcrete)") 

          (vla-put-color hatch-obj 7) 

          (setq sa (vlax-make-safearray 9 (cons 0 0))) 

          (vlax-safearray-fill sa (list (vlax-ename->vla-object sc-pl))) 

          (vla-appendouterloop hatch-obj sa) 

          (vla-evaluate hatch-obj)))

    )

  )

  (princ "\n단면 H-Pile 작도 완료.")

)



;;; --------------------------------------------------------------------------

;;; Function: create-strut-jack-block

;;; Description: 유압잭 형상을 블록으로 정의

;;; --------------------------------------------------------------------------

(defun create-strut-jack-block (/ block-name layer-name _v _pline)

  (setq block-name "유압잭")

  (setq layer-name "_유압잭")

  

  ;; 1. 레이어 생성 (노란색: 2번)

  (create-layer-if-not-exists layer-name "2")



  ;; 2. 블록 정의 (없는 경우에만 생성)

  (if (not (tblsearch "BLOCK" block-name))

    (progn

      ;; 블록 정의 시작 (기준점 0,0,0)

      (entmake (list '(0 . "BLOCK") (cons 2 block-name) '(70 . 0) '(10 0.0 0.0 0.0)))

      

      ;; [내부 헬퍼 함수] 좌표 단순화

      (defun _v (dx dy) (list dx dy))

      

      ;; [내부 헬퍼 함수] 폴리라인 생성 (레이어: _유압잭, 색상: ByLayer)

      (defun _pline (pts closed / lst)

        (setq lst (list '(0 . "LWPOLYLINE") 

                        '(100 . "AcDbEntity") 

                        '(100 . "AcDbPolyline") 

                        (cons 8 layer-name)  ;; 요청하신 레이어 적용

                        (cons 62 256)        ;; ByLayer (레이어가 노랑이므로 노랑으로 나옴)

                        (cons 90 (length pts)) 

                        (cons 70 (if closed 1 0))))

        (foreach p pts

          (setq lst (append lst (list (cons 10 p))))

          (if (> (length p) 2) (setq lst (append lst (list (cons 42 (caddr p))))))

        )

        (entmake lst)

      )



      ;; 잭 형상 작도

      (_pline (list (_v 0 150) (_v 15 150) (_v 15 -150) (_v 0 -150)) T)

      (_pline (list (_v 568 150) (_v 583 150) (_v 583 -150) (_v 568 -150)) T)



      (_pline (list (_v 15 150) (_v 160 85)) nil)

      (_pline (list (_v 15 -150) (_v 160 -85)) nil)



      (_pline (list (_v 15 85) (_v 375 85)) nil)

      (_pline (list (_v 15 -85) (_v 375 -85)) nil)

      (_pline (list (_v 375 85) (_v 375 -85)) nil)



      (_pline (list (_v 15 7) (_v 160 7)) nil)

      (_pline (list (_v 15 -7) (_v 160 -7)) nil)

      (_pline (list (_v 160 7) (_v 160 -7)) nil)



      (_pline (list (_v 446 55) (append (_v 496 55) '(0.102)) (_v 568 70)) nil)

      (_pline (list (_v 568 150) (_v 446 55)) nil)



      (_pline (list (_v 446 -55) (append (_v 496 -55) '(-0.102)) (_v 568 -70)) nil)

      (_pline (list (_v 568 -150) (_v 446 -55)) nil)



      (_pline (list (_v 446 7) (_v 568 7)) nil)

      (_pline (list (_v 445 -7) (_v 568 -7)) nil)



      (_pline (list (_v 400 75) (_v 400 95) (_v 420 95) (_v 420 75)) nil)

      (_pline (list (_v 400 10) (_v 420 10) (_v 420 -10) (_v 400 -10)) T)

      (_pline (list (_v 400 -75) (_v 400 -95) (_v 420 -95) (_v 420 -75)) nil)



      (_pline (list (_v 375 75) (_v 446 75)) nil)

      (_pline (list (_v 375 -75) (_v 446 -75)) nil)

      (_pline (list (_v 446 75) (_v 446 -75)) nil)



      ;; 기준점 및 끝점 포인트 (디버깅용, 필요 시 삭제 가능)

      (entmake (list '(0 . "POINT") (cons 8 layer-name) (cons 10 '(0 0 0))))

      (entmake (list '(0 . "POINT") (cons 8 layer-name) (cons 10 '(583 0 0))))



      (entmake (list '(0 . "ENDBLK"))) ;; 블록 정의 종료

    )

  )

  block-name ;; 블록 이름 반환

)



;;; --------------------------------------------------------------------------

;;; Function: create-screw-jack-block

;;; Description: 스크류잭 형상을 블록으로 정의

;;; --------------------------------------------------------------------------

(defun create-strut-screw-jack-block (/ block-name layer-name _v _pline)

  (setq block-name "스크류잭")

  (setq layer-name "_스크류잭")

  

  ;; 1. 레이어 생성 (노란색: 2번)

  (create-layer-if-not-exists layer-name "2")



  ;; 2. 블록 정의 (없는 경우에만 생성)

  (if (not (tblsearch "BLOCK" block-name))

    (progn

      (entmake (list '(0 . "BLOCK") (cons 2 block-name) '(70 . 0) '(10 0.0 0.0 0.0)))

      

      ;; [좌표 보정 함수] 

      ;; 원본은 중심(0) 기준 -291.5 ~ +291.5 이므로, 

      ;; 시작점을 0으로 맞추기 위해 X에 291.5를 더해줍니다.

      (defun _v (dx dy) (list (+ dx 291.5) dy))

      

      (defun _pline (pts closed / lst)

        (setq lst (list '(0 . "LWPOLYLINE") 

                        '(100 . "AcDbEntity") 

                        '(100 . "AcDbPolyline") 

                        (cons 8 layer-name)

                        (cons 62 256)

                        (cons 90 (length pts)) 

                        (cons 70 (if closed 1 0))))

        (foreach p pts (setq lst (append lst (list (cons 10 p)))))

        (entmake lst)

      )



      ;; --- 스크류잭 형상 (사용자 정의 + 좌표보정) ---

      (entmake (list '(0 . "CIRCLE") (cons 8 layer-name) (cons 62 256) (cons 10 (_v 0 0)) (cons 40 7.0)))



      (_pline (list (_v -22.5 50) (_v 22.5 50) (_v 22.5 -50) (_v -22.5 -50)) T)

      (_pline (list (_v -7 50) (_v -7 100) (_v 7 100) (_v 7 50)) nil)

      (_pline (list (_v -7 -50) (_v -7 -100) (_v 7 -100) (_v 7 -50)) nil)

      (_pline (list (_v -57.5 25) (_v -22.5 25)) nil)

      (_pline (list (_v -57.5 -25) (_v -22.5 -25)) nil)

      (_pline (list (_v 57.5 25) (_v 22.5 25)) nil)

      (_pline (list (_v 57.5 -25) (_v 22.5 -25)) nil)

      (_pline (list (_v -97.5 50) (_v -57.5 50) (_v -57.5 -50) (_v -97.5 -50)) nil)

      (_pline (list (_v 97.5 50) (_v 57.5 50) (_v 57.5 -50) (_v 97.5 -50)) nil)

      (_pline (list (_v -277.5 70) (_v -97.5 70) (_v -97.5 -70) (_v -277.5 -70)) nil)

      (_pline (list (_v 277.5 70) (_v 97.5 70) (_v 97.5 -70) (_v 277.5 -70)) nil)

      (_pline (list (_v -277.5 7) (_v -132.5 7) (_v -132.5 -7) (_v -277.5 -7)) nil)

      (_pline (list (_v 277.5 7) (_v 132.5 7) (_v 132.5 -7) (_v 277.5 -7)) nil)

      (_pline (list (_v -291.5 150) (_v -277.5 150) (_v -277.5 -150) (_v -291.5 -150)) T)

      (_pline (list (_v 291.5 150) (_v 277.5 150) (_v 277.5 -150) (_v 291.5 -150)) T)

      (_pline (list (_v -277.5 150) (_v -132.5 95) (_v -132.5 70)) nil)

      (_pline (list (_v 277.5 150) (_v 132.5 95) (_v 132.5 70)) nil)

      (_pline (list (_v -277.5 -150) (_v -132.5 -95) (_v -132.5 -70)) nil)

      (_pline (list (_v 277.5 -150) (_v 132.5 -95) (_v 132.5 -70)) nil)



      (entmake (list '(0 . "POINT") (cons 8 layer-name) (cons 10 (_v -291.5 0))))



      (entmake (list '(0 . "ENDBLK")))

    )

  )

  block-name

)



;;; --------------------------------------------------------------------------

;;; Function: draw-section-ground-line

;;; Description: 단면도 그리기 메인 진입점 (지반선 그리기 및 UCS 변환)

;;; --------------------------------------------------------------------------

(defun draw-section-ground-line (/ side start-pt end-pt layer-name ltype-name pt1 pt2 h-spec-list h b tw tf)

  (setq *tsp-cancel-flag* nil) ; 취소 신호 초기화

  (initget "L R")

  

  (setq side (vl-catch-all-apply 'getkword (list "\n단면선택 [좌측반단면(L) / 우측반단면(R)] <L>: ")))

  (if (vl-catch-all-error-p side)

    (progn (setq side nil) (setq *tsp-cancel-flag* T)) ;; ESC 누르면 신호 ON

    (if (or (null side) (= side "")) (setq side "L"))

  )

  

  (setq start-pt nil)

  (if side

    (progn

      (setq start-pt (vl-catch-all-apply 'getpoint (list "\n지반선 기준점 지정: ")))

      (if (vl-catch-all-error-p start-pt)

        (progn (setq start-pt nil) (setq *tsp-cancel-flag* T))

      )

    )

  )



  (if start-pt

    (progn

      ;; UCS -> WCS 변환

      (setq pt1 (trans start-pt 1 0))

      

      (if (= side "L")

        (setq end-pt (polar start-pt pi 30000.0))

        (setq end-pt (polar start-pt 0.0 30000.0))

      )

      

      (setq layer-name "_지반선")

      (create-layer-if-not-exists layer-name "8")     

      (setq ltype-name "DASHED")

      (if (not (tblsearch "LTYPE" ltype-name))

        (vl-catch-all-apply 'vla-load (list (vla-get-linetypes (vla-get-activedocument (vlax-get-acad-object))) ltype-name "acad.lin"))

      )

      

      (setq pt2 (trans end-pt 1 0))

      (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) (cons 6 ltype-name) (cons 48 1000.0) '(90 . 2) '(70 . 0) (cons 10 (list (car pt1) (cadr pt1))) (cons 10 (list (car pt2) (cadr pt2)))))      

      ;; 주상도 및 H-Pile 작도 호출

      (draw-soil-column pt2 side)

      (if (= *tsp-hpile-spec* "User-defined")

        (setq h-spec-list *tsp-hpile-custom*)

        (setq h-spec-list (parse-h-spec *tsp-hpile-spec*))

      )

      (setq h (nth 0 h-spec-list) b (nth 1 h-spec-list) tw (nth 2 h-spec-list) tf (nth 3 h-spec-list))

      (draw-section-hpile pt1 side h b tw tf *tsp-max-excavation-depth* *tsp-embedment-depth*)

      T

    )

    nil

  )

)





;;; ==========================================================================

;;; [SECTION 5] 평면도 작도 로직 (Plan View Logic)

;;; ==========================================================================



;;; --------------------------------------------------------------------------

;;; Function: create-wale-offsets

;;; Description: 각 세그먼트 데이터에 맞춰 띠장(Wale)생성 (거리 보정 및 모든 절단면 마감 적용)

;;; --------------------------------------------------------------------------

(defun create-wale-offsets (boundary-ent boundary-orient / seg-idx max-idx seg-data is-def v-start v-end v-angle wale-spec h b tw tf wale-offset-sign offset-dir next-idx next-data next-def next-start next-angle prev-idx prev-data prev-def prev-end prev-angle diff-start diff-end int-angle-start int-angle-end is-butt-start is-0-start is-butt-end is-0-end cut-start-p1 cut-start-p2 cut-end-p1 cut-end-p2 p1 p2 p-outer-s p-outer-e p-of-s p-of-e p-if-s p-if-e p-inner-s p-inner-e p-inner-corner prev-offset-dir prev-wale-spec prev-wale-vals h-prev limit-angle upg-s-vals u-h-s u-tf-s L-start upg-e-vals u-h-e u-tf-e L-end seg-length eff-h-start eff-h-end h-prev-base eff-h-prev next-wale-spec next-wale-vals h-next-base eff-h-next next-offset-dir p-splice1 p-splice2 splice1-cut-p1 splice1-cut-p2 splice2-cut-p1 splice2-cut-p2 res-start res-base res-end base-start-cut base-start-cut-p2 base-end-cut base-end-cut-p2 get-corner-upg-vals get-corner-splice-length draw-wale-subsegment make-wale-lw wale-vals is-upgraded-any)



  ;; [보조 1] LWPOLYLINE 강제 생성

  (defun make-wale-lw (p1 p2 layer color)

    (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")

                   (cons 8 layer) (cons 62 color) '(90 . 2) '(70 . 0)

                   (list 10 (car p1) (cadr p1)) (list 10 (car p2) (cadr p2))))

  )



  ;; [보조 2] 코너의 상향(Upgrade) 띠장 데이터 추출

  (defun get-corner-upg-vals (corner-idx / c-seg c-upg)

    (if (and (>= corner-idx 0) (< corner-idx (length *segment-list*)))

      (progn

        (setq c-seg (nth corner-idx *segment-list*))

        (setq c-upg (cdr (assoc 'UPGRADE-WALE c-seg)))

        (if (and c-upg (= (car c-upg) "Y"))

          (parse-h-spec (cadr c-upg))

          nil

        )

      )

      nil

    )

  )



  ;; [보조 3] 코너의 보강 띠장 설치 길이(Splice Length) 자동 계산 (200 여유분 제거)

  (defun get-corner-splice-length (corner-idx / c-seg c-brace max-l b-dist b-h)

    (if (and (>= corner-idx 0) (< corner-idx (length *segment-list*)))

      (progn

        (setq c-seg (nth corner-idx *segment-list*))

        (setq c-brace (cdr (assoc 'CORNER-BRACE c-seg)))

        (setq max-l 0.0)

        (if (and c-brace (= (car c-brace) "Y") (cadr c-brace))

          (foreach b-row (cadr c-brace)

            (setq b-dist (* (nth 2 b-row) 1000.0))

            (setq b-h (nth 0 (cadr b-row)))

            (if (> (+ b-dist (/ b-h 2.0)) max-l)

              (setq max-l (+ b-dist (/ b-h 2.0)))

            )

          )

        )

        ;; 사보강재 간섭 공간 + 화타쐐기 및 마감 150 = 150 확보 (시공 여유분 200 제거됨)

        (if (> max-l 0) (+ max-l 150.0) 0.0)

      )

      0.0

    )

  )



  ;; [보조 4] 지정된 도막(Sub-segment)의 띠장 라인 4줄 작도

  (defun draw-wale-subsegment (p-start p-end cut-s-p1 cut-s-p2 cut-e-p1 cut-e-p2 sub-h sub-tf / p1_outer p2_outer p_outer_s p_outer_e p1_of p2_of p_of_s p_of_e p1_if p2_if p_if_s p_if_e p1_inner p2_inner p_inner_s p_inner_e)

    (if (> (distance p-start p-end) 0.01)

      (progn

        (setq p1_outer (polar p-start offset-dir 0))

        (setq p2_outer (polar p-end offset-dir 0))

        (setq p_outer_s (tsp-get-intersection-point p1_outer p2_outer cut-s-p1 cut-s-p2))

        (setq p_outer_e (tsp-get-intersection-point p1_outer p2_outer cut-e-p1 cut-e-p2))



        (setq p1_of (polar p-start offset-dir sub-tf))

        (setq p2_of (polar p-end offset-dir sub-tf))

        (setq p_of_s (tsp-get-intersection-point p1_of p2_of cut-s-p1 cut-s-p2))

        (setq p_of_e (tsp-get-intersection-point p1_of p2_of cut-e-p1 cut-e-p2))



        (setq p1_if (polar p-start offset-dir (- sub-h sub-tf)))

        (setq p2_if (polar p-end offset-dir (- sub-h sub-tf)))

        (setq p_if_s (tsp-get-intersection-point p1_if p2_if cut-s-p1 cut-s-p2))

        (setq p_if_e (tsp-get-intersection-point p1_if p2_if cut-e-p1 cut-e-p2))



        (setq p1_inner (polar p-start offset-dir sub-h))

        (setq p2_inner (polar p-end offset-dir sub-h))

        (setq p_inner_s (tsp-get-intersection-point p1_inner p2_inner cut-s-p1 cut-s-p2))

        (setq p_inner_e (tsp-get-intersection-point p1_inner p2_inner cut-e-p1 cut-e-p2))



        (if (null p_outer_s) (setq p_outer_s p1_outer))

        (if (null p_outer_e) (setq p_outer_e p2_outer))

        (if (null p_inner_s) (setq p_inner_s p1_inner))

        (if (null p_inner_e) (setq p_inner_e p2_inner))

        (if (null p_of_s) (setq p_of_s p1_of))

        (if (null p_of_e) (setq p_of_e p2_of))

        (if (null p_if_s) (setq p_if_s p1_if))

        (if (null p_if_e) (setq p_if_e p2_if))



        (make-wale-lw p_outer_s p_outer_e "_띠장(wale)" 3)

        (make-wale-lw p_of_s p_of_e "_띠장(wale)" 1)

        (make-wale-lw p_if_s p_if_e "_띠장(wale)" 1)

        (make-wale-lw p_inner_s p_inner_e "_띠장(wale)" 3)



        (list p_outer_s p_inner_s p_outer_e p_inner_e)

      )

      nil

    )

  )



  ;; 1. 레이어 설정 및 초기화

  (create-layer-if-not-exists "_띠장(wale)" "3")

  (setq wale-offset-sign boundary-orient) 

  (setq max-idx (length *segment-list*))

  (setq seg-idx 0)

  (setq is-upgraded-any nil) 

  

  (princ "\n[작도] 띠장(Wale) 작도 처리 시작...")



  ;; 2. 전체 세그먼트 순회

  (while (< seg-idx max-idx)

    (setq seg-data (nth seg-idx *segment-list*))

    (setq is-def (cdr (assoc 'IS-DEFINED seg-data)))

    

    (if is-def

      (progn

        ;; [STEP 1] 현재 세그먼트 데이터 파싱

        (setq v-start (cdr (assoc 'V-START seg-data)))

        (setq v-end (cdr (assoc 'V-END seg-data)))

        (setq v-angle (cdr (assoc 'ANGLE seg-data)))

        (setq seg-length (distance v-start v-end))

        (setq wale-spec (cdr (assoc 'WALE-SPEC seg-data)))

        

        (if (= wale-spec "User-defined")

          (setq wale-vals (cdr (assoc 'WALE-CUSTOM seg-data)))

          (setq wale-vals (parse-h-spec wale-spec))

        )

        (if (null wale-vals) (setq wale-vals '(300 300 10 15))) 

        (setq h (nth 0 wale-vals) b (nth 1 wale-vals) tw (nth 2 wale-vals) tf (nth 3 wale-vals))

        

        (setq offset-dir (+ v-angle (* (/ pi 2.0) wale-offset-sign)))



        ;; [STEP 2] 인접 및 보강(Upgrade) 데이터 계산

        (setq next-idx (if (is-closed-polyline boundary-ent) (rem (1+ seg-idx) max-idx) (1+ seg-idx)))

        (setq prev-idx (if (is-closed-polyline boundary-ent) (if (= seg-idx 0) (1- max-idx) (1- seg-idx)) (1- seg-idx)))

        

        (setq upg-s-vals (get-corner-upg-vals seg-idx))

        (setq u-h-s (if upg-s-vals (nth 0 upg-s-vals) nil))

        (setq u-tf-s (if upg-s-vals (nth 3 upg-s-vals) nil))

        (setq L-start (if upg-s-vals (get-corner-splice-length seg-idx) 0.0))



        (setq upg-e-vals (get-corner-upg-vals next-idx))

        (setq u-h-e (if upg-e-vals (nth 0 upg-e-vals) nil))

        (setq u-tf-e (if upg-e-vals (nth 3 upg-e-vals) nil))

        (setq L-end (if upg-e-vals (get-corner-splice-length next-idx) 0.0))



        ;; 겹침 방지 보정 (양쪽 상향이 만나면 중간에서 이등분)

        (if (> (+ L-start L-end) seg-length)

          (progn

            (setq L-start (min L-start (/ seg-length 2.0)))

            (setq L-end (min L-end (/ seg-length 2.0)))

          )

        )



        (if (or (> L-start 0.0) (> L-end 0.0))

          (setq is-upgraded-any T)

        )



        (setq eff-h-start (if u-h-s u-h-s h))

        (setq eff-h-end (if u-h-e u-h-e h))



        (setq next-def nil prev-def nil h-prev-base h h-next-base h)

        

        (if (and (< next-idx max-idx) (>= next-idx 0))

          (progn 

            (setq next-data (nth next-idx *segment-list*)) 

            (setq next-def (cdr (assoc 'IS-DEFINED next-data))) 

            (setq next-start (cdr (assoc 'V-START next-data))) 

            (setq next-angle (cdr (assoc 'ANGLE next-data)))

            (if next-def

              (progn

                (setq next-wale-spec (cdr (assoc 'WALE-SPEC next-data)))

                (setq next-wale-vals (if (= next-wale-spec "User-defined") (cdr (assoc 'WALE-CUSTOM next-data)) (parse-h-spec next-wale-spec)))

                (if (null next-wale-vals) (setq next-wale-vals '(300 300 10 15)))

                (setq h-next-base (nth 0 next-wale-vals))

              )

            )

          )

        )

        

        (if (and (< prev-idx max-idx) (>= prev-idx 0))

          (progn 

            (setq prev-data (nth prev-idx *segment-list*)) 

            (setq prev-def (cdr (assoc 'IS-DEFINED prev-data))) 

            (setq prev-end (cdr (assoc 'V-END prev-data))) 

            (setq prev-angle (cdr (assoc 'ANGLE prev-data)))

            (if prev-def

              (progn

                (setq prev-wale-spec (cdr (assoc 'WALE-SPEC prev-data)))

                (setq prev-wale-vals (if (= prev-wale-spec "User-defined") (cdr (assoc 'WALE-CUSTOM prev-data)) (parse-h-spec prev-wale-spec)))

                (if (null prev-wale-vals) (setq prev-wale-vals '(300 300 10 15)))

                (setq h-prev-base (nth 0 prev-wale-vals))

              )

            )

          )

        )



        ;; 코너는 상향 규격을 공유함

        (setq eff-h-prev (if u-h-s u-h-s h-prev-base))

        (setq eff-h-next (if u-h-e u-h-e h-next-base))



        ;; [STEP 3] 시작점 절단면 계산 (eff-h 기준)

        (setq is-butt-start nil is-0-start nil)

        (setq limit-angle (* 120.0 (/ pi 180.0)))

        

        (if prev-def

          (progn

            (setq diff-start (abs (- v-angle prev-angle)))

            (while (> diff-start pi) (setq diff-start (abs (- diff-start (* 2.0 pi)))))

            (setq int-angle-start (- pi diff-start))

            (if (< diff-start 0.01) (setq is-0-start T))

            (if (and (>= diff-start 0.01) (<= int-angle-start (+ limit-angle 0.01))) (setq is-butt-start T))

          )

        )



        (cond

          ((or (not prev-def) is-0-start)

           (setq cut-start-p1 v-start)

           (setq cut-start-p2 (polar v-start offset-dir 100.0)) 

          )

          (is-butt-start

           (setq prev-offset-dir (+ prev-angle (* (/ pi 2.0) wale-offset-sign)))

           (setq cut-start-p1 (polar v-start prev-offset-dir eff-h-prev)) 

           (setq cut-start-p2 (polar cut-start-p1 prev-angle 100.0))

          )

          (t

           (setq prev-offset-dir (+ prev-angle (* (/ pi 2.0) wale-offset-sign)))

           (setq p-inner-corner (tsp-get-intersection-point 

                                  (polar v-start offset-dir eff-h-start) (polar v-end offset-dir eff-h-start)

                                  (polar prev-end prev-offset-dir eff-h-prev)

                                  (polar (cdr (assoc 'V-START prev-data)) prev-offset-dir eff-h-prev)))

           (if (null p-inner-corner) (setq p-inner-corner (polar v-start offset-dir eff-h-start)))

           (setq cut-start-p1 v-start)

           (setq cut-start-p2 p-inner-corner)

          )

        )



        ;; [STEP 4] 끝점 절단면 계산 (eff-h 기준)

        (setq is-butt-end nil is-0-end nil)

        (if next-def

          (progn

            (setq diff-end (abs (- next-angle v-angle)))

            (while (> diff-end pi) (setq diff-end (abs (- diff-end (* 2.0 pi)))))

            (setq int-angle-end (- pi diff-end))

            (if (< diff-end 0.01) (setq is-0-end T))

            (if (and (>= diff-end 0.01) (<= int-angle-end (+ limit-angle 0.01))) (setq is-butt-end T))

          )

        )



        (cond

          ((or (not next-def) is-0-end)

           (setq cut-end-p1 v-end)

           (setq cut-end-p2 (polar v-end offset-dir 100.0))

          )

          (is-butt-end

           (setq cut-end-p1 v-end)

           (setq cut-end-p2 (polar v-end next-angle 100.0))

          )

          (t

           (setq next-offset-dir (+ next-angle (* (/ pi 2.0) wale-offset-sign)))

           (setq p-inner-corner (tsp-get-intersection-point 

                                  (polar v-start offset-dir eff-h-end) (polar v-end offset-dir eff-h-end)

                                  (polar next-start next-offset-dir eff-h-next)

                                  (polar (cdr (assoc 'V-END next-data)) next-offset-dir eff-h-next)))

           (if (null p-inner-corner) (setq p-inner-corner (polar v-end offset-dir eff-h-end)))

           (setq cut-end-p1 v-end)

           (setq cut-end-p2 p-inner-corner)

          )

        )



        ;; [STEP 5] Splice 절단면 계산 (직각 절단)

        (setq p-splice1 (polar v-start v-angle L-start))

        (setq p-splice2 (polar v-end (+ v-angle pi) L-end))



        (setq splice1-cut-p1 p-splice1)

        (setq splice1-cut-p2 (polar p-splice1 offset-dir 100.0))



        (setq splice2-cut-p1 p-splice2)

        (setq splice2-cut-p2 (polar p-splice2 offset-dir 100.0))



        ;; [STEP 6] 3도막(Sub-segments) 작도

        (setq res-start nil res-base nil res-end nil)



        ;; 6.1 Start Upgrade Wale

        (if (> L-start 0.0)

          (setq res-start (draw-wale-subsegment v-start p-splice1 cut-start-p1 cut-start-p2 splice1-cut-p1 splice1-cut-p2 u-h-s u-tf-s))

        )



        ;; 6.2 Base Wale

        (setq base-start-cut (if (> L-start 0.0) splice1-cut-p1 cut-start-p1))

        (setq base-start-cut-p2 (if (> L-start 0.0) splice1-cut-p2 cut-start-p2))

        (setq base-end-cut (if (> L-end 0.0) splice2-cut-p1 cut-end-p1))

        (setq base-end-cut-p2 (if (> L-end 0.0) splice2-cut-p2 cut-end-p2))



        (setq res-base (draw-wale-subsegment (if (> L-start 0.0) p-splice1 v-start)

                                             (if (> L-end 0.0) p-splice2 v-end)

                                             base-start-cut base-start-cut-p2

                                             base-end-cut base-end-cut-p2

                                             h tf))



        ;; 6.3 End Upgrade Wale

        (if (> L-end 0.0)

          (setq res-end (draw-wale-subsegment p-splice2 v-end splice2-cut-p1 splice2-cut-p2 cut-end-p1 cut-end-p2 u-h-e u-tf-e))

        )



        ;; [STEP 7] 모든 절단면 마감 및 단차 연결 완벽 적용 (Capping)

        ;; 7.1 코너부 끝단 (Extreme Ends)

        (if res-start

          (make-wale-lw (nth 0 res-start) (nth 1 res-start) "_띠장(wale)" 3)

          (if res-base (make-wale-lw (nth 0 res-base) (nth 1 res-base) "_띠장(wale)" 3))

        )

        (if res-end

          (make-wale-lw (nth 2 res-end) (nth 3 res-end) "_띠장(wale)" 3)

          (if res-base (make-wale-lw (nth 2 res-base) (nth 3 res-base) "_띠장(wale)" 3))

        )



        ;; 7.2 Splice 부분 절단면 및 단차 완전 마감 (Cut Ends Capping)

        (if res-start

          (make-wale-lw (nth 2 res-start) (nth 3 res-start) "_띠장(wale)" 3)

        )

        (if res-base

          (progn

            (if res-start (make-wale-lw (nth 0 res-base) (nth 1 res-base) "_띠장(wale)" 3))

            (if res-end   (make-wale-lw (nth 2 res-base) (nth 3 res-base) "_띠장(wale)" 3))

          )

        )

        (if res-end

          (make-wale-lw (nth 0 res-end) (nth 1 res-end) "_띠장(wale)" 3)

        )



      ) ; end progn (if is-def)

    )

    (setq seg-idx (1+ seg-idx))

  )

  

  (if is-upgraded-any

    (princ "\n띠장 상향 완료.")

    (princ "\n띠장 작도 완료.")

  )

  (princ)

)



;;; --------------------------------------------------------------------------

;;; Function: calculate-corner-params

;;; Description: 코너 부위에서 H-Pile의 회전각 및 삽입 위치 계산

;;; --------------------------------------------------------------------------

(defun calculate-corner-params (vertex prev-vertex next-vertex h b timber-offset boundary-orient / is-convex half-b angle1 angle2 prev-dir next-dir wale-corner wale-corner-angle hpile-rotation offset-dir insert-point half-angle offset-dist)

  (setq half-b (/ b 2.0))

  

  ;; 1. 코너 방향 판별 (볼록/오목)

  (setq is-convex (is-corner-convex prev-vertex vertex next-vertex boundary-orient))

  

  ;; 2. 각도 계산

  (setq angle1 (angle prev-vertex vertex))

  (setq angle2 (angle vertex next-vertex))

  

  ;; 방향 벡터 설정

  (setq prev-dir (+ angle1 (* boundary-orient (/ pi 2.0))))

  (setq next-dir (+ angle2 (* boundary-orient (/ pi 2.0))))

  

  ;; 3. 이등분선(Wale Corner) 위치 및 각도 계산

  (setq wale-corner (list 

    (/ (+ (car (polar vertex prev-dir timber-offset)) (car (polar vertex next-dir timber-offset))) 2.0)

    (/ (+ (cadr (polar vertex prev-dir timber-offset)) (cadr (polar vertex next-dir timber-offset))) 2.0)

    0.0))

  (setq wale-corner-angle (angle vertex wale-corner))

  

  ;; 4. H-Pile 회전 각도 결정 (이등분선 + 90도)

  (setq hpile-rotation (+ wale-corner-angle (/ pi 2.0)))



  ;; 5. 삽입점(Insert Point) 결정

  (if is-convex

    (progn 

      ;; [볼록한 경우] - 정점에 배치

      (setq insert-point vertex) 

    )

    (progn 

      ;; [오목한 경우] - 사용자 지정 공식 적용

      ;; 공식: l = B/2 * tan(90-a)

      ;; 여기서 a는 half-angle

      

      ;; a (Half Angle) 계산

      (setq half-angle (abs (- angle2 wale-corner-angle)))

      

      ;; 각도 정규화 (PI보다 크면 반대쪽 각도)

      (if (> half-angle pi) (setq half-angle (- (* 2.0 pi) half-angle)))

      

      ;; 0으로 나누기 방지 (매우 뾰족한 경우 안전장치)

      (if (< half-angle 0.001) (setq half-angle 0.001))

      

      ;; 거리 l 계산: tan(90-a) = cot(a) = cos(a)/sin(a)

      (setq offset-dist (* half-b (/ (cos half-angle) (sin half-angle))))

      

      ;; 방향: 이등분선 방향으로 이격

      (setq offset-dir wale-corner-angle)

      

      ;; 최종 좌표 계산

      (setq insert-point (polar vertex offset-dir offset-dist))

    )

  )

  

  ;; 결과 반환

  (list insert-point hpile-rotation is-convex)

)



;;; --------------------------------------------------------------------------

;;; Function: get-hpile-limit-offset

;;; Description: H-Pile 배치 시 플랜지 간섭을 피하기 위한 한계 오프셋 계산

;;; --------------------------------------------------------------------------

(defun get-hpile-limit-offset (hpile-pt h b tf rotation seg-origin seg-angle is-start is-convex / half-h half-b corners pt dx dy proj-dist best-dist wx wy)

  (setq half-h (/ h 2.0) half-b (/ b 2.0))

  (if is-convex

    (setq corners (list 

      (list (- half-b) 0) 

      (list half-b 0)

      (list half-b h)

      (list (- half-b) h)

    ))

    (setq corners (list 

      (list (- half-b) tf) 

      (list half-b tf)

    ))

  )

  (setq best-dist (if is-start -1e99 1e99))

  (foreach pt corners

    (setq wx (+ (car hpile-pt) (- (* (car pt) (cos rotation)) (* (cadr pt) (sin rotation)))))

    (setq wy (+ (cadr hpile-pt) (+ (* (car pt) (sin rotation)) (* (cadr pt) (cos rotation)))))

    (setq dx (- wx (car seg-origin)))

    (setq dy (- wy (cadr seg-origin)))

    (setq proj-dist (+ (* dx (cos seg-angle)) (* dy (sin seg-angle))))

    (if is-start

      (if (> proj-dist best-dist) (setq best-dist proj-dist))

      (if (< proj-dist best-dist) (setq best-dist proj-dist))

    )

  )

  best-dist

)



;;; --------------------------------------------------------------------------

;;; Function: create-hpile-section

;;; Description: H-Pile 폴리라인 형상 생성

;;; --------------------------------------------------------------------------

(defun create-hpile-section (insert-pt h b tw tf layer-name / pts)

  ;; 공통 함수를 통해 좌표 리스트 획득 (insert-pt가 이미 중심점)

  (setq pts (calc-hpile-vertices insert-pt h b tw tf))



  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") 

    (cons 8 layer-name) (cons 62 3) '(90 . 20) '(70 . 1)

    (cons 10 (nth 0 pts))  (cons 10 (nth 1 pts)) 

    (cons 10 (nth 2 pts))  (cons 42 0.4142135623730951) (cons 10 (nth 3 pts))

    (cons 10 (nth 4 pts))  (cons 42 0.4142135623730951) (cons 10 (nth 5 pts))

    (cons 10 (nth 6 pts))  (cons 10 (nth 7 pts)) 

    (cons 10 (nth 8 pts))  (cons 10 (nth 9 pts))

    (cons 10 (nth 10 pts)) (cons 42 0.4142135623730951) (cons 10 (nth 11 pts))

    (cons 10 (nth 12 pts)) (cons 42 0.4142135623730951) (cons 10 (nth 13 pts))

    (cons 10 (nth 14 pts)) (cons 10 (nth 15 pts))))

  (entlast)

)



;;; --------------------------------------------------------------------------

;;; Function: create-hpile-section-block

;;; Description: H-Pile 단면 형상을 블록으로 정의

;;; --------------------------------------------------------------------------

(defun create-hpile-section-block (h b tw tf / hpile-ent point-ent block-name layer-name old-ucs old-osnap half-h)

  (setq block-name (strcat "_HPILE(" (itoa h) "x" (itoa b) "x" (itoa tw) "x" (itoa tf) ")"))

  (setq layer-name (strcat "_H-Pile_" (itoa h) "x" (itoa b) "x" (itoa tw) "-" (itoa tf)))

  (create-layer-if-not-exists layer-name "3")

  (create-layer-if-not-exists "_측면말뚝" "3")

  

  (if (not (tblsearch "BLOCK" block-name))

    (progn

      (setq old-ucs (getvar "UCSNAME") old-osnap (getvar "OSMODE"))

      (command "._UCS" "_W") (setvar "OSMODE" 0)

      

      ;; 수정된 create-hpile-section 함수를 호출하여 형상을 그립니다.

      (setq hpile-ent (create-hpile-section '(0 0) h b tw tf layer-name))

      

      (setq half-h (/ h 2.0))

      (entmake (list '(0 . "POINT") '(8 . "_측면말뚝") (cons 10 (list 0.0 (- half-h) 0.0))))

      (setq point-ent (entlast))

      

      (command "._-BLOCK" block-name (list 0.0 (- half-h) 0.0) hpile-ent point-ent "")

      

      (if old-ucs (command "._UCS" "_R" old-ucs) (command "._UCS" "_W"))

      (setvar "OSMODE" old-osnap)

    )

  )

  block-name

)



;;; --------------------------------------------------------------------------

;;; Function: create-timber-panel-object

;;; Description: 평면도 토류판 폴리라인 및 해치 생성

;;; --------------------------------------------------------------------------

(defun create-timber-panel-object (center-pt width height rot-angle / half-width half-height cos-a sin-a pt1 pt2 pt3 pt4 timber-pline hatch-obj sa doc mspace top-left top-right origin-sa)

  (setq half-width (/ width 2.0) half-height (/ height 2.0))

  (setq cos-a (cos rot-angle) sin-a (sin rot-angle))

  (setq pt1 (list (+ (car center-pt) (- (* (- half-width) cos-a) (* (- half-height) sin-a))) (+ (cadr center-pt) (+ (* (- half-width) sin-a) (* (- half-height) cos-a)))))

  (setq pt2 (list (+ (car center-pt) (- (* half-width cos-a) (* (- half-height) sin-a))) (+ (cadr center-pt) (+ (* half-width sin-a) (* (- half-height) cos-a)))))

  (setq pt3 (list (+ (car center-pt) (- (* half-width cos-a) (* half-height sin-a))) (+ (cadr center-pt) (+ (* half-width sin-a) (* half-height cos-a)))))

  (setq pt4 (list (+ (car center-pt) (- (* (- half-width) cos-a) (* half-height sin-a))) (+ (cadr center-pt) (+ (* (- half-width) sin-a) (* half-height cos-a)))))

  (setq top-right pt3)

  (setq top-left pt4)

  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") '(8 . "_토류판(timber)") '(62 . 1) '(90 . 4) '(70 . 1) (cons 10 pt1) (cons 10 pt2) (cons 10 pt3) (cons 10 pt4)))

  (setq timber-pline (entlast))

  (if timber-pline

    (progn

      (setq doc (vla-get-activedocument (vlax-get-acad-object)))

      (setq mspace (vla-get-modelspace doc))

      (setq hatch-obj (vla-addhatch mspace 1 "ANSI36" :vlax-true))

      (vla-put-patternscale hatch-obj (* 80.0 (get-hatch-scale-factor)))

      (vla-put-patternangle hatch-obj (* 15.0 (/ pi 180.0)))

      (vla-put-layer hatch-obj "_토류판(timber)")

      (vla-put-color hatch-obj 9)

      (setq sa (vlax-make-safearray 9 (cons 0 0)))

      (vlax-safearray-fill sa (list (vlax-ename->vla-object timber-pline)))

      (vla-appendouterloop hatch-obj sa)

      (setq origin-sa (vlax-make-safearray 5 '(0 . 1)))

      (vlax-safearray-fill origin-sa (list (car center-pt) (cadr center-pt)))

      (vla-put-origin hatch-obj origin-sa)

      (vla-evaluate hatch-obj)

    )

  )

  (entmake (list '(0 . "POINT") '(8 . "_토류판(timber)") '(62 . 1) (cons 10 top-left)))

  (entmake (list '(0 . "POINT") '(8 . "_토류판(timber)") '(62 . 1) (cons 10 top-right)))

  (list timber-pline top-left top-right)

)



;;; --------------------------------------------------------------------------

;;; Function: place-hpile-at-corner-simple

;;; Description: 코너 부위에 단일 H-Pile 블록 배치

;;; --------------------------------------------------------------------------

(defun place-hpile-at-corner-simple (vertex angle1 angle2 h b tw tf hpile-block timber-offset boundary-orient prev-vertex next-vertex / hpile-info insert-point hpile-rotation is-convex half-b local-y flange-left flange-right)

  (setq half-b (/ b 2.0))

  (setq hpile-info (calculate-corner-params vertex prev-vertex next-vertex h b timber-offset boundary-orient))

  (setq insert-point (car hpile-info))

  (setq hpile-rotation (cadr hpile-info))

  (setq is-convex (nth 2 hpile-info))

  (entmake (list '(0 . "INSERT") (cons 2 hpile-block) '(8 . "_측면말뚝") (cons 10 insert-point) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) (cons 50 hpile-rotation)))

  (setq local-y (if is-convex 0.0 tf))

  (setq flange-left (list

    (+ (car insert-point) (- (* (- half-b) (cos hpile-rotation)) (* local-y (sin hpile-rotation))))

    (+ (cadr insert-point) (+ (* (- half-b) (sin hpile-rotation)) (* local-y (cos hpile-rotation))))

    0.0))

  (setq flange-right (list

    (+ (car insert-point) (- (* half-b (cos hpile-rotation)) (* local-y (sin hpile-rotation))))

    (+ (cadr insert-point) (+ (* half-b (sin hpile-rotation)) (* local-y (cos hpile-rotation))))

    0.0))

  (create-layer-if-not-exists "DEBUG_FLANGE" "2")

  (entmake (list '(0 . "POINT") '(8 . "DEBUG_FLANGE") '(62 . 2) (cons 10 flange-left)))

  (entmake (list '(0 . "POINT") '(8 . "DEBUG_FLANGE") '(62 . 2) (cons 10 flange-right)))

)



;;; --------------------------------------------------------------------------

;;; Function: place-hpile-timber-along-boundary

;;; Description: 평면도 작도

;;; --------------------------------------------------------------------------

(defun place-hpile-timber-along-boundary (boundary-ent boundary-orient / make-lw-line h b tw tf hpile-values hpile-block timber-width timber-offset corner-gap boundary-data orig-vertices is-closed num-vertices i prev-idx next-idx prev-vertex curr-vertex next-vertex angle1 angle2 angle-diff turn-direction bisector-angle corner-rotation seg-idx max-segments n-verts pt1 pt2 seg-length seg-angle mid-pt ctc-mm outward-normal start-idx start-prev start-next start-vertex end-idx end-prev end-next end-vertex start-hpile-info end-hpile-info start-limit end-limit safe-start safe-end final-width final-center-dist timber-pt timber-pt-offset dist new-pt hpile-rotation timber-result timber-pline top-left top-right edge-gap left-hpile-pt right-hpile-pt left-offset-pt right-offset-pt timber-idx total-timbers d-start piles-A piles-B timbers-A timbers-B count-A count-B use-method-B final-piles final-timbers temp-dist center start-pile-placed end-pile-placed start-pile-pt end-pile-pt start-is-corner end-is-corner range new-start new-end raw-start raw-end valid-timbers potential-start potential-end seg-data is-def current-spec current-ctc prev-seg-idx next-seg-idx prev-spec next-spec prev-vals next-vals h-prev b-prev h-next b-next current-thick target-seg-data theory-count-A theory-count-B h-start b-start h-end b-end tf-start tf-end wale-spec wale-vals wale-h max-anchor-L max-anchor-ang max-anchor-free max-anchor-bond has-anchor cur-free cur-bond cur-ang cur-L proj-free proj-bond pt-base pt-free-end pt-bond-end pt-h1 pt-h2 pt-h3 pt-h4 cos-val proj-prot pt-anchor-start mid-pile-count prev-def next-def next-seg-real-idx cur-brace prev-brace dist-to-start dist-to-end brace-blocked p-brace b-spec b-custom b-dist-m b-dist b-h dir-A dir-B norm-A norm-B pt-A-base pt-B-base pt-A-inner pt-B-inner brace-dir half-w L1-A L1-B L2-A L2-B pA1 pB1 pA2 pB2 wA1 wA2 wB1 wB2 brace-b-rows b-row chk-last-b-row chk-b-custom chk-b-dist chk-b-h chk-margin next-brace idx w-h-prev w-h-next draw-closed-poly vW-A dot-A pBaseExt-A pBaseOth-A isExtA2 p1_foot-A p1_14-A p1-A l2_p2-A p2-A p2_foot-A p2_14-A p3-A p2_end-A p3_end-A brace_A1 brace_A2 brace-dir-B vW-B dot-B pBaseExt-B pBaseOth-B isExtB2 p1_foot-B p1_14-B p1-B l2_p2-B p2-B p2_foot-B p2_14-B p3-B p2_end-B p3_end-B brace_B1 brace_B2 brace_A1_in brace_A2_in brace_B1_in brace_B2_in b-tw half-tw mid_A_in mid_B_in web1_A web1_B web2_A web2_B b-row-idx b-jack jack-side jack-len pt-jack-ins jack-rot)

  

  ;; [내부 헬퍼] 2D 좌표 변환 및 LWPOLYLINE 강제 생성

  (defun make-lw-line (p1 p2 layer color)

    (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")

                   (cons 8 layer) (cons 62 color) '(90 . 2) '(70 . 0)

                   (list 10 (car p1) (cadr p1)) (list 10 (car p2) (cadr p2))))

  )



  (setq corner-gap 50.0)

  (setq edge-gap 25.0)

  (setq *tsp-segment-pile-counts* '())

  (setq is-closed (is-closed-polyline boundary-ent))

  (setq orig-vertices (mapcar '(lambda (item) (cdr (assoc 'V-START item))) *segment-list*))

  (if (not is-closed) (setq orig-vertices (append orig-vertices (list (cdr (assoc 'V-END (last *segment-list*)))))))

  (setq num-vertices (length orig-vertices))

  (setq seg-idx 0 n-verts num-vertices)

  (setq max-segments (if is-closed n-verts (- n-verts 1)))

  

  (while (< seg-idx max-segments)

    (setq seg-data (nth seg-idx *segment-list*))

    (setq is-def (cdr (assoc 'IS-DEFINED seg-data))) 

    (setq mid-pile-count 0) 

    ;; [7단계] CIP 세그먼트는 place-hpile-timber에서 건너뜀
    ;; (CIP는 tsp-draw-cip-plan-per-segment에서 별도 처리)
    (if (= (cdr (assoc 'WALL-TYPE seg-data)) "CIP")
      (setq is-def nil)
    )

    

    (if is-def

      (progn

        (setq current-spec (cdr (assoc 'WALL-SPEC seg-data)))

        (if (= current-spec "User-defined") (setq hpile-values (cdr (assoc 'WALL-CUSTOM seg-data))) (setq hpile-values (parse-h-spec current-spec)))

        (setq h (nth 0 hpile-values) b (nth 1 hpile-values) tw (nth 2 hpile-values) tf (nth 3 hpile-values))

        (setq current-ctc (cdr (assoc 'CTC seg-data)))

        (setq ctc-mm (* current-ctc 1000.0))

        (setq current-thick (cdr (assoc 'TIMBER-THICKNESS seg-data)))

        (if (null current-thick) (setq current-thick 60)) 

        (setq timber-offset (+ tf (/ current-thick 2.0)))

        (setq timber-width (- (* current-ctc 1000) 50))

        (setq hpile-block (create-hpile-section-block h b tw tf))

        

        (setq pt1 (nth seg-idx orig-vertices))

        (setq pt2 (nth (if is-closed (rem (+ seg-idx 1) n-verts) (+ seg-idx 1)) orig-vertices))

        (setq seg-length (distance pt1 pt2) seg-angle (angle pt1 pt2))

        (setq mid-pt (list (/ (+ (car pt1) (car pt2)) 2.0) (/ (+ (cadr pt1) (cadr pt2)) 2.0) 0.0))

        (setq outward-normal (+ seg-angle (* boundary-orient (/ pi 2.0))))

        (setq hpile-rotation (+ outward-normal (/ pi 2.0)))



        (setq wale-spec (cdr (assoc 'WALE-SPEC seg-data)))

        (if (= wale-spec "User-defined") (setq wale-vals (cdr (assoc 'WALE-CUSTOM seg-data))) (setq wale-vals (parse-h-spec wale-spec)))

        (if (null wale-vals) (setq wale-vals '(300 300 10 15)))

        (setq wale-h (nth 0 wale-vals))

        

        (setq max-anchor-L 0.0 max-anchor-ang 0.0 max-anchor-free 0.0 max-anchor-bond 0.0 has-anchor nil)

        (foreach s-item (cdr (assoc 'SUPPORT-LIST seg-data))

          (if (wcmatch (car s-item) "앵커*")

            (progn

              (setq cur-free (nth 5 s-item) cur-bond (nth 6 s-item) cur-ang (nth 4 s-item))

              (setq cur-L (+ cur-free cur-bond))

              (if (> cur-L max-anchor-L) (progn (setq max-anchor-L cur-L max-anchor-ang cur-ang max-anchor-free cur-free max-anchor-bond cur-bond has-anchor T)))

            )

          )

        )



        (setq start-pile-placed nil end-pile-placed nil start-is-corner nil end-is-corner nil)

        (setq start-idx seg-idx end-idx (if is-closed (rem (+ seg-idx 1) n-verts) (+ seg-idx 1)))

        

        (setq h-start h b-start b tf-start tf)

        (setq prev-idx (if is-closed (if (= start-idx 0) (- n-verts 1) (- start-idx 1)) (- start-idx 1)))

        (setq prev-def (if (>= prev-idx 0) (cdr (assoc 'IS-DEFINED (nth prev-idx *segment-list*))) nil))

        (if prev-def

          (progn

            (setq prev-spec (cdr (assoc 'WALL-SPEC (nth prev-idx *segment-list*))))

            (if (= prev-spec "User-defined") (setq prev-vals (cdr (assoc 'WALL-CUSTOM (nth prev-idx *segment-list*)))) (setq prev-vals (parse-h-spec prev-spec)))

            (setq h-prev (nth 0 prev-vals) b-prev (nth 1 prev-vals))

            (if (or (> h-prev h) (and (= h-prev h) (> b-prev b))) (setq h-start h-prev b-start b-prev tf-start (nth 3 prev-vals)))

          )

        )



        (setq h-end h b-end b tf-end tf)

        (setq next-seg-real-idx (if is-closed (rem (+ seg-idx 1) n-verts) (+ seg-idx 1)))

        (setq next-def (if (< next-seg-real-idx max-segments) (cdr (assoc 'IS-DEFINED (nth next-seg-real-idx *segment-list*))) nil))

        (if next-def

          (progn

             (setq next-spec (cdr (assoc 'WALL-SPEC (nth next-seg-real-idx *segment-list*))))

             (if (= next-spec "User-defined") (setq next-vals (cdr (assoc 'WALL-CUSTOM (nth next-seg-real-idx *segment-list*)))) (setq next-vals (parse-h-spec next-spec)))

             (setq h-next (nth 0 next-vals) b-next (nth 1 next-vals))

             (if (or (> h-next h) (and (= h-next h) (> b-next b))) (setq h-end h-next b-end b-next tf-end (nth 3 next-vals)))

          )

        )



        (if prev-def

          (progn 

            (setq start-is-corner T start-vertex (nth start-idx orig-vertices) start-prev (nth (if is-closed (if (= start-idx 0) (- n-verts 1) (- start-idx 1)) (- start-idx 1)) orig-vertices) start-next (nth (if is-closed (if (= start-idx (- n-verts 1)) 0 (+ start-idx 1)) (+ start-idx 1)) orig-vertices)) 

            (setq start-hpile-info (calculate-corner-params start-vertex start-prev start-next h-start b-start timber-offset boundary-orient)) 

            (setq start-limit (get-hpile-limit-offset (car start-hpile-info) h-start b-start tf-start (cadr start-hpile-info) pt1 seg-angle T (nth 2 start-hpile-info)))

          )

          (progn 

            (setq start-pile-pt (polar pt1 seg-angle (/ b 2.0))) 

            (entmake (list '(0 . "INSERT") (cons 2 hpile-block) '(8 . "_측면말뚝") (cons 10 start-pile-pt) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) (cons 50 hpile-rotation))) 

            (setq start-limit (+ (/ b 2.0) 25.0) start-pile-placed T start-is-corner nil)

          )

        )



        (if next-def

          (progn 

            (setq end-is-corner T end-vertex (nth end-idx orig-vertices) end-prev (nth (if is-closed (if (= end-idx 0) (- n-verts 1) (- end-idx 1)) (- end-idx 1)) orig-vertices) end-next (nth (if is-closed (if (= end-idx (- n-verts 1)) 0 (+ end-idx 1)) (+ end-idx 1)) orig-vertices)) 

            (setq end-hpile-info (calculate-corner-params end-vertex end-prev end-next h-end b-end timber-offset boundary-orient)) 

            (setq end-limit (get-hpile-limit-offset (car end-hpile-info) h-end b-end tf-end (cadr end-hpile-info) pt1 seg-angle nil (nth 2 end-hpile-info)))

          )

          (progn 

            (setq end-pile-pt (polar pt2 (+ seg-angle pi) (/ b 2.0))) 

            (entmake (list '(0 . "INSERT") (cons 2 hpile-block) '(8 . "_측면말뚝") (cons 10 end-pile-pt) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) (cons 50 hpile-rotation))) 

            (setq end-limit (- seg-length (+ (/ b 2.0) 25.0)) end-pile-placed T end-is-corner nil)

          )

        )

        

        (if (<= start-limit end-limit)

          (progn

            (setq piles-A '() count-A 0 theory-count-A 0 i 0) (while (<= (* (+ i 0.5) ctc-mm) (/ seg-length 2.0)) (foreach dir '(1.0 -1.0) (setq theory-count-A (1+ theory-count-A) temp-dist (* (+ i 0.5) ctc-mm dir) center (+ (/ seg-length 2.0) temp-dist)) (if (and (> center (+ start-limit (/ b 2.0))) (< center (- end-limit (/ b 2.0)))) (progn (setq piles-A (append piles-A (list (polar mid-pt seg-angle temp-dist))) count-A (1+ count-A))))) (setq i (1+ i)))

            (setq piles-B '() count-B 0 theory-count-B 1 center (/ seg-length 2.0)) (if (and (> center (+ start-limit (/ b 2.0))) (< center (- end-limit (/ b 2.0)))) (progn (setq piles-B (list mid-pt) count-B 1))) (setq i 1) (while (<= (* i ctc-mm) (/ seg-length 2.0)) (foreach dir '(1.0 -1.0) (setq theory-count-B (1+ theory-count-B) temp-dist (* i ctc-mm dir) center (+ (/ seg-length 2.0) temp-dist)) (if (and (> center (+ start-limit (/ b 2.0))) (< center (- end-limit (/ b 2.0)))) (progn (setq piles-B (append piles-B (list (polar mid-pt seg-angle temp-dist))) count-B (1+ count-B))))) (setq i (1+ i)))

            (cond ((< theory-count-B theory-count-A) (setq use-method-B T)) ((> theory-count-B theory-count-A) (setq use-method-B nil)) (t (if (>= count-B count-A) (setq use-method-B T) (setq use-method-B nil))))

            (if use-method-B (setq final-piles piles-B) (setq final-piles piles-A))

            

            (setq mid-pile-count (length final-piles))

            

            (setq final-timbers '())

            (if use-method-B (progn (setq i 0) (while (<= (* (+ i 0.5) ctc-mm) (+ (/ seg-length 2.0) ctc-mm)) (foreach dir '(1.0 -1.0) (setq temp-dist (* (+ i 0.5) ctc-mm dir) center (+ (/ seg-length 2.0) temp-dist) final-timbers (append final-timbers (list (cons (- center (/ timber-width 2.0)) (+ center (/ timber-width 2.0))))))) (setq i (1+ i)))) (progn (setq center (/ seg-length 2.0) final-timbers (list (cons (- center (/ timber-width 2.0)) (+ center (/ timber-width 2.0)))) i 1) (while (<= (* i ctc-mm) (+ (/ seg-length 2.0) ctc-mm)) (foreach dir '(1.0 -1.0) (setq temp-dist (* i ctc-mm dir) center (+ (/ seg-length 2.0) temp-dist) final-timbers (append final-timbers (list (cons (- center (/ timber-width 2.0)) (+ center (/ timber-width 2.0))))))) (setq i (1+ i)))))

            (setq final-timbers (vl-sort final-timbers (function (lambda (a b) (< (car a) (car b))))))

            (setq valid-timbers '()) (foreach range final-timbers (setq potential-start (max (car range) start-limit) potential-end (min (cdr range) end-limit)) (if (> (- potential-end potential-start) 100.0) (setq valid-timbers (append valid-timbers (list (cons potential-start potential-end)))))) (setq final-timbers valid-timbers)

            (setq timber-idx 0 total-timbers (length final-timbers))

            

            (foreach range final-timbers 

              (setq safe-start (car range) safe-end (cdr range) final-width (- safe-end safe-start) final-center-dist (/ (+ safe-start safe-end) 2.0)) 

              (setq timber-pt (polar pt1 seg-angle final-center-dist) timber-pt-offset (polar timber-pt outward-normal (- timber-offset))) 

              (setq timber-result (create-timber-panel-object timber-pt-offset final-width current-thick seg-angle) top-left (nth 1 timber-result) top-right (nth 2 timber-result)) 

              

              ;; [간섭 배제]

              (setq cur-brace (cdr (assoc 'CORNER-BRACE seg-data)))

              (setq next-brace nil)

              (if (and next-seg-real-idx (< next-seg-real-idx (length *segment-list*)))

                (setq next-brace (cdr (assoc 'CORNER-BRACE (nth next-seg-real-idx *segment-list*))))

              )

              

              (setq dist-to-start final-center-dist dist-to-end (- seg-length final-center-dist) brace-blocked nil)

              

              ;; 1. 시작점(pt1) 사보강재 간섭 체크

              (if (and cur-brace (= (car cur-brace) "Y") (cadr cur-brace))

                (progn

                  (setq chk-last-b-row (last (cadr cur-brace)))

                  (setq chk-b-custom (cadr chk-last-b-row))

                  (setq chk-b-dist (* (nth 2 chk-last-b-row) 1000.0))

                  (setq chk-b-h (nth 0 chk-b-custom))

                  (setq chk-margin (+ (/ chk-b-h 2.0) 150.0)) 

                  (if (< dist-to-start (+ chk-b-dist chk-margin)) (setq brace-blocked T))

                )

              )

              

              ;; 2. 끝점(pt2) 사보강재 간섭 체크

              (if (and next-brace (= (car next-brace) "Y") (cadr next-brace))

                (progn

                  (setq chk-last-b-row (last (cadr next-brace)))

                  (setq chk-b-custom (cadr chk-last-b-row))

                  (setq chk-b-dist (* (nth 2 chk-last-b-row) 1000.0))

                  (setq chk-b-h (nth 0 chk-b-custom))

                  (setq chk-margin (+ (/ chk-b-h 2.0) 150.0))

                  (if (< dist-to-end (+ chk-b-dist chk-margin)) (setq brace-blocked T))

                )

              )

              

              (if (and has-anchor (not brace-blocked))

                (progn

                  (create-layer-if-not-exists "_앵커(Anchor)" "6")

                  (setq cos-val (cos (* max-anchor-ang (/ pi 180.0))))

                  (setq proj-free (* max-anchor-free 1000.0 cos-val) proj-bond (* max-anchor-bond 1000.0 cos-val) proj-prot (* 150.0 cos-val))

                  (setq pt-base (polar timber-pt outward-normal wale-h))

                  (setq pt-free-end (polar pt-base (+ outward-normal pi) proj-free) pt-bond-end (polar pt-free-end (+ outward-normal pi) proj-bond))

                  

                  (setq pt-h1 (polar pt-base (+ outward-normal (/ pi 2.0)) 100.0) pt-h2 (polar pt-base (- outward-normal (/ pi 2.0)) 100.0))

                  (setq pt-h3 (polar pt-h2 outward-normal 60.0) pt-h4 (polar pt-h1 outward-normal 60.0))

                  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 "_앵커(Anchor)") (cons 62 6) '(90 . 4) '(70 . 1) (list 10 (car pt-h1) (cadr pt-h1)) (list 10 (car pt-h2) (cadr pt-h2)) (list 10 (car pt-h3) (cadr pt-h3)) (list 10 (car pt-h4) (cadr pt-h4))))

                                 

                  (setq pt-anchor-start (polar pt-base outward-normal (+ 60.0 proj-prot)))

                  (make-lw-line pt-anchor-start pt-base "_앵커(Anchor)" 6)

                  (make-lw-line pt-base pt-free-end "_앵커(Anchor)" 6)

                  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 "_앵커(Anchor)") (cons 62 6) '(90 . 2) '(70 . 0) '(43 . 400.0) (list 10 (car pt-free-end) (cadr pt-free-end)) (list 10 (car pt-bond-end) (cadr pt-bond-end))))

                )

              )

              

              (if (and (= timber-idx 0) (not start-is-corner) (not start-pile-placed)) (progn (setq left-offset-pt (polar top-left (+ seg-angle pi) edge-gap) left-hpile-pt (list (car left-offset-pt) (cadr timber-pt) 0.0)) (entmake (list '(0 . "INSERT") (cons 2 hpile-block) '(8 . "_측면말뚝") (cons 10 left-hpile-pt) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) (cons 50 hpile-rotation))))) 

              (if (and (= timber-idx (- total-timbers 1)) (not end-is-corner) (not end-pile-placed)) (progn (setq right-offset-pt (polar top-right seg-angle edge-gap) right-hpile-pt (list (car right-offset-pt) (cadr timber-pt) 0.0)) (entmake (list '(0 . "INSERT") (cons 2 hpile-block) '(8 . "_측면말뚝") (cons 10 right-hpile-pt) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) (cons 50 hpile-rotation))))) 

              (setq timber-idx (1+ timber-idx))

            )

            (foreach pt final-piles (entmake (list '(0 . "INSERT") (cons 2 hpile-block) '(8 . "_측면말뚝") (cons 10 pt) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) (cons 50 hpile-rotation))))

          )

        )

      ) 

    )

    (setq *tsp-segment-pile-counts* (append *tsp-segment-pile-counts* (list mid-pile-count)))

    (setq seg-idx (1+ seg-idx))

  )

  

  ;; [코너 파일 및 사보강재 작도 (화타쐐기 추가 및 기하학 개선)]

  (setq idx 0)

  (while (< idx num-vertices)

    (setq prev-seg-idx (if (= idx 0) (if is-closed (1- num-vertices) -1) (1- idx)))

    (setq next-seg-idx (if (>= idx max-segments) -1 idx)) 

    (setq prev-def (if (>= prev-seg-idx 0) (cdr (assoc 'IS-DEFINED (nth prev-seg-idx *segment-list*))) nil))

    (setq next-def (if (>= next-seg-idx 0) (cdr (assoc 'IS-DEFINED (nth next-seg-idx *segment-list*))) nil))

    

    (if (and prev-def next-def)

      (progn

        (setq prev-vertex (nth prev-seg-idx orig-vertices) curr-vertex (nth idx orig-vertices) next-vertex (nth (if is-closed (if (= idx (1- num-vertices)) 0 (1+ idx)) (1+ idx)) orig-vertices))

        (setq seg-data (nth prev-seg-idx *segment-list*)) 

        (setq prev-spec (cdr (assoc 'WALL-SPEC seg-data))) (if (= prev-spec "User-defined") (setq prev-vals (cdr (assoc 'WALL-CUSTOM seg-data))) (setq prev-vals (parse-h-spec prev-spec))) (setq h-prev (nth 0 prev-vals) b-prev (nth 1 prev-vals)) 

        (setq target-seg-data (nth next-seg-idx *segment-list*)) 

        (setq next-spec (cdr (assoc 'WALL-SPEC target-seg-data))) (if (= next-spec "User-defined") (setq next-vals (cdr (assoc 'WALL-CUSTOM target-seg-data))) (setq next-vals (parse-h-spec next-spec))) (setq h-next (nth 0 next-vals) b-next (nth 1 next-vals)) 

        

        (if (or (> h-next h-prev) (and (= h-next h-prev) (> b-next b-prev))) 

          (setq h h-next b b-next tw (nth 2 next-vals) tf (nth 3 next-vals) target-seg-data (nth next-seg-idx *segment-list*)) 

          (setq h h-prev b b-prev tw (nth 2 prev-vals) tf (nth 3 prev-vals) target-seg-data (nth prev-seg-idx *segment-list*))

        ) 

        (setq current-thick (cdr (assoc 'TIMBER-THICKNESS target-seg-data))) (if (null current-thick) (setq current-thick 60)) (setq hpile-block (create-hpile-section-block h b tw tf)) (setq timber-offset (+ tf (/ current-thick 2.0)))

        (place-hpile-at-corner-simple curr-vertex (angle prev-vertex curr-vertex) (angle curr-vertex next-vertex) h b tw tf hpile-block timber-offset boundary-orient prev-vertex next-vertex)

        

        ;; 사보강재 작도 로직

        (setq p-brace nil)

        (if (and (>= idx 0) (< idx (length *segment-list*)))

          (setq p-brace (cdr (assoc 'CORNER-BRACE (nth idx *segment-list*))))

        )



        (if (and p-brace (= (car p-brace) "Y"))

          (progn

            (setq prev-wale-spec (cdr (assoc 'WALE-SPEC (nth prev-seg-idx *segment-list*))))

            (setq prev-wale-vals (if (= prev-wale-spec "User-defined") (cdr (assoc 'WALE-CUSTOM (nth prev-seg-idx *segment-list*))) (parse-h-spec prev-wale-spec)))

            (setq w-h-prev (if prev-wale-vals (nth 0 prev-wale-vals) 300))

            (setq next-wale-spec (cdr (assoc 'WALE-SPEC (nth next-seg-idx *segment-list*))))

            (setq next-wale-vals (if (= next-wale-spec "User-defined") (cdr (assoc 'WALE-CUSTOM (nth next-seg-idx *segment-list*))) (parse-h-spec next-wale-spec)))

            (setq w-h-next (if next-wale-vals (nth 0 next-wale-vals) 300))

            

            (setq dir-A (angle curr-vertex prev-vertex) dir-B (angle curr-vertex next-vertex))

            (setq norm-A (+ (angle prev-vertex curr-vertex) (* boundary-orient (/ pi 2.0))))

            (setq norm-B (+ (angle curr-vertex next-vertex) (* boundary-orient (/ pi 2.0))))



            ;; 다열 사보강재 반복 작도 루프

            (setq brace-b-rows (cadr p-brace))

            (setq b-row-idx 0) ;; 열수 카운터

            (create-strut-jack-block) 

            (create-strut-screw-jack-block) 

            

            (foreach b-row brace-b-rows

              (setq b-spec (nth 0 b-row) b-custom (nth 1 b-row) b-dist-m (nth 2 b-row))

              (setq b-jack (nth 3 b-row))

              (if (null b-jack) (setq b-jack "스크류잭"))



              (setq b-dist (* b-dist-m 1000.0))

              (setq b-h (nth 0 b-custom) half-w (/ b-h 2.0))

            

              (setq pt-A-base (polar curr-vertex dir-A b-dist) pt-B-base (polar curr-vertex dir-B b-dist))

              

              (setq pt-A-inner (list (car (polar pt-A-base norm-A w-h-prev)) (cadr (polar pt-A-base norm-A w-h-prev)) 0.0))

              (setq pt-B-inner (list (car (polar pt-B-base norm-B w-h-next)) (cadr (polar pt-B-base norm-B w-h-next)) 0.0))

              (setq brace-dir (angle pt-A-inner pt-B-inner))

              

              (setq L1-A (polar pt-A-inner (+ brace-dir (/ pi 2.0)) half-w) L1-B (polar pt-B-inner (+ brace-dir (/ pi 2.0)) half-w))

              (setq L2-A (polar pt-A-inner (- brace-dir (/ pi 2.0)) half-w) L2-B (polar pt-B-inner (- brace-dir (/ pi 2.0)) half-w))

              

              (setq pA1 (inters pt-A-inner (polar pt-A-inner dir-A 100.0) L1-A L1-B nil))

              (setq pA2 (inters pt-A-inner (polar pt-A-inner dir-A 100.0) L2-A L2-B nil))

              (setq pB1 (inters pt-B-inner (polar pt-B-inner dir-B 100.0) L1-A L1-B nil))

              (setq pB2 (inters pt-B-inner (polar pt-B-inner dir-B 100.0) L2-A L2-B nil))

              

              (create-layer-if-not-exists "_사보강재(Brace)" "3")

              (create-layer-if-not-exists "_화타쐐기(Wedge)" "2")

              

              (if (and pA1 pB1 pA2 pB2)

                (progn

                  (defun draw-closed-poly (pts layer color / lst)

                    (setq lst (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")

                                    (cons 8 layer) (cons 62 color) (cons 90 (length pts)) '(70 . 1)))

                    (foreach p pts (setq lst (append lst (list (list 10 (car p) (cadr p))))))

                    (entmake lst)

                  )

                  

                  ;; ===== Side A 화타쐐기 =====

                  (setq vW-A (list (- (car pA2) (car pA1)) (- (cadr pA2) (cadr pA1))))

                  (setq dot-A (+ (* (car vW-A) (cos brace-dir)) (* (cadr vW-A) (sin brace-dir))))

                  (if (> dot-A 0)

                    (setq pBaseExt-A pA2 pBaseOth-A pA1 isExtA2 T)

                    (setq pBaseExt-A pA1 pBaseOth-A pA2 isExtA2 nil)

                  )

                  

                  (setq p1_foot-A pBaseOth-A)

                  (setq p1_14-A (polar p1_foot-A norm-A 14.0))

                  (setq p1-A (polar p1_foot-A norm-A 64.0))

                  

                  (setq l2_p2-A (polar p1-A (angle pA1 pA2) 100.0))

                  (setq p2-A (inters p1-A l2_p2-A pBaseExt-A (polar pBaseExt-A brace-dir 100.0) nil))

                  

                  (setq p2_foot-A (polar p2-A (+ norm-A pi) 64.0))

                  (setq p2_14-A (polar p2_foot-A norm-A 14.0))

                  (setq p3-A (inters p2-A (polar p2-A (+ brace-dir (/ pi 2.0)) 100.0) pBaseOth-A (polar pBaseOth-A brace-dir 100.0) nil))

                  

                  (setq p2_end-A (polar p2-A brace-dir 14.0))

                  (setq p3_end-A (polar p3-A brace-dir 14.0))

                  

                  (if isExtA2

                    (setq brace_A1 p3_end-A brace_A2 p2_end-A)

                    (setq brace_A1 p2_end-A brace_A2 p3_end-A)

                  )

                  

                  (draw-closed-poly (list p1_foot-A p2_foot-A p2_14-A p1_14-A) "_화타쐐기(Wedge)" 2)

                  (draw-closed-poly (list p1_14-A p1-A p3-A p2-A p2_14-A) "_화타쐐기(Wedge)" 2)

                  (draw-closed-poly (list p2-A p3-A p3_end-A p2_end-A) "_화타쐐기(Wedge)" 2)



                  ;; ===== Side B 화타쐐기 =====

                  (setq brace-dir-B (+ brace-dir pi))

                  (setq vW-B (list (- (car pB2) (car pB1)) (- (cadr pB2) (cadr pB1))))

                  (setq dot-B (+ (* (car vW-B) (cos brace-dir-B)) (* (cadr vW-B) (sin brace-dir-B))))

                  (if (> dot-B 0)

                    (setq pBaseExt-B pB2 pBaseOth-B pB1 isExtB2 T)

                    (setq pBaseExt-B pB1 pBaseOth-B pB2 isExtB2 nil)

                  )

                  

                  (setq p1_foot-B pBaseOth-B)

                  (setq p1_14-B (polar p1_foot-B norm-B 14.0))

                  (setq p1-B (polar p1_foot-B norm-B 64.0))

                  

                  (setq l2_p2-B (polar p1-B (angle pB1 pB2) 100.0))

                  (setq p2-B (inters p1-B l2_p2-B pBaseExt-B (polar pBaseExt-B brace-dir-B 100.0) nil))

                  

                  (setq p2_foot-B (polar p2-B (+ norm-B pi) 64.0))

                  (setq p2_14-B (polar p2_foot-B norm-B 14.0))

                  (setq p3-B (inters p2-B (polar p2-B (+ brace-dir-B (/ pi 2.0)) 100.0) pBaseOth-B (polar pBaseOth-B brace-dir-B 100.0) nil))

                  

                  (setq p2_end-B (polar p2-B brace-dir-B 14.0))

                  (setq p3_end-B (polar p3-B brace-dir-B 14.0))

                  

                  (if isExtB2

                    (setq brace_B1 p3_end-B brace_B2 p2_end-B)

                    (setq brace_B1 p2_end-B brace_B2 p3_end-B)

                  )

                  

                  (draw-closed-poly (list p1_foot-B p2_foot-B p2_14-B p1_14-B) "_화타쐐기(Wedge)" 2)

                  (draw-closed-poly (list p1_14-B p1-B p3-B p2-B p2_14-B) "_화타쐐기(Wedge)" 2)

                  (draw-closed-poly (list p2-B p3-B p3_end-B p2_end-B) "_화타쐐기(Wedge)" 2)



                  ;; 잭 삽입

                  (setq jack-side nil)

                  (if (> b-row-idx 0) ;; 1열(0)은 제외, 2열 이상부터

                    (if (= (rem b-row-idx 2) 1) (setq jack-side "A") (setq jack-side "B")) ;; 홀수=A, 짝수=B

                  )

                  

                  (setq jack-len 583.0)

                  

                  (if (= jack-side "A")

                    (progn

                      (setq pt-jack-ins (list (/ (+ (car p2_end-A) (car p3_end-A)) 2.0) (/ (+ (cadr p2_end-A) (cadr p3_end-A)) 2.0) 0.0))

                      (setq jack-rot brace-dir)

                      (entmake (list '(0 . "INSERT") (cons 2 b-jack) (cons 8 (if (= b-jack "스크류잭") "_스크류잭" "_유압잭")) (cons 62 2) (cons 10 pt-jack-ins) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) (cons 50 jack-rot)))

                      

                      (setq brace_A1 (polar brace_A1 brace-dir jack-len))

                      (setq brace_A2 (polar brace_A2 brace-dir jack-len))

                    )

                  )

                  

                  (if (= jack-side "B")

                    (progn

                      (setq pt-jack-ins (list (/ (+ (car p2_end-B) (car p3_end-B)) 2.0) (/ (+ (cadr p2_end-B) (cadr p3_end-B)) 2.0) 0.0))

                      (setq jack-rot (+ brace-dir pi))

                      (entmake (list '(0 . "INSERT") (cons 2 b-jack) (cons 8 (if (= b-jack "스크류잭") "_스크류잭" "_유압잭")) (cons 62 2) (cons 10 pt-jack-ins) '(41 . 1.0) '(42 . 1.0) '(43 . 1.0) (cons 50 jack-rot)))

                      

                      (setq brace_B1 (polar brace_B1 (+ brace-dir pi) jack-len))

                      (setq brace_B2 (polar brace_B2 (+ brace-dir pi) jack-len))

                    )

                  )



                  (draw-closed-poly (list brace_A1 brace_A2 brace_B2 brace_B1) "_사보강재(Brace)" 3)

                  

                  (setq brace_A1_in (polar brace_A1 brace-dir 14.0))

                  (setq brace_A2_in (polar brace_A2 brace-dir 14.0))

                  (setq brace_B1_in (polar brace_B1 (+ brace-dir pi) 14.0))

                  (setq brace_B2_in (polar brace_B2 (+ brace-dir pi) 14.0))

                  

                  (make-lw-line brace_A1_in brace_A2_in "_사보강재(Brace)" 3)

                  (make-lw-line brace_B1_in brace_B2_in "_사보강재(Brace)" 3)

                  

                  (setq b-tw (nth 2 b-custom))

                  (setq half-tw (/ b-tw 2.0))

                  

                  (setq mid_A_in (list (/ (+ (car brace_A1_in) (car brace_A2_in)) 2.0) (/ (+ (cadr brace_A1_in) (cadr brace_A2_in)) 2.0) 0.0))

                  (setq mid_B_in (list (/ (+ (car brace_B1_in) (car brace_B2_in)) 2.0) (/ (+ (cadr brace_B1_in) (cadr brace_B2_in)) 2.0) 0.0))

                  

                  (setq web1_A (polar mid_A_in (+ brace-dir (/ pi 2.0)) half-tw))

                  (setq web1_B (polar mid_B_in (+ brace-dir (/ pi 2.0)) half-tw))

                  (setq web2_A (polar mid_A_in (- brace-dir (/ pi 2.0)) half-tw))

                  (setq web2_B (polar mid_B_in (- brace-dir (/ pi 2.0)) half-tw))

                  

                  (if (not (tblsearch "LTYPE" "DASHED"))

                    (vl-catch-all-apply 'vla-load (list (vla-get-linetypes (vla-get-activedocument (vlax-get-acad-object))) "DASHED" "acad.lin"))

                  )

                  

                  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")

                                 (cons 8 "_사보강재(Brace)") (cons 62 1) (cons 6 "DASHED") (cons 48 500.0)

                                 '(90 . 2) '(70 . 0)

                                 (list 10 (car web1_A) (cadr web1_A)) (list 10 (car web1_B) (cadr web1_B))))

                  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")

                                 (cons 8 "_사보강재(Brace)") (cons 62 1) (cons 6 "DASHED") (cons 48 500.0)

                                 '(90 . 2) '(70 . 0)

                                 (list 10 (car web2_A) (cadr web2_A)) (list 10 (car web2_B) (cadr web2_B))))

                )

              )

              (setq b-row-idx (1+ b-row-idx)) ;; 루프 마지막에 열수 카운터 증가

            ) ; end foreach b-row

          )

        )

      )

    )

    (setq idx (1+ idx))

  )

  (princ "\n평면도 작도 완료!")

)



;;; --------------------------------------------------------------------------

;;; Function: create-hpile-set-on-boundary

;;; Description: 평면도 생성 (H-Pile / C.I.P 분기 처리 적용)

;;; --------------------------------------------------------------------------

(defun create-hpile-set-on-boundary (boundary-ent boundary-orient / last-ent vertices
                                      seg-data seg-wall-type has-cip has-hpile)

  ;; 1. 기존 그룹 정리 (번호표 포함 모든 객체 삭제)

  (tsp-clear-drawing-group boundary-ent)

  

  ;; 2. 마지막 엔티티 추적 시작

  (setq last-ent (entlast))

  

  ;; 3. 띠장(Wale) 작도 (공통 적용)

  (princ "\n[작도] 띠장(Wale) 생성 중...")

  (create-wale-offsets boundary-ent boundary-orient)

  

  ;; 4. 흙막이벽 작도 - [7단계] 세그먼트별 공법 분기
  ;; 전역 *tsp-wall-type* 하나로 전체 경계선을 그리던 방식에서
  ;; 각 세그먼트의 저장된 WALL-TYPE을 읽어 공법별로 나눠 그리는 방식으로 변경

  ;; 4-1. 세그먼트에 CIP / HPILE 이 섞여있는지 파악
  (setq has-cip nil  has-hpile nil)

  (if *segment-list*
    (foreach seg-data *segment-list*
      (if (cdr (assoc 'IS-DEFINED seg-data))
        (progn
          (setq seg-wall-type (cdr (assoc 'WALL-TYPE seg-data)))
          (if (= seg-wall-type "CIP")
            (setq has-cip T)
            (setq has-hpile T)
          )
        )
      )
    )
  )

  ;; 4-2. H-Pile+토류판 세그먼트가 하나라도 있으면 place-hpile-timber 실행
  ;;      (place-hpile은 내부 루프에서 IS-DEFINED 체크로 세그먼트별 처리)
  (if has-hpile
    (progn
      (princ "\n[작도] H-Pile 및 토류판 생성 중...")
      (place-hpile-timber-along-boundary boundary-ent boundary-orient)
    )
  )

  ;; 4-3. CIP 세그먼트는 하나씩 전역변수 임시 설정 후 작도
  ;;      각 세그먼트의 CIP 파라미터를 전역에 로드 → 해당 세그먼트 범위만 작도
  (if has-cip
    (progn
      (princ "\n[작도] C.I.P(현장타설말뚝) 배열 생성 중...")
      (tsp-draw-cip-plan-per-segment boundary-ent boundary-orient)
    )
  )

  ;; 4-4. 세그먼트가 없거나 모두 미정의 상태이면 전역값으로 폴백 (안전장치)
  (if (and (not has-cip) (not has-hpile))
    (progn
      (if (= *tsp-wall-type* "CIP")
        (progn
          (princ "\n[작도] C.I.P(현장타설말뚝) 배열 생성 중... (전역 폴백)")
          (tsp-draw-cip-plan boundary-ent boundary-orient)
        )
        (progn
          (princ "\n[작도] H-Pile 및 토류판 생성 중... (전역 폴백)")
          (place-hpile-timber-along-boundary boundary-ent boundary-orient)
        )
      )
    )
  )



  ;; 5. 세그먼트 번호표 및 모서리 번호표 재생성

  (if *segment-list*

    (progn

      (setq vertices (mapcar '(lambda (item) (cdr (assoc 'V-START item))) *segment-list*))

      (if (not (is-closed-polyline boundary-ent))

        (setq vertices (append vertices (list (cdr (assoc 'V-END (last *segment-list*))))))

      )

      (draw-segment-numbers boundary-ent boundary-orient vertices)

      (draw-corner-numbers boundary-ent boundary-orient vertices)

    )

  )

  

  ;; 6. 생성된 모든 객체(번호표 포함)를 그룹으로 묶음

  (tsp-group-last-entities last-ent boundary-ent)

  

  ;; 경계선을 객체들의 가장 위로(Front) 보냄

  (command "._DRAWORDER" boundary-ent "" "_F")

  

  (princ "\n평면도 생성 및 갱신 완료.")

)



;;;;==========================================================================

;;;; defun tsp-ceil : 숫자 올림 보조 함수 (LISP 기본 기능 보완)

;;;;==========================================================================

(defun tsp-ceil (val)

  (if (= (type val) 'INT)

    val

    (if (= val (float (fix val)))

      (fix val)

      (1+ (fix val))

    )

  )

)



;;;;==========================================================================

;;;; defun tsp-create-cip-guideline : 외측 방향을 고려한 D/2 오프셋 보조선 생성

;;;;==========================================================================

(defun tsp-create-cip-guideline (boundary-ent dia boundary-orient is-closed / vobj d start-pt p2 seg-angle out-norm target-pt off-obj off-ent off-start)

  (setq vobj (vlax-ename->vla-object boundary-ent))

  ;; dia가 문자열인지 숫자인지 판별하여 안전하게 처리 (에러 방지)

  (setq d (/ (if (= (type dia) 'STR) (atof dia) dia) 2.0))

  

  ;; H-Pile 로직에서 추출한 완벽한 외측 벡터 계산 (안팎 판별 오류 방지)

  (setq start-pt (vlax-curve-getStartPoint vobj))

  (setq p2 (vlax-curve-getPointAtParam vobj 1.0))

  (setq seg-angle (angle start-pt p2))

  (setq out-norm (+ seg-angle (* boundary-orient (/ pi 2.0))))

  (setq target-pt (polar start-pt out-norm d))

  

  ;; 양수 거리로 오프셋 후, 수학적으로 계산된 target-pt와 일치하는지 교차 검증

  (setq off-obj (vl-catch-all-apply 'vla-offset (list vobj d)))

  (if (not (vl-catch-all-error-p off-obj))

    (progn

      (setq off-ent (vlax-vla-object->ename (vlax-safearray-get-element (vlax-variant-value off-obj) 0)))

      (setq off-start (vlax-curve-getStartPoint (vlax-ename->vla-object off-ent)))

      

      ;; 방향이 반대로 튀었을 경우 (오차 허용 0.1) 삭제 후 음수로 재오프셋

      (if (> (distance (list (car target-pt) (cadr target-pt)) (list (car off-start) (cadr off-start))) 0.1)

        (progn

          (entdel off-ent)

          (setq off-obj (vl-catch-all-apply 'vla-offset (list vobj (- d))))

          (if (not (vl-catch-all-error-p off-obj))

            (setq off-ent (vlax-vla-object->ename (vlax-safearray-get-element (vlax-variant-value off-obj) 0)))

            (setq off-ent nil)

          )

        )

      )

      off-ent

    )

    nil

  )

)



;;;;==========================================================================

;;;; defun tsp-calc-cip-layout : C.I.P 배열 중심점 및 간격 계산 (양끝 고정, 내부 균등 분배)

;;;;==========================================================================

(defun tsp-calc-cip-layout (curve-ent is-closed dia mode overlap / total-length target-pitch usable count actual-pitch i dist pt ang deriv centers)

  (setq total-length (vlax-curve-getDistAtParam curve-ent (vlax-curve-getEndParam curve-ent)))

  (setq target-pitch (if (= mode "0") dia (- dia overlap))) ; 0: 맞닿음(Tangent), 1: 겹침(Secant)

  (setq centers '())

  

  (if is-closed

    (progn ;; 닫힌 경계 (Closed boundary)

      (setq count (tsp-ceil (/ total-length target-pitch)))

      (setq actual-pitch (/ total-length count))

      (setq i 0)

      (while (< i count)

        (setq dist (* i actual-pitch))

        (setq pt (vlax-curve-getPointAtDist curve-ent dist))

        (setq deriv (vlax-curve-getFirstDeriv curve-ent (vlax-curve-getParamAtDist curve-ent dist)))

        (setq ang (angle '(0 0 0) deriv))

        (setq centers (append centers (list (list i dist pt ang))))

        (setq i (1+ i))

      )

    )

    (progn ;; 열린 경계 (Open boundary)

      (setq usable (- total-length dia))

      (if (<= usable 0) (setq usable 0.1)) ; 예외 안전장치

      (setq count (tsp-ceil (/ usable target-pitch)))

      (if (<= count 0) (setq count 1))

      (setq actual-pitch (/ usable count))

      (setq i 0)

      (while (<= i count)

        (setq dist (+ (/ dia 2.0) (* i actual-pitch))) ; 양끝 D/2 보정 적용

        (setq pt (vlax-curve-getPointAtDist curve-ent dist))

        (setq deriv (vlax-curve-getFirstDeriv curve-ent (vlax-curve-getParamAtDist curve-ent dist)))

        (setq ang (angle '(0 0 0) deriv))

        (setq centers (append centers (list (list i dist pt ang))))

        (setq i (1+ i))

      )

    )

  )

  (list (cons 'MODE (if is-closed "CLOSED" "OPEN"))

        (cons 'COUNT count)

        (cons 'ACTUAL-PITCH actual-pitch)

        (cons 'ITEMS centers)

  )

)



;;;;==========================================================================

;;;; defun tsp-draw-cip-elements : 배열 데이터를 바탕으로 도면에 실제 엔티티(원, H-Pile) 생성

;;;;==========================================================================

(defun tsp-draw-cip-elements (layout dia interval-idx hpile-idx boundary-orient / interval items hpile-spec-name hpile-vals h b tw tf hpile-block idx dist pt ang outward-normal hpile-rotation insert-pt half-h-offset)

  (setq interval (cond ((= interval-idx "1") 2) ((= interval-idx "2") 3) (t 1)))

  (setq items (cdr (assoc 'ITEMS layout)))

  

  ;; 기존 *tsp-std-wall-list* 완벽 연동

  (setq hpile-spec-name (nth (atoi hpile-idx) *tsp-std-wall-list*))

  (setq hpile-vals (parse-h-spec hpile-spec-name))

  (setq h (nth 0 hpile-vals) b (nth 1 hpile-vals) tw (nth 2 hpile-vals) tf (nth 3 hpile-vals))

  (setq hpile-block (create-hpile-section-block h b tw tf))

  

  (create-layer-if-not-exists "_CIP원형" "8") ;; 외곽선 회색 지정

  

  (foreach item items

    (setq idx (nth 0 item) dist (nth 1 item) pt (nth 2 item) ang (nth 3 item))

    

    ;; 1. C.I.P 원형 외곽선 생성

    (entmake (list '(0 . "CIRCLE") (cons 8 "_CIP원형") (list 10 (car pt) (cadr pt) 0.0) (cons 40 (/ dia 2.0))))

    

    ;; 2. H-Pile 생성 (간격 모듈러 연산 적용)

    (if (= (rem idx interval) 0)

      (progn

         ;; 접선 각도(ang)에서 외측 벡터를 도출 후 90도 회전(H-Pile 웹 방향 세팅)

         (setq outward-normal (+ ang (* boundary-orient (/ pi 2.0))))

         (setq hpile-rotation (+ outward-normal (/ pi 2.0)))

         ;; [③ 수정] 블록 기준점(0, -half_h)이 플렌지 바닥이므로
         ;; H-Pile 단면 중심을 CIP 원 중심 pt에 맞추려면
         ;; INSERT 점을 outward-normal 방향으로 -(h/2) 이동
         (setq half-h-offset (/ h 2.0))
         (setq insert-pt (polar pt outward-normal (- half-h-offset)))

         (entmake (list '(0 . "INSERT") (cons 2 hpile-block) '(8 . "_측면말뚝")
                        (list 10 (car insert-pt) (cadr insert-pt) 0.0)
                        '(41 . 1.0) '(42 . 1.0) '(43 . 1.0)
                        (cons 50 hpile-rotation)))

      )

    )

  )

)



;;;;==========================================================================

;;;; defun tsp-draw-cip-plan-per-segment : 세그먼트별 CIP 작도 (공법 혼용 지원)
;;;; CIP로 설정된 세그먼트만 골라 각 세그먼트의 저장 파라미터로 개별 작도

;;;;==========================================================================

(defun tsp-draw-cip-plan-per-segment (boundary-ent boundary-orient
                                      / is-closed seg-data seg-idx seg-wall-type
                                        orig-vertices n-verts max-segs
                                        pt1 pt2 seg-guide layout
                                        save-dia save-mode save-overlap
                                        save-interval save-hpile-idx)

  (setq is-closed (is-closed-polyline boundary-ent))

  (setq orig-vertices (mapcar '(lambda (s) (cdr (assoc 'V-START s))) *segment-list*))

  (if (not is-closed)
    (setq orig-vertices (append orig-vertices (list (cdr (assoc 'V-END (last *segment-list*))))))
  )

  (setq n-verts (length orig-vertices))

  (setq max-segs (if is-closed n-verts (1- n-verts)))

  (setq seg-idx 0)

  (while (< seg-idx max-segs)

    (setq seg-data (nth seg-idx *segment-list*))

    (setq seg-wall-type (cdr (assoc 'WALL-TYPE seg-data)))

    ;; CIP 세그먼트이고 IS-DEFINED인 경우에만 작도
    (if (and (= seg-wall-type "CIP")
             (cdr (assoc 'IS-DEFINED seg-data)))

      (progn

        ;; 해당 세그먼트의 CIP 파라미터를 전역변수에 임시 로드
        (setq save-dia          *tsp-cip-dia*)
        (setq save-mode         *tsp-cip-mode-idx*)
        (setq save-overlap      *tsp-cip-overlap*)
        (setq save-interval     *tsp-cip-interval-idx*)
        (setq save-hpile-idx    *tsp-cip-hpile-idx*)

        (setq *tsp-cip-dia*          (cdr (assoc 'CIP-DIA          seg-data)))
        (setq *tsp-cip-mode-idx*     (cdr (assoc 'CIP-MODE-IDX     seg-data)))
        (setq *tsp-cip-overlap*      (cdr (assoc 'CIP-OVERLAP      seg-data)))
        (setq *tsp-cip-interval-idx* (cdr (assoc 'CIP-INTERVAL-IDX seg-data)))
        (setq *tsp-cip-hpile-idx*    (cdr (assoc 'CIP-HPILE-IDX    seg-data)))

        ;; 해당 세그먼트 구간만 임시 폴리선 생성 후 CIP 작도
        (setq pt1 (nth seg-idx orig-vertices))
        (setq pt2 (nth (if is-closed (rem (+ seg-idx 1) n-verts) (+ seg-idx 1)) orig-vertices))

        (setq seg-guide (tsp-make-segment-guideline pt1 pt2
                          (atof *tsp-cip-dia*) boundary-orient))

        (if seg-guide

          (progn

            (setq layout (tsp-calc-cip-layout seg-guide nil
                           (atof *tsp-cip-dia*)
                           *tsp-cip-mode-idx*
                           (atof *tsp-cip-overlap*)))

            (tsp-draw-cip-elements layout
                                   (atof *tsp-cip-dia*)
                                   *tsp-cip-interval-idx*
                                   *tsp-cip-hpile-idx*
                                   boundary-orient)

            (entdel seg-guide)

          )

          (princ (strcat "\n[경고] 세그먼트 " (itoa (1+ seg-idx)) " CIP 보조선 생성 실패 - 건너뜀"))

        )

        ;; 전역변수 원상복귀
        (setq *tsp-cip-dia*          save-dia)
        (setq *tsp-cip-mode-idx*     save-mode)
        (setq *tsp-cip-overlap*      save-overlap)
        (setq *tsp-cip-interval-idx* save-interval)
        (setq *tsp-cip-hpile-idx*    save-hpile-idx)

      )

    )

    (setq seg-idx (1+ seg-idx))

  )

)



;;;;==========================================================================

;;;; defun tsp-make-segment-guideline : 두 점으로 세그먼트 구간 보조선 생성

;;;;==========================================================================

(defun tsp-make-segment-guideline (pt1 pt2 dia boundary-orient
                                   / seg-angle out-norm target-pt guide-ent d
                                     off-ent off-start offset-pt)

  (setq d (/ dia 2.0))

  (setq seg-angle (angle pt1 pt2))

  (setq out-norm (+ seg-angle (* boundary-orient (/ pi 2.0))))

  ;; 수학적으로 외측 위치 계산 (tsp-create-cip-guideline과 동일 방식)
  (setq target-pt (polar pt1 out-norm d))

  ;; pt1 → pt2 임시 직선 폴리선 생성
  (entmake (list '(0 . "LWPOLYLINE")
                 '(100 . "AcDbEntity")
                 '(100 . "AcDbPolyline")
                 '(8 . "0") '(62 . 256) '(90 . 2) '(70 . 0)
                 (list 10 (car pt1) (cadr pt1))
                 (list 10 (car pt2) (cadr pt2))))

  (setq guide-ent (entlast))

  ;; [수정] 오프셋 후 방향 검증 (tsp-create-cip-guideline 외측 재검증 로직 이식)
  ;; 1차 시도: target-pt 방향으로 OFFSET
  (setq offset-pt (list (car target-pt) (cadr target-pt) 0.0))
  (command "._OFFSET" d guide-ent offset-pt "")
  (setq off-ent (entlast))

  ;; off-ent가 guide-ent와 동일하면 OFFSET 실패
  (if (equal off-ent guide-ent)
    (setq off-ent nil)
    (progn
      ;; 오프셋된 선의 시작점 추출
      (setq off-start (cdr (assoc 10 (entget off-ent))))

      ;; target-pt와 거리가 0.1 초과이면 방향이 반대로 튄 것 → 삭제 후 반대 방향 재오프셋
      (if (> (distance (list (car target-pt) (cadr target-pt))
                       (list (car off-start) (cadr off-start)))
             0.1)
        (progn
          (entdel off-ent)
          ;; 반대 방향 OFFSET: target-pt의 반대편 점 사용
          (setq offset-pt (list (car (polar pt1 (+ out-norm pi) d))
                                (cadr (polar pt1 (+ out-norm pi) d))
                                0.0))
          (command "._OFFSET" d guide-ent offset-pt "")
          (setq off-ent (entlast))
          (if (equal off-ent guide-ent) (setq off-ent nil))
        )
      )
    )
  )

  (entdel guide-ent)

  off-ent

)



;;;;==========================================================================

;;;; defun tsp-draw-cip-plan : C.I.P 평면도 작도 메인 제어 흐름

;;;;==========================================================================

(defun tsp-draw-cip-plan (boundary-ent boundary-orient / is-closed guide-ent layout)

  (setq is-closed (is-closed-polyline boundary-ent))

  

  ;; STEP 1. 보조선 오프셋

  (setq guide-ent (tsp-create-cip-guideline boundary-ent (atof *tsp-cip-dia*) boundary-orient is-closed))

  

  (if guide-ent

    (progn

      ;; STEP 2. 중심점 배열 계산

      (setq layout (tsp-calc-cip-layout guide-ent is-closed (atof *tsp-cip-dia*) *tsp-cip-mode-idx* (atof *tsp-cip-overlap*)))

      

      ;; STEP 3. 도면 작도

      (tsp-draw-cip-elements layout (atof *tsp-cip-dia*) *tsp-cip-interval-idx* *tsp-cip-hpile-idx* boundary-orient)

      

      ;; 임시 보조선 흔적 없이 삭제

      (entdel guide-ent)

    )

    (princ "\n[에러] C.I.P 작도를 위한 보조선 생성에 실패했습니다. 경계선의 곡률이 직경보다 작거나 형상이 꼬여있는지 확인하세요.")

  )

)



;;; --------------------------------------------------------------------------

;;; Function: draw-segment-numbers

;;; Description: 정렬된 vertices를 인자로 받아 세그먼트 번호 표기

;;; --------------------------------------------------------------------------

(defun draw-segment-numbers (boundary-ent boundary-orient vertices / is-closed num-verts max-segments seg-idx pt1 pt2 mid-pt seg-angle inward-angle text-pt text-h offset-dist layer-name circle-rad current-scale seg-data)

  (setq current-scale (if (and *tsp-scale-plan* (> *tsp-scale-plan* 0)) *tsp-scale-plan* 600.0))

  (princ (strcat "\n[INFO] 현재 적용 스케일: 1/" (rtos current-scale 2 0)))



  (setq text-h (* current-scale 2.5))

  (setq offset-dist (* current-scale 2.5))

  (setq circle-rad (* text-h 0.8))

  (setq layer-name "_번호")

  (create-layer-if-not-exists layer-name "2")



  (setq is-closed (is-closed-polyline boundary-ent))

  (setq num-verts (length vertices))

  (setq max-segments (if is-closed num-verts (- num-verts 1)))

  

  (setq seg-idx 0)



  (while (< seg-idx max-segments)

    (setq seg-data (nth seg-idx *segment-list*))

    

    ;; 세그먼트에 데이터가 입력된 경우에만 번호 작도

    (if (cdr (assoc 'IS-DEFINED seg-data))

      (progn

        (setq pt1 (nth seg-idx vertices))

        (setq pt2 (nth (if is-closed (rem (+ seg-idx 1) num-verts) (+ seg-idx 1)) vertices))

        

        (setq mid-pt (list (/ (+ (car pt1) (car pt2)) 2.0) (/ (+ (cadr pt1) (cadr pt2)) 2.0) 0.0))

        (setq seg-angle (angle pt1 pt2))

        

        (setq inward-angle (+ seg-angle (* boundary-orient (/ pi 2.0))))

        (setq text-pt (polar mid-pt inward-angle offset-dist))

        

        ;; 1. 텍스트 객체 생성 및 투명도 60 적용

        (entmake (list '(0 . "TEXT") (cons 8 layer-name) (cons 62 2) 

                       (cons 10 text-pt) (cons 40 text-h) 

                       ;; 강제 보정 없이 클릭 순서 그대로 1번부터 출력

                       (cons 1 (itoa (1+ seg-idx))) 

                       '(72 . 4) (cons 11 text-pt)))

        (vla-put-EntityTransparency (vlax-ename->vla-object (entlast)) "60")

        

        ;; 2. 원(Circle) 객체 생성 및 투명도 60 적용

        (entmake (list '(0 . "CIRCLE") (cons 8 layer-name) (cons 62 2) 

                       (cons 10 text-pt) (cons 40 circle-rad)))

        (vla-put-EntityTransparency (vlax-ename->vla-object (entlast)) "60")

      )

    )

    (setq seg-idx (1+ seg-idx))

  )

)



;;; --------------------------------------------------------------------------

;;; Function: draw-corner-numbers

;;; Description: 모서리 지시선 작도

;;; --------------------------------------------------------------------------

(defun draw-corner-numbers (boundary-ent boundary-orient vertices / is-closed num-verts i v-prev v-curr v-next p1-s p1-e p2-s p2-e n1 n2 offset-pt angle-dir line-start line1-end cap-center cap-top cap-bot line2-end circle-center text-h-num text-h-cnt layer-name text-val current-scale s-factor cap-w-half cap-h-half cap-rad angle-perp offset-dist line1-len line2-len circle-rad text-angle is-convex base-data final-data wedge-len wedge-half-ang cur-item prev-idx next-idx prev-item next-item ang-to-prev ang-to-next diff orig-dist cumulative-counts current-count p-seg-idx c-seg-idx p-def c-def is-active max-segments prev-active next-active is-open-end can-rotate seg-mid-count total-count cur-cum-count fully-closed active-flags mid-line-len cap1-bot cap1-center cap1-top cap2-bot cap2-center cap2-top draw-capsule-shape check-wedge-collision evade-angles orig-angle-dir test-idx evade-sign)

  

  (defun draw-capsule-shape (center-pt ang-dir ang-perp h-half w-half rad l-name col / p-c1 p-c2 p-c3 p-c4 p-c5 p-c6 p-c7 p-c8)

    (setq p-c1 (polar (polar center-pt ang-perp (- w-half rad)) ang-dir h-half))

    (setq p-c2 (polar (polar center-pt ang-perp w-half) ang-dir (- h-half rad)))

    (setq p-c3 (polar (polar center-pt ang-perp w-half) (+ ang-dir pi) (- h-half rad)))

    (setq p-c4 (polar (polar center-pt ang-perp (- w-half rad)) (+ ang-dir pi) h-half))

    (setq p-c5 (polar (polar center-pt (- ang-perp pi) (- w-half rad)) (+ ang-dir pi) h-half))

    (setq p-c6 (polar (polar center-pt (- ang-perp pi) w-half) (+ ang-dir pi) (- h-half rad)))

    (setq p-c7 (polar (polar center-pt (- ang-perp pi) w-half) ang-dir (- h-half rad)))

    (setq p-c8 (polar (polar center-pt (- ang-perp pi) (- w-half rad)) ang-dir h-half))

    

    (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline")

                   (cons 8 l-name) (cons 62 col) '(90 . 8) '(70 . 1)

                   (cons 10 p-c1) '(42 . 0.41421356) (cons 10 p-c2) '(42 . 0.0)

                   (cons 10 p-c3) '(42 . 0.41421356) (cons 10 p-c4) '(42 . 0.0)

                   (cons 10 p-c5) '(42 . 0.41421356) (cons 10 p-c6) '(42 . 0.0)

                   (cons 10 p-c7) '(42 . 0.41421356) (cons 10 p-c8) '(42 . 0.0)))

  )



  (defun check-wedge-collision (pt1 ang1 pt2 ang2 len half-ang s-factor / p1-L p1-R p2-L p2-R p1-C p2-C res C1 C2 dist-limit)

    (setq p1-L (polar pt1 (+ ang1 half-ang) len)) 

    (setq p1-R (polar pt1 (- ang1 half-ang) len))

    (setq p2-L (polar pt2 (+ ang2 half-ang) len)) 

    (setq p2-R (polar pt2 (- ang2 half-ang) len))

    (setq p1-C (polar pt1 ang1 len)) 

    (setq p2-C (polar pt2 ang2 len))

    

    (setq res nil)

    (if (inters pt1 p1-L pt2 p2-R) (setq res T)) 

    (if (inters pt1 p1-R pt2 p2-L) (setq res T))

    (if (inters pt1 p1-L pt2 p2-L) (setq res T)) 

    (if (inters pt1 p1-R pt2 p2-R) (setq res T))

    (if (inters pt1 p1-C pt2 p2-C) (setq res T))

    

    (setq C1 (polar pt1 ang1 (* 4000.0 s-factor))) 

    (setq C2 (polar pt2 ang2 (* 4000.0 s-factor)))

    (setq dist-limit (* 4500.0 s-factor)) 

    

    (if (< (distance C1 C2) dist-limit) (setq res T))

    res

  )



  (setq current-scale (if (and *tsp-scale-plan* (> *tsp-scale-plan* 0)) *tsp-scale-plan* 600.0))

  (setq s-factor (/ current-scale 600.0)) 

  (setq layer-name "_지시")

  (create-layer-if-not-exists layer-name "2")



  (setq offset-dist (* 1000.0 s-factor))

  (setq line1-len (* 1300.0 s-factor))

  (setq cap-h-half (* 600.0 s-factor))

  (setq cap-w-half (* 1195.0 s-factor))

  (setq cap-rad (* 480.0 s-factor))

  (setq line2-len (* 1300.0 s-factor))

  (setq circle-rad (* 1200.0 s-factor))

  (setq text-h-num (* 840.0 s-factor))

  (setq text-h-cnt (* 720.0 s-factor))

  (setq wedge-len (* 7614.0 s-factor))

  (setq wedge-half-ang 0.245) 



  (setq is-closed (is-closed-polyline boundary-ent))

  (setq num-verts (length vertices))

  (setq max-segments (if is-closed num-verts (- num-verts 1)))



  (setq fully-closed is-closed)

  (if is-closed

    (foreach seg *segment-list*

      (if (not (cdr (assoc 'IS-DEFINED seg))) (setq fully-closed nil))

    )

  )



  (setq cumulative-counts '())

  (setq current-count 0)

  (setq active-flags '())

  (setq i 0)

  (while (< i num-verts)

    (setq p-seg-idx (if (= i 0) (if is-closed (1- num-verts) -1) (1- i)))

    (setq c-seg-idx (if (>= i max-segments) -1 i))

    

    (setq p-def (if (>= p-seg-idx 0) (cdr (assoc 'IS-DEFINED (nth p-seg-idx *segment-list*))) nil))

    (setq c-def (if (>= c-seg-idx 0) (cdr (assoc 'IS-DEFINED (nth c-seg-idx *segment-list*))) nil))

    

    (setq is-active (or p-def c-def))

    (setq active-flags (append active-flags (list (cons i is-active))))

    

    (if is-active

      (progn

        (setq current-count (1+ current-count))

        (setq cumulative-counts (append cumulative-counts (list (cons i current-count))))

        (if c-def

          (progn

            (setq seg-mid-count (nth c-seg-idx *tsp-segment-pile-counts*))

            (if (null seg-mid-count) (setq seg-mid-count 0))

            (setq current-count (+ current-count seg-mid-count))

          )

        )

      )

    )

    (setq i (1+ i))

  )

  (setq total-count current-count)



  (setq base-data '())

  (setq i 0)

  (while (< i num-verts)

    (setq v-curr (nth i vertices))

    (if is-closed

      (progn (setq v-prev (nth (if (= i 0) (1- num-verts) (1- i)) vertices)) (setq v-next (nth (if (= i (1- num-verts)) 0 (1+ i)) vertices)))

      (progn (setq v-prev (if (= i 0) nil (nth (1- i) vertices))) (setq v-next (if (= i (1- num-verts)) nil (nth (1+ i) vertices))))

    )



    (setq is-convex nil)

    (if (and v-prev v-next) (setq is-convex (is-corner-convex v-prev v-curr v-next boundary-orient)))

    (setq is-open-end (not (and v-prev v-next)))



    (setq offset-pt nil)

    (if (and v-prev v-next)

      (progn

        (setq n1 (- (angle v-prev v-curr) (* boundary-orient (/ pi 2.0))))

        (setq n2 (- (angle v-curr v-next) (* boundary-orient (/ pi 2.0))))

        (setq p1-s (polar v-prev n1 offset-dist)) (setq p1-e (polar v-curr n1 offset-dist))

        (setq p2-s (polar v-curr n2 offset-dist)) (setq p2-e (polar v-next n2 offset-dist))

        (setq offset-pt (inters p1-s p1-e p2-s p2-e nil))

        (if (null offset-pt) (setq offset-pt (polar v-curr n1 offset-dist))) 

      )

      (if v-next

        (progn (setq n2 (- (angle v-curr v-next) (* boundary-orient (/ pi 2.0)))) (setq offset-pt (polar v-curr n2 offset-dist)))

        (progn (setq n1 (- (angle v-prev v-curr) (* boundary-orient (/ pi 2.0)))) (setq offset-pt (polar v-curr n1 offset-dist)))

      )

    )

    (setq angle-dir (angle v-curr offset-pt))

    (setq is-active (cdr (assoc i active-flags)))

    (setq base-data (append base-data (list (list offset-pt angle-dir is-convex v-curr is-active is-open-end))))

    (setq i (1+ i))

  )



  (setq final-data '())

  (setq i 0)

  (while (< i num-verts)

    (setq cur-item (nth i base-data))

    (setq offset-pt (car cur-item)) (setq angle-dir (cadr cur-item)) 

    (setq is-convex (caddr cur-item)) (setq v-curr (cadddr cur-item)) 

    (setq is-active (nth 4 cur-item)) (setq is-open-end (nth 5 cur-item))

    

    (setq can-rotate (or is-convex is-open-end))

    

    (if (and is-active can-rotate)

      (progn

        (setq orig-dist (distance v-curr offset-pt))

        (setq prev-idx (if (= i 0) (1- num-verts) (1- i)))

        

        ;; [핵심] 현규 님의 다이나믹 회피 각도 리스트 (45 -> 90 -> 30 -> 60 -> 15 -> 75)

        (setq evade-angles (list (/ pi 4.0) (/ pi 2.0) (/ pi 6.0) (/ pi 3.0) (/ pi 12.0) (* 5.0 (/ pi 12.0))))

        

        ;; 이전 모서리 검사

        (if (or is-closed (and (> i 0) (not is-closed)))

          (progn

            (setq prev-item (nth prev-idx base-data))

            (setq prev-active (nth 4 prev-item))

            (setq orig-angle-dir angle-dir)

            (setq test-idx 0)

            (while (and prev-active (< test-idx (length evade-angles)) (check-wedge-collision offset-pt angle-dir (car prev-item) (cadr prev-item) wedge-len wedge-half-ang s-factor))

              (setq ang-to-prev (angle offset-pt (car prev-item)))

              (setq diff (- ang-to-prev orig-angle-dir))

              (while (> diff pi) (setq diff (- diff (* 2.0 pi)))) 

              (while (< diff (- pi)) (setq diff (+ diff (* 2.0 pi))))

              ;; 상대방의 반대 방향으로 회피 부호 결정

              (setq evade-sign (if (> diff 0) -1.0 1.0))

              (setq angle-dir (+ orig-angle-dir (* evade-sign (nth test-idx evade-angles))))

              (setq offset-pt (polar v-curr angle-dir orig-dist))

              (setq test-idx (1+ test-idx))

            )

          )

        )

        

        (setq next-idx (if (= i (1- num-verts)) 0 (1+ i)))

        ;; 다음 모서리 검사

        (if (or is-closed (and (< i (1- num-verts)) (not is-closed)))

          (progn

            (setq next-item (nth next-idx base-data))

            (setq next-active (nth 4 next-item))

            (setq orig-angle-dir angle-dir)

            (setq test-idx 0)

            (while (and next-active (< test-idx (length evade-angles)) (check-wedge-collision offset-pt angle-dir (car next-item) (cadr next-item) wedge-len wedge-half-ang s-factor))

              (setq ang-to-next (angle offset-pt (car next-item)))

              (setq diff (- ang-to-next orig-angle-dir))

              (while (> diff pi) (setq diff (- diff (* 2.0 pi)))) 

              (while (< diff (- pi)) (setq diff (+ diff (* 2.0 pi))))

              (setq evade-sign (if (> diff 0) -1.0 1.0))

              (setq angle-dir (+ orig-angle-dir (* evade-sign (nth test-idx evade-angles))))

              (setq offset-pt (polar v-curr angle-dir orig-dist))

              (setq test-idx (1+ test-idx))

            )

          )

        )

      )

    )

    (setq final-data (append final-data (list (list offset-pt angle-dir v-curr is-active))))

    (setq i (1+ i))

  )



  (setq i 0)

  (while (< i num-verts)

    (setq cur-item (nth i final-data))

    (setq line-start (car cur-item)) (setq angle-dir (cadr cur-item)) 

    (setq v-curr (caddr cur-item)) (setq is-active (nth 3 cur-item))

    

    (if is-active

      (progn

        (setq angle-perp (+ angle-dir (/ pi 2.0)))

        (setq text-angle (- angle-dir (/ pi 2.0)))

        

        (setq text-val (1+ i))

        (setq cur-cum-count (cdr (assoc i cumulative-counts)))



        (if (and fully-closed (= i 0))

          (progn

            (setq mid-line-len (* 600.0 s-factor))

            (setq cap1-bot (polar line-start angle-dir line1-len))

            (setq cap1-center (polar cap1-bot angle-dir cap-h-half))

            (setq cap1-top (polar cap1-center angle-dir cap-h-half))

            (setq cap2-bot (polar cap1-top angle-dir mid-line-len))

            (setq cap2-center (polar cap2-bot angle-dir cap-h-half))

            (setq cap2-top (polar cap2-center angle-dir cap-h-half))

            (setq line2-end (polar cap2-top angle-dir line2-len))

            (setq circle-center (polar line2-end angle-dir circle-rad))

            

            (entmake (list '(0 . "LINE") (cons 8 layer-name) (cons 62 1) (cons 10 line-start) (cons 11 cap1-bot)))

            (entmake (list '(0 . "LINE") (cons 8 layer-name) (cons 62 1) (cons 10 cap1-top) (cons 11 cap2-bot)))

            (entmake (list '(0 . "LINE") (cons 8 layer-name) (cons 62 2) (cons 10 cap2-top) (cons 11 line2-end)))

            

            (draw-capsule-shape cap1-center angle-dir angle-perp cap-h-half cap-w-half cap-rad layer-name 1)

            (draw-capsule-shape cap2-center angle-dir angle-perp cap-h-half cap-w-half cap-rad layer-name 1)

            (entmake (list '(0 . "CIRCLE") (cons 8 layer-name) (cons 62 2) (cons 10 circle-center) (cons 40 circle-rad)))

            

            (entmake (list '(0 . "TEXT") (cons 8 layer-name) (cons 62 7) (cons 10 cap1-center) (cons 40 text-h-cnt) (cons 50 text-angle) (cons 1 (itoa total-count)) '(72 . 4) (cons 11 cap1-center)))

            (entmake (list '(0 . "TEXT") (cons 8 layer-name) (cons 62 7) (cons 10 cap2-center) (cons 40 text-h-cnt) (cons 50 text-angle) (cons 1 (itoa cur-cum-count)) '(72 . 4) (cons 11 cap2-center)))

            (entmake (list '(0 . "TEXT") (cons 8 layer-name) (cons 62 7) (cons 10 circle-center) (cons 40 text-h-num) (cons 50 text-angle) (cons 1 (itoa text-val)) '(72 . 4) (cons 11 circle-center)))

          )

          (progn

            (setq cap-bot (polar line-start angle-dir line1-len))

            (setq cap-center (polar cap-bot angle-dir cap-h-half))

            (setq cap-top (polar cap-center angle-dir cap-h-half))

            (setq line2-end (polar cap-top angle-dir line2-len))

            (setq circle-center (polar line2-end angle-dir circle-rad))

            

            (entmake (list '(0 . "LINE") (cons 8 layer-name) (cons 62 1) (cons 10 line-start) (cons 11 cap-bot)))

            (entmake (list '(0 . "LINE") (cons 8 layer-name) (cons 62 2) (cons 10 cap-top) (cons 11 line2-end)))

            

            (draw-capsule-shape cap-center angle-dir angle-perp cap-h-half cap-w-half cap-rad layer-name 1)

            (entmake (list '(0 . "CIRCLE") (cons 8 layer-name) (cons 62 2) (cons 10 circle-center) (cons 40 circle-rad)))

            

            (entmake (list '(0 . "TEXT") (cons 8 layer-name) (cons 62 7) (cons 10 cap-center) (cons 40 text-h-cnt) (cons 50 text-angle) (cons 1 (itoa cur-cum-count)) '(72 . 4) (cons 11 cap-center)))

            (entmake (list '(0 . "TEXT") (cons 8 layer-name) (cons 62 7) (cons 10 circle-center) (cons 40 text-h-num) (cons 50 text-angle) (cons 1 (itoa text-val)) '(72 . 4) (cons 11 circle-center)))

          )

        )

      )

    )

    (setq i (1+ i))

  )

)



;;; --------------------------------------------------------------------------

;;; Function: tsp-calculate-brace-limit

;;; Description: 사보강재 한계거리(절대/권장) 산출

;;; --------------------------------------------------------------------------

(defun tsp-calculate-brace-limit (idx / is-closed num-segs prev-idx next-idx curr-seg prev-seg next-seg L1 L2 curr-brace prev-brace next-brace prev-used next-used margin1 margin2 avail1 avail2 hard-limit soft-limit curr-b prev-b next-b s-limit1 s-limit2)

  (setq num-segs (length *segment-list*))

  (setq curr-seg (nth idx *segment-list*))

  (setq L2 (/ (cdr (assoc 'LENGTH curr-seg)) 1000.0))

  

  ;; 현재 모서리(idx)의 사보강재 폭(B) 추출 (없으면 기본 300)

  (setq curr-brace (cdr (assoc 'CORNER-BRACE curr-seg)))

  (setq curr-b 300.0)

  (if (and curr-brace (= (car curr-brace) "Y") (cadr curr-brace))

    (setq curr-b (float (nth 1 (cadr (last (cadr curr-brace))))))

  )



  (if (and *tsp-boundary-ent* (is-closed-polyline *tsp-boundary-ent*))

    (setq is-closed T)

    (setq is-closed nil)

  )



  (setq prev-idx (if (= idx 0) (if is-closed (1- num-segs) -1) (1- idx)))

  (setq next-idx (if (= idx (1- num-segs)) (if is-closed 0 -1) (1+ idx)))



  ;; 1. 이전 선분(L1) 가용성 검사

  (if (>= prev-idx 0)

    (progn

      (setq prev-seg (nth prev-idx *segment-list*))

      (setq L1 (/ (cdr (assoc 'LENGTH prev-seg)) 1000.0))

      (setq prev-brace (cdr (assoc 'CORNER-BRACE prev-seg)))

      (setq prev-b 300.0 prev-used 0.0)

      

      (if (and prev-brace (= (car prev-brace) "Y"))

        (if (cadr prev-brace)

          (progn

            (setq prev-used (nth 2 (last (cadr prev-brace))))

            (setq prev-b (float (nth 1 (cadr (last (cadr prev-brace))))))

          )

          ;; 미입력 상태: 무조건 최소 1.0m만 예약 (유연한 배분)

          (setq prev-used 1.0)

        )

      )

      ;; [동적 마진 계산] (나의 B/2 + 이웃의 B/2) * 1.414(45도 빗변) + 화타쐐기 여유 150mm

      (setq margin1 (+ (* (+ (/ curr-b 2000.0) (/ prev-b 2000.0)) 1.414) 0.15))

      (setq avail1 (- L1 prev-used margin1))

    )

    (progn (setq L1 999.0) (setq avail1 999.0) (setq prev-brace nil))

  )



  ;; 2. 현재 선분(L2) 가용성 검사

  (if (>= next-idx 0)

    (progn

      (setq next-seg (nth next-idx *segment-list*))

      (setq next-brace (cdr (assoc 'CORNER-BRACE next-seg)))

      (setq next-b 300.0 next-used 0.0)

      

      (if (and next-brace (= (car next-brace) "Y"))

        (if (cadr next-brace)

          (progn

            (setq next-used (nth 2 (last (cadr next-brace))))

            (setq next-b (float (nth 1 (cadr (last (cadr next-brace))))))

          )

          ;; 미입력 상태: 최소 1.0m 예약

          (setq next-used 1.0)

        )

      )

      (setq margin2 (+ (* (+ (/ curr-b 2000.0) (/ next-b 2000.0)) 1.414) 0.15))

      (setq avail2 (- L2 next-used margin2))

    )

    (progn (setq L2 999.0) (setq avail2 999.0) (setq next-brace nil))

  )



  ;; 3. 최종 이중 한계치 도출 (List 형태로 반환)

  (setq hard-limit (min avail1 avail2))

  (if (< hard-limit 0.0) (setq hard-limit 0.0))

  

  ;; 이웃 모서리에 사보강재가 없을 경우 50% 제한을 해제하고 해당 방향의 물리적 한계치 적용

  (setq s-limit1 (if (and prev-brace (= (car prev-brace) "Y")) (/ L1 2.0) avail1))

  (setq s-limit2 (if (and next-brace (= (car next-brace) "Y")) (/ L2 2.0) avail2))

  

  (setq soft-limit (min s-limit1 s-limit2))

  ;; 만약 권장 한계(soft-limit)가 계산상 절대 한계(hard-limit)보다 커진다면 절대 한계로 하향 고정

  (if (> soft-limit hard-limit) (setq soft-limit hard-limit))

  

  (list hard-limit soft-limit)

)



;;; ==========================================================================

;;; [SECTION 6] DCL UI 및 콜백 (Dialog Interface)

;;; ==========================================================================



;;; --------------------------------------------------------------------------

;;; Function: create-tsp-dcl

;;; Description: 임시 DCL 파일 생성

;;; --------------------------------------------------------------------------

(defun create-tsp-dcl (/ dcl-file dcl-path)

  (setq dcl-path (strcat (getvar "TEMPPREFIX") "tsp.dcl"))

  (setq dcl-file (open dcl-path "w"))

  (if dcl-file

    (progn

      (write-line "tsp_scale : dialog { label = \"스케일 설정\"; " dcl-file)

      (write-line "  : column { alignment = centered; " dcl-file)

      

      (write-line "    : row { alignment = centered; fixed_width = true; " dcl-file)

      (write-line "      : edit_box { key = \"scale_section\"; label = \"단면도 A3 : 1/\"; edit_width = 3; width = 10; fixed_width = true; } " dcl-file)

      (write-line "    } " dcl-file)



      (write-line "    : row { alignment = centered; fixed_width = true; " dcl-file)

      (write-line "      : edit_box { key = \"scale_plan\"; label = \"평면도 A3 : 1/\"; edit_width = 3; width = 10; fixed_width = true; } " dcl-file)

      (write-line "    } " dcl-file)



      (write-line "    } " dcl-file)

      (write-line "  : spacer { height = 1.0; } " dcl-file)

      (write-line "  : row { alignment = centered; : button { key = \"accept\"; label = \"확인\"; is_default = true; width = 12; } : button { key = \"cancel\"; label = \"취소\"; is_cancel = true; width = 12; } } }" dcl-file)

      

      (write-line "tsp_load_list : dialog { label = \"프로젝트 관리\"; " dcl-file)

      (write-line "  : list_box { key = \"proj_list\"; label = \"저장된 프로젝트:\"; width = 30; height = 10; } " dcl-file)

      (write-line "  : row { alignment = centered; : button { key = \"accept\"; label = \"불러오기\"; is_default = true; } : button { key = \"btn_del_proj\"; label = \"삭제\"; } : button { key = \"cancel\"; label = \"취소\"; is_cancel = true; } } }" dcl-file)



      (write-line "tsp_save_prompt : dialog { label = \"프로젝트 저장\"; " dcl-file)

      (write-line "  : text { label = \"다른 이름으로 저장하시겠습니까?\"; alignment = centered; } : spacer { height = 0.5; } " dcl-file)

      (write-line "  : row { alignment = centered; : button { key = \"btn_yes\"; label = \"예 (새로 저장)\"; is_default = true; width = 18; } : button { key = \"btn_no\"; label = \"아니오 (덮어쓰기)\"; width = 18; } : button { key = \"cancel\"; label = \"취소\"; is_cancel = true; width = 12; } } }" dcl-file)



      (write-line "tsp_section_choice : dialog { label = \"단면도 생성 선택\"; : column { : text { label = \"작성할 단면도를 선택하세요:\"; alignment = centered; } : spacer { height = 0.5; } : toggle { key = \"chk_seg_sec\"; label = \"세그먼트(일반) 단면도\"; value = \"0\"; } : toggle { key = \"chk_cor_sec\"; label = \"모서리(사보강재) 단면도\"; value = \"0\"; } : spacer { height = 0.5; } : row { alignment = centered; : button { key = \"accept\"; label = \"확인\"; is_default = true; width = 12; } : button { key = \"cancel\"; label = \"취소\"; is_cancel = true; width = 12; } } } }" dcl-file)

      (write-line "tsp_manager : dialog { key = \"manager_title\"; label = \"세그먼트 설정 (Segment Manager)\"; : column { " dcl-file)

      (write-line "  : row { " dcl-file)

      (write-line "    : text { label = \"No.\"; width = 4; } " dcl-file)          

      (write-line "    : text { label = \"이름\"; width = 10; } " dcl-file)        

      (write-line "    : text { label = \"단면\"; width = 6; } " dcl-file)           

      (write-line "    : text { label = \"흙막이벽\"; width = 12; } " dcl-file)   

      (write-line "    : text { label = \"H-pile/띠장 규격\"; width = 28; } " dcl-file)

      (write-line "    : text { label = \"지보재\"; width = 15; } " dcl-file)   

      (write-line "    : text { label = \"사보강재\"; width = 25; } " dcl-file)   

      (write-line "  }" dcl-file)

      (write-line "  : list_box { key = \"seg_list\"; width = 115; height = 15; fixed_width = true; fixed_height = true; allow_accept = true; tabs = \"5 15 21 33 60 75\"; } " dcl-file)

      (write-line "  : text { key = \"status_msg\"; value = \"목록을 선택하고 입력 버튼을 누르세요.\"; alignment = centered; } } : spacer { height = 0.2; } " dcl-file)

      (write-line "  : row { alignment = centered; : edit_box { key = \"project_name\"; label = \"프로젝트 이름:\"; width = 25; } : button { key = \"btn_load_proj\"; label = \"불러오기\"; width = 12; } : button { key = \"btn_save_proj\"; label = \"저장\"; width = 12; } } : spacer { height = 0.2; } " dcl-file)

      (write-line "  : row { alignment = centered; : button { key = \"btn_input\"; label = \"입력 (Input)\"; is_default = true; width = 18; } : button { key = \"btn_modify\"; label = \"수정 (Edit)\"; width = 18; } : button { key = \"btn_delete\"; label = \"초기화 (Reset)\"; width = 18; } : button { key = \"btn_copy_from\"; label = \"설정 가져오기\"; width = 22; } } " dcl-file)

      (write-line "  : row { alignment = centered; : button { key = \"btn_apply_all\"; label = \"전체 설정 일괄 적용\"; width = 30; } : button { key = \"btn_delete_all\"; label = \"전체 목록 초기화 (Reset All)\"; width = 30; } } " dcl-file)

      (write-line "  : row { alignment = centered; : button { key = \"btn_apply_soil\"; label = \"지반 정보 일괄 적용\"; width = 30; } : button { key = \"btn_apply_support\"; label = \"지보재 정보 일괄 적용\"; width = 30; } } : spacer { height = 1.0; } " dcl-file)

      (write-line "  : row { alignment = centered; : button { key = \"btn_draw_section\"; label = \"단면도 생성 (Section)\"; width = 25; } : button { key = \"btn_draw_plan\"; label = \"평면도 생성 (Plan)\"; width = 25; } : button { key = \"cancel\"; label = \"종료 (Exit)\"; is_cancel = true; width = 20; } } }" dcl-file)

      

      (write-line "tsp_edit_menu : dialog { key = \"menu_title\"; label = \"세그먼트 설정 수정\"; " dcl-file)

      (write-line "  : column { " dcl-file)

      (write-line "    : row { : edit_box { key = \"seg_name\"; label = \"이름 입력 :\"; width = 20; } : toggle { key = \"chk_section\"; label = \"단면생성 (지반 정보 포함)\"; } } " dcl-file)

      (write-line "    : spacer { height = 0.5; } " dcl-file)

      (write-line "    : button { key = \"btn_soil\"; label = \"지반 정의 (Soil)\"; width = 40; fixed_width = true; alignment = centered; } " dcl-file)

      (write-line "    : button { key = \"btn_wall\"; label = \"흙막이벽 정의 (Wall)\"; width = 40; fixed_width = true; alignment = centered; } " dcl-file)

      (write-line "    : button { key = \"btn_support\"; label = \"지보재 정의 (Support)\"; width = 40; fixed_width = true; alignment = centered; } " dcl-file)

      (write-line "    : button { key = \"btn_brace\"; label = \"사보강재 정의 (Corner Brace)\"; width = 40; fixed_width = true; alignment = centered; } " dcl-file)

      (write-line "  } " dcl-file)

      (write-line "  : spacer { height = 1; } " dcl-file)

      (write-line "  : row { alignment = centered; : button { key = \"accept\"; label = \"확인 (저장)\"; is_default = true; width = 15; } : button { key = \"cancel\"; label = \"취소\"; is_cancel = true; width = 15; } } }" dcl-file)



      (write-line "tsp_brace : dialog { key = \"brace_dlg_title\"; label = \"사보강재(Corner Brace) 설정\"; : column { " dcl-file)

      (write-line "  : text { key = \"brace_info\"; label = \"최대 한계 거리: -- m\"; alignment = centered; } " dcl-file)

      (write-line "  : list_box { key = \"brace_list\"; height = 6; } " dcl-file)

      (write-line "  : row { " dcl-file)

      (write-line "    : text { label = \"이격 거리(m):\"; width = 12; } " dcl-file)

      (write-line "    : edit_box { key = \"brace_dist\"; width = 10; } " dcl-file)

      (write-line "  } " dcl-file)

      (write-line "  : text { label = \"※ 1열은 코너 기준, 2열부터는 이전 열 기준 상대거리입니다.\"; } " dcl-file)

      (write-line "  : spacer { height = 0.5; } " dcl-file)

      (write-line "  : text { label = \"사보강재 규격\"; } : popup_list { key = \"brace_spec\"; width = 30; } " dcl-file)

      (write-line "  : row { : edit_box { key = \"brace_h\"; width = 5; is_enabled = false; } : text { label = \"x\"; } : edit_box { key = \"brace_b\"; width = 5; is_enabled = false; } : text { label = \"x\"; } : edit_box { key = \"brace_tw\"; width = 5; is_enabled = false; } : text { label = \"/\"; } : edit_box { key = \"brace_tf\"; width = 5; is_enabled = false; } } " dcl-file)

      (write-line "  : spacer { height = 0.5; } " dcl-file)

      (write-line "  : row { : popup_list { key = \"brace_jack\"; label = \"잭 종류:\"; width = 20; } } " dcl-file)

      (write-line "  : spacer { height = 0.5; } " dcl-file)

      (write-line "  : row { alignment = centered; : button { key = \"btn_add\"; label = \"추가\"; } : button { key = \"btn_mod\"; label = \"수정\"; } : button { key = \"btn_del\"; label = \"삭제\"; } } " dcl-file)

      (write-line "  : spacer { height = 1; } " dcl-file)

      (write-line "  : boxed_row { label = \"코너부 띠장 상향 (Splice)\"; " dcl-file)

      (write-line "    : toggle { key = \"chk_upgrade_wale\"; label = \"사보강재 구간 띠장 상향 적용\"; is_enabled = false; } " dcl-file)

      (write-line "    : popup_list { key = \"upgrade_wale_spec\"; width = 20; is_enabled = false; } " dcl-file)

      (write-line "  } " dcl-file)

      (write-line "  : spacer { height = 0.5; } " dcl-file)

      (write-line "  : row { alignment = centered; : button { key = \"accept\"; label = \"다음\"; is_default = true; width = 12; } : button { key = \"btn_comp\"; label = \"완료\"; width = 12; } : button { key = \"cancel\"; label = \"취소\"; is_cancel = true; width = 12; } } } }" dcl-file)



      (write-line "tsp_brace_single : dialog {" dcl-file)

      (write-line "  key = \"brace_dlg_title\";" dcl-file)

      (write-line "  label = \"사보강재(Corner Brace) 설정\";" dcl-file)

      (write-line "  : column {" dcl-file)

      (write-line "    : text { key = \"brace_info\"; label = \"최대 한계 거리: -- m\"; alignment = centered; }" dcl-file)

      (write-line "    : list_box { key = \"brace_list\"; height = 6; }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : text { label = \"이격 거리(m):\"; width = 12; }" dcl-file)

      (write-line "      : edit_box { key = \"brace_dist\"; width = 10; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : text { label = \"※ 1열은 코너 기준, 2열부터는 이전 열 기준 상대거리입니다.\"; }" dcl-file)

      (write-line "    : spacer { height = 0.5; }" dcl-file)

      (write-line "    : text { label = \"사보강재 규격\"; }" dcl-file)

      (write-line "    : popup_list { key = \"brace_spec\"; width = 30; }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : edit_box { key = \"brace_h\"; width = 5; is_enabled = false; }" dcl-file)

      (write-line "      : text { label = \"x\"; }" dcl-file)

      (write-line "      : edit_box { key = \"brace_b\"; width = 5; is_enabled = false; }" dcl-file)

      (write-line "      : text { label = \"x\"; }" dcl-file)

      (write-line "      : edit_box { key = \"brace_tw\"; width = 5; is_enabled = false; }" dcl-file)

      (write-line "      : text { label = \"/\"; }" dcl-file)

      (write-line "      : edit_box { key = \"brace_tf\"; width = 5; is_enabled = false; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : spacer { height = 0.5; }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : popup_list { key = \"brace_jack\"; label = \"잭 종류:\"; width = 20; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : spacer { height = 0.5; }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      alignment = centered;" dcl-file)

      (write-line "      : button { key = \"btn_add\"; label = \"추가\"; }" dcl-file)

      (write-line "      : button { key = \"btn_mod\"; label = \"수정\"; }" dcl-file)

      (write-line "      : button { key = \"btn_del\"; label = \"삭제\"; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : spacer { height = 1; }" dcl-file)

      (write-line "    : boxed_row {" dcl-file)

      (write-line "      label = \"코너부 띠장 상향 (Splice)\";" dcl-file)

      (write-line "      : toggle { key = \"chk_upgrade_wale\"; label = \"사보강재 구간 띠장 상향 적용\"; is_enabled = false; }" dcl-file)

      (write-line "      : popup_list { key = \"upgrade_wale_spec\"; width = 20; is_enabled = false; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : spacer { height = 0.5; }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      alignment = centered;" dcl-file)

      (write-line "      : button { key = \"accept\"; label = \"다음\"; width = 12; }" dcl-file)

      (write-line "      : button { key = \"btn_comp\"; label = \"완료\"; is_default = true; width = 12; }" dcl-file)

      (write-line "      : button { key = \"cancel\"; label = \"취소\"; is_cancel = true; width = 12; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "  }" dcl-file)

      (write-line "}" dcl-file)



      (write-line "tsp_brace_warn : dialog {" dcl-file)

      (write-line "  label = \"경고: 권장 공간 초과\";" dcl-file)

      (write-line "  : text { label = \"입력한 누적거리가 권장 한계(50%)를 초과했습니다.\"; alignment = centered; }" dcl-file)

      (write-line "  : text { label = \"이웃 모서리의 사보강재 설치 공간이 부족해질 수 있습니다.\"; alignment = centered; }" dcl-file)

      (write-line "  : text { label = \"그래도 리스트에 추가(수정)하시겠습니까?\"; alignment = centered; }" dcl-file)

      (write-line "  : spacer { height = 0.5; }" dcl-file)

      (write-line "  : row {" dcl-file)

      (write-line "    alignment = centered;" dcl-file)

      (write-line "    : button { key = \"accept\"; label = \"확인(계속)\"; is_default = true; width = 12; }" dcl-file)

      (write-line "    : button { key = \"cancel\"; label = \"취소\"; is_cancel = true; width = 12; }" dcl-file)

      (write-line "  }" dcl-file)

      (write-line "}" dcl-file)



      (write-line "tsp_brace_hard_warn : dialog {" dcl-file)

      (write-line "  label = \"경고: 절대 한계 초과\";" dcl-file)

      (write-line "  : text { key = \"warn_msg1\"; label = \"입력한 거리가 물리적 절대 한계를 초과했습니다!\"; alignment = centered; }" dcl-file)

      (write-line "  : text { key = \"warn_msg2\"; label = \"부재 간 간섭이 발생할 수 있습니다. 강제로 적용하시겠습니까?\"; alignment = centered; }" dcl-file)

      (write-line "  : spacer { height = 0.5; }" dcl-file)

      (write-line "  : row {" dcl-file)

      (write-line "    alignment = centered;" dcl-file)

      (write-line "    : button { key = \"accept\"; label = \"강제 적용\"; is_default = true; width = 12; }" dcl-file)

      (write-line "    : button { key = \"cancel\"; label = \"취소\"; is_cancel = true; width = 12; }" dcl-file)

      (write-line "  }" dcl-file)

      (write-line "}" dcl-file)



      (write-line "tsp_section_mode : dialog {" dcl-file)

      (write-line "  key = \"dlg_title\";" dcl-file)

      (write-line "  label = \"세그먼트 설정\";" dcl-file)

      (write-line "  : column {" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : text { label = \"이름 입력 :\"; }" dcl-file)

      (write-line "      : edit_box { key = \"seg_name\"; width = 20; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : spacer { height = 0.5; }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : toggle { key = \"chk_section\"; label = \"단면생성 (지반 정보 포함)\"; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "  }" dcl-file)

      (write-line "  : spacer { height = 1; }" dcl-file)

      (write-line "  : row {" dcl-file)

      (write-line "    alignment = centered;" dcl-file)

      (write-line "    : button { key = \"accept\"; label = \"다음\"; is_default = true; width = 12; }" dcl-file)

      (write-line "    : button { key = \"cancel\"; label = \"취소\"; is_cancel = true; width = 12; }" dcl-file)

      (write-line "  }" dcl-file)

      (write-line "}" dcl-file)



      (write-line "tsp_soil : dialog {" dcl-file)

      (write-line "  label = \"지반 정의\";" dcl-file)

      (write-line "  : column {" dcl-file)

      (write-line "    : text { label = \"지층 정보를 입력하세요\"; alignment = centered; }" dcl-file)

      (write-line "    : spacer { height = 0.5; }" dcl-file)

      (write-line "    : list_box { key = \"soil_list\"; label = \"지층 목록:\"; width = 50; height = 12; fixed_width = true; fixed_height = true; multiple_select = false; }" dcl-file)

      (write-line "    : spacer { height = 0.5; }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : edit_box { key = \"soil_user_name\"; label = \"이름:\"; width = 15; fixed_width = true; }" dcl-file)

      (write-line "      : popup_list { key = \"soil_type\"; label = \"종류:\"; width = 20; fixed_width = true; }" dcl-file)

      (write-line "      : edit_box { key = \"soil_depth\"; label = \"깊이(m):\"; width = 10; fixed_width = true; }" dcl-file)

      (write-line "      : edit_box { key = \"soil_phi\"; label = \"내부마찰각(도):\"; width = 5; fixed_width = true; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : spacer { height = 0.3; }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      alignment = centered;" dcl-file)

      (write-line "      : button { key = \"btn_add\"; label = \"추가\"; width = 10; fixed_width = true; }" dcl-file)

      (write-line "      : button { key = \"btn_modify\"; label = \"수정\"; width = 10; fixed_width = true; }" dcl-file)

      (write-line "      : button { key = \"btn_delete\"; label = \"삭제\"; width = 10; fixed_width = true; }" dcl-file)

      (write-line "      : button { key = \"btn_clear\"; label = \"전체삭제\"; width = 10; fixed_width = true; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : spacer { height = 0.5; }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : toggle { key = \"soil_water_chk\"; label = \"지하수위\"; }" dcl-file)

      (write-line "      : edit_box { key = \"soil_water_depth\"; label = \"GL -\"; width = 10; fixed_width = true; is_enabled = false; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : spacer { height = 0.5; }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : toggle { key = \"soil_shotcrete_enable\"; label = \"숏크리트 생성\"; }" dcl-file)

      (write-line "      : edit_box { key = \"soil_shotcrete_thick\"; label = \"T(mm) : \"; width = 8; fixed_width = true; alignment = right; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : toggle { key = \"soil_shotcrete_chk\"; label = \"숏크리트 암반층 선택\"; }" dcl-file)

      (write-line "      : popup_list { key = \"soil_shotcrete_layer\"; width = 20; fixed_width = true; is_enabled = false; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "  }" dcl-file)

      (write-line "  : spacer { height = 1.0; }" dcl-file)

      (write-line "  : row {" dcl-file)

      (write-line "    alignment = centered;" dcl-file)

      (write-line "    : button { key = \"btn_legend\"; label = \"범례출력\"; width = 12; fixed_width = true; }" dcl-file)

      (write-line "    : button { key = \"accept\"; label = \"확인\"; is_default = true; width = 12; fixed_width = true; }" dcl-file)

      (write-line "    : button { key = \"back\"; label = \"이전\"; width = 12; fixed_width = true; }" dcl-file)

      (write-line "    : button { key = \"cancel\"; label = \"취소\"; is_cancel = true; width = 12; fixed_width = true; }" dcl-file)

      (write-line "  }" dcl-file)

      (write-line "}" dcl-file)



      (write-line "tsp_main : dialog {" dcl-file)

      (write-line "  label = \"TSP - Temporary Structure Plan\";" dcl-file)

      (write-line "  : column {" dcl-file)

      (write-line "    alignment = centered;" dcl-file)

      (write-line "    : text { label = \"흙막이벽 정의\"; alignment = centered; }" dcl-file)

      (write-line "    : spacer { height = 0.5; }" dcl-file)

      (write-line "    : button { key = \"btn_hpile\"; label = \"H-Pile + 토류판\"; width = 30; fixed_width = true; }" dcl-file)

      (write-line "    : button { key = \"btn_cip\"; label = \"C.I.P (현장타설말뚝)\"; width = 30; fixed_width = true; }" dcl-file)

      (write-line "    : button { key = \"btn_scw\"; label = \"S.C.W (소일시멘트벽)\"; width = 30; fixed_width = true; is_enabled = false; }" dcl-file)

      (write-line "    : spacer { height = 0.5; }" dcl-file)

      (write-line "    : button { key = \"back\"; label = \"이전\"; width = 12; fixed_width = true; alignment = centered; }" dcl-file)

      (write-line "    : button { key = \"cancel\"; label = \"취소\"; is_cancel = true; fixed_width = true; alignment = centered; }" dcl-file)

      (write-line "  }" dcl-file)

      (write-line "}" dcl-file)



      (write-line "tsp_hpile : dialog {" dcl-file)

      (write-line "  label = \"H-Pile+토류판 설정\";" dcl-file)

      (write-line "  : column {" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : edit_box { key = \"hpile_max_depth\"; label = \"최대 굴착 깊이(m)\"; width = 10; fixed_width = true; }" dcl-file)

      (write-line "      : edit_box { key = \"hpile_embed_depth\"; label = \"근입 깊이(m)\"; width = 10; fixed_width = true; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : text { label = \"H-Pile 규격\"; }" dcl-file)

      (write-line "    : popup_list { key = \"hpile_spec\"; width = 30; fixed_width = true; }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : edit_box { key = \"hpile_h\"; width = 5; is_enabled = false; }" dcl-file)

      (write-line "      : text { label = \"x\"; }" dcl-file)

      (write-line "      : edit_box { key = \"hpile_b\"; width = 5; is_enabled = false; }" dcl-file)

      (write-line "      : text { label = \"x\"; }" dcl-file)

      (write-line "      : edit_box { key = \"hpile_tw\"; width = 5; is_enabled = false; }" dcl-file)

      (write-line "      : text { label = \"/\"; }" dcl-file)

      (write-line "      : edit_box { key = \"hpile_tf\"; width = 5; is_enabled = false; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : spacer { height = 0.5; }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : text { label = \"C.T.C (m)\"; width = 10; }" dcl-file)

      (write-line "      : edit_box { key = \"ctc\"; width = 5; value = \"2\"; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : spacer { height = 0.5; }" dcl-file)

      (write-line "    : text { label = \"띠장 규격\"; }" dcl-file)

      (write-line "    : popup_list { key = \"wale_spec\"; width = 30; fixed_width = true; }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : edit_box { key = \"wale_h\"; width = 5; is_enabled = false; }" dcl-file)

      (write-line "      : text { label = \"x\"; }" dcl-file)

      (write-line "      : edit_box { key = \"wale_b\"; width = 5; is_enabled = false; }" dcl-file)

      (write-line "      : text { label = \"x\"; }" dcl-file)

      (write-line "      : edit_box { key = \"wale_tw\"; width = 5; is_enabled = false; }" dcl-file)

      (write-line "      : text { label = \"/\"; }" dcl-file)

      (write-line "      : edit_box { key = \"wale_tf\"; width = 5; is_enabled = false; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : spacer { height = 0.5; }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : text { label = \"토류판 두께 (mm)\"; width = 10; }" dcl-file)

      (write-line "      : edit_box { key = \"timber_thickness\"; width = 5; value = \"60\"; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "  }" dcl-file)

      (write-line "  : spacer { height = 1.0; }" dcl-file)

      (write-line "  : row {" dcl-file)

      (write-line "    alignment = centered;" dcl-file)

      (write-line "    : button { key = \"accept\"; label = \"확인\"; is_default = true; width = 12; }" dcl-file)

      (write-line "    : button { key = \"back\"; label = \"이전\"; width = 12; }" dcl-file)

      (write-line "    : button { key = \"cancel\"; label = \"취소\"; is_cancel = true; width = 12; }" dcl-file)

      (write-line "  }" dcl-file)

      (write-line "}" dcl-file)



      (write-line "tsp_support : dialog {" dcl-file)

      (write-line "  label = \"지보재 정의\";" dcl-file)

      (write-line "  : column {" dcl-file)

      (write-line "    : text { label = \"지보재 목록 관리\"; alignment = centered; }" dcl-file)

      (write-line "    : spacer { height = 0.5; }" dcl-file)

      (write-line "    : list_box { key = \"support_list\"; label = \"목록:\"; width = 60; height = 12; fixed_width = true; fixed_height = true; multiple_select = false; }" dcl-file)

      (write-line "    : spacer { height = 0.5; }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : popup_list { key = \"support_type_sel\"; label = \"추가할 유형:\"; width = 20; fixed_width = true; }" dcl-file)

      (write-line "      : button { key = \"btn_add\"; label = \"추가\"; width = 10; fixed_width = true; }" dcl-file)

      (write-line "      : button { key = \"btn_modify\"; label = \"수정\"; width = 10; fixed_width = true; }" dcl-file)

      (write-line "      : button { key = \"btn_delete\"; label = \"삭제\"; width = 10; fixed_width = true; }" dcl-file)

      (write-line "      : button { key = \"btn_clear\"; label = \"전체삭제\"; width = 10; fixed_width = true; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "  }" dcl-file)

      (write-line "  : spacer { height = 1.0; }" dcl-file)

      (write-line "  : row {" dcl-file)

      (write-line "    alignment = centered;" dcl-file)

      (write-line "    : button { key = \"accept\"; label = \"확인\"; is_default = true; width = 12; fixed_width = true; }" dcl-file)

      (write-line "    : button { key = \"back\"; label = \"이전\"; width = 12; fixed_width = true; }" dcl-file)

      (write-line "    : button { key = \"cancel\"; label = \"취소\"; is_cancel = true; width = 12; fixed_width = true; }" dcl-file)

      (write-line "  }" dcl-file)

      (write-line "}" dcl-file)



      (write-line "tsp_support_strut : dialog {" dcl-file)

      (write-line "  label = \"버팀보(Strut) 입력\";" dcl-file)

      (write-line "  : column {" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : edit_box { key = \"s_name\"; label = \"이름:\"; width = 15; }" dcl-file)

      (write-line "      : popup_list { key = \"s_shape\"; label = \"형상:\"; width = 15; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : edit_box { key = \"s_depth\"; label = \"설치깊이(m):\"; width = 10; }" dcl-file)

      (write-line "      : edit_box { key = \"s_count\"; label = \"개수(ea):\"; width = 5; value = \"1\"; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : popup_list { key = \"s_mat\"; label = \"재질:\"; width = 15; }" dcl-file)

      (write-line "      : popup_list { key = \"s_sec\"; label = \"단면:\"; width = 20; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : popup_list { key = \"s_jack\"; label = \"잭 종류:\"; width = 20; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : spacer { height = 1; }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      alignment = centered;" dcl-file)

      (write-line "      : button { key = \"accept\"; label = \"확인\"; is_default = true; width = 12; }" dcl-file)

      (write-line "      : button { key = \"cancel\"; label = \"취소\"; is_cancel = true; width = 12; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "  }" dcl-file)

      (write-line "}" dcl-file)



      (write-line "tsp_support_anchor : dialog {" dcl-file)

      (write-line "  label = \"앵커(Earth Anchor) 입력\";" dcl-file)

      (write-line "  : column {" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : edit_box { key = \"a_name\"; label = \"이름:\"; width = 15; }" dcl-file)

      (write-line "      : popup_list { key = \"a_shape\"; label = \"형상:\"; width = 15; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : edit_box { key = \"a_depth\"; label = \"설치깊이(m):\"; width = 10; }" dcl-file)

      (write-line "      : edit_box { key = \"a_angle\"; label = \"설치각도(도):\"; width = 5; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : edit_box { key = \"a_free\"; label = \"자유장(m):\"; width = 10; }" dcl-file)

      (write-line "      : edit_box { key = \"a_bond\"; label = \"정착장(m):\"; width = 10; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      : popup_list { key = \"a_mat\"; label = \"재질:\"; width = 15; }" dcl-file)

      (write-line "      : popup_list { key = \"a_sec\"; label = \"단면:\"; width = 20; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "    : spacer { height = 1; }" dcl-file)

      (write-line "    : row {" dcl-file)

      (write-line "      alignment = centered;" dcl-file)

      (write-line "      : button { key = \"accept\"; label = \"확인\"; is_default = true; width = 12; }" dcl-file)

      (write-line "      : button { key = \"cancel\"; label = \"취소\"; is_cancel = true; width = 12; }" dcl-file)

      (write-line "    }" dcl-file)

      (write-line "  }" dcl-file)

      (write-line "}" dcl-file)



      (close dcl-file)

      dcl-path

    )

    nil

  )

)



;;;;;==========================================================================

;;;; defun tsp-create-cip-dcl : C.I.P 전용 DCL 다이얼로그 파일 생성 (원본 완벽 동기화)

;;;;==========================================================================

(defun tsp-create-cip-dcl ( / dcl-file fn )

  (setq dcl-file (strcat (getvar "TEMPPREFIX") "tsp_cip.dcl"))

  (setq fn (open dcl-file "w"))

  (write-line "tsp_cip_dlg : dialog {" fn)

  (write-line "  label = \"C.I.P 흙막이벽 설정\";" fn)

  (write-line "  : column {" fn)

  

  ;; 1. 깊이 설정 (원본 동일)

  (write-line "    : row {" fn)

  (write-line "      : edit_box { key = \"cip_max_depth\"; label = \"최대 굴착 깊이(m)\"; width = 10; fixed_width = true; }" fn)

  (write-line "      : edit_box { key = \"cip_embed_depth\"; label = \"근입 깊이(m)\"; width = 10; fixed_width = true; }" fn)

  (write-line "    }" fn)

  

  ;; 2. H-Pile 규격 및 치수 박스 (원본 동일)

  (write-line "    : text { label = \"H-Pile 규격\"; }" fn)

  (write-line "    : popup_list { key = \"cip_hpile_spec\"; width = 30; fixed_width = true; }" fn)

  (write-line "    : row {" fn)

  (write-line "      : edit_box { key = \"cip_hpile_h\"; width = 5; is_enabled = false; }" fn)

  (write-line "      : text { label = \"x\"; }" fn)

  (write-line "      : edit_box { key = \"cip_hpile_b\"; width = 5; is_enabled = false; }" fn)

  (write-line "      : text { label = \"x\"; }" fn)

  (write-line "      : edit_box { key = \"cip_hpile_tw\"; width = 5; is_enabled = false; }" fn)

  (write-line "      : text { label = \"/\"; }" fn)

  (write-line "      : edit_box { key = \"cip_hpile_tf\"; width = 5; is_enabled = false; }" fn)

  (write-line "    }" fn)

  (write-line "    : spacer { height = 0.5; }" fn)

  

  ;; 3. 띠장 규격 및 치수 박스 (원본 동일)

  (write-line "    : text { label = \"띠장 규격\"; }" fn)

  (write-line "    : popup_list { key = \"cip_wale_spec\"; width = 30; fixed_width = true; }" fn)

  (write-line "    : row {" fn)

  (write-line "      : edit_box { key = \"cip_wale_h\"; width = 5; is_enabled = false; }" fn)

  (write-line "      : text { label = \"x\"; }" fn)

  (write-line "      : edit_box { key = \"cip_wale_b\"; width = 5; is_enabled = false; }" fn)

  (write-line "      : text { label = \"x\"; }" fn)

  (write-line "      : edit_box { key = \"cip_wale_tw\"; width = 5; is_enabled = false; }" fn)

  (write-line "      : text { label = \"/\"; }" fn)

  (write-line "      : edit_box { key = \"cip_wale_tf\"; width = 5; is_enabled = false; }" fn)

  (write-line "    }" fn)

  (write-line "    : spacer { height = 0.5; }" fn)



  ;; 4. C.I.P 전용 배열 설정 (토류판/CTC 대체)

  (write-line "    : row {" fn)

  (write-line "      : text { label = \"C.I.P 직경 (D)\"; width = 10; }" fn)

  (write-line "      : edit_box { key = \"cip_dia\"; width = 5; }" fn)

  (write-line "    }" fn)

  (write-line "    : spacer { height = 0.5; }" fn)

  (write-line "    : row {" fn)

  (write-line "      : text { label = \"배열 모드\"; width = 10; }" fn)

  (write-line "      : popup_list { key = \"cip_layout_mode\"; width = 15; }" fn)

  (write-line "      : text { label = \"겹침량(mm)\"; }" fn)

  (write-line "      : edit_box { key = \"cip_overlap\"; width = 5; }" fn)

  (write-line "    }" fn)

  (write-line "    : spacer { height = 0.5; }" fn)

  (write-line "    : row {" fn)

  (write-line "      : text { label = \"H-Pile 삽입\"; width = 10; }" fn)

  (write-line "      : popup_list { key = \"cip_interval\"; width = 15; }" fn)

  (write-line "    }" fn)

  (write-line "  }" fn)

  

  ;; 5. 하단 버튼 영역 (원본 동일)

  (write-line "  : spacer { height = 1.0; }" fn)

  (write-line "  : row {" fn)

  (write-line "    alignment = centered;" fn)

  (write-line "    : button { key = \"accept\"; label = \"확인\"; is_default = true; width = 12; }" fn)

  (write-line "    : button { key = \"back\"; label = \"이전\"; width = 12; }" fn)

  (write-line "    : button { key = \"cancel\"; label = \"취소\"; is_cancel = true; width = 12; }" fn)

  (write-line "  }" fn)

  (write-line "}" fn)

  (close fn)

  dcl-file

)



;;;;==========================================================================

;;;; defun tsp-cip-update-hpile-dims : H-Pile 규격에 맞춰 치수 박스 및 직경(D) 자동 업데이트

;;;;==========================================================================

(defun tsp-cip-update-hpile-dims (idx-str / idx hpile-spec hpile-vals h)

  (setq idx (atoi idx-str))

  (setq hpile-spec (nth idx *tsp-std-wall-list*))

  (if hpile-spec

    (progn

      (setq hpile-vals (parse-h-spec hpile-spec))

      (setq h (nth 0 hpile-vals))

      ;; 치수 박스 표시 (비활성화 상태이므로 값만 갱신)

      (set_tile "cip_hpile_h" (itoa (nth 0 hpile-vals)))

      (set_tile "cip_hpile_b" (itoa (nth 1 hpile-vals)))

      (set_tile "cip_hpile_tw" (itoa (nth 2 hpile-vals)))

      (set_tile "cip_hpile_tf" (itoa (nth 3 hpile-vals)))

      ;; 직경(D) 자동 연동 로직

      (cond

        ((<= h 250) (set_tile "cip_dia" "450"))

        ((<= h 300) (set_tile "cip_dia" "500"))

        ((<= h 350) (set_tile "cip_dia" "600"))

        ((<= h 400) (set_tile "cip_dia" "700"))

        (t (set_tile "cip_dia" "500"))

      )

    )

  )

)



;;;;==========================================================================

;;;; defun tsp-cip-update-wale-dims : 띠장 규격에 맞춰 치수 박스 자동 업데이트

;;;;==========================================================================

(defun tsp-cip-update-wale-dims (idx-str / idx wale-spec wale-vals)

  (setq idx (atoi idx-str))

  (setq wale-spec (nth idx *tsp-std-wall-list*))

  (if wale-spec

    (progn

      (setq wale-vals (parse-h-spec wale-spec))

      (set_tile "cip_wale_h" (itoa (nth 0 wale-vals)))

      (set_tile "cip_wale_b" (itoa (nth 1 wale-vals)))

      (set_tile "cip_wale_tw" (itoa (nth 2 wale-vals)))

      (set_tile "cip_wale_tf" (itoa (nth 3 wale-vals)))

    )

  )

)



;;;;==========================================================================

;;;; defun tsp-cip-update-mode : 배열 모드 변경 시 겹침량 입력창 활성/비활성 제어

;;;;==========================================================================

(defun tsp-cip-update-mode (val)

  (if (= val "0") ; 맞닿음 (Tangent)

    (progn

      (mode_tile "cip_overlap" 1)

      (set_tile "cip_overlap" "0")

    )

    (progn ; 겹침 (Secant)

      (mode_tile "cip_overlap" 0)

      (if (or (not *tsp-cip-overlap*) (= *tsp-cip-overlap* "") (= *tsp-cip-overlap* "0"))

        (set_tile "cip_overlap" "50")

        (set_tile "cip_overlap" *tsp-cip-overlap*)

      )

    )

  )

)



;;;;==========================================================================

;;;; defun tsp-run-cip-dialog : C.I.P 설정 대화상자 실행 및 콜백 제어

;;;;==========================================================================

(defun tsp-run-cip-dialog
  ( / dcl-id dcl-file mode-list interval-list dcl-status
      ;; [4단계] 진입 전 상태 백업용 로컬변수
      bak-wall-type
      bak-cip-max-depth bak-cip-embed-depth
      bak-cip-hpile-idx bak-cip-wale-idx
      bak-cip-dia bak-cip-mode-idx
      bak-cip-overlap bak-cip-interval-idx
  )


  (setq dcl-file (tsp-create-cip-dcl))

  (setq dcl-id (load_dialog dcl-file))

  (if (not (new_dialog "tsp_cip_dlg" dcl-id)) (exit))



  ;; [4단계] 다이얼로그 진입 직후 전역 상태 백업
  ;; Cancel / Back 시 이 값으로 복원하여 전역변수 오염 방지
  (setq bak-wall-type        *tsp-wall-type*)
  (setq bak-cip-max-depth    *tsp-cip-max-depth*)
  (setq bak-cip-embed-depth  *tsp-cip-embed-depth*)
  (setq bak-cip-hpile-idx    *tsp-cip-hpile-idx*)
  (setq bak-cip-wale-idx     *tsp-cip-wale-idx*)
  (setq bak-cip-dia          *tsp-cip-dia*)
  (setq bak-cip-mode-idx     *tsp-cip-mode-idx*)
  (setq bak-cip-overlap      *tsp-cip-overlap*)
  (setq bak-cip-interval-idx *tsp-cip-interval-idx*)



  ;; 표준 규격 리스트 세팅 (원본 완벽 동기화)

  (start_list "cip_hpile_spec") (mapcar 'add_list *tsp-std-wall-list*) (end_list)

  (start_list "cip_wale_spec") (mapcar 'add_list *tsp-std-wall-list*) (end_list)



  (setq mode-list '("맞닿음 (Tangent)" "겹침 (Secant)"))

  (setq interval-list '("모든 공 삽입 (1/1)" "1공 건너 삽입 (1/2)" "2공 건너 삽입 (1/3)"))

  (start_list "cip_layout_mode") (mapcar 'add_list mode-list) (end_list)

  (start_list "cip_interval") (mapcar 'add_list interval-list) (end_list)



  ;; 초기값 화면에 반영

  (set_tile "cip_max_depth" *tsp-cip-max-depth*)

  (set_tile "cip_embed_depth" *tsp-cip-embed-depth*)

  (set_tile "cip_hpile_spec" *tsp-cip-hpile-idx*)

  (set_tile "cip_wale_spec" *tsp-cip-wale-idx*)

  (set_tile "cip_dia" *tsp-cip-dia*)

  (set_tile "cip_layout_mode" *tsp-cip-mode-idx*)

  (set_tile "cip_overlap" *tsp-cip-overlap*)

  (set_tile "cip_interval" *tsp-cip-interval-idx*)



  ;; 초기 치수 박스 업데이트 실행

  (tsp-cip-update-hpile-dims *tsp-cip-hpile-idx*)

  (tsp-cip-update-wale-dims *tsp-cip-wale-idx*)

  (tsp-cip-update-mode *tsp-cip-mode-idx*)



  ;; 사용자 입력 액션(콜백) - 전역 변수 오염 방지 적용

  (action_tile "cip_hpile_spec" "(tsp-cip-update-hpile-dims $value)")

  (action_tile "cip_wale_spec" "(tsp-cip-update-wale-dims $value)")

  (action_tile "cip_layout_mode" "(tsp-cip-update-mode $value)")

  

  ;; [4단계] Back/Cancel: 진입 전 백업값으로 전역변수 전체 복원
  (action_tile "back"
    "(progn
       (setq *tsp-wall-type*        bak-wall-type)
       (setq *tsp-cip-max-depth*    bak-cip-max-depth)
       (setq *tsp-cip-embed-depth*  bak-cip-embed-depth)
       (setq *tsp-cip-hpile-idx*    bak-cip-hpile-idx)
       (setq *tsp-cip-wale-idx*     bak-cip-wale-idx)
       (setq *tsp-cip-dia*          bak-cip-dia)
       (setq *tsp-cip-mode-idx*     bak-cip-mode-idx)
       (setq *tsp-cip-overlap*      bak-cip-overlap)
       (setq *tsp-cip-interval-idx* bak-cip-interval-idx)
       (done_dialog 2)
     )")

  (action_tile "cancel"
    "(progn
       (setq *tsp-wall-type*        bak-wall-type)
       (setq *tsp-cip-max-depth*    bak-cip-max-depth)
       (setq *tsp-cip-embed-depth*  bak-cip-embed-depth)
       (setq *tsp-cip-hpile-idx*    bak-cip-hpile-idx)
       (setq *tsp-cip-wale-idx*     bak-cip-wale-idx)
       (setq *tsp-cip-dia*          bak-cip-dia)
       (setq *tsp-cip-mode-idx*     bak-cip-mode-idx)
       (setq *tsp-cip-overlap*      bak-cip-overlap)
       (setq *tsp-cip-interval-idx* bak-cip-interval-idx)
       (done_dialog 0)
     )")

  (action_tile "accept" 

    "(progn 

       (setq *tsp-wall-type* \"CIP\")

       (setq *tsp-cip-max-depth* (get_tile \"cip_max_depth\"))

       (setq *tsp-cip-embed-depth* (get_tile \"cip_embed_depth\"))

       (setq *tsp-cip-hpile-idx* (get_tile \"cip_hpile_spec\"))

       (setq *tsp-cip-wale-idx* (get_tile \"cip_wale_spec\"))

       (setq *tsp-cip-dia* (get_tile \"cip_dia\"))

       (setq *tsp-cip-mode-idx* (get_tile \"cip_layout_mode\"))

       (setq *tsp-cip-overlap* (get_tile \"cip_overlap\"))

       (setq *tsp-cip-interval-idx* (get_tile \"cip_interval\"))

       (done_dialog 1)

     )"

  )



  (setq dcl-status (start_dialog))

  (unload_dialog dcl-id)

  (vl-file-delete dcl-file) 

  dcl-status

)



;;; --------------------------------------------------------------------------

;;; Function: section-choice-dialog-callback

;;; Description: 단면도 생성 시 세그먼트/모서리 선택 다이얼로그 콜백

;;; --------------------------------------------------------------------------

(defun section-choice-dialog-callback (dcl-path / dcl-id result draw-seg draw-cor)

  (setq dcl-id (load_dialog dcl-path))

  (if (new_dialog "tsp_section_choice" dcl-id)

    (progn

      (action_tile "accept" "(progn (setq draw-seg (= (get_tile \"chk_seg_sec\") \"1\")) (setq draw-cor (= (get_tile \"chk_cor_sec\") \"1\")) (if (or draw-seg draw-cor) (done_dialog 1) (alert \"최소 하나 이상의 단면도를 선택해야 합니다.\")))")

      (action_tile "cancel" "(done_dialog 0)")

      (setq result (start_dialog))

      (unload_dialog dcl-id)

      (if (= result 1)

        (list draw-seg draw-cor)

        nil

      )

    )

    nil

  )

)



;;; --------------------------------------------------------------------------

;;; Function: tsp-apply-settings-to-all

;;; Description: 선택한 세그먼트의 설정값을 모든 세그먼트에 일괄 적용

;;; --------------------------------------------------------------------------

(defun tsp-apply-settings-to-all (source-idx / source-data i new-list item new-item)

  (setq source-data (nth source-idx *segment-list*))

  (setq i 0 new-list '())

  (foreach item *segment-list*

    (if (= i source-idx)

      (setq new-list (append new-list (list item))) 

      (progn

        (setq new-item (list

          (cons 'ID (cdr (assoc 'ID item)))          

          (cons 'NAME (cdr (assoc 'NAME item)))      

          (cons 'IS-DEFINED T)                    

          (cons 'SECTION-DRAW (cdr (assoc 'SECTION-DRAW source-data)))

          (cons 'SOIL-DATA (cdr (assoc 'SOIL-DATA source-data)))

          (cons 'WALL-SPEC (cdr (assoc 'WALL-SPEC source-data)))

          (cons 'WALL-CUSTOM (cdr (assoc 'WALL-CUSTOM source-data)))

          (cons 'WALE-SPEC (cdr (assoc 'WALE-SPEC source-data)))

          (cons 'WALE-CUSTOM (cdr (assoc 'WALE-CUSTOM source-data)))

          (cons 'CTC (cdr (assoc 'CTC source-data)))

          (cons 'TIMBER-THICKNESS (cdr (assoc 'TIMBER-THICKNESS source-data)))

          (cons 'MAX-DEPTH (cdr (assoc 'MAX-DEPTH source-data)))

          (cons 'EMBED-DEPTH (cdr (assoc 'EMBED-DEPTH source-data)))

          (cons 'WATER-CHK (cdr (assoc 'WATER-CHK source-data)))

          (cons 'WATER-DEPTH (cdr (assoc 'WATER-DEPTH source-data)))

          (cons 'SHOTCRETE-ENABLE (cdr (assoc 'SHOTCRETE-ENABLE source-data)))

          (cons 'SHOTCRETE-CHK (cdr (assoc 'SHOTCRETE-CHK source-data)))

          (cons 'SHOTCRETE-LAYER (cdr (assoc 'SHOTCRETE-LAYER source-data)))

          (cons 'SHOTCRETE-THICK (cdr (assoc 'SHOTCRETE-THICK source-data)))

          (cons 'SUPPORT-LIST (cdr (assoc 'SUPPORT-LIST source-data)))

          (cons 'CORNER-BRACE (cdr (assoc 'CORNER-BRACE item)))

          (cons 'UPGRADE-WALE (cdr (assoc 'UPGRADE-WALE item)))

          (cons 'V-START (cdr (assoc 'V-START item)))

          (cons 'V-END (cdr (assoc 'V-END item)))     

          (cons 'ANGLE (cdr (assoc 'ANGLE item)))     

          (cons 'LENGTH (cdr (assoc 'LENGTH item)))  

        ))

        (setq new-list (append new-list (list new-item)))

      )

    )

    (setq i (1+ i))

  )

  (setq *segment-list* new-list)

  (setq *tsp-data-dirty* T)

)



;;; --------------------------------------------------------------------------

;;; Function: tsp-apply-soil-to-all

;;; Description: 선택된 세그먼트의 지반 정보 및 숏크리트 설정 일괄 적용

;;; --------------------------------------------------------------------------

(defun tsp-apply-soil-to-all (source-idx / source-data soil-data shotcrete-enable shotcrete-chk shotcrete-layer shotcrete-thick i new-list item new-item)

  (setq source-data (nth source-idx *segment-list*))

  (setq soil-data (cdr (assoc 'SOIL-DATA source-data)))

  (setq shotcrete-enable (cdr (assoc 'SHOTCRETE-ENABLE source-data)))

  (setq shotcrete-chk (cdr (assoc 'SHOTCRETE-CHK source-data)))

  (setq shotcrete-layer (cdr (assoc 'SHOTCRETE-LAYER source-data)))

  (setq shotcrete-thick (cdr (assoc 'SHOTCRETE-THICK source-data)))

  (setq i 0 new-list '())

  

  (foreach item *segment-list*

    (if (= i source-idx)

      (setq new-list (append new-list (list item)))

      (progn

        (setq new-item (subst (cons 'SOIL-DATA soil-data) (assoc 'SOIL-DATA item) item))

        

        ;; 각 키가 있으면 교체하고 없으면 리스트 끝에 추가

        (if (assoc 'SHOTCRETE-ENABLE new-item) (setq new-item (subst (cons 'SHOTCRETE-ENABLE shotcrete-enable) (assoc 'SHOTCRETE-ENABLE new-item) new-item)) (setq new-item (append new-item (list (cons 'SHOTCRETE-ENABLE shotcrete-enable)))))

        (if (assoc 'SHOTCRETE-CHK new-item) (setq new-item (subst (cons 'SHOTCRETE-CHK shotcrete-chk) (assoc 'SHOTCRETE-CHK new-item) new-item)) (setq new-item (append new-item (list (cons 'SHOTCRETE-CHK shotcrete-chk)))))

        (if (assoc 'SHOTCRETE-LAYER new-item) (setq new-item (subst (cons 'SHOTCRETE-LAYER shotcrete-layer) (assoc 'SHOTCRETE-LAYER new-item) new-item)) (setq new-item (append new-item (list (cons 'SHOTCRETE-LAYER shotcrete-layer)))))

        (if (assoc 'SHOTCRETE-THICK new-item) (setq new-item (subst (cons 'SHOTCRETE-THICK shotcrete-thick) (assoc 'SHOTCRETE-THICK new-item) new-item)) (setq new-item (append new-item (list (cons 'SHOTCRETE-THICK shotcrete-thick)))))



        (setq new-list (append new-list (list new-item)))

      )

    )

    (setq i (1+ i))

  )

  (setq *segment-list* new-list)

  (setq *tsp-data-dirty* T)

)



;;; --------------------------------------------------------------------------

;;; Function: tsp-apply-support-to-all

;;; Description: 선택된 세그먼트의 지보재 정보 일괄 적용

;;; --------------------------------------------------------------------------

(defun tsp-apply-support-to-all (src-idx / src-data src-support i seg-data new-data)

  (setq src-data (nth src-idx *segment-list*))

  (setq src-support (cdr (assoc 'SUPPORT-LIST src-data)))

  

  (setq i 0)

  (setq new-data '())

  

  (foreach seg *segment-list*

    (if (= i src-idx)

      (setq new-data (append new-data (list seg))) ;; 원본은 그대로 유지

      (progn

        ;; 다른 세그먼트의 SUPPORT-LIST를 원본 값으로 교체

        (setq seg (subst (cons 'SUPPORT-LIST src-support) (assoc 'SUPPORT-LIST seg) seg))

        (setq new-data (append new-data (list seg)))

      )

    )

    (setq i (1+ i))

  )

  

  (setq *segment-list* new-data)

  (setq *tsp-data-dirty* T)

)



;;; --------------------------------------------------------------------------

;;; Function: tsp-reset-segment-data

;;; Description: 특정 세그먼트의 설정을 초기화

;;; --------------------------------------------------------------------------

(defun tsp-reset-segment-data (idx / old-data new-data)

  (setq old-data (nth idx *segment-list*))

  (setq new-data (list

    (cons 'ID (cdr (assoc 'ID old-data)))          

    (cons 'NAME (strcat "Seg-" (itoa (1+ idx))))   

    (cons 'IS-DEFINED nil)                        

    (cons 'SECTION-DRAW nil)                      

    (cons 'SOIL-DATA nil)                         

    (cons 'WALL-SPEC *tsp-hpile-spec*)            

    (cons 'WALL-CUSTOM '(300 300 10 15))           

    (cons 'WALE-SPEC *tsp-wale-spec*)           

    (cons 'WALE-CUSTOM '(300 300 10 15))          

    (cons 'CTC 2.0)                

    (cons 'TIMBER-THICKNESS 60)

    (cons 'MAX-DEPTH 10.0)

    (cons 'EMBED-DEPTH 3.0)

    (cons 'WATER-CHK "0")

    (cons 'WATER-DEPTH 0.0)

    (cons 'SHOTCRETE-ENABLE "1")

    (cons 'SHOTCRETE-CHK "0")

    (cons 'SHOTCRETE-LAYER "")

    (cons 'SHOTCRETE-THICK 0.0)

    (cons 'SUPPORT-LIST '())

    (cons 'CORNER-BRACE (cdr (assoc 'CORNER-BRACE old-data)))

    (cons 'UPGRADE-WALE nil)

    (cons 'V-START (cdr (assoc 'V-START old-data)))

    (cons 'V-END (cdr (assoc 'V-END old-data))) 

    (cons 'ANGLE (cdr (assoc 'ANGLE old-data)))   

    (cons 'LENGTH (cdr (assoc 'LENGTH old-data)))  

  ))

  (setq *segment-list* (update-list-item *segment-list* idx new-data))

  (setq *tsp-data-dirty* T)

)



;;; --------------------------------------------------------------------------

;;; Function: tsp-reset-all-segments

;;; Description: 모든 세그먼트의 설정을 일괄 초기화

;;; --------------------------------------------------------------------------

(defun tsp-reset-all-segments (/ i)

  (setq i 0)

  (repeat (length *segment-list*)

    (tsp-reset-segment-data i)

    (setq i (1+ i))

  )

)



;;; --------------------------------------------------------------------------

;;; Function: scale-dialog-callback

;;; Description: 스케일 설정 대화상자 콜백

;;; --------------------------------------------------------------------------

(defun scale-dialog-callback (dcl-path / dcl-id result s-sec s-pln)

  (setq dcl-id (load_dialog dcl-path))

  (if (new_dialog "tsp_scale" dcl-id)

    (progn

      (set_tile "scale_section" (itoa *tsp-scale-section*)) 

      (set_tile "scale_plan" (itoa *tsp-scale-plan*))

      

      (action_tile "accept" 

        "(progn 

           (setq s-sec (get_tile \"scale_section\")) 

           (setq s-pln (get_tile \"scale_plan\")) 

           (if (and (is-numeric s-sec) (is-numeric s-pln)) 

             (progn 

               (setq *tsp-scale-section* (atoi s-sec)) 

               (setq *tsp-scale-plan* (atoi s-pln)) 

               (done_dialog 1)) 

             (alert \"스케일은 숫자를 입력해주세요.\")

           )

         )"

      )

      

      (action_tile "cancel" "(done_dialog 0)")

      

      (setq result (start_dialog)) 

      (unload_dialog dcl-id) 

      result

    )

  )

)



;;; --------------------------------------------------------------------------

;;; Function: project-list-callback

;;; Description: 프로젝트 불러오기 및 삭제 목록 팝업 

;;; --------------------------------------------------------------------------

(defun project-list-callback (dcl-path / dcl-id result proj-list sel-idx refresh-proj-list)

  (setq dcl-id (load_dialog dcl-path))

  (setq proj-list (tsp-get-project-list))

  (if (null proj-list)

    (progn (alert "저장된 프로젝트가 없습니다.") (unload_dialog dcl-id) nil)

    (if (new_dialog "tsp_load_list" dcl-id)

      (progn

        ;; 내부 함수: 목록 새로고침

        (defun refresh-proj-list ()

          (setq proj-list (tsp-get-project-list))

          (start_list "proj_list")

          (mapcar 'add_list proj-list)

          (end_list)

        )

        (refresh-proj-list)

        

        (action_tile "accept" 

          "(progn 

             (setq sel-idx (atoi (get_tile \"proj_list\"))) 

             (if (and (>= sel-idx 0) (< sel-idx (length proj-list)))

               (progn

                 (tsp-load-project-data (nth sel-idx proj-list))

                 (done_dialog 1)

               )

               (alert \"불러올 목록을 선택해주세요.\")

             )

           )"

        )

        (action_tile "btn_del_proj" 

          "(progn 

             (setq sel-idx (atoi (get_tile \"proj_list\"))) 

             (if (and (>= sel-idx 0) (< sel-idx (length proj-list)))

               (progn

                 (tsp-delete-project-data (nth sel-idx proj-list))

                 (refresh-proj-list) ;; 삭제 후 목록 리프레쉬

               )

               (alert \"삭제할 목록을 선택해주세요.\")

             )

           )"

        )

        (action_tile "cancel" "(done_dialog 0)")

        

        (setq result (start_dialog))

        (unload_dialog dcl-id)

        (= result 1) ; 로드 성공 여부 반환 (T/nil)

      )

    )

  )

)



;;; --------------------------------------------------------------------------

;;; Function: main-dialog-callback

;;; Description: 메인 선택 대화상자 콜백

;;; --------------------------------------------------------------------------

(defun main-dialog-callback (dcl-path / dcl-id result)

  (setq dcl-id (load_dialog dcl-path))

  (if (new_dialog "tsp_main" dcl-id)

    (progn

      ;; [3단계] H-Pile: 즉시 "HPILE" 확정
      (action_tile "btn_hpile" "(progn (setq *tsp-wall-type* \"HPILE\") (done_dialog 1))")

      ;; [3단계] C.I.P: wall-type 건드리지 않고 step만 넘김 (확정은 CIP 다이얼로그 OK에서만)
      (action_tile "btn_cip" "(done_dialog 11)")

      (action_tile "btn_scw" "(alert \"SCW 공법은 아직 구현되지 않았습니다.\")")

      (action_tile "back" "(done_dialog 2)") 

      (action_tile "cancel" "(done_dialog 0)")

      (setq result (start_dialog)) 

      (unload_dialog dcl-id) 

      result

    )

  )

)



;;; --------------------------------------------------------------------------

;;; Function: update-list-item

;;; Description: 리스트의 특정 인덱스(idx) 요소를 new-val로 안전하게 교체

;;; --------------------------------------------------------------------------

(defun update-list-item (lst idx new-val / i new-list)

  (setq i 0 new-list '())

  (foreach item lst

    (if (= i idx)

      (setq new-list (append new-list (list new-val)))

      (setq new-list (append new-list (list item)))

    )

    (setq i (1+ i))

  )

  new-list

)



;;; --------------------------------------------------------------------------

;;; Function: input-segment-wizard

;;; Description: [입력 모드] 단계별 마법사 방식

;;; --------------------------------------------------------------------------

(defun input-segment-wizard (dcl-path idx / seg-data current-draw current-name sub-status step input-name dcl-id b-data max-limit new-brace temp-seg-data limits res cur-idx brace-loop found-valid search-idx loop-count max-loop check-b)

  (load-segment-to-globals idx)

  (setq seg-data (nth idx *segment-list*))

  ;; [2단계 고정] 신규 세그먼트 진입 시 wall-type을 "HPILE"로 초기화
  ;; 이전 세그먼트의 C.I.P 상태가 다음 세그먼트로 새는 것을 방지
  ;; (기존 세그먼트 수정 시에는 load-segment-to-globals가 저장값을 복원하므로
  ;;  IS-DEFINED=T인 경우는 건드리지 않음)
  (if (not (cdr (assoc 'IS-DEFINED seg-data)))
    (setq *tsp-wall-type* "HPILE")
  )

  (setq current-draw (cdr (assoc 'SECTION-DRAW seg-data)))

  

  (if (and (not (cdr (assoc 'IS-DEFINED seg-data)))

           *tsp-soil-layers*

           (/= *tsp-soil-layers* '()))

    (setq current-draw T)

  )

  

  (setq current-name (cdr (assoc 'NAME seg-data)))

  (setq b-data (cdr (assoc 'CORNER-BRACE seg-data))) 

  

  (setq step 1) 

  

  ;; [step 41 허용] CIP 다이얼로그 step(=41)도 루프 내에서 처리되도록 조건 확장
  (while (and (>= step 1) (or (< step 7) (= step 41)))

    (if *tsp-boundary-ent* (tsp-zoom-to-right-half (extract-vertices *tsp-boundary-ent*)))

    

    (setq sub-status 0)

    (cond

      ((= step 1)

        (setq dcl-id (load_dialog dcl-path))

        (if (new_dialog "tsp_section_mode" dcl-id)

          (progn

            (set_tile "dlg_title" (strcat current-name " 입력 (단계별)"))

            (set_tile "seg_name" current-name)

            (set_tile "chk_section" (if current-draw "1" "0"))

            (action_tile "accept" "(setq input-name (get_tile \"seg_name\")) (setq current-draw (= (get_tile \"chk_section\") \"1\")) (done_dialog 1)")

            (action_tile "cancel" "(done_dialog 0)")

            (setq sub-status (start_dialog))

          )

        )

        (unload_dialog dcl-id)

        (if (= sub-status 1)

          (progn

            (if (and input-name (/= input-name "")) (setq current-name input-name))

            (if current-draw (setq step 2) (setq step 3))

          )

          (setq step -1)

        )

      )

      ((= step 2) (setq sub-status (soil-dialog-callback dcl-path)) (if (= sub-status 1) (setq step 3) (if (= sub-status 2) (setq step 1) (setq step -1))))

      ((= step 3) 

        (setq sub-status (main-dialog-callback dcl-path)) 

        (cond 

          ;; H-Pile 선택: main-dialog-callback에서 이미 "HPILE" 확정됨
          ((= sub-status 1) (setq step 4)) 

          ;; C.I.P 선택: step 41로만 전환, wall-type은 CIP 다이얼로그 OK에서만 "CIP" 확정
          ((= sub-status 11) (setq step 41)) 

          ((= sub-status 2) (if current-draw (setq step 2) (setq step 1))) 

          (t (setq step -1))

        )

      )

      ((= step 4) (setq sub-status (hpile-dialog-callback dcl-path)) (if (= sub-status 1) (setq step 5) (if (= sub-status 2) (setq step 3) (setq step -1))))

      ((= step 41) (setq sub-status (tsp-run-cip-dialog)) (if (= sub-status 1) (setq step 5) (if (= sub-status 2) (setq step 3) (setq step -1))))

      

      ((= step 5)

       (setq sub-status (support-dialog-callback dcl-path)) 

       (if (= sub-status 1) 

         (if (and b-data (= (car b-data) "Y")) 

           (setq step 6) 

           (setq step 7)) 

         ;; [수정] 이전(Back) 버튼 클릭 시 현재 공법에 맞는 창으로 되돌아감

         (if (= sub-status 2) 

           (if (= *tsp-wall-type* "CIP") (setq step 41) (setq step 4)) 

           (setq step -1)

         )

       )

      )

      

      ((= step 6)

       (setq cur-idx idx)

       (setq brace-loop T)

       (while brace-loop

         (setq temp-seg-data (nth cur-idx *segment-list*))

         (setq limits (tsp-calculate-brace-limit cur-idx))

         

         (setq res (brace-dialog-callback dcl-path temp-seg-data limits 0))

         

         (if (and res (listp res))

           (progn

             (setq b-data (cadr res))

             (setq temp-seg-data (subst (cons 'CORNER-BRACE b-data) (assoc 'CORNER-BRACE temp-seg-data) temp-seg-data))

             (setq temp-seg-data (subst (cons 'UPGRADE-WALE (caddr res)) (assoc 'UPGRADE-WALE temp-seg-data) temp-seg-data))

             (setq *segment-list* (update-list-item *segment-list* cur-idx temp-seg-data))

             

             (if (and *tsp-boundary-ent* *tsp-boundary-orient*)

               (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*)

             )

             

             (if (= (car res) "NEXT")

               (progn

                 (setq found-valid nil search-idx cur-idx loop-count 0 max-loop (length *segment-list*))

                 (while (and (not found-valid) (< loop-count max-loop))

                   (setq search-idx (1+ search-idx))

                   (if (>= search-idx max-loop)

                     (if (is-closed-polyline *tsp-boundary-ent*)

                       (setq search-idx 0)

                       (setq search-idx 9999) 

                     )

                   )

                   (if (< search-idx max-loop)

                     (progn

                       (setq check-b (cdr (assoc 'CORNER-BRACE (nth search-idx *segment-list*))))

                       (if (not (and check-b (= (car check-b) "N")))

                         (setq found-valid T)

                       )

                     )

                   )

                   (setq loop-count (1+ loop-count))

                 )

                 

                 (if found-valid

                   (setq cur-idx search-idx)

                   (progn

                     (alert "더 이상 사보강재를 입력할 유효한 모서리가 없습니다.\n입력을 완료합니다.")

                     (setq brace-loop nil)

                   )

                 )

               )

               (setq brace-loop nil) 

             )

             (setq step 7) 

           )

           (progn

             (setq brace-loop nil)

             (setq step -1) 

           )

         )

       )

      )

    )

  )

  

  (if (= step 7)

    (progn

      (setq seg-data (nth idx *segment-list*))

      (setq seg-data (subst (cons 'NAME current-name) (assoc 'NAME seg-data) seg-data))

      (setq seg-data (subst (cons 'SECTION-DRAW current-draw) (assoc 'SECTION-DRAW seg-data) seg-data))

      (setq *segment-list* (update-list-item *segment-list* idx seg-data))

      (sync-globals-to-segment idx)

      

      (setq *tsp-data-dirty* T)

    )

  )

)



;;; --------------------------------------------------------------------------

;;; Function: modify-segment-dashboard

;;; Description: [수정 모드] 통합 메뉴 대시보드 방식

;;; --------------------------------------------------------------------------

(defun modify-segment-dashboard (dcl-path idx / seg-data current-draw current-name loop-active menu-status input-name sub-status wall-choice dcl-id brace-data new-brace max-limit limits res cur-idx brace-loop temp-seg-data found-valid search-idx loop-count max-loop check-b)

  (load-segment-to-globals idx)

  (setq seg-data (nth idx *segment-list*))

  

  (setq current-draw (cdr (assoc 'SECTION-DRAW seg-data)))

  (if (and (not (cdr (assoc 'IS-DEFINED seg-data)))

           *tsp-soil-layers*

           (/= *tsp-soil-layers* '()))

    (setq current-draw T)

  )

  (setq current-name (cdr (assoc 'NAME seg-data)))

  

  (setq loop-active T)

  (while loop-active

    (if *tsp-boundary-ent* (tsp-zoom-to-right-half (extract-vertices *tsp-boundary-ent*)))

    

    (setq seg-data (nth idx *segment-list*))

    (setq dcl-id (load_dialog dcl-path))

    (if (new_dialog "tsp_edit_menu" dcl-id)

      (progn

        (set_tile "menu_title" (strcat current-name " 설정 수정"))

        (set_tile "seg_name" current-name)

        (set_tile "chk_section" (if current-draw "1" "0"))

        

        (setq brace-data (cdr (assoc 'CORNER-BRACE seg-data)))

        (if (and brace-data (= (car brace-data) "Y"))

          (mode_tile "btn_brace" 0)

          (mode_tile "btn_brace" 1)

        )

        

        (action_tile "btn_soil"    "(setq current-name (get_tile \"seg_name\")) (setq current-draw (= (get_tile \"chk_section\") \"1\")) (done_dialog 2)")

        (action_tile "btn_wall"    "(setq current-name (get_tile \"seg_name\")) (setq current-draw (= (get_tile \"chk_section\") \"1\")) (done_dialog 3)")

        (action_tile "btn_support" "(setq current-name (get_tile \"seg_name\")) (setq current-draw (= (get_tile \"chk_section\") \"1\")) (done_dialog 4)")

        (action_tile "btn_brace"   "(setq current-name (get_tile \"seg_name\")) (setq current-draw (= (get_tile \"chk_section\") \"1\")) (done_dialog 5)")

        

        (action_tile "accept"      "(setq current-name (get_tile \"seg_name\")) (setq current-draw (= (get_tile \"chk_section\") \"1\")) (done_dialog 1)")

        (action_tile "cancel"      "(done_dialog 0)")

        

        (setq menu-status (start_dialog))

      )

      (setq menu-status 0)

    )

    (unload_dialog dcl-id)

    

    (cond

      ((= menu-status 1) (setq loop-active nil))

      ((= menu-status 0) (setq loop-active nil) (princ "\n수정이 취소되었습니다."))

      

      ((= menu-status 2)

       (soil-dialog-callback dcl-path)

       (if (and *tsp-soil-layers* (/= *tsp-soil-layers* '())) (setq current-draw T))

      )

      

      ((= menu-status 3)

       (setq wall-choice (main-dialog-callback dcl-path))

       (cond

         ((= wall-choice 1) (hpile-dialog-callback dcl-path))

         ((= wall-choice 11) (tsp-run-cip-dialog))

       )

      )

      

      ((= menu-status 4) (support-dialog-callback dcl-path))

      

      ((= menu-status 5)

       (setq cur-idx idx)

       (setq brace-loop T)

       (while brace-loop

         (setq temp-seg-data (nth cur-idx *segment-list*))

         (setq limits (tsp-calculate-brace-limit cur-idx))

         

         (setq res (brace-dialog-callback dcl-path temp-seg-data limits 0))

         

         (if (and res (listp res))

           (progn

             (setq temp-seg-data (subst (cons 'CORNER-BRACE (cadr res)) (assoc 'CORNER-BRACE temp-seg-data) temp-seg-data))

             (setq temp-seg-data (subst (cons 'UPGRADE-WALE (caddr res)) (assoc 'UPGRADE-WALE temp-seg-data) temp-seg-data))

             (setq *segment-list* (update-list-item *segment-list* cur-idx temp-seg-data))

             

             (if (and *tsp-boundary-ent* *tsp-boundary-orient*)

               (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*)

             )

             

             (if (= (car res) "NEXT")

               (progn

                 (setq found-valid nil search-idx cur-idx loop-count 0 max-loop (length *segment-list*))

                 (while (and (not found-valid) (< loop-count max-loop))

                   (setq search-idx (1+ search-idx))

                   (if (>= search-idx max-loop)

                     (if (is-closed-polyline *tsp-boundary-ent*)

                       (setq search-idx 0)

                       (setq search-idx 9999) 

                     )

                   )

                   (if (< search-idx max-loop)

                     (progn

                       (setq check-b (cdr (assoc 'CORNER-BRACE (nth search-idx *segment-list*))))

                       (if (not (and check-b (= (car check-b) "N")))

                         (setq found-valid T)

                       )

                     )

                   )

                   (setq loop-count (1+ loop-count))

                 )

                 

                 (if found-valid

                   (setq cur-idx search-idx)

                   (progn

                     (alert "더 이상 사보강재를 수정할 유효한 모서리가 없습니다.\n수정을 완료합니다.")

                     (setq brace-loop nil)

                   )

                 )

               )

               (setq brace-loop nil) 

             )

           )

           (setq brace-loop nil) 

         )

       )

       (setq seg-data (nth idx *segment-list*))

      )

    )

  )

  

  (if (= menu-status 1)

    (progn

      (setq seg-data (nth idx *segment-list*))

      (setq seg-data (subst (cons 'NAME current-name) (assoc 'NAME seg-data) seg-data))

      (setq seg-data (subst (cons 'SECTION-DRAW current-draw) (assoc 'SECTION-DRAW seg-data) seg-data))

      (setq *segment-list* (update-list-item *segment-list* idx seg-data))

      (sync-globals-to-segment idx)

      (setq *tsp-data-dirty* T)

      (princ "\n설정이 저장되었습니다.")

    )

  )

)



;;; --------------------------------------------------------------------------

;;; Function: format-h-spec-short

;;; Description: H형강 규격을 폭x높이 형태로 짧게 변환하고 2중 표기 적용

;;; --------------------------------------------------------------------------

(defun format-h-spec-short (spec-val custom-val double-prefix / res parsed)

  (setq res "")

  (if (or (= spec-val "User-defined") (= spec-val "Custom"))

    (setq res (strcat "H " (itoa (nth 0 custom-val)) "x" (itoa (nth 1 custom-val))))

    (progn

      (setq parsed (parse-h-spec spec-val))

      (if parsed

         (setq res (strcat "H " (itoa (nth 0 parsed)) "x" (itoa (nth 1 parsed))))

         (setq res spec-val)

      )

    )

  )

  (if double-prefix

    (setq res (strcat "2" res))

  )

  res

)



;;; --------------------------------------------------------------------------

;;; Function: get-segment-display-string

;;; Description: 세그먼트 데이터를 리스트박스 표시용 문자열로 변환 (공법 분기 적용)

;;; --------------------------------------------------------------------------

(defun get-segment-display-string (idx seg-data / s-id s-name is-defined s-draw s-support wall-str support-str s-brace-data s-brace-str s-spec s-custom wall-short w-spec w-custom wale-short has-anchor-str brace-short b-row-list num-b-rows first-b-row w-type cip-dia cip-hidx-raw cip-hidx-val cip-hidx-safe cip-widx-raw cip-widx-val cip-widx-safe)

  (setq s-id (strcat (itoa (1+ idx))))

  (setq s-name (cdr (assoc 'NAME seg-data)))

  (setq is-defined (cdr (assoc 'IS-DEFINED seg-data)))

  

  (setq s-brace-data (cdr (assoc 'CORNER-BRACE seg-data)))

  (setq s-brace-str "")

  (if s-brace-data

    (if (= (car s-brace-data) "Y")

      (progn

        (setq b-row-list (cadr s-brace-data))

        (setq num-b-rows (length b-row-list))

        (if (> num-b-rows 0)

          (progn

            (setq first-b-row (car b-row-list))

            (setq brace-short (format-h-spec-short (car first-b-row) (cadr first-b-row) nil))

            (setq s-brace-str (strcat "Y / " (itoa num-b-rows) "열 (" brace-short ")"))

          )

          (setq s-brace-str "Y (미입력)")

        )

      )

      (setq s-brace-str "N")

    )

    (setq s-brace-str "-")

  )

  

  (if is-defined

    (progn

      (if (cdr (assoc 'SECTION-DRAW seg-data)) (setq s-draw "Y") (setq s-draw "N"))

      

      (setq s-support (cdr (assoc 'SUPPORT-LIST seg-data)))

      (setq support-str "")

      (setq has-anchor-str nil)

      (if s-support

        (foreach item s-support

          (if (wcmatch (car item) "앵커*") (setq has-anchor-str T))

          (if (= support-str "")

            (setq support-str (car item))

            (if (not (vl-string-search (car item) support-str))

               (setq support-str (strcat support-str ", " (car item)))

            )

          )

        )

        (setq support-str "-")

      )



      ;; [7단계] 벽체 타입(WALL-TYPE)에 따라 출력 텍스트 분기 (nil 안전 처리 포함)

      (setq w-type (cdr (assoc 'WALL-TYPE seg-data)))

      (if (not w-type) (setq w-type "HPILE")) ; 과거 데이터 호환성

      

      (if (= w-type "CIP")

        (progn

          ;; [7단계 수정] CIP-HPILE-IDX nil 가드 (let* 제거 → setq 순차 방식)
          (setq cip-hidx-raw  (cdr (assoc 'CIP-HPILE-IDX seg-data)))
          (setq cip-hidx-val  (if cip-hidx-raw (atoi cip-hidx-raw) 0))
          (setq cip-hidx-safe (if (and (>= cip-hidx-val 0)
                                       (< cip-hidx-val (length *tsp-std-wall-list*)))
                                  cip-hidx-val 0))
          (setq s-spec (nth cip-hidx-safe *tsp-std-wall-list*))

          (setq wall-short (format-h-spec-short s-spec nil nil))

          ;; [7단계 수정] CIP-DIA nil 가드
          (setq cip-dia (cdr (assoc 'CIP-DIA seg-data)))
          (if (not cip-dia) (setq cip-dia "450"))

          ;; [7단계 수정] CIP-WALE-IDX nil 가드 (let* 제거 → setq 순차 방식)
          (setq cip-widx-raw  (cdr (assoc 'CIP-WALE-IDX seg-data)))
          (setq cip-widx-val  (if cip-widx-raw (atoi cip-widx-raw) 0))
          (setq cip-widx-safe (if (and (>= cip-widx-val 0)
                                       (< cip-widx-val (length *tsp-std-wall-list*)))
                                  cip-widx-val 0))
          (setq w-spec (nth cip-widx-safe *tsp-std-wall-list*))

          (setq wale-short (format-h-spec-short w-spec nil has-anchor-str))

          (setq wall-str (strcat "D" cip-dia "(" wall-short ") / " wale-short))

          (strcat s-id "\t" s-name "\t" s-draw "\t" "CIP (현장타설)" "\t" wall-str "\t" support-str "\t" s-brace-str)

        )

        (progn ; HPILE

          (setq s-spec (cdr (assoc 'WALL-SPEC seg-data)))

          (setq s-custom (cdr (assoc 'WALL-CUSTOM seg-data)))

          (setq wall-short (format-h-spec-short s-spec s-custom nil))

          (setq w-spec (cdr (assoc 'WALE-SPEC seg-data)))

          (setq w-custom (cdr (assoc 'WALE-CUSTOM seg-data)))

          (setq wale-short (format-h-spec-short w-spec w-custom has-anchor-str))

          (setq wall-str (strcat wall-short " / " wale-short))

          (strcat s-id "\t" s-name "\t" s-draw "\t" "H-Pile+토류판" "\t" wall-str "\t" support-str "\t" s-brace-str)

        )

      )

    )

    ;; [7단계] 미정의 세그먼트: WALL-TYPE 읽어서 공법명 표시 (내부 상태와 UI 동기화)
    (progn

      (setq w-type (cdr (assoc 'WALL-TYPE seg-data)))
      (if (not w-type) (setq w-type "HPILE"))

      (strcat s-id "\t" s-name "\t" "-" "\t"
              (if (= w-type "CIP") "CIP (현장타설)" "H-Pile+토류판")
              "\t" "(미설정)" "\t" "-" "\t" s-brace-str)

    )

  )

)



;;; --------------------------------------------------------------------------

;;; Function: reset-globals-to-defaults

;;; Description:  입력 버튼 클릭 시 전역 변수를 시스템 기본값으로 초기화

;;; --------------------------------------------------------------------------

(defun reset-globals-to-defaults ()

  ;; [1단계 고정] 공법 타입을 항상 HPILE로 초기화
  ;; C.I.P는 사용자가 다이얼로그에서 최종 OK를 눌렀을 때만 "CIP"로 변경됨
  (setq *tsp-wall-type* "HPILE")

  (setq *tsp-hpile-spec* (nth 0 *tsp-std-wall-list*))

  (setq *tsp-hpile-custom* '(298 201 9 14))

  (setq *tsp-wale-spec* (nth 1 *tsp-std-wall-list*))

  (setq *tsp-wale-custom* '(300 300 10 15))

  (setq *tsp-ctc* 2.0)

  (setq *tsp-timber-thickness* 60)

  (setq *tsp-max-excavation-depth* 10.0)

  (setq *tsp-embedment-depth* 3.0)

  (setq *tsp-soil-layers* '())

  (setq *tsp-water-chk* "0")

  (setq *tsp-water-depth* 0.0)

  (setq *tsp-shotcrete-enable* "1")

  (setq *tsp-shotcrete-chk* "0")

  (setq *tsp-shotcrete-layer* "")

  (setq *tsp-shotcrete-thick* 0.0)

  (setq *tsp-support-list* '())

)



;;; --------------------------------------------------------------------------

;;; Function: update-segment-manager-list

;;; Description: 관리자 리스트박스 갱신

;;; --------------------------------------------------------------------------

(defun update-segment-manager-list ()

  (start_list "seg_list")

  (setq i 0)

  (foreach seg *segment-list*

    (add_list (get-segment-display-string i seg))

    (setq i (1+ i))

  )

  (end_list)

)



;;; --------------------------------------------------------------------------

;;; Function: sync-globals-to-segment

;;; Description: 전역 변수(DCL입력값)를 세그먼트 데이터 리스트로 저장

;;; --------------------------------------------------------------------------

(defun sync-globals-to-segment (idx / old-data new-data)

  (setq old-data (nth idx *segment-list*))

  (setq new-data (list

    (cons 'ID (cdr (assoc 'ID old-data)))

    (cons 'NAME (cdr (assoc 'NAME old-data)))

    (cons 'IS-DEFINED T) 

    (cons 'WALL-TYPE (if *tsp-wall-type* *tsp-wall-type* "HPILE")) ; 벽체 공법 타입 추가

    (cons 'SECTION-DRAW (cdr (assoc 'SECTION-DRAW old-data)))

    (cons 'SOIL-DATA *tsp-soil-layers*)

    (cons 'WALL-SPEC *tsp-hpile-spec*)

    (cons 'WALL-CUSTOM *tsp-hpile-custom*)

    (cons 'WALE-SPEC *tsp-wale-spec*)

    (cons 'WALE-CUSTOM *tsp-wale-custom*)

    (cons 'CTC *tsp-ctc*)

    (cons 'TIMBER-THICKNESS *tsp-timber-thickness*)

    (cons 'MAX-DEPTH *tsp-max-excavation-depth*)

    (cons 'EMBED-DEPTH *tsp-embedment-depth*)

    ;; === [6단계] C.I.P 전용 데이터 저장 (세그먼트별 독립 보관) ===
    ;; 이 9개 키가 세그먼트에 저장되어야 load-segment-to-globals로 완전 복원 가능

    (cons 'CIP-MAX-DEPTH *tsp-cip-max-depth*)

    (cons 'CIP-EMBED-DEPTH *tsp-cip-embed-depth*)

    (cons 'CIP-HPILE-IDX *tsp-cip-hpile-idx*)

    (cons 'CIP-WALE-IDX *tsp-cip-wale-idx*)

    (cons 'CIP-DIA *tsp-cip-dia*)

    (cons 'CIP-MODE-IDX *tsp-cip-mode-idx*)

    (cons 'CIP-OVERLAP *tsp-cip-overlap*)

    (cons 'CIP-INTERVAL-IDX *tsp-cip-interval-idx*)

    ;; === [6단계] C.I.P 전용 데이터 저장 끝 ===

    (cons 'WATER-CHK *tsp-water-chk*)

    (cons 'WATER-DEPTH *tsp-water-depth*)

    (cons 'SHOTCRETE-ENABLE *tsp-shotcrete-enable*)

    (cons 'SHOTCRETE-CHK *tsp-shotcrete-chk*)

    (cons 'SHOTCRETE-LAYER *tsp-shotcrete-layer*)

    (cons 'SHOTCRETE-THICK *tsp-shotcrete-thick*)

    (cons 'SUPPORT-LIST *tsp-support-list*)

    (cons 'CORNER-BRACE (cdr (assoc 'CORNER-BRACE old-data)))

    (cons 'UPGRADE-WALE (cdr (assoc 'UPGRADE-WALE old-data)))

    (cons 'V-START (cdr (assoc 'V-START old-data)))

    (cons 'V-END (cdr (assoc 'V-END old-data)))

    (cons 'ANGLE (cdr (assoc 'ANGLE old-data)))

    (cons 'LENGTH (cdr (assoc 'LENGTH old-data)))

  ))

  (setq *segment-list* (update-list-item *segment-list* idx new-data))

)



;;; --------------------------------------------------------------------------

;;; Function: load-segment-to-globals

;;; Description: 세그먼트 데이터를 전역 변수로 불러오기

;;; --------------------------------------------------------------------------

(defun load-segment-to-globals (idx / seg-data val)

  (setq seg-data (nth idx *segment-list*))

  

  ;; 벽체 타입 복원 (과거 데이터 호환을 위해 없으면 HPILE로 간주)

  (setq val (cdr (assoc 'WALL-TYPE seg-data)))

  (setq *tsp-wall-type* (if val val "HPILE"))



  (setq *tsp-soil-layers* (cdr (assoc 'SOIL-DATA seg-data)))

  (setq *tsp-hpile-spec* (cdr (assoc 'WALL-SPEC seg-data)))

  (setq *tsp-hpile-custom* (cdr (assoc 'WALL-CUSTOM seg-data)))

  (setq *tsp-wale-spec* (cdr (assoc 'WALE-SPEC seg-data)))

  (setq *tsp-wale-custom* (cdr (assoc 'WALE-CUSTOM seg-data)))

  (setq *tsp-ctc* (cdr (assoc 'CTC seg-data)))

  (setq *tsp-support-list* (cdr (assoc 'SUPPORT-LIST seg-data)))

  (setq val (cdr (assoc 'TIMBER-THICKNESS seg-data)))

  (setq *tsp-timber-thickness* (if val val 60))

  (setq val (cdr (assoc 'MAX-DEPTH seg-data)))

  (setq *tsp-max-excavation-depth* (if val val 10.0))

  (setq val (cdr (assoc 'EMBED-DEPTH seg-data)))

  (setq *tsp-embedment-depth* (if val val 3.0))



  ;; === [6단계] C.I.P 전용 데이터 복원 (폴백 기본값은 init-segment-data와 통일) ===

  (setq val (cdr (assoc 'CIP-MAX-DEPTH seg-data))) (setq *tsp-cip-max-depth* (if val val "10.0"))

  (setq val (cdr (assoc 'CIP-EMBED-DEPTH seg-data))) (setq *tsp-cip-embed-depth* (if val val "3.0"))

  (setq val (cdr (assoc 'CIP-HPILE-IDX seg-data))) (setq *tsp-cip-hpile-idx* (if val val "0"))  ; [6단계] 기본값 통일: "2"->"0" (init-segment-data 일치)

  (setq val (cdr (assoc 'CIP-WALE-IDX seg-data))) (setq *tsp-cip-wale-idx* (if val val "0"))  ; [6단계] 기본값 통일: "2"->"0"

  (setq val (cdr (assoc 'CIP-DIA seg-data))) (setq *tsp-cip-dia* (if val val "450"))  ; [6단계] 기본값 통일: "500"->"450" (init-segment-data 일치)

  (setq val (cdr (assoc 'CIP-MODE-IDX seg-data))) (setq *tsp-cip-mode-idx* (if val val "0"))

  (setq val (cdr (assoc 'CIP-OVERLAP seg-data))) (setq *tsp-cip-overlap* (if val val "0"))  ; [6단계] 기본값 통일: "50"->"0"

  (setq val (cdr (assoc 'CIP-INTERVAL-IDX seg-data))) (setq *tsp-cip-interval-idx* (if val val "0"))

  ;; === [6단계] C.I.P 전용 데이터 복원 끝 ===



  (setq val (cdr (assoc 'WATER-CHK seg-data)))

  (setq *tsp-water-chk* (if val val "0"))

  (setq val (cdr (assoc 'WATER-DEPTH seg-data)))

  (setq *tsp-water-depth* (if val val 0.0))

  (setq val (cdr (assoc 'SHOTCRETE-CHK seg-data)))

  (setq *tsp-shotcrete-chk* (if val val "0"))

  (setq val (cdr (assoc 'SHOTCRETE-LAYER seg-data)))

  (setq *tsp-shotcrete-layer* (if val val ""))

  (setq val (cdr (assoc 'SHOTCRETE-THICK seg-data)))

  (setq *tsp-shotcrete-thick* (if val val 0.0))

  (setq val (cdr (assoc 'SHOTCRETE-ENABLE seg-data)))

  (setq *tsp-shotcrete-enable* (if val val "1"))

)



;;; --------------------------------------------------------------------------

;;; Function: manager-dialog-callback

;;; Description: [메인] 세그먼트 관리자 DCL 콜백

;;; --------------------------------------------------------------------------

(defun manager-dialog-callback (dcl-path / dcl-id result paste-mode apply-all-mode apply-soil-mode apply-support-mode source-idx target-idx source-data target-data loop-active status-msg-text is-def max-idx check-data new-target load-result saved-list clicked-idx save-opt new-name)

  (setq selected-idx 0)

  

  ;; 모드 플래그 초기화

  (setq paste-mode nil)

  (setq apply-all-mode nil)

  (setq apply-soil-mode nil)

  (setq apply-support-mode nil)

  

  (setq loop-active T)

  (setq status-msg-text "목록을 선택하고 입력/수정 버튼을 누르세요.")

  

  (while loop-active

    (setq dcl-id (load_dialog dcl-path))

    (if (new_dialog "tsp_manager" dcl-id "" '(50 50))

      (progn

        (setq saved-list (tsp-get-project-list))

        (if (or (null *tsp-current-project-name*) (= *tsp-current-project-name* ""))

           (setq *tsp-current-project-name* (tsp-generate-default-project-name))

        )

        (set_tile "manager_title" (strcat "세그먼트 설정 (Segment Manager) - " *tsp-current-project-name*))

        (set_tile "project_name" *tsp-current-project-name*)

        

        (update-segment-manager-list)

        (set_tile "seg_list" (itoa selected-idx))

        (set_tile "status_msg" status-msg-text)

        

        (action_tile "project_name" "(set_tile \"manager_title\" (strcat \"세그먼트 설정 (Segment Manager) - \" $value))")

        

        (action_tile "btn_save_proj"

          "(progn

             (setq save-opt (save-prompt-dialog-callback dcl-path))

             (cond

               ((= save-opt 1)

                 (setq new-name (get_tile \"project_name\"))

                 (if (or (= new-name \"\") (= new-name nil)) (setq new-name (tsp-generate-default-project-name)))

                 (tsp-save-project-data new-name)

                 (set_tile \"project_name\" *tsp-current-project-name*)

                 (set_tile \"manager_title\" (strcat \"세그먼트 설정 (Segment Manager) - \" *tsp-current-project-name*))

                 (setq *tsp-data-dirty* nil)

                 (alert (strcat \"'\" *tsp-current-project-name* \"' (으)로 새로 저장되었습니다.\"))

               )

               ((= save-opt 2)

                 (tsp-save-project-data *tsp-current-project-name*) 

                 (set_tile \"project_name\" *tsp-current-project-name*)

                 (set_tile \"manager_title\" (strcat \"세그먼트 설정 (Segment Manager) - \" *tsp-current-project-name*))

                 (setq *tsp-data-dirty* nil)

                 (alert (strcat \"'\" *tsp-current-project-name* \"' 에 덮어쓰기 완료되었습니다.\"))

               )

               (t

                 (princ \"\\n저장이 취소되었습니다.\")

               )

             )

           )"

        )



        ;; [프로젝트 불러오기 - 로드 후 실시간 작도를 위해 12번 반환 추가]

        (action_tile "btn_load_proj" 

          "(progn 

             (setq load-result (project-list-callback dcl-path)) 

             (if load-result 

               (progn 

                 (set_tile \"project_name\" *tsp-current-project-name*) 

                 (set_tile \"manager_title\" (strcat \"세그먼트 설정 (Segment Manager) - \" *tsp-current-project-name*)) 

                 (update-segment-manager-list) 

                 (alert (strcat \"프로젝트 [\" *tsp-current-project-name* \"] 가 로드되었습니다.\")) 

                 (setq *tsp-data-dirty* nil)

                 (done_dialog 12)

               )

             )

           )"

        )



        (action_tile "seg_list" 

          "(progn 

             (setq clicked-idx (atoi $value))

             (cond

               (paste-mode 

                 (setq source-idx clicked-idx)

                 (if (= source-idx selected-idx) 

                   (alert \"자기 자신의 설정을 복사할 수 없습니다.\") 

                   (done_dialog 3)

                 )

               )

               (apply-all-mode (setq selected-idx clicked-idx) (done_dialog 7))

               (apply-soil-mode (setq selected-idx clicked-idx) (done_dialog 8))

               (apply-support-mode (setq selected-idx clicked-idx) (done_dialog 11))

               (T

                 (setq selected-idx clicked-idx)

                 (set_tile \"status_msg\" (strcat (itoa (1+ selected-idx)) \"번이 선택되었습니다.\"))

               )

             )

           )"

        )



        (action_tile "btn_input" "(done_dialog 4)")

        (action_tile "btn_modify" "(progn (setq check-data (nth selected-idx *segment-list*)) (if (cdr (assoc 'IS-DEFINED check-data)) (done_dialog 2) (alert \"아직 입력되지 않은 목록입니다.\\n먼저 [입력] 버튼을 눌러 초기 설정을 진행하세요.\")))")

        (action_tile "btn_delete" "(done_dialog 9)")

        (action_tile "btn_delete_all" "(done_dialog 10)")



        (action_tile "btn_copy_from" "(progn (setq paste-mode T apply-all-mode nil apply-soil-mode nil apply-support-mode nil) (set_tile \"status_msg\" \"[설정 가져오기] 설정을 복사해올 '원본' 세그먼트를 클릭하세요. (취소: ESC)\"))")

        (action_tile "btn_apply_all" "(progn (setq apply-all-mode T paste-mode nil apply-soil-mode nil apply-support-mode nil) (set_tile \"status_msg\" \"[전체 일괄적용] 원본 세그먼트를 클릭하세요. (취소: ESC)\"))")

        (action_tile "btn_apply_soil" "(progn (setq apply-soil-mode T paste-mode nil apply-all-mode nil apply-support-mode nil) (set_tile \"status_msg\" \"[지반 일괄적용] 지반 정보를 가진 원본 세그먼트를 클릭하세요. (취소: ESC)\"))")

        (action_tile "btn_apply_support" "(progn (setq apply-support-mode T paste-mode nil apply-all-mode nil apply-soil-mode nil) (set_tile \"status_msg\" \"[지보재 일괄적용] 지보재 정보를 가진 원본 세그먼트를 클릭하세요. (취소: ESC)\"))")



        (action_tile "btn_draw_section" "(progn (setq check-data (nth selected-idx *segment-list*)) (if (cdr (assoc 'IS-DEFINED check-data)) (progn (load-segment-to-globals selected-idx) (setq *tsp-target-draw-flag* (cdr (assoc 'SECTION-DRAW check-data))) (done_dialog 5)) (alert \"입력되지 않은 목록입니다.\")))")

        (action_tile "btn_draw_plan" "(progn (setq check-data (nth selected-idx *segment-list*)) (if (cdr (assoc 'IS-DEFINED check-data)) (progn (load-segment-to-globals selected-idx) (done_dialog 6)) (alert \"입력되지 않은 목록입니다.\")))")

        (action_tile "cancel" "(done_dialog 0)")

        (setq result (start_dialog))

      )

      (setq result 0)

    )

    (unload_dialog dcl-id)

    

    (cond

      ((= result 0) 

       (if (or paste-mode apply-all-mode apply-soil-mode apply-support-mode)

         (progn

           (setq paste-mode nil apply-all-mode nil apply-soil-mode nil apply-support-mode nil)

           (setq status-msg-text "작업이 취소되었습니다.")

           (setq loop-active T)

         )

         (setq loop-active nil)

       )

      )

      

      ((= result 5) (setq loop-active nil))

      ((= result 6) (setq loop-active nil))

      

      ;; -------------------------------------------------------------

      ;; 데이터가 변경된 작업들은 종료 시 모두 실시간 작도 업데이트 진행

      ;; -------------------------------------------------------------

      

      ((= result 4)

       (reset-globals-to-defaults)

       (input-segment-wizard dcl-path selected-idx)

       (setq max-idx (1- (length *segment-list*)))

       (if (< selected-idx max-idx) (setq selected-idx (1+ selected-idx)))

       (setq status-msg-text (strcat (itoa (1+ selected-idx)) "번 목록이 선택되었습니다."))

       (setq paste-mode nil apply-all-mode nil apply-soil-mode nil apply-support-mode nil)

       ;; [버그2 수정] 실시간 작도 전 저장된 세그먼트 데이터를 전역변수로 복원
       ;; wizard/수정 완료 후 전역변수가 마지막 입력값으로 남아 wall-type 오염 방지
       (if (and *tsp-boundary-ent* *tsp-boundary-orient*)
         (progn
           (load-segment-to-globals selected-idx)
           (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*)
         )
       )

      )

      

      ((= result 2)

       (modify-segment-dashboard dcl-path selected-idx)

       (setq status-msg-text "수정이 완료되었습니다.")

       (setq paste-mode nil apply-all-mode nil apply-soil-mode nil apply-support-mode nil)

       ;; [버그2 수정] 수정 완료 후도 동일하게 해당 세그먼트 데이터 복원 후 작도
       (if (and *tsp-boundary-ent* *tsp-boundary-orient*)
         (progn
           (load-segment-to-globals selected-idx)
           (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*)
         )
       )

      )

      

      ((= result 3) 

       (setq target-idx selected-idx)

       (setq source-data (nth source-idx *segment-list*)) 

       (setq target-data (nth target-idx *segment-list*))

       (if (cdr (assoc 'IS-DEFINED source-data))

         (progn

           (setq new-target (list (cons 'ID (cdr (assoc 'ID target-data))) (cons 'NAME (cdr (assoc 'NAME target-data))) (cons 'IS-DEFINED (cdr (assoc 'IS-DEFINED source-data))) (cons 'SECTION-DRAW (cdr (assoc 'SECTION-DRAW source-data))) (cons 'SOIL-DATA (cdr (assoc 'SOIL-DATA source-data))) (cons 'WALL-SPEC (cdr (assoc 'WALL-SPEC source-data))) (cons 'WALL-CUSTOM (cdr (assoc 'WALL-CUSTOM source-data))) (cons 'WALE-SPEC (cdr (assoc 'WALE-SPEC source-data))) (cons 'WALE-CUSTOM (cdr (assoc 'WALE-CUSTOM source-data))) (cons 'CTC (cdr (assoc 'CTC source-data))) (cons 'TIMBER-THICKNESS (cdr (assoc 'TIMBER-THICKNESS source-data))) (cons 'MAX-DEPTH (cdr (assoc 'MAX-DEPTH source-data))) (cons 'EMBED-DEPTH (cdr (assoc 'EMBED-DEPTH source-data))) (cons 'WATER-CHK (cdr (assoc 'WATER-CHK source-data))) (cons 'WATER-DEPTH (cdr (assoc 'WATER-DEPTH source-data))) (cons 'SUPPORT-LIST (cdr (assoc 'SUPPORT-LIST source-data))) (cons 'V-START (cdr (assoc 'V-START target-data))) (cons 'V-END (cdr (assoc 'V-END target-data))) (cons 'ANGLE (cdr (assoc 'ANGLE target-data))) (cons 'LENGTH (cdr (assoc 'LENGTH target-data)))))

           (setq *segment-list* (update-list-item *segment-list* target-idx new-target)) 

           (setq paste-mode nil) 

           (setq *tsp-data-dirty* T) 

           (setq status-msg-text "설정 복사가 완료되었습니다.")

           (if (and *tsp-boundary-ent* *tsp-boundary-orient*) (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*))

         )

         (progn (alert "비어있는 세그먼트에서 설정을 가져올 수 없습니다.") (setq paste-mode nil))

       )

      )

      

      ((= result 7) 

       (setq check-data (nth selected-idx *segment-list*)) 

       (if (cdr (assoc 'IS-DEFINED check-data)) 

         (progn 

           (tsp-apply-settings-to-all selected-idx) 

           (setq status-msg-text "모든 세그먼트에 설정이 일괄 적용되었습니다.") 

           (alert (strcat "No." (itoa (1+ selected-idx)) " 설정값이 나머지 전체 목록에 적용되었습니다."))

           (if (and *tsp-boundary-ent* *tsp-boundary-orient*) (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*))

         ) 

         (alert "복사할 원본이 정의되지 않았습니다.\\n먼저 [입력]을 완료하세요.")

       )

       (setq apply-all-mode nil)

      )

      

      ((= result 8) 

       (setq check-data (nth selected-idx *segment-list*)) 

       (if (and (cdr (assoc 'SOIL-DATA check-data)) (/= (cdr (assoc 'SOIL-DATA check-data)) '())) 

         (progn 

           (tsp-apply-soil-to-all selected-idx) 

           (setq status-msg-text "모든 세그먼트에 지반 정보가 적용되었습니다.") 

           (alert (strcat "No." (itoa (1+ selected-idx)) " 지반 정보가 나머지 전체 목록에 적용되었습니다."))

           (if (and *tsp-boundary-ent* *tsp-boundary-orient*) (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*))

         ) 

         (alert "복사할 지반 정보가 없습니다.")

       )

       (setq apply-soil-mode nil)

      )

      

      ((= result 11) 

       (setq check-data (nth selected-idx *segment-list*)) 

       (if (and (cdr (assoc 'SUPPORT-LIST check-data)) (/= (cdr (assoc 'SUPPORT-LIST check-data)) '())) 

         (progn 

           (tsp-apply-support-to-all selected-idx) 

           (setq status-msg-text "모든 세그먼트에 지보재 정보가 적용되었습니다.") 

           (alert (strcat "No." (itoa (1+ selected-idx)) " 지보재 정보가 나머지 전체 목록에 적용되었습니다."))

           (if (and *tsp-boundary-ent* *tsp-boundary-orient*) (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*))

         ) 

         (alert "복사할 지보재 정보가 없습니다.\\n먼저 지보재를 정의해 주세요.")

       )

       (setq apply-support-mode nil)

      )

      

      ((= result 9) 

       (tsp-reset-segment-data selected-idx) 

       (setq status-msg-text (strcat "No." (itoa (1+ selected-idx)) " 설정이 초기화되었습니다."))

       (if (and *tsp-boundary-ent* *tsp-boundary-orient*) (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*))

      )

      

      ((= result 10) 

       (tsp-reset-all-segments) 

       (setq status-msg-text "모든 목록의 설정이 초기화되었습니다.") 

       (alert "모든 세그먼트 데이터가 초기 상태로 되돌려졌습니다.")

       (if (and *tsp-boundary-ent* *tsp-boundary-orient*) (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*))

      )

      

      ((= result 12)

       ;; 프로젝트 로드 후 도면 갱신

       (if (and *tsp-boundary-ent* *tsp-boundary-orient*) (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*))

      )

    )

  )

  result

)



;;; --------------------------------------------------------------------------

;;; Function: draw-tsp-legend

;;; Description: 지반 범례(Legend) 도면 작도

;;; --------------------------------------------------------------------------

(defun draw-tsp-legend (pt mode temp-layers / all-data target-data box-w text-w row-h title-h s-factor layer-name item p-name p-pat p-scale p-angle p1 p2 p3 p4 center-pt text-pt final-scale rect-pl num-items num-rows total-w i col row box-x box-y cx cy p-type abbr-name already ad base-y tri-h tri-w-half line1-len line1-start line1-end line1-mid l2-half off-y1 line2-y line2-start line2-end l3-half off-y2 line3-y line3-start line3-end tri-p1 tri-p2 tri-p3 tri-p_top_center tot-p1 tot-p2 tot-p3 tot-p4)

  (setq layer-name "_지반선")

  (setq s-factor (if *tsp-scale-section* (/ *tsp-scale-section* 200.0) 1.0))

  

  (setq title-h (* 2000.0 s-factor))

  (setq row-h (* 2000.0 s-factor))

  (setq box-w (* 2000.0 s-factor))

  (setq text-w (* 4000.0 s-factor))

  (setq total-w (* 12000.0 s-factor))

  

  (setq all-data '(

    ("입도양호 자갈(GW)" "GW_" 100.0 0.0) 

    ("입도불량 자갈(GP)" "GP_" 100.0 0.0) 

    ("실트질 자갈(GM)" "GM_" 100.0 0.0) 

    ("점토질 자갈(GC)" "GC_" 100.0 0.0) 

    ("입도양호 모래(SW)" "SW_" 100.0 0.0) 

    ("입도불량 모래(SP)" "SP_" 100.0 0.0) 

    ("실트질 모래(SM)" "SM_" 100.0 0.0) 

    ("점토질 모래(SC)" "SC_" 100.0 0.0) 

    ("저소성 실트(ML)" "ML_" 100.0 0.0) 

    ("고소성 실트(MH)" "MH_" 100.0 0.0) 

    ("저소성 점토(CL)" "CL_" 100.0 0.0) 

    ("고소성 점토(CH)" "CH_" 100.0 0.0) 

    ("전석(BOULDER)" "GRAVEL" 30.0 0.0)

    ("풍화잔류토(RS)" "RS_" 2500.0 0.0) 

    ("풍화암(WR)" "WR_" 100.0 0.0) 

    ("연암(SR)" "SR_" 100.0 0.0) 

    ("경암(HR)" "HR_" 100.0 0.0) 

    ("기타" "LINE" 78.74 0.0)

  ))

  

  (setq target-data '())

  (if (= (strcase mode) "USED")

    (progn

      (foreach layer temp-layers

        (setq p-type (nth 5 layer))

        (setq abbr-name (tsp-get-abbr p-type))

        (setq already nil)

        (foreach td target-data

          (if (= (car td) abbr-name) (setq already t))

        )

        (if (not already)

          (foreach ad all-data

            (if (= (car ad) p-type)

              (setq target-data (append target-data (list (list abbr-name (nth 1 ad) (nth 2 ad) (nth 3 ad)))))

            )

          )

        )

      )

      (if (= *tsp-water-chk* "1")

        (setq target-data (append target-data (list (list "지하수위" "GWL" 1.0 0.0))))

      )

    )

    (progn

      (foreach ad all-data 

        ;; "전체" 모드일 때는 "기타" 지층을 범례에서 제외

        (if (/= (nth 0 ad) "기타")

          (setq target-data (append target-data (list (list (tsp-get-abbr (nth 0 ad)) (nth 1 ad) (nth 2 ad) (nth 3 ad)))))

        )

      )

      (setq target-data (append target-data (list (list "지하수위" "GWL" 1.0 0.0))))

    )

  )

  

  (setq num-items (length target-data))

  (setq num-rows (fix (/ (+ num-items 1) 2)))

  

  ;; --- 표 외곽선 및 타이틀 작도 ---

  (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText") (cons 8 layer-name) (cons 62 8) 

                 (cons 10 (list (+ (car pt) (/ total-w 2.0)) (- (cadr pt) (/ title-h 2.0)) 0.0)) 

                 (cons 40 (* 800.0 s-factor)) (cons 1 "범   례") 

                 (cons 72 1) (cons 73 2) (cons 11 (list (+ (car pt) (/ total-w 2.0)) (- (cadr pt) (/ title-h 2.0)) 0.0))))

                 

  ;; [수정됨] 줌 기능을 위해 전체 범례 외곽 좌표를 독립된 변수에 저장

  (setq tot-p1 (list (car pt) (cadr pt) 0.0))

  (setq tot-p2 (list (+ (car pt) total-w) (cadr pt) 0.0))

  (setq tot-p3 (list (+ (car pt) total-w) (- (cadr pt) (+ title-h (* num-rows row-h))) 0.0))

  (setq tot-p4 (list (car pt) (- (cadr pt) (+ title-h (* num-rows row-h))) 0.0))

  

  (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 4) '(70 . 1) 

                 (cons 10 tot-p1) (cons 10 tot-p2) (cons 10 tot-p3) (cons 10 tot-p4)))

                 

  (entmake (list '(0 . "LINE") (cons 8 layer-name) (cons 62 8) 

                 (cons 10 (list (car pt) (- (cadr pt) title-h) 0.0)) 

                 (cons 11 (list (+ (car pt) total-w) (- (cadr pt) title-h) 0.0))))

                 

  (foreach vx (list box-w (+ box-w text-w) (+ box-w text-w box-w))

    (entmake (list '(0 . "LINE") (cons 8 layer-name) (cons 62 8) 

                   (cons 10 (list (+ (car pt) vx) (- (cadr pt) title-h) 0.0)) 

                   (cons 11 (list (+ (car pt) vx) (- (cadr pt) (+ title-h (* num-rows row-h))) 0.0))))

  )

  

  (setq i 1)

  (while (< i num-rows)

    (entmake (list '(0 . "LINE") (cons 8 layer-name) (cons 62 8) 

                   (cons 10 (list (car pt) (- (cadr pt) (+ title-h (* i row-h))) 0.0)) 

                   (cons 11 (list (+ (car pt) total-w) (- (cadr pt) (+ title-h (* i row-h))) 0.0))))

    (setq i (1+ i))

  )

  

  ;; --- 데이터 및 해치/기호 채우기 ---

  (setq i 0)

  (while (< i num-items)

    (setq item (nth i target-data))

    (setq p-name (nth 0 item) p-pat (nth 1 item) p-scale (nth 2 item) p-angle (nth 3 item))

    

    (setq col (/ i num-rows)) 

    (setq row (rem i num-rows))

    

    (setq box-x (+ (car pt) (* col (+ box-w text-w))))

    (setq box-y (- (cadr pt) (+ title-h (* row row-h))))

    

    ;; 여기서 내부 해치 박스 그리느라 p1, p2, p3, p4가 오염됨

    (setq p1 (list box-x box-y 0.0))

    (setq p2 (list (+ box-x box-w) box-y 0.0))

    (setq p3 (list (+ box-x box-w) (- box-y row-h) 0.0))

    (setq p4 (list box-x (- box-y row-h) 0.0))

    (setq center-pt (list (+ box-x (/ box-w 2.0)) (- box-y (/ row-h 2.0)) 0.0))

    

    (if (= p-pat "GWL")

      (progn

        (setq cx (car center-pt) cy (cadr center-pt))

        (setq base-y (- cy (* 300.0 s-factor)))

        

        (setq tri-h (* 800.0 s-factor))

        (setq tri-w-half (/ tri-h (sqrt 3.0)))

        

        (setq line1-len (* 2000.0 s-factor))

        (setq line1-start (list (- cx (/ line1-len 2.0)) base-y 0.0))

        (setq line1-end (list (+ cx (/ line1-len 2.0)) base-y 0.0))

        (setq line1-mid (list cx base-y 0.0))

        (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 2) '(70 . 0) (cons 10 line1-start) (cons 10 line1-end)))

        

        (setq l2-half (* 750.0 s-factor)) (setq off-y1 (* 100.0 s-factor)) (setq line2-y (- base-y off-y1))

        (setq line2-start (list (- cx l2-half) line2-y 0.0)) (setq line2-end (list (+ cx l2-half) line2-y 0.0))

        (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 2) '(70 . 0) (cons 10 line2-start) (cons 10 line2-end)))

        

        (setq l3-half (* 500.0 s-factor)) (setq off-y2 (* 200.0 s-factor)) (setq line3-y (- base-y off-y2))

        (setq line3-start (list (- cx l3-half) line3-y 0.0)) (setq line3-end (list (+ cx l3-half) line3-y 0.0))

        (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 2) '(70 . 0) (cons 10 line3-start) (cons 10 line3-end)))

        

        (setq tri-p3 line1-mid)

        (setq tri-p1 (list (- cx tri-w-half) (+ base-y tri-h) 0.0))

        (setq tri-p2 (list (+ cx tri-w-half) (+ base-y tri-h) 0.0))

        (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 3) '(70 . 1) (cons 10 tri-p1) (cons 10 tri-p2) (cons 10 tri-p3)))

        

        (setq tri-p_top_center (list cx (+ base-y tri-h) 0.0))

        (entmake (list '(0 . "SOLID") '(100 . "AcDbEntity") '(100 . "AcDbTrace") (cons 8 layer-name) (cons 62 8) (cons 10 tri-p1) (cons 11 tri-p_top_center) (cons 12 tri-p3) (cons 13 tri-p3)))

      )

      (progn

        (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") '(100 . "AcDbPolyline") (cons 8 layer-name) (cons 62 8) '(90 . 4) '(70 . 1) 

                       (cons 10 p1) (cons 10 p2) (cons 10 p3) (cons 10 p4)))

        (setq rect-pl (entlast))

        (setq final-scale (* p-scale s-factor 2.0))

        (if rect-pl

          (tsp-apply-custom-hatch rect-pl p-pat final-scale (* p-angle (/ pi 180.0)) layer-name 8 center-pt)

        )

      )

    )

    

    (setq text-pt (list (+ box-x box-w (/ text-w 2.0)) (- box-y (/ row-h 2.0)) 0.0))

    (entmake (list '(0 . "TEXT") '(100 . "AcDbEntity") '(100 . "AcDbText") (cons 8 layer-name) (cons 62 8) 

                   (cons 10 text-pt) (cons 40 (* 600.0 s-factor)) (cons 1 p-name) 

                   (cons 72 1) (cons 73 2) (cons 11 text-pt)))

    

    (setq i (1+ i))

  )

  (princ "\n범례 생성 완료.")

  

  (list tot-p1 tot-p2 tot-p3 tot-p4)

)



;;; --------------------------------------------------------------------------

;;; Function: tsp-get-abbr, tsp-format-soil-name

;;; Description: 지층명칭 추출 및 변환

;;; --------------------------------------------------------------------------

(defun tsp-get-abbr (str / p1 p2 res)

  (setq p1 (vl-string-search "(" str))

  (setq p2 (vl-string-search ")" str))

  (if (and p1 p2 (> p2 p1))

    (setq res (substr str (+ p1 2) (- p2 p1 1)))

    (setq res str)

  )

  (if (= res "BOULDER") (setq res "BO"))

  res

)



(defun tsp-format-soil-name (str / p1 base-str)

  (setq p1 (vl-string-search "(" str))

  (if p1

    (setq base-str (substr str 1 p1))

    (setq base-str str)

  )

  (while (and (> (strlen base-str) 0) (= (substr base-str (strlen base-str) 1) " "))

    (setq base-str (substr base-str 1 (1- (strlen base-str))))

  )

  (while (vl-string-search "저소성" base-str)

    (setq base-str (vl-string-subst "압축성 낮은" "저소성" base-str))

  )

  (while (vl-string-search "고소성" base-str)

    (setq base-str (vl-string-subst "압축성 높은" "고소성" base-str))

  )

  base-str

)



;;; --------------------------------------------------------------------------

;;; Function: soil-dialog-callback

;;; Description: 지반 정보 입력 대화상자 콜백

;;; --------------------------------------------------------------------------

(defun soil-dialog-callback (dcl-path / dcl-id result soil-data-list temp-layers soil-labels h-val phi-val r-idx sel-idx sel-item sel-name sel-depth sel-type-name sel-phi type-idx i user-name soil-depth-str soil-phi-str soil-depth soil-phi last-depth h-pattern h-scale h-angle new-layer new-list selected-type-info lg-mode pt leg-box)

  (setq dcl-id (load_dialog dcl-path))

  (setq temp-layers (if *tsp-soil-layers* *tsp-soil-layers* '()))

  

  (setq soil-data-list '(

    ("입도양호 자갈(GW)" "GW_" 100.0 0.0) ("입도불량 자갈(GP)" "GP_" 100.0 0.0) 

    ("실트질 자갈(GM)" "GM_" 100.0 0.0) ("점토질 자갈(GC)" "GC_" 100.0 0.0) 

    ("입도양호 모래(SW)" "SW_" 100.0 0.0) ("입도불량 모래(SP)" "SP_" 100.0 0.0) 

    ("실트질 모래(SM)" "SM_" 100.0 0.0) ("점토질 모래(SC)" "SC_" 100.0 0.0) 

    ("저소성 실트(ML)" "ML_" 100.0 0.0) ("고소성 실트(MH)" "MH_" 100.0 0.0) 

    ("저소성 점토(CL)" "CL_" 100.0 0.0) ("고소성 점토(CH)" "CH_" 100.0 0.0) 

    ("전석(BOULDER)" "GRAVEL" 30.0 0.0) ("풍화잔류토(RS)" "RS_" 2500.0 0.0) 

    ("풍화암(WR)" "WR_" 100.0 0.0) ("연암(SR)" "SR_" 100.0 0.0) 

    ("경암(HR)" "HR_" 100.0 0.0) ("기타" "LINE" 78.74 0.0)

  ))

  

  (setq soil-labels (mapcar 'car soil-data-list))

  (setq loop-dcl t)

  (setq result 0)

  

  (while loop-dcl

    (setq dcl-id (load_dialog dcl-path))

    (if (new_dialog "tsp_soil" dcl-id)

      (progn

        (start_list "soil_type") (mapcar 'add_list soil-labels) (end_list) (set_tile "soil_type" "0")

        

        ;; 1. 동적 암반 팝업 업데이트

        (defun update-rock-popup (/ rock-list l-name l-type)

          (setq rock-list '())

          (foreach layer temp-layers

            (setq l-name (car layer) l-type (nth 5 layer))

            (if (or (and l-type (or (wcmatch l-type "*풍화암*") (wcmatch l-type "*연암*") (wcmatch l-type "*경암*")))

                    (wcmatch l-name "*풍화암*") (wcmatch l-name "*연암*") (wcmatch l-name "*경암*"))

              (setq rock-list (append rock-list (list l-name)))

            )

          )

          (if (> (length rock-list) 0)

            (progn (start_list "soil_shotcrete_layer") (mapcar 'add_list rock-list) (end_list))

            (progn (start_list "soil_shotcrete_layer") (add_list "암반층 없음") (end_list))

          )

          rock-list

        )



        ;; 2. 마스터 스위치 연쇄 제어 함수 (활성화/비활성화 처리)

        (defun toggle-shotcrete-ui (is-master-on has-rock)

          (if has-rock

            (progn

              (mode_tile "soil_shotcrete_enable" 0) ;; 암반 있으면 마스터 체크 활성화

              (if is-master-on

                (progn ;; 체크 켜면 두께 및 암반지정 켜기

                  (mode_tile "soil_shotcrete_thick" 0)

                  (mode_tile "soil_shotcrete_chk" 0)

                  (if (= (get_tile "soil_shotcrete_chk") "1") (mode_tile "soil_shotcrete_layer" 0) (mode_tile "soil_shotcrete_layer" 1))

                )

                (progn ;; 체크 끄면 아래 옵션 전부 회색 비활성화

                  (mode_tile "soil_shotcrete_thick" 1)

                  (mode_tile "soil_shotcrete_chk" 1)

                  (mode_tile "soil_shotcrete_layer" 1)

                )

              )

            )

            (progn ;; 암반이 아예 없으면 전부 회색 비활성화

              (set_tile "soil_shotcrete_enable" "0")

              (setq *tsp-shotcrete-enable* "0")

              (mode_tile "soil_shotcrete_enable" 1)

              (mode_tile "soil_shotcrete_thick" 1)

              (mode_tile "soil_shotcrete_chk" 1)

              (mode_tile "soil_shotcrete_layer" 1)

            )

          )

        )



        ;; 3. 지층 목록 갱신

        (defun update-soil-list () 

          (start_list "soil_list") 

          (foreach layer temp-layers 

            (setq phi-val (if (nth 6 layer) (rtos (nth 6 layer) 2 1) "0.0")) 

            (add_list (strcat (car layer) " - " (nth 5 layer) " - " (rtos (cadr layer) 2 2) "m - " phi-val "도"))) 

          (end_list)

          (setq current-rock-list (update-rock-popup))

          (toggle-shotcrete-ui (= (get_tile "soil_shotcrete_enable") "1") (> (length current-rock-list) 0))

        )



        ;; ---------------- UI 초기값 세팅 ----------------

        ;; 숏크리트 생성 (마스터 스위치)

        (set_tile "soil_shotcrete_enable" (if *tsp-shotcrete-enable* *tsp-shotcrete-enable* "1"))

        

        ;; 숏크리트 두께 (H/2 계산)

        (if (or (null *tsp-shotcrete-thick*) (= *tsp-shotcrete-thick* 0.0))

          (progn

            (if (= *tsp-hpile-spec* "User-defined")

              (setq h-val (float (car *tsp-hpile-custom*)))

              (setq h-val (float (car (parse-h-spec *tsp-hpile-spec*))))

            )

            (setq *tsp-shotcrete-thick* (/ h-val 2.0))

          )

        )

        (set_tile "soil_shotcrete_thick" (rtos *tsp-shotcrete-thick* 2 0))

        

        ;; 숏크리트 암반 지정 체크

        (set_tile "soil_shotcrete_chk" (if *tsp-shotcrete-chk* *tsp-shotcrete-chk* "0"))

        

        ;; 지하수위

        (set_tile "soil_water_chk" (if *tsp-water-chk* *tsp-water-chk* "0"))

        (if (= (get_tile "soil_water_chk") "1") (progn (mode_tile "soil_water_depth" 0) (set_tile "soil_water_depth" (rtos *tsp-water-depth* 2 2))) (mode_tile "soil_water_depth" 1))

        

        ;; 목록 및 상태 초기 갱신

        (update-soil-list)

        (if (and *tsp-shotcrete-layer* (/= *tsp-shotcrete-layer* "") current-rock-list)

          (if (setq r-idx (vl-position *tsp-shotcrete-layer* current-rock-list))

            (set_tile "soil_shotcrete_layer" (itoa r-idx))

          )

        )



        ;; ---------------- 이벤트 (액션 타일) ----------------

        (action_tile "soil_water_chk" "(if (= $value \"1\") (mode_tile \"soil_water_depth\" 0) (mode_tile \"soil_water_depth\" 1))")

        (action_tile "soil_shotcrete_enable" "(progn (setq *tsp-shotcrete-enable* $value) (toggle-shotcrete-ui (= $value \"1\") (> (length current-rock-list) 0)))")

        (action_tile "soil_shotcrete_thick" "(setq *tsp-shotcrete-thick* (atof $value))")

        (action_tile "soil_shotcrete_chk" "(if (= $value \"1\") (mode_tile \"soil_shotcrete_layer\" 0) (mode_tile \"soil_shotcrete_layer\" 1))")

        

        (action_tile "soil_list" "(progn (setq sel-idx (atoi $value)) (if (and temp-layers (>= sel-idx 0) (< sel-idx (length temp-layers))) (progn (setq sel-item (nth sel-idx temp-layers)) (setq sel-name (car sel-item)) (setq sel-depth (cadr sel-item)) (setq sel-type-name (nth 5 sel-item)) (setq sel-phi (nth 6 sel-item)) (set_tile \"soil_user_name\" sel-name) (set_tile \"soil_depth\" (rtos sel-depth 2 2)) (if sel-phi (set_tile \"soil_phi\" (rtos sel-phi 2 1)) (set_tile \"soil_phi\" \"\")) (setq type-idx 0) (setq i 0) (foreach item soil-data-list (if (= (car item) sel-type-name) (setq type-idx i)) (setq i (1+ i))) (set_tile \"soil_type\" (itoa type-idx)))))")

        

        (action_tile "btn_add" "(progn (setq type-idx (atoi (get_tile \"soil_type\"))) (setq selected-type-info (nth type-idx soil-data-list)) (setq type-name (car selected-type-info)) (setq user-name (get_tile \"soil_user_name\")) (if (or (not user-name) (= user-name \"\")) (setq user-name (tsp-format-soil-name type-name))) (setq soil-depth-str (get_tile \"soil_depth\")) (setq soil-phi-str (get_tile \"soil_phi\")) (if (and soil-depth-str (/= soil-depth-str \"\") (numberp (read soil-depth-str))) (progn (setq soil-depth (atof soil-depth-str)) (setq soil-phi (if (numberp (read soil-phi-str)) (atof soil-phi-str) 0.0)) (setq last-depth (if temp-layers (cadr (last temp-layers)) 0.0)) (if (> soil-depth last-depth) (progn (setq h-pattern (nth 1 selected-type-info)) (setq h-scale (nth 2 selected-type-info)) (setq h-angle (nth 3 selected-type-info)) (setq new-layer (list user-name soil-depth h-pattern h-scale h-angle type-name soil-phi)) (setq temp-layers (append temp-layers (list new-layer))) (update-soil-list) (set_tile \"soil_user_name\" \"\") (set_tile \"soil_depth\" \"\") (set_tile \"soil_phi\" \"\") (mode_tile \"soil_user_name\" 2)) (alert \"깊이는 이전 지층보다 깊어야 합니다!\"))) (alert \"깊이를 올바르게 입력하세요!\")))")

        

        (action_tile "btn_modify" "(progn (setq sel-idx-str (get_tile \"soil_list\")) (if (and sel-idx-str (/= sel-idx-str \"\")) (progn (setq sel-idx (atoi sel-idx-str)) (setq type-idx (atoi (get_tile \"soil_type\"))) (setq selected-type-info (nth type-idx soil-data-list)) (setq type-name (car selected-type-info)) (setq user-name (get_tile \"soil_user_name\")) (if (or (not user-name) (= user-name \"\")) (setq user-name (tsp-format-soil-name type-name))) (setq soil-depth-str (get_tile \"soil_depth\")) (setq soil-phi-str (get_tile \"soil_phi\")) (if (and soil-depth-str (/= soil-depth-str \"\") (numberp (read soil-depth-str))) (progn (setq soil-depth (atof soil-depth-str)) (setq soil-phi (if (numberp (read soil-phi-str)) (atof soil-phi-str) 0.0)) (setq h-pattern (nth 1 selected-type-info)) (setq h-scale (nth 2 selected-type-info)) (setq h-angle (nth 3 selected-type-info)) (setq new-layer (list user-name soil-depth h-pattern h-scale h-angle type-name soil-phi)) (setq i 0) (setq new-list '()) (foreach item temp-layers (if (= i sel-idx) (setq new-list (append new-list (list new-layer))) (setq new-list (append new-list (list item)))) (setq i (1+ i))) (setq temp-layers new-list) (update-soil-list) (set_tile \"soil_list\" (itoa sel-idx))) (alert \"수정할 깊이를 올바르게 입력하세요!\"))) (alert \"수정할 항목을 목록에서 선택하세요!\")))")

        

        (action_tile "btn_delete" "(progn (setq sel-idx (atoi (get_tile \"soil_list\"))) (if (and temp-layers (>= sel-idx 0) (< sel-idx (length temp-layers))) (progn (setq temp-layers (vl-remove (nth sel-idx temp-layers) temp-layers)) (update-soil-list)) (alert \"삭제할 항목을 선택하세요!\")))")

        (action_tile "btn_clear" "(progn (setq temp-layers '()) (update-soil-list))")

        

        (action_tile "btn_legend" "(progn (setq *tsp-water-chk* (get_tile \"soil_water_chk\")) (if (= *tsp-water-chk* \"1\") (setq *tsp-water-depth* (atof (get_tile \"soil_water_depth\"))) (setq *tsp-water-depth* 0.0)) (done_dialog 3))")

        

        (action_tile "accept" "(progn 

          (setq *tsp-soil-layers* temp-layers) 

          (setq *tsp-water-chk* (get_tile \"soil_water_chk\")) 

          (if (= *tsp-water-chk* \"1\") (setq *tsp-water-depth* (atof (get_tile \"soil_water_depth\"))) (setq *tsp-water-depth* 0.0)) 

          (setq *tsp-shotcrete-enable* (get_tile \"soil_shotcrete_enable\"))

          (setq *tsp-shotcrete-chk* (get_tile \"soil_shotcrete_chk\"))

          (setq *tsp-shotcrete-thick* (atof (get_tile \"soil_shotcrete_thick\")))

          (if (and (= *tsp-shotcrete-chk* \"1\") current-rock-list (> (length current-rock-list) 0))

            (setq *tsp-shotcrete-layer* (nth (atoi (get_tile \"soil_shotcrete_layer\")) current-rock-list))

            (setq *tsp-shotcrete-layer* \"\")

          )

          (done_dialog 1))")

        

        (action_tile "back" "(done_dialog 2)") 

        (action_tile "cancel" "(done_dialog 0)")

        

        (setq result (start_dialog))

        (unload_dialog dcl-id)

        

        (cond

          ((= result 3) 

           (setq lg-mode nil pt nil)

           (if (null temp-layers)

             (setq lg-mode "All")

             (progn

               (initget "All Used")

               (setq lg-mode (vl-catch-all-apply 'getkword (list "\n범례 출력 방식을 선택하세요 [전체(A)/사용된지층(U)] <All>: ")))

               (if (vl-catch-all-error-p lg-mode) (setq lg-mode nil) (if (null lg-mode) (setq lg-mode "All")))

             )

           )

           (if lg-mode

             (progn

               (setq pt (vl-catch-all-apply 'getpoint (list "\n범례를 삽입할 기준점(좌측상단)을 클릭하세요: ")))

               (if (vl-catch-all-error-p pt) (setq pt nil))

             )

           )

           (if (and lg-mode pt)

             (progn (setq leg-box (draw-tsp-legend pt lg-mode temp-layers)) (if leg-box (tsp-zoom-legend leg-box)))

             (princ "\n범례 생성이 취소되었습니다. 지반 정의 창으로 되돌아갑니다.")

           )

          )

          (t (setq loop-dcl nil))

        )

      )

      (setq loop-dcl nil)

    )

  )

  result

)



;;; --------------------------------------------------------------------------

;;; Function: hpile-dialog-callback

;;; Description: H-Pile 규격 설정 대화상자 콜백

;;; --------------------------------------------------------------------------

(defun hpile-dialog-callback (dcl-path / dcl-id result idx std-len val-h val-b val-tw val-tf)

  (setq dcl-id (load_dialog dcl-path))

  (if (new_dialog "tsp_hpile" dcl-id)

    (progn

      ;; 1. 규격 목록 정의 (전역 변수 사용 + User-defined 자동 추가)

      (start_list "hpile_spec") 

      (mapcar 'add_list (append *tsp-std-wall-list* '("User-defined"))) 

      (end_list)

      

      (start_list "wale_spec") 

      (mapcar 'add_list (append *tsp-std-wall-list* '("User-defined"))) 

      (end_list)

      

      ;; 표준 리스트 길이 (User-defined 판별용)

      (setq std-len (length *tsp-std-wall-list*))



      ;; 2. 초기값 설정 로직 (리스트에서 현재 값의 위치를 찾음)

      ;; [H-Pile 설정]

      (if (setq idx (vl-position *tsp-hpile-spec* *tsp-std-wall-list*))

        (set_tile "hpile_spec" (itoa idx))      ; 리스트에 있으면 해당 인덱스 선택

        (set_tile "hpile_spec" (itoa std-len))  ; 없으면 마지막(User-defined) 선택

      )

      

      ;; 사용자 정의 입력창 활성/비활성 상태 동기화

      (if (= (get_tile "hpile_spec") (itoa std-len))

        (progn 

           (if *tsp-hpile-custom*

             (progn 

               (set_tile "hpile_h" (itoa (nth 0 *tsp-hpile-custom*))) 

               (set_tile "hpile_b" (itoa (nth 1 *tsp-hpile-custom*))) 

               (set_tile "hpile_tw" (itoa (nth 2 *tsp-hpile-custom*))) 

               (set_tile "hpile_tf" (itoa (nth 3 *tsp-hpile-custom*)))

             )

           )

           (mode_tile "hpile_h" 0) (mode_tile "hpile_b" 0) (mode_tile "hpile_tw" 0) (mode_tile "hpile_tf" 0)

        )

        (progn (mode_tile "hpile_h" 1) (mode_tile "hpile_b" 1) (mode_tile "hpile_tw" 1) (mode_tile "hpile_tf" 1))

      )



      ;; [Wale 설정]

      (if (setq idx (vl-position *tsp-wale-spec* *tsp-std-wall-list*))

        (set_tile "wale_spec" (itoa idx))

        (set_tile "wale_spec" (itoa std-len))

      )

      (if (= (get_tile "wale_spec") (itoa std-len))

        (progn 

           (if *tsp-wale-custom*

             (progn 

               (set_tile "wale_h" (itoa (nth 0 *tsp-wale-custom*))) 

               (set_tile "wale_b" (itoa (nth 1 *tsp-wale-custom*))) 

               (set_tile "wale_tw" (itoa (nth 2 *tsp-wale-custom*))) 

               (set_tile "wale_tf" (itoa (nth 3 *tsp-wale-custom*)))

             )

           )

           (mode_tile "wale_h" 0) (mode_tile "wale_b" 0) (mode_tile "wale_tw" 0) (mode_tile "wale_tf" 0)

        )

        (progn (mode_tile "wale_h" 1) (mode_tile "wale_b" 1) (mode_tile "wale_tw" 1) (mode_tile "wale_tf" 1))

      )



      (set_tile "ctc" (rtos *tsp-ctc* 2 2))

      (set_tile "hpile_max_depth" (rtos *tsp-max-excavation-depth* 2 2))

      (set_tile "hpile_embed_depth" (rtos *tsp-embedment-depth* 2 2))

      (set_tile "timber_thickness" (itoa *tsp-timber-thickness*))



      ;; 3. 액션 처리 (std-len 변수를 사용하여 동적으로 처리)

      (action_tile "hpile_spec" "(if (= (atoi $value) std-len) (progn (mode_tile \"hpile_h\" 0)(mode_tile \"hpile_b\" 0)(mode_tile \"hpile_tw\" 0)(mode_tile \"hpile_tf\" 0)) (progn (mode_tile \"hpile_h\" 1)(mode_tile \"hpile_b\" 1)(mode_tile \"hpile_tw\" 1)(mode_tile \"hpile_tf\" 1)))")

      (action_tile "wale_spec" "(if (= (atoi $value) std-len) (progn (mode_tile \"wale_h\" 0)(mode_tile \"wale_b\" 0)(mode_tile \"wale_tw\" 0)(mode_tile \"wale_tf\" 0)) (progn (mode_tile \"wale_h\" 1)(mode_tile \"wale_b\" 1)(mode_tile \"wale_tw\" 1)(mode_tile \"wale_tf\" 1)))")



      (action_tile "accept" 

        "(progn 

           (setq *tsp-wall-type* \"HPILE\")

           

           ;; H-Pile 저장

           (setq idx (atoi (get_tile \"hpile_spec\")))

           (if (< idx std-len)

             (setq *tsp-hpile-spec* (nth idx *tsp-std-wall-list*)) ;; 리스트에서 선택

             (setq *tsp-hpile-spec* \"User-defined\" *tsp-hpile-custom* (list (atoi (get_tile \"hpile_h\")) (atoi (get_tile \"hpile_b\")) (atoi (get_tile \"hpile_tw\")) (atoi (get_tile \"hpile_tf\"))))

           ) 

           ;; Wale 저장

           (setq idx (atoi (get_tile \"wale_spec\"))) 

           (if (< idx std-len)

             (setq *tsp-wale-spec* (nth idx *tsp-std-wall-list*)) ;; 리스트에서 선택

             (setq *tsp-wale-spec* \"User-defined\" *tsp-wale-custom* (list (atoi (get_tile \"wale_h\")) (atoi (get_tile \"wale_b\")) (atoi (get_tile \"wale_tw\")) (atoi (get_tile \"wale_tf\"))))

           ) 

           ;; 기타 변수 저장

           (setq *tsp-ctc* (atof (get_tile \"ctc\"))) 

           (setq *tsp-timber-thickness* (atoi (get_tile \"timber_thickness\"))) 

           (setq *tsp-max-excavation-depth* (atof (get_tile \"hpile_max_depth\"))) 

           (setq *tsp-embedment-depth* (atof (get_tile \"hpile_embed_depth\"))) 

           (done_dialog 1)

         )"

      )

      

      (action_tile "back" "(done_dialog 2)") 

      (action_tile "cancel" "(done_dialog 0)")

      

      (setq result (start_dialog)) 

      (unload_dialog dcl-id) 

      result

    )

  )

)



;;; --------------------------------------------------------------------------

;;; Function: brace-dialog-callback

;;; Description: 다단 사보강재 설정 대화상자 콜백 (동적 띠장 상향 체크 및 규격 동기화 적용)

;;; --------------------------------------------------------------------------

(defun brace-dialog-callback (dcl-path temp-seg-data limits rem-count / dcl-id result temp-b-rows std-len idx res-spec res-custom input-dist abs-dist prev-dist update-brace-list i str-dist disp num-b-rows min-req next-b-row next-dist sel-idx new-list allowed-dist hard-limit soft-limit tag-str jack-list res-jack b-jack dlg-name do-proceed tmp-id seg-id-str init-data init-upg cur-wale-spec cur-wale-vals cur-wale-h update-upgrade-ui res-upg upg-idx last-auto-max-spec is-init-phase)

  (setq seg-id-str (itoa (cdr (assoc 'ID temp-seg-data))))

  (setq init-data (cdr (assoc 'CORNER-BRACE temp-seg-data)))

  (setq init-upg (cdr (assoc 'UPGRADE-WALE temp-seg-data)))

  (setq cur-wale-spec (cdr (assoc 'WALE-SPEC temp-seg-data)))

  (setq cur-wale-vals (if (= cur-wale-spec "User-defined") (cdr (assoc 'WALE-CUSTOM temp-seg-data)) (parse-h-spec cur-wale-spec)))

  (setq cur-wale-h (if cur-wale-vals (nth 0 cur-wale-vals) 300))

  

  (setq dcl-id (load_dialog dcl-path))

  (setq hard-limit (car limits))

  (setq soft-limit (cadr limits))

  

  (if (and init-data (= (car init-data) "Y") (cadr init-data))

    (setq temp-b-rows (cadr init-data))

    (setq temp-b-rows '())

  )

  

  (setq dlg-name (if (and rem-count (= rem-count 0)) "tsp_brace_single" "tsp_brace"))

  

  (if (new_dialog dlg-name dcl-id)

    (progn

      (set_tile "brace_dlg_title" (strcat seg-id-str "번 모서리 사보강재(Corner Brace) 설정"))

      (set_tile "brace_info" (strcat "▶ 절대 한계: " (rtos hard-limit 2 2) "m / 권장(50%): " (rtos soft-limit 2 2) "m"))

      

      (start_list "brace_spec") 

      (mapcar 'add_list (append *tsp-std-wall-list* '("User-defined"))) 

      (end_list)

      (setq std-len (length *tsp-std-wall-list*))



      (setq jack-list '("스크류잭" "유압잭"))

      (start_list "brace_jack") (mapcar 'add_list jack-list) (end_list)



      ;; 상향 띠장 리스트 채우기 및 초기값 설정

      (start_list "upgrade_wale_spec") (mapcar 'add_list *tsp-std-wall-list*) (end_list)

      (if init-upg

        (progn

          (set_tile "chk_upgrade_wale" (if (= (car init-upg) "Y") "1" "0"))

          (if (setq upg-idx (vl-position (cadr init-upg) *tsp-std-wall-list*))

            (set_tile "upgrade_wale_spec" (itoa upg-idx)) (set_tile "upgrade_wale_spec" "2")

          )

        )

        (progn (set_tile "chk_upgrade_wale" "0") (set_tile "upgrade_wale_spec" "2"))

      )

      

      ;; [핵심] 규격 비교 및 UI 동적 제어 함수 (스마트 체크 및 규격 동기화 로직 적용)

      (defun update-upgrade-ui (/ max-h max-spec check-h b-idx check-vals spec-idx)

        (setq max-h 0 max-spec nil)

        (foreach b temp-b-rows

          (setq check-h (nth 0 (cadr b)))

          (if (> check-h max-h) (progn (setq max-h check-h) (setq max-spec (car b))))

        )

        (setq b-idx (atoi (get_tile "brace_spec")))

        (if (< b-idx std-len)

          (progn

            (setq check-vals (parse-h-spec (nth b-idx *tsp-std-wall-list*)))

            (if (and check-vals (> (nth 0 check-vals) max-h))

              (progn (setq max-h (nth 0 check-vals)) (setq max-spec (nth b-idx *tsp-std-wall-list*)))

            )

          )

          (progn

            (setq check-vals (list (atoi (get_tile "brace_h")) 0 0 0))

            (if (> (nth 0 check-vals) max-h)

              (progn (setq max-h (nth 0 check-vals)) (setq max-spec "User-defined"))

            )

          )

        )

        

        (if (> max-h cur-wale-h)

          (progn 

            (mode_tile "chk_upgrade_wale" 0) 

            (mode_tile "upgrade_wale_spec" 0)

            

            (if (not (equal last-auto-max-spec max-spec))

              (progn

                ;; 다이얼로그 초기 로딩 시에는 기존 사용자 설정을 보호, 이후 변경 시에는 자동 On 및 동기화

                (if (not is-init-phase)

                  (progn

                    (set_tile "chk_upgrade_wale" "1")

                    (if (and max-spec (/= max-spec "User-defined") (setq spec-idx (vl-position max-spec *tsp-std-wall-list*)))

                      (set_tile "upgrade_wale_spec" (itoa spec-idx))

                    )

                  )

                )

                (setq last-auto-max-spec max-spec)

              )

            )

          )

          (progn 

            (mode_tile "chk_upgrade_wale" 1) 

            (mode_tile "upgrade_wale_spec" 1) 

            (set_tile "chk_upgrade_wale" "0")

            (setq last-auto-max-spec nil)

          )

        )

      )



      (defun update-brace-list (/ i prev item spec m-dist rel-dist disp tag-str b-jack)

        (start_list "brace_list")

        (setq i 0 prev 0.0)

        (foreach item temp-b-rows

          (setq spec (car item) m-dist (nth 2 item) b-jack (nth 3 item))

          (if (null b-jack) (setq b-jack "스크류잭"))

          (setq rel-dist (- m-dist prev))

          (if (> m-dist hard-limit)

            (setq tag-str "  [!] 절대 한계 초과")

            (if (> m-dist soft-limit) (setq tag-str "  [!] 권장 한계 초과") (setq tag-str ""))

          )

          (if (= i 0)

            (setq disp (strcat (itoa (1+ i)) "열: " spec " / " (rtos m-dist 2 2) "m" tag-str))

            (setq disp (strcat (itoa (1+ i)) "열: " spec " (" b-jack ") / +" (rtos rel-dist 2 2) "m (누적 " (rtos m-dist 2 2) "m)" tag-str))

          )

          (add_list disp)

          (setq prev m-dist i (1+ i))

        )

        (end_list)

      )

      (update-brace-list)

      

      (if (>= hard-limit 1.5) (set_tile "brace_dist" "1.5") (set_tile "brace_dist" "1.0"))

      (set_tile "brace_spec" "2")

      (set_tile "brace_jack" "0")

      (mode_tile "brace_h" 1) (mode_tile "brace_b" 1) (mode_tile "brace_tw" 1) (mode_tile "brace_tf" 1)

      

      (action_tile "brace_spec" "(if (= (atoi $value) std-len) (progn (mode_tile \"brace_h\" 0)(mode_tile \"brace_b\" 0)(mode_tile \"brace_tw\" 0)(mode_tile \"brace_tf\" 0)) (progn (mode_tile \"brace_h\" 1)(mode_tile \"brace_b\" 1)(mode_tile \"brace_tw\" 1)(mode_tile \"brace_tf\" 1))) (update-upgrade-ui)")

      (action_tile "brace_h" "(update-upgrade-ui)")

      

      (setq is-init-phase T)

      (update-upgrade-ui) ;; 초기 진입 시 한 번 실행

      (setq is-init-phase nil)

      

      (action_tile "btn_add" 

        (strcat

          "(progn (setq input-dist (atof (get_tile \"brace_dist\"))) (setq num-b-rows (length temp-b-rows)) (setq prev-dist (if (> num-b-rows 0) (nth 2 (last temp-b-rows)) 0.0)) (setq abs-dist (+ prev-dist input-dist)) (setq min-req (if (= num-b-rows 0) 1.0 0.8)) "

          "(if (< input-dist min-req) (if (= num-b-rows 0) (alert \"1열 사보강재는 코너 여유 공간을 위해 최소 1.0m 이상 이격해야 합니다.\") (alert \"여러 열 설치 시 부재 간 간섭을 방지하기 위해 최소 0.8m 이상 이격해야 합니다.\")) "

          "(progn (setq do-proceed T) (if (> abs-dist hard-limit) (progn (setq tmp-id (load_dialog dcl-path)) (if (new_dialog \"tsp_brace_hard_warn\" tmp-id) (progn (if (< hard-limit min-req) (progn (set_tile \"warn_msg1\" (strcat \"물리적 여유 공간(\" (rtos hard-limit 2 2) \"m)이 최소 기준(\" (rtos min-req 2 1) \"m) 부족입니다.\")) (set_tile \"warn_msg2\" \"시공 불가 예상 구간이나, 이대로 강제 돌파(적용)하시겠습니까?\"))) (action_tile \"accept\" \"(done_dialog 1)\") (action_tile \"cancel\" \"(done_dialog 0)\") (if (= (start_dialog) 0) (setq do-proceed nil)))) (unload_dialog tmp-id)) "

          "(if (> abs-dist soft-limit) (progn (setq tmp-id (load_dialog dcl-path)) (if (new_dialog \"tsp_brace_warn\" tmp-id) (progn (action_tile \"accept\" \"(done_dialog 1)\") (action_tile \"cancel\" \"(done_dialog 0)\") (if (= (start_dialog) 0) (setq do-proceed nil)))) (unload_dialog tmp-id)))) "

          "(if do-proceed (progn (setq idx (atoi (get_tile \"brace_spec\"))) (if (< idx std-len) (progn (setq res-spec (nth idx *tsp-std-wall-list*)) (setq res-custom (parse-h-spec res-spec))) (progn (setq res-spec \"User-defined\") (setq res-custom (list (atoi (get_tile \"brace_h\")) (atoi (get_tile \"brace_b\")) (atoi (get_tile \"brace_tw\")) (atoi (get_tile \"brace_tf\")))))) (setq res-jack (nth (atoi (get_tile \"brace_jack\")) jack-list)) (setq temp-b-rows (append temp-b-rows (list (list res-spec res-custom abs-dist res-jack)))) (update-brace-list) (update-upgrade-ui) )))))"

        )

      )

      

      (action_tile "btn_mod" 

        (strcat

          "(progn (setq sel-idx (atoi (get_tile \"brace_list\"))) (if (and temp-b-rows (>= sel-idx 0) (< sel-idx (length temp-b-rows))) (progn (setq input-dist (atof (get_tile \"brace_dist\"))) (setq prev-dist (if (> sel-idx 0) (nth 2 (nth (1- sel-idx) temp-b-rows)) 0.0)) (setq abs-dist (+ prev-dist input-dist)) (setq min-req (if (= sel-idx 0) 1.0 0.8)) (setq next-b-row (if (< (1+ sel-idx) (length temp-b-rows)) (nth (1+ sel-idx) temp-b-rows) nil)) (setq next-dist (if next-b-row (nth 2 next-b-row) nil)) "

          "(if (< input-dist min-req) (if (= sel-idx 0) (alert \"1열 사보강재는 코너 여유 공간을 위해 최소 1.0m 이상 이격해야 합니다.\") (alert \"여러 열 설치 시 부재 간 간섭을 방지하기 위해 최소 0.8m 이상 이격해야 합니다.\")) "

          "(if (and next-dist (< (- next-dist abs-dist) 0.8)) (alert \"수정 시 다음 열과의 간격이 0.8m 미만이 되어 간섭이 발생합니다. 다음 열을 먼저 수정하거나 거리를 조절하세요.\") (progn (setq do-proceed T) (if (> abs-dist hard-limit) (progn (setq tmp-id (load_dialog dcl-path)) (if (new_dialog \"tsp_brace_hard_warn\" tmp-id) (progn (if (< hard-limit min-req) (progn (set_tile \"warn_msg1\" (strcat \"물리적 여유 공간(\" (rtos hard-limit 2 2) \"m)이 최소 기준(\" (rtos min-req 2 1) \"m) 부족입니다.\")) (set_tile \"warn_msg2\" \"시공 불가 예상 구간이나, 이대로 강제 돌파(적용)하시겠습니까?\"))) (action_tile \"accept\" \"(done_dialog 1)\") (action_tile \"cancel\" \"(done_dialog 0)\") (if (= (start_dialog) 0) (setq do-proceed nil)))) (unload_dialog tmp-id)) "

          "(if (> abs-dist soft-limit) (progn (setq tmp-id (load_dialog dcl-path)) (if (new_dialog \"tsp_brace_warn\" tmp-id) (progn (action_tile \"accept\" \"(done_dialog 1)\") (action_tile \"cancel\" \"(done_dialog 0)\") (if (= (start_dialog) 0) (setq do-proceed nil)))) (unload_dialog tmp-id)))) "

          "(if do-proceed (progn (setq idx (atoi (get_tile \"brace_spec\"))) (if (< idx std-len) (progn (setq res-spec (nth idx *tsp-std-wall-list*)) (setq res-custom (parse-h-spec res-spec))) (progn (setq res-spec \"User-defined\") (setq res-custom (list (atoi (get_tile \"brace_h\")) (atoi (get_tile \"brace_b\")) (atoi (get_tile \"brace_tw\")) (atoi (get_tile \"brace_tf\")))))) (setq res-jack (nth (atoi (get_tile \"brace_jack\")) jack-list)) (setq i 0 new-list '()) (foreach item temp-b-rows (if (= i sel-idx) (setq new-list (append new-list (list (list res-spec res-custom abs-dist res-jack)))) (setq new-list (append new-list (list item)))) (setq i (1+ i))) (setq temp-b-rows new-list) (update-brace-list) (update-upgrade-ui) )))))) (alert \"수정할 항목을 선택하세요!\")))"

        )

      )

      

      (action_tile "btn_del" "(progn (setq sel-idx (atoi (get_tile \"brace_list\"))) (if (and temp-b-rows (>= sel-idx 0) (< sel-idx (length temp-b-rows))) (progn (setq temp-b-rows (vl-remove (nth sel-idx temp-b-rows) temp-b-rows)) (update-brace-list) (update-upgrade-ui)) (alert \"삭제할 항목을 선택하세요!\")))")

      

      (action_tile "btn_comp" "(progn (setq res-upg (if (= (get_tile \"chk_upgrade_wale\") \"1\") (list \"Y\" (nth (atoi (get_tile \"upgrade_wale_spec\")) *tsp-std-wall-list*) nil) nil)) (done_dialog 1))")

      (action_tile "accept" 

        (if (and rem-count (= rem-count 1))

          "(progn (setq res-upg (if (= (get_tile \"chk_upgrade_wale\") \"1\") (list \"Y\" (nth (atoi (get_tile \"upgrade_wale_spec\")) *tsp-std-wall-list*) nil) nil)) (done_dialog 1))" 

          "(progn (setq res-upg (if (= (get_tile \"chk_upgrade_wale\") \"1\") (list \"Y\" (nth (atoi (get_tile \"upgrade_wale_spec\")) *tsp-std-wall-list*) nil) nil)) (done_dialog 2))" 

        )

      )

      (action_tile "cancel" "(done_dialog 0)")

      

      (setq result (start_dialog))

      (unload_dialog dcl-id)

      

      (cond

        ((= result 1)

         (list "COMP" (if (> (length temp-b-rows) 0) (list "Y" temp-b-rows) (list "Y" nil)) res-upg)

        )

        ((= result 2)

         (list "NEXT" (if (> (length temp-b-rows) 0) (list "Y" temp-b-rows) (list "Y" nil)) res-upg)

        )

        (t nil)

      )

    )

  )

)



;;; --------------------------------------------------------------------------

;;; Function: support-dialog-callback

;;; Description: 지보재(Strut/Anchor) 관리 대화상자 콜백 (잭 옵션 처리 추가)

;;; --------------------------------------------------------------------------

(defun support-dialog-callback (dcl-path / dcl-id result temp-supports update-support-list support-types strut-input-dialog anchor-input-dialog shape-list-strut shape-list-anchor)

  (setq dcl-id (load_dialog dcl-path))

  (setq temp-supports (if *tsp-support-list* *tsp-support-list* '()))

  (setq support-types '("버팀보(Strut)" "앵커(Earth Anchor)"))

  (setq shape-list-strut '("H형강" "강관"))

  (setq shape-list-anchor '("Earth Anchor" "Roller Turning Anchor"))

  

  ;; 버팀보 입력창 (잭 옵션 추가)

  (defun strut-input-dialog (existing-data default-name min-depth / s-result s-data mat-list-h mat-list-pipe sec-list-h sec-list-pipe update-ui-strut current-hpile-str default-h-idx jack-list jack-idx)

    (setq mat-list-h '("SS275" "SM275" "SM355" "SM420")) 

    (setq mat-list-pipe '("STP275S" "STP355S" "STP450S" "STP500S"))

    (setq sec-list-h *tsp-std-strut-list*)

    (setq sec-list-pipe '("D406.4×12t" "D508.0×12t" "D609.6×12t"))

    (setq jack-list '("유압잭" "스크류잭")) ;; 잭 종류 리스트



    (setq current-hpile-str "")

    (if (and *tsp-hpile-spec* (= *tsp-hpile-spec* "User-defined") *tsp-hpile-custom*)

       (setq current-hpile-str (strcat "H " (itoa (nth 0 *tsp-hpile-custom*)) "×" (itoa (nth 1 *tsp-hpile-custom*)) "×" (itoa (nth 2 *tsp-hpile-custom*)) "/" (itoa (nth 3 *tsp-hpile-custom*))))

       (setq current-hpile-str *tsp-hpile-spec*)

    )

    (if (and current-hpile-str (/= current-hpile-str "") (not (vl-position current-hpile-str sec-list-h))) 

      (setq sec-list-h (cons current-hpile-str sec-list-h))

    )



    (setq default-h-idx (vl-position "H 300×300×10/15" sec-list-h))

    (if (null default-h-idx) (setq default-h-idx 3))

    

    (if (new_dialog "tsp_support_strut" dcl-id)

      (progn

        (start_list "s_shape") (mapcar 'add_list shape-list-strut) (end_list)

        (start_list "s_jack") (mapcar 'add_list jack-list) (end_list) ;; 잭 리스트 채우기



        (defun update-ui-strut (shape-idx-str / shape-str cur-mats cur-secs)

          (setq shape-str (nth (atoi shape-idx-str) shape-list-strut))

          (if (wcmatch shape-str "H형강") (setq cur-mats mat-list-h cur-secs sec-list-h) (setq cur-mats mat-list-pipe cur-secs sec-list-pipe))

          (start_list "s_mat") (mapcar 'add_list cur-mats) (end_list) (set_tile "s_mat" "0")

          (start_list "s_sec") (mapcar 'add_list cur-secs) (end_list) 

          

          (if (and (wcmatch shape-str "H형강") default-h-idx)

             (set_tile "s_sec" (itoa default-h-idx))

             (set_tile "s_sec" "0")

          )

        )

        

        (action_tile "s_shape" "(update-ui-strut $value)")

        

        (if existing-data

          (progn

            ;; 수정 모드

            (set_tile "s_name" (cadr existing-data))

            (setq init-shape (caddr existing-data)) (setq shape-idx (vl-position init-shape shape-list-strut))

            (if shape-idx (progn (set_tile "s_shape" (itoa shape-idx)) (update-ui-strut (itoa shape-idx))) (progn (set_tile "s_shape" "0") (update-ui-strut "0")))

            (set_tile "s_depth" (rtos (cadddr existing-data) 2 2)) (set_tile "s_count" (itoa (nth 4 existing-data)))

            (setq saved-sec (nth 6 existing-data)) (setq sec-list-to-search (if (wcmatch init-shape "H형강") sec-list-h sec-list-pipe)) (setq sec-idx (vl-position saved-sec sec-list-to-search))

            (if sec-idx (set_tile "s_sec" (itoa sec-idx)))

            

            ;; 잭 정보 로드 (7번째 요소, 없으면 기본값)

            (setq saved-jack (nth 7 existing-data))

            (setq jack-idx (vl-position saved-jack jack-list))

            (if jack-idx (set_tile "s_jack" (itoa jack-idx)) (set_tile "s_jack" "0"))

          )

          (progn 

            ;; 신규 입력 모드

            (set_tile "s_name" default-name) 

            (set_tile "s_shape" "0") 

            (set_tile "s_jack" "1")

            (update-ui-strut "0")

          )

        )

        (action_tile "accept" "(progn (setq res-name (get_tile \"s_name\")) (setq res-shape (nth (atoi (get_tile \"s_shape\")) shape-list-strut)) (setq res-depth (atof (get_tile \"s_depth\"))) (setq res-count (atoi (get_tile \"s_count\"))) (setq m-idx (atoi (get_tile \"s_mat\"))) (if (wcmatch res-shape \"H형강\") (setq res-mat (nth m-idx mat-list-h)) (setq res-mat (nth m-idx mat-list-pipe))) (setq sec-idx (atoi (get_tile \"s_sec\"))) (if (wcmatch res-shape \"H형강\") (setq res-sec (nth sec-idx sec-list-h)) (setq res-sec (nth sec-idx sec-list-pipe))) (setq res-jack (nth (atoi (get_tile \"s_jack\")) jack-list)) (cond ((or (= res-name \"\") (<= res-depth 0.0)) (alert \"이름과 설치깊이를 확인하세요.\")) ((<= res-depth min-depth) (alert \"설치 깊이는 이전 단계보다 깊어야 합니다!\")) (t (setq s-data (list \"버팀보(Strut)\" res-name res-shape res-depth res-count res-mat res-sec res-jack nil)) (done_dialog 1))))")

        (action_tile "cancel" "(done_dialog 0)") (if (= (start_dialog) 1) s-data nil)

      )

    )

  )



  (defun anchor-input-dialog (existing-data default-name min-depth / a-result a-data mat-list-anchor sec-list-anchor)

    (setq mat-list-anchor '("SWPC7A" "SWPC7B")) (setq sec-list-anchor '()) (setq i 1)

    (repeat 15 (setq sec-list-anchor (append sec-list-anchor (list (strcat "Strand12.7×" (itoa i) "EA")))) (setq i (1+ i)))

    (if (new_dialog "tsp_support_anchor" dcl-id)

      (progn

        (start_list "a_shape") (mapcar 'add_list shape-list-anchor) (end_list)

        (start_list "a_mat") (mapcar 'add_list mat-list-anchor) (end_list)

        (start_list "a_sec") (mapcar 'add_list sec-list-anchor) (end_list)

        (if existing-data

          (progn

            (set_tile "a_name" (cadr existing-data))

            (setq init-shape (caddr existing-data)) (setq shape-idx (vl-position init-shape shape-list-anchor))

            (if shape-idx (set_tile "a_shape" (itoa shape-idx)) (set_tile "a_shape" "0"))

            (set_tile "a_depth" (rtos (cadddr existing-data) 2 2)) (set_tile "a_angle" (rtos (nth 4 existing-data) 2 1)) (set_tile "a_free" (rtos (nth 5 existing-data) 2 2)) (set_tile "a_bond" (rtos (nth 6 existing-data) 2 2))

            (set_tile "a_mat" "0") (set_tile "a_sec" "0")

          )

          (progn (set_tile "a_name" default-name) (set_tile "a_shape" "0") (set_tile "a_angle" "30") (set_tile "a_bond" "3") (set_tile "a_mat" "1") (set_tile "a_sec" "3"))

        )

        (action_tile "accept" "(progn (setq res-name (get_tile \"a_name\")) (setq res-shape (nth (atoi (get_tile \"a_shape\")) shape-list-anchor)) (setq res-depth (atof (get_tile \"a_depth\"))) (setq res-ang (atof (get_tile \"a_angle\"))) (setq res-free (atof (get_tile \"a_free\"))) (setq res-bond (atof (get_tile \"a_bond\"))) (setq res-mat (nth (atoi (get_tile \"a_mat\")) mat-list-anchor)) (setq res-sec (nth (atoi (get_tile \"a_sec\")) sec-list-anchor)) (cond ((or (= res-name \"\") (<= res-depth 0.0)) (alert \"이름과 설치깊이를 확인하세요.\")) ((< res-free 4.5) (alert \"자유장은 최소 4.5m 이상이어야 합니다.\")) ((> res-ang 40.0) (alert \"설치각도는 40도 이하여야 합니다.\")) ((<= res-depth min-depth) (alert \"설치 깊이는 이전 단계보다 깊어야 합니다!\")) (t (setq a-data (list \"앵커(Earth Anchor)\" res-name res-shape res-depth res-ang res-free res-bond res-mat res-sec)) (done_dialog 1))))")

        (action_tile "cancel" "(done_dialog 0)") (if (= (start_dialog) 1) a-data nil)

      )

    )

  )



  (if (new_dialog "tsp_support" dcl-id)

    (progn

      (start_list "support_type_sel") (mapcar 'add_list support-types) (end_list) (set_tile "support_type_sel" "0")

      (defun update-support-list (/ s-type s-code disp-str free-len bond-len)

        (start_list "support_list")

        (foreach item temp-supports

          (setq s-type (car item))

          (cond

            ((wcmatch s-type "버팀보*") (setq s-code "ST") (setq disp-str (strcat (cadr item) "     " s-code "     " (rtos (cadddr item) 2 2) "m")))

            ((wcmatch s-type "앵커*") (setq s-code "EA") (setq free-len (rtos (nth 5 item) 2 2)) (setq bond-len (rtos (nth 6 item) 2 2)) (setq disp-str (strcat (cadr item) "     " s-code "     " (rtos (cadddr item) 2 2) "m" "   (정착장 " bond-len "m 자유장 " free-len "m)")))

            (t (setq s-code "??") (setq disp-str "..."))

          )

          (add_list disp-str)

        )

        (end_list)

      )

      (update-support-list)

      (action_tile "btn_add" "(progn (setq t-idx (atoi (get_tile \"support_type_sel\"))) (setq t-str (nth t-idx support-types)) (setq count 1) (foreach item temp-supports (if (= (car item) t-str) (setq count (1+ count)))) (if (wcmatch t-str \"버팀보*\") (setq def-name (strcat \"Strut_\" (itoa count)))) (if (wcmatch t-str \"앵커*\") (setq def-name (strcat \"Anchor_\" (itoa count)))) (setq last-depth (if temp-supports (cadddr (last temp-supports)) 0.0)) (setq new-data nil) (if (wcmatch t-str \"버팀보*\") (setq new-data (strut-input-dialog nil def-name last-depth)) (setq new-data (anchor-input-dialog nil def-name last-depth))) (if new-data (progn (setq temp-supports (append temp-supports (list new-data))) (update-support-list))))")

      (action_tile "btn_modify" "(progn (setq sel-idx-str (get_tile \"support_list\")) (if (and sel-idx-str (/= sel-idx-str \"\")) (progn (setq sel-idx (atoi sel-idx-str)) (setq existing-item (nth sel-idx temp-supports)) (setq ex-type (car existing-item)) (setq min-depth 0.0) (if (> sel-idx 0) (setq min-depth (cadddr (nth (1- sel-idx) temp-supports)))) (setq mod-data nil) (if (wcmatch ex-type \"버팀보*\") (setq mod-data (strut-input-dialog existing-item nil min-depth)) (setq mod-data (anchor-input-dialog existing-item nil min-depth))) (if mod-data (progn (setq i 0) (setq new-list '()) (foreach item temp-supports (if (= i sel-idx) (setq new-list (append new-list (list mod-data))) (setq new-list (append new-list (list item)))) (setq i (1+ i))) (setq temp-supports new-list) (update-support-list)))) (alert \"수정할 항목을 선택하세요!\")))")

      (action_tile "btn_delete" "(progn (setq sel-idx (atoi (get_tile \"support_list\"))) (if (and temp-supports (>= sel-idx 0) (< sel-idx (length temp-supports))) (progn (setq temp-supports (vl-remove (nth sel-idx temp-supports) temp-supports)) (update-support-list)) (alert \"삭제할 항목을 선택하세요!\")))")

      (action_tile "btn_clear" "(progn (setq temp-supports '()) (update-support-list))")

      (action_tile "accept" "(progn (setq *tsp-support-list* temp-supports) (done_dialog 1))")

      (action_tile "back" "(done_dialog 2)") (action_tile "cancel" "(done_dialog 0)")

      (setq result (start_dialog)) (unload_dialog dcl-id) result

    )

  )

)



;;; ==========================================================================

;;; [SECTION 7] 메인 실행 명령어 (Main Commands)

;;; ==========================================================================



;;; ==========================================================================

;;; Function: tsp-zoom-to-right-half

;;; Description: 평면도 전용 화면 우측 정렬 줌

;;; ==========================================================================

(defun tsp-zoom-to-right-half (vertices / minX maxX minY maxY w h newMinX newMaxX newMinY newMaxY)

  (setq minX (apply 'min (mapcar 'car vertices)))

  (setq maxX (apply 'max (mapcar 'car vertices)))

  (setq minY (apply 'min (mapcar 'cadr vertices)))

  (setq maxY (apply 'max (mapcar 'cadr vertices)))

  

  (setq w (- maxX minX) h (- maxY minY))

  

  (setq minX (- minX (* w 0.3)) maxX (+ maxX (* w 0.3)))

  (setq minY (- minY (* h 0.3)) maxY (+ maxY (* h 0.3)))

  (setq w (- maxX minX) h (- maxY minY))

  

  ;; 평면도는 기본 비율(w) 적용

  (setq newMinX (- minX w))

  (setq newMaxX maxX)

  (setq newMinY minY)

  (setq newMaxY maxY)

  

  (command "_.ZOOM" "_W" (list newMinX newMinY) (list newMaxX newMaxY))

  (princ)

)



;;; ==========================================================================

;;; Function: tsp-zoom-legend

;;; Description: 범례 전용 화면 우측 정렬 줌

;;; ==========================================================================

(defun tsp-zoom-legend (vertices / minX maxX minY maxY w h newMinX newMaxX newMinY newMaxY)

  (setq minX (apply 'min (mapcar 'car vertices)))

  (setq maxX (apply 'max (mapcar 'car vertices)))

  (setq minY (apply 'min (mapcar 'cadr vertices)))

  (setq maxY (apply 'max (mapcar 'cadr vertices)))

  

  (setq w (- maxX minX) h (- maxY minY))

  

  (setq minX (- minX (* w 0.3)) maxX (+ maxX (* w 0.3)))

  (setq minY (- minY (* h 0.3)) maxY (+ maxY (* h 0.3)))

  (setq w (- maxX minX) h (- maxY minY))

  

  ;; 범례는 대화상자를 피하기 위해 1.6배(1.6*w) 적용

  (setq newMinX (- minX (* w 1.6)))

  (setq newMaxX maxX)

  (setq newMinY minY)

  (setq newMaxY maxY)

  

  (command "_.ZOOM" "_W" (list newMinX newMinY) (list newMaxX newMaxY))

  (princ)

)



;;; ==========================================================================

;;; 실시간 중첩 방지 작도 (Real-time Redraw with Clean-up)

;;; ==========================================================================

(if (not (boundp '*tsp-realtime-entities*)) (setq *tsp-realtime-entities* '()))



(defun tsp-redraw-realtime-plan (boundary-ent boundary-orient / next-ent)

  ;; 1. 이전에 그려진 실시간 객체가 있다면 싹 지우기 (중첩 방지)

  (if *tsp-realtime-entities*

    (foreach ent *tsp-realtime-entities*

      (if (entget ent) (vl-catch-all-apply 'entdel (list ent)))

    )

  )

  (setq *tsp-realtime-entities* '())

  

  ;; 2. 도면 새로 그리기 시작 지점 마킹

  (setq next-ent (entlast))

  

  ;; 3. 실제 작도 함수 호출

  (create-hpile-set-on-boundary boundary-ent boundary-orient)

  

  ;; 4. 방금 새로 생성된 모든 객체를 추적 리스트에 담기

  (while (setq next-ent (entnext next-ent))

    (setq *tsp-realtime-entities* (append *tsp-realtime-entities* (list next-ent)))

  )

)



;;; ==========================================================================

;;; Function: tsp-fix-mirrored-polyline

;;; Description: 경계선 Z축 양수화(OCS 좌표계 재조정)

;;; ==========================================================================

(defun tsp-fix-mirrored-polyline (ent / ent-data normal new-data item pt bulge)

  (setq ent-data (entget ent))

  (setq normal (cdr (assoc 210 ent-data)))

  

  (if (and normal (< (caddr normal) 0.0))

    (progn

      (princ "\n[시스템] OCS 좌표 및 Z축을 양수(+)로 자동 보정")

      (setq new-data '())

      (foreach item ent-data

        (cond

          ((= (car item) 10)

           (setq pt (cdr item))

           (setq new-data (append new-data (list (cons 10 (list (- (car pt)) (cadr pt))))))

          )

          ;; 곡선(Bulge) 부호 반전

          ((= (car item) 42)

           (setq bulge (cdr item))

           (setq new-data (append new-data (list (cons 42 (- bulge)))))

          )

          ;; Extrusion Direction (Z축) 정상화

          ((= (car item) 210)

           (setq new-data (append new-data (list (cons 210 '(0.0 0.0 1.0)))))

          )

          (t

           (setq new-data (append new-data (list item)))

          )

        )

      )

      ;; 객체 업데이트

      (entmod new-data)

      (entupd ent)

      T

    )

    nil

  )

)



;;; --------------------------------------------------------------------------

;;; Command: TSP

;;; Description: 메인 명령

;;; --------------------------------------------------------------------------

(defun C:TSP (/ dcl-path boundary-ent boundary-orient step status sel loop-program loop-draw seg-data section-draw-flag opt soil-src valid-sources idx s-data src-num src-idx perform-draw input-valid default-src save-chk loop-save saved-list user-start-pt vertices poly-orient start-idx dist-start dist-end saved-start-pt use-saved last-ent handle group-name dict group-exists has-brace-y ans g-brace i new-list missing-brace-list b-data curr-len prev-idx prev-data prev-len limits new-brace abort-draw rem-count res ans2 mseg midx check-data draw-seg draw-cor proceed-draw choice-res ans-cor brace-res selected-idx)

  (setvar "cmdecho" 0)

  (princ "\n========================================")

  (princ "\nTSP - Segment Manager System")

  (princ "\n========================================\n")

  

  (setq dcl-path (create-tsp-dcl))

  (setq step 0)

  (setq loop-program T)

  (setq *tsp-data-dirty* nil) 

  

  (setq *tsp-realtime-entities* '())

  

  (setq status (scale-dialog-callback dcl-path))

  (if (= status 0) (setq loop-program nil))



  (setq *tsp-data-dirty* nil)

  

  (if loop-program

    (progn

      (setq boundary-ent nil)

      (while (not boundary-ent)

        (initget "Exit") 

        (setq sel (entsel "\n'STEP 1' 전체 구간의 경계선(Polyline)을 선택하세요 [종료(Exit)]: "))

        

        (cond

          ((= sel "Exit") (princ "\n명령을 종료합니다.") (setq boundary-ent "CANCEL"))

          ((/= sel nil) (setq boundary-ent (car sel)))

          ((= sel nil) (princ "\n선택되지 않았습니다. 다시 선택해주세요."))

          ((= (getvar "ERRNO") 7) (princ "\n빈 공간입니다. 다시 선택하세요.") (setvar "ERRNO" 0))

          (t (princ "\n선택 취소됨.") (setq boundary-ent "CANCEL"))

        )

      )

      

      (if (and boundary-ent (/= boundary-ent "CANCEL"))

        (progn

          

          (setq boundary-orient (determine-boundary-orientation boundary-ent))

          

          (setq *tsp-boundary-ent* boundary-ent)

          (setq *tsp-boundary-orient* boundary-orient)

          

          (if boundary-orient

            (progn

              (setq vertices (extract-vertices boundary-ent))

              (tsp-zoom-to-right-half vertices)

              

              (setq saved-start-pt (vlax-ldata-get boundary-ent "TSP_START_PT"))

              (setq user-start-pt nil)

              (setq use-saved nil)



              (if saved-start-pt

                (progn

                  (initget "Yes No")

                  (if (= (getkword "\n 이전에 사용된 시작점이 있습니다. 그대로 사용하시겠습니까? [Yes(Y)/No(N)] <Y>: ") "No")

                    (setq use-saved nil)

                    (setq use-saved T)

                  )

                )

              )

              

              (if use-saved

                (setq user-start-pt saved-start-pt)

                (progn

                  (setq *segment-list* nil)

                  (setq *tsp-current-project-name* "") 

                  (princ "\n[시스템] 새로운 시작점이 지정되어 기존 세그먼트 설정 및 프로젝트 이름이 초기화됩니다.")

                  

                  (setq user-start-pt (getpoint "\n세그먼트 시작점(No.1)이 될 위치를 클릭하세요: "))

                  (if user-start-pt

                    (progn

                      (setq user-start-pt (trans user-start-pt 1 0))

                      (vlax-ldata-put boundary-ent "TSP_START_PT" user-start-pt) 

                    )

                  )

                )

              )

              

              (if (is-closed-polyline boundary-ent)

                (progn

                   (setq poly-orient (get-polygon-orientation vertices))

                   (if (= poly-orient 1)

                     (progn 

                       (setq vertices (reverse vertices)) 

                       (setq boundary-orient (- boundary-orient))

                       (setq *tsp-boundary-orient* boundary-orient)

                     )

                   )

                   (if user-start-pt

                     (progn

                       (setq start-idx (tsp-get-closest-vertex-index vertices user-start-pt))

                       (setq vertices (tsp-shift-vertex-list vertices start-idx))

                     )

                   )

                )

                (progn

                   (if user-start-pt

                     (progn

                       (setq dist-start (distance (car vertices) user-start-pt))

                       (setq dist-end (distance (last vertices) user-start-pt))

                       (if (< dist-end dist-start)

                         (progn 

                           (setq vertices (reverse vertices)) 

                           (setq boundary-orient (- boundary-orient))

                           (setq *tsp-boundary-orient* boundary-orient)

                         )

                       )

                     )

                   )

                )

              )

              

              (init-segment-data boundary-ent boundary-orient vertices)

              

              (setq handle (tsp-get-entity-handle boundary-ent))

              (setq group-name (strcat "TSP_GRP_" handle))

              (setq dict (dictsearch (namedobjdict) "ACAD_GROUP"))

              (setq group-exists (if (and dict (dictsearch (cdr (assoc -1 dict)) group-name)) T nil))



              (if (not group-exists)

                (progn

                  (setq last-ent (entlast))

                  (draw-segment-numbers boundary-ent boundary-orient vertices) 

                  (tsp-group-last-entities last-ent boundary-ent)

                )

                (princ "\n[알림] 기존 평면도를 유지한 상태로 설정창에 진입합니다.")

              )

              

              (tsp-redraw-realtime-plan boundary-ent boundary-orient)



              (setq loop-draw T)

              (while loop-draw

                (if vertices (tsp-zoom-to-right-half vertices))

                

                (setq status (manager-dialog-callback dcl-path))

                

                (cond

                  ((= status 0) 

                    (setq loop-draw nil)

                    (if *tsp-data-dirty*

                      (progn

                        (initget "Yes No")

                        (setq save-chk (getkword (strcat "\n변경된 내용을 저장하시겠습니까? (프로젝트: " *tsp-current-project-name* ") [Yes(Y)/No(N)] <Y>: ")))

                        (if (null save-chk) (setq save-chk "Yes"))

                        (if (= save-chk "Yes")

                          (tsp-save-project-data *tsp-current-project-name*)

                          (princ "\n[알림] 저장하지 않고 종료합니다.")

                        )

                      )

                      (princ "\n[알림] 변경된 내용이 없어 종료합니다.")

                    )

                    (princ "\nTSP 명령을 종료합니다.")

                  )

                  

                  ((= status 5)

                   (setq check-data (nth selected-idx *segment-list*))

                   (setq b-data (cdr (assoc 'CORNER-BRACE check-data)))

                   (setq draw-seg T draw-cor nil)

                   (setq proceed-draw T)

                   

                   (if (and b-data (= (car b-data) "Y"))

                     (progn

                       (setq choice-res (section-choice-dialog-callback dcl-path))

                       (if choice-res

                         (progn

                           (setq draw-seg (car choice-res) draw-cor (cadr choice-res))

                           

                           (if draw-cor

                             (progn

                               (if (null (cadr b-data))

                                 (progn

                                   (initget "Input Skip Return")

                                   (setq ans-cor (getkword "\n선택한 모서리에 사보강재 정보가 없습니다. 작업선택 [입력(Input)/생략(Skip)/되돌리기(Return)] <Input>: "))

                                   (if (null ans-cor) (setq ans-cor "Input"))

                                   (cond

                                     ((= ans-cor "Input")

                                      (setq limits (tsp-calculate-brace-limit selected-idx))

                                      (setq brace-res (brace-dialog-callback dcl-path check-data limits 0))

                                      (if (and (listp brace-res) (or (= (car brace-res) "COMP") (= (car brace-res) "NEXT")))

                                        (progn

                                          (setq new-brace (cadr brace-res))

                                          (setq check-data (subst (cons 'CORNER-BRACE new-brace) (assoc 'CORNER-BRACE check-data) check-data))

                                          (setq check-data (subst (cons 'UPGRADE-WALE (caddr brace-res)) (assoc 'UPGRADE-WALE check-data) check-data))

                                          (setq *segment-list* (update-list-item *segment-list* selected-idx check-data))

                                          (setq b-data new-brace)

                                          (if (and new-brace (cadr new-brace) (> (length (cadr new-brace)) 0))

                                            (alert "모서리 단면도 작도는 아직 작업중입니다.")

                                            (princ "\n사보강재 정보가 입력되지 않았습니다.")

                                          )

                                        )

                                        (princ "\n입력이 취소되었습니다.")

                                      )

                                     )

                                     ((= ans-cor "Skip")

                                      (princ "\n모서리 단면도 생성을 생략합니다.")

                                     )

                                     ((= ans-cor "Return")

                                      (setq proceed-draw nil draw-seg nil)

                                     )

                                   )

                                 )

                                 (alert "모서리 단면도 작도는 아직 작업중입니다.")

                               )

                             )

                           )

                         )

                         (setq proceed-draw nil draw-seg nil)

                       )

                     )

                   )

                   

                   (if (and proceed-draw draw-seg)

                     (progn

                       (setq perform-draw T)

                       (if (or (not *tsp-target-draw-flag*) (null *tsp-soil-layers*) (= *tsp-soil-layers* '()))

                         (progn

                           (initget "Input Copy Skip Return")

                           (setq opt (getkword "\n선택한 세그먼트에 지반 정보가 없거나 '단면생성 N'입니다. 작업 선택 [입력(Input)/복사(Copy)/생략(Skip)/되돌리기(Return)] <Skip>: "))

                           (cond

                             ((= opt "Input") (if (= (soil-dialog-callback dcl-path) 1) (princ "\n지반 정보 입력 완료.") (progn (princ "\n입력이 취소되었습니다.") (setq perform-draw nil))))

                             ((= opt "Copy")

                              (setq valid-sources '() idx 0)

                              (foreach seg *segment-list* (setq s-data (cdr (assoc 'SOIL-DATA seg))) (if (and s-data (/= s-data '())) (setq valid-sources (append valid-sources (list (1+ idx))))) (setq idx (1+ idx)))

                              (if valid-sources

                                (progn

                                  (princ (strcat "\n복사 가능한 세그먼트 번호: " (vl-princ-to-string valid-sources)))

                                  (setq input-valid nil default-src (car valid-sources))

                                  (while (not input-valid)

                                    (setq src-num (getint (strcat "\n지반 정보를 가져올 세그먼트 번호를 입력하세요 [기본값: " (itoa default-src) "]: ")))

                                    (if (null src-num) (setq src-num default-src))

                                    (if (vl-position src-num valid-sources)

                                      (progn (setq input-valid T src-idx (1- src-num)) (setq *tsp-soil-layers* (cdr (assoc 'SOIL-DATA (nth src-idx *segment-list*)))) (princ (strcat "\n" (itoa src-num) "번 세그먼트의 지반 정보를 적용했습니다.")))

                                      (princ "\n잘못된 번호입니다.")

                                    )

                                  )

                                )

                                (progn (alert "지반 정보가 입력된 세그먼트가 하나도 없습니다.") (setq perform-draw nil))

                              )

                             )

                             ((= opt "Return") (setq perform-draw nil))

                             (t (princ "\n지반 정보 없이 구조물 단면만 그립니다."))

                           )

                         )

                       )

                       (if perform-draw 

                         (progn 

                           (if (draw-section-ground-line) 

                             (princ "\n단면도 작도 완료.") 

                             (progn 

                               (princ "\n단면도 작도 취소됨.")

                               (setq proceed-draw nil) ;; ESC 취소 시 질문을 건너뛰도록 플래그 변경

                             )

                           ) 

                         )

                       )

                     )

                   )

                   (if proceed-draw

                     (progn

                       (initget "Yes No") 

                       (setq choice-res (vl-catch-all-apply 'getkword (list "\n설정창으로 돌아가시겠습니까? [Yes(Y)/No(N)] <Y>: ")))

                       (if (vl-catch-all-error-p choice-res)

                         (setq loop-draw nil)

                         (if (= choice-res "No") (setq loop-draw nil))

                       )

                     )

                   )

                  )

                  

                  ((= status 6)

                   (setq abort-draw nil)

                   (setq missing-brace-list '())

                   

                   (if vertices (tsp-zoom-to-right-half vertices))

                   

                   (setq i 0)

                   (while (< i (length *segment-list*))

                     (setq seg-data (nth i *segment-list*))

                     (setq b-data (cdr (assoc 'CORNER-BRACE seg-data)))

                     (if (and b-data (= (car b-data) "Y") (null (cadr b-data)))

                       (setq missing-brace-list (append missing-brace-list (list i)))

                     )

                     (setq i (1+ i))

                   )



                   (if (> (length missing-brace-list) 0)

                     (progn

                       (initget "Yes No")

                       (setq ans (getkword (strcat "\n사보강재 설정이 누락된 모서리가 " (itoa (length missing-brace-list)) "곳 있습니다. 추가하시겠습니까? [Yes(Y)/No(N)] <Y>: ")))

                       (if (or (null ans) (= ans "Yes"))

                         (progn

                           (while (and (> (length missing-brace-list) 0) (not abort-draw))

                             (setq idx (car missing-brace-list))

                             (setq missing-brace-list (cdr missing-brace-list))

                             

                             (setq seg-data (nth idx *segment-list*))

                             

                             (setq limits (tsp-calculate-brace-limit idx))

                             

                             (setq rem-count (1+ (length missing-brace-list)))

                             (setq res (brace-dialog-callback dcl-path seg-data limits rem-count))

                             

                             (cond

                               ((and (listp res) (= (car res) "NEXT"))

                                (setq new-brace (cadr res))

                                (if (and new-brace (cadr new-brace) (> (length (cadr new-brace)) 0))

                                  (progn

                                    (setq seg-data (subst (cons 'CORNER-BRACE new-brace) (assoc 'CORNER-BRACE seg-data) seg-data))

                                    (setq seg-data (subst (cons 'UPGRADE-WALE (caddr res)) (assoc 'UPGRADE-WALE seg-data) seg-data))

                                    (setq *segment-list* (update-list-item *segment-list* idx seg-data))

                                    (if (and *tsp-boundary-ent* *tsp-boundary-orient*)

                                      (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*)

                                    )

                                  )

                                  (progn

                                    (princ (strcat "\n[" (itoa (cdr (assoc 'ID seg-data))) "번 모서리] 데이터가 비어있어 순서를 맨 뒤로 미룹니다."))

                                    (setq missing-brace-list (append missing-brace-list (list idx)))

                                  )

                                )

                               )

                               

                               ((and (listp res) (= (car res) "COMP"))

                                (setq new-brace (cadr res))

                                (if (and new-brace (cadr new-brace) (> (length (cadr new-brace)) 0))

                                  (progn

                                    (setq seg-data (subst (cons 'CORNER-BRACE new-brace) (assoc 'CORNER-BRACE seg-data) seg-data))

                                    (setq seg-data (subst (cons 'UPGRADE-WALE (caddr res)) (assoc 'UPGRADE-WALE seg-data) seg-data))

                                    (setq *segment-list* (update-list-item *segment-list* idx seg-data))

                                    (if (and *tsp-boundary-ent* *tsp-boundary-orient*)

                                      (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*)

                                    )

                                  )

                                  (progn

                                    (princ (strcat "\n[" (itoa (cdr (assoc 'ID seg-data))) "번 모서리] 데이터가 비어있습니다. 해당 사보강재 설정을 건너뜁니다."))

                                    (setq seg-data (subst (cons 'CORNER-BRACE (list "N" nil)) (assoc 'CORNER-BRACE seg-data) seg-data))

                                    (setq *segment-list* (update-list-item *segment-list* idx seg-data))

                                    (if (and *tsp-boundary-ent* *tsp-boundary-orient*)

                                      (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*)

                                    )

                                  )

                                )

                                

                                (if (> (length missing-brace-list) 0)

                                  (progn

                                    (initget "Yes No")

                                    (setq ans2 (getkword (strcat "\n아직 사보강재가 입력되지 않은 세그먼트가 " (itoa (length missing-brace-list)) "곳 남았습니다. 이대로 설정을 완전히 종료하시겠습니까? (Yes:완료 / No:마저입력) [Yes/No] <No>: ")))

                                    (if (= ans2 "Yes")

                                      (progn

                                        (foreach midx missing-brace-list

                                          (setq mseg (nth midx *segment-list*))

                                          (setq mseg (subst (cons 'CORNER-BRACE (list "N" nil)) (assoc 'CORNER-BRACE mseg) mseg))

                                          (setq *segment-list* (update-list-item *segment-list* midx mseg))

                                        )

                                        (setq missing-brace-list '())

                                      )

                                    )

                                  )

                                )

                               )

                               

                               (t

                                 (princ "\n입력이 취소되었습니다. 평면도 생성을 중단합니다.")

                                 (setq abort-draw T)

                               )

                             )

                           ) ;; while end

                         )

                         (progn

                           (princ "\n사보강재 추가를 건너뜁니다. 해당 모서리는 사보강재 없이 작도됩니다.")

                           (foreach idx missing-brace-list

                             (setq seg-data (nth idx *segment-list*))

                             (setq seg-data (subst (cons 'CORNER-BRACE (list "N" nil)) (assoc 'CORNER-BRACE seg-data) seg-data))

                             (setq *segment-list* (update-list-item *segment-list* idx seg-data))

                           )

                         )

                       )

                     )

                   )



                   (if (not abort-draw)

                     (progn

                       (princ "\n모든 데이터가 확인되었습니다. 최종 평면도를 갱신합니다.")

                       (if (and *tsp-boundary-ent* *tsp-boundary-orient*)

                         (tsp-redraw-realtime-plan *tsp-boundary-ent* *tsp-boundary-orient*)

                       )

                     )

                   )

                   

                   (if (not abort-draw)

                     (if *tsp-cancel-flag*

                       (setq *tsp-cancel-flag* nil) 

                       

                       (progn

                         (initget "Yes No")

                         (setq user-ans (vl-catch-all-apply 'getkword (list "\n설정창으로 다시 돌아가시겠습니까? [Yes(Y)/No(N)] <Y>: ")))

                         (if (vl-catch-all-error-p user-ans)

                           (setq loop-draw nil)

                           (if (= user-ans "No") (setq loop-draw nil))

                         )

                       )

                     )

                   )

                  )

                ) ;; end cond

              ) ;; end while loop-draw

            ) ;; end progn (if boundary-orient)

          ) ;; end if boundary-orient

        ) ;; end progn (if boundary-ent)

      ) ;; end if boundary-ent

    ) ;; end progn (if loop-program)

  ) ;; end if loop-program

  

  (setvar "cmdecho" 1)

  (princ)

)



(princ "\nTSP.lsp 로드 완료!")

(princ "\n  명령어: TSP")

(princ)







;;;;==========================================================================

;;;; defun c:TESTCIP : C.I.P DCL UI 독립 테스트용 임시 명령어

;;;;==========================================================================

(defun c:TESTCIP ()

  (princ "\n--- C.I.P DCL UI 테스트 모드 ---")

  (tsp-run-cip-dialog)

  (princ "\n--- UI 테스트 종료 ---")

  (princ)

)