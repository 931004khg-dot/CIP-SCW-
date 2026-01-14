# TSP.lsp - Temporary Structure Plan (가시설 흙막이 벽체 자동 작도 시스템)

## 📋 개요

AutoCAD에서 가시설 흙막이 벽체를 자동으로 작도하는 LISP 프로그램입니다.

**버전**: 1.0.0  
**작성일**: 2026-01-13  

## 🎯 주요 기능

### 구현 완료된 기능
- ✅ **H-Pile + 토류판 공법**
  - H-Pile 규격 선택 (4가지 표준 규격 + 사용자 정의)
  - 띠장(Wale) 규격 선택 (4가지 표준 규격 + 사용자 정의)
  - C.T.C(Center To Center) 간격 설정
  - 경계선 선택 및 띠장 옵셋 자동 생성
  - H-Pile 세트 자동 생성 (I자 형태 단면)
  - 토류판 자동 생성 및 ANSI36 해치 적용

### 향후 구현 예정
- ⏳ CIP (현장타설말뚝) 공법
- ⏳ SCW (소일시멘트벽) 공법
- ⏳ 버팀보(Strut) 배치
- ⏳ 치수선 및 문자 자동 표기
- ⏳ 물량 산출

## 📐 H-Pile 규격

### 표준 규격
| 번호 | 규격 | H (높이) | B (폭) | tw (웹 두께) | tf (플랜지 두께) |
|------|------|----------|--------|--------------|------------------|
| 1 | H 298×201×9/14 | 298mm | 201mm | 9mm | 14mm |
| 2 | H 300×300×10/15 | 300mm | 300mm | 10mm | 15mm |
| 3 | H 350×350×12/19 | 350mm | 350mm | 12mm | 19mm |
| 4 | H 400×400×13/21 | 400mm | 400mm | 13mm | 21mm |

### 사용자 정의
- User-defined 옵션 선택 시 직접 입력 가능

## 🎨 레이어 및 색상 구성

| 객체 | 레이어명 | 색상 | 설명 |
|------|----------|------|------|
| 띠장 옵셋 (0mm, H) | `_띠장(wale)` | 초록색 (3) | 경계선 및 최대 옵셋 |
| 띠장 옵셋 (tf, H-tf) | `_띠장(wale)` | 빨간색 (1) | 중간 옵셋 |
| H-Pile | `_H-Pile_298x201x9-14` | 초록색 (3) | 규격별 동적 레이어 |
| 토류판 | `_토류판(timber)` | 빨간색 (1) | ANSI36 해치 포함 |

## 🚀 사용 방법

### 1. 기본 사용

1. AutoCAD 실행
2. APPLOAD 명령 입력
3. `TSP.lsp` 파일 선택 및 로드
4. 명령줄에 `TSP` 입력
5. 공법 선택 (H-Pile + 토류판)
6. H-Pile 및 띠장 규격 선택
7. C.T.C 간격 입력 (기본값: 2m)
8. 경계선 선택 (Polyline 또는 Line)
9. 자동 생성 완료!

### 2. 디버깅 모드 사용

디버깅이 필요한 경우:

1. `TSP.lsp` 파일을 텍스트 에디터로 엽니다
2. `TSP-debug.lsp` 파일의 내용을 **복사**합니다
3. `TSP.lsp` 파일의 **맨 끝**에 붙여넣기 합니다
4. 저장 후 AutoCAD에서 다시 로드합니다

#### 디버그 명령어

```lisp
; 디버그 로그 출력
(debug-show-log)

; 디버그 로그 초기화
(debug-clear)

; 디버그 모드 비활성화
(setq *tsp-debug* nil)

; 디버그 모드 활성화
(setq *tsp-debug* T)
```

## 🔍 디버그 로그 예시

```
[DEBUG] ========================================
[DEBUG] TSP 명령 시작
[DEBUG] 버전: 1.0.0
[DEBUG] 날짜: 2026-01-13
[DEBUG] ========================================
[DEBUG] DIAG_START: DCL 생성 시작
[DEBUG] TSP 버전: 1.0.0, 날짜: 2026-01-13
[DEBUG] *tsp-debug* = true
[DEBUG] LISP 파일 경로: C:\...\TSP.lsp
[DEBUG] DCL 경로 (LISP 디렉토리): C:\...\tsp.dcl
[DEBUG] DCL 파일 열기 성공
[DEBUG] DCL 파일 생성 완료: C:\...\tsp.dcl
[DEBUG] === hpile-dialog-callback 시작 ===
[DEBUG] Dialog 결과: 1
[DEBUG] 선택된 H-Pile: H 298×201×9/14
[DEBUG] 선택된 띠장: H 300×300×10/15
[DEBUG] C.T.C: 2.00
[DEBUG] parse-h-spec 입력: "H 298×201×9/14"
[DEBUG] 'H ' 제거 후: "298×201×9/14"
[DEBUG] 숫자 추출 결과: "298 201 9 14"
[DEBUG] 파싱 완료: (298 201 9 14)
[DEBUG] === create-hpile-set 시작 ===
[DEBUG] H-Pile 단면 생성 완료
[DEBUG] 토류판 및 해치 생성 완료
[DEBUG] === create-hpile-set 완료 ===
```

## 📂 파일 구성

```
/home/user/webapp/
├── TSP.lsp          # 메인 프로그램 (935줄)
├── TSP-debug.lsp    # 디버깅 모듈 (1022줄)
├── tsp.dcl          # Dialog 정의 (자동 생성)
└── TSP-README.md    # 본 문서
```

## ⚙️ 작동 원리

### 1. DCL Dialog 생성
- LISP 파일 위치에서 `tsp.dcl` 자동 생성
- 메인 Dialog: 공법 선택
- H-Pile Dialog: 규격 및 C.T.C 설정

### 2. 경계선 분석
- LWPOLYLINE 또는 LINE 엔티티 지원
- 가장 긴 세그먼트 자동 탐색
- 중점 계산

### 3. 띠장 옵셋 생성
- 경계선 기준 내부 방향으로 옵셋
- 4개의 옵셋 라인 생성:
  - 0mm (경계선, 초록색)
  - tf (플랜지 두께, 빨간색)
  - H-tf (높이 - 플랜지 두께, 빨간색)
  - H (전체 높이, 초록색)

### 4. H-Pile 생성
- I자 형태 12점 폴리라인
- 중심점 기준 좌우 대칭
- 웹과 플랜지 두께 정확히 반영

### 5. 토류판 생성
- 두 H-Pile 사이 배치
- 두께: 70mm
- 길이: C.T.C - 50mm (양쪽 25mm 여유)
- ANSI36 패턴 해치 (축척 30)

## 🐛 알려진 이슈 및 해결 방법

### 문제 1: 파싱 실패
**증상**: `[DEBUG] 파싱 결과: (298 1 14)` - 일부 값만 파싱됨

**원인**: UTF-8 문자 `×`의 바이트 수 계산 오류

**해결**: 문자 단위 순회 방식으로 숫자만 추출하도록 개선 완료

### 문제 2: 레이어 생성 실패
**증상**: "도면층 이름이 유효하지 않음"

**원인**: 레이어명에 `/` 또는 `×` 문자 포함

**해결**: `_H-Pile_298x201x9-14` 형식으로 변경 (`x`와 `-` 사용)

### 문제 3: BHATCH 명령 오류
**증상**: "거리를 요구함"

**원인**: `-BHATCH` 명령 순서 및 매개변수 오류

**해결**: 
```lisp
(command "._-BHATCH"
  "_P" "ANSI36"   ; 패턴
  "30"            ; 축척 (숫자)
  "0"             ; 각도
  "_SEL" pline-ent ""
  ""
)
```

### 문제 4: H-Pile 좌표 계산 오류
**증상**: 모든 점이 동일한 위치

**원인**: `(setq half-tf tf)` - half-tf가 tf 전체 값

**해결**: `(setq half-tf (/ tf 2.0))` - 정확히 반으로 나눔

## 📝 코드 구조

### 전역 변수
```lisp
*tsp-hpile-spec*     ; H-Pile 규격
*tsp-hpile-custom*   ; User-defined H-Pile 값
*tsp-wale-spec*      ; 띠장 규격
*tsp-wale-custom*    ; User-defined 띠장 값
*tsp-ctc*            ; C.T.C 값
*tsp-debug*          ; 디버그 모드 (디버그 파일에서)
*tsp-debug-log*      ; 디버그 로그 (디버그 파일에서)
```

### 주요 함수

| 함수명 | 역할 |
|--------|------|
| `create-tsp-dcl` | DCL 파일 동적 생성 |
| `create-layer-if-not-exists` | 레이어 생성 |
| `is-numeric` | 숫자 검증 |
| `parse-h-spec` | H형강 규격 파싱 |
| `create-wale-offsets` | 띠장 옵셋 생성 |
| `hpile-dialog-callback` | H-Pile 설정 Dialog |
| `main-dialog-callback` | 메인 Dialog |
| `C:TSP` | 메인 명령 함수 |
| `get-longest-segment` | 가장 긴 세그먼트 탐색 |
| `create-hpile-set-on-boundary` | 경계선에 H-Pile 세트 배치 |
| `create-hpile-section` | H-Pile I자 단면 생성 |
| `create-timber-panel` | 토류판 생성 |
| `create-hpile-set` | H-Pile 세트 생성 (2개 + 토류판) |

### 디버그 함수 (TSP-debug.lsp에서)

| 함수명 | 역할 |
|--------|------|
| `debug-log` | 타임스탬프와 함께 로그 기록 |
| `debug-clear` | 로그 초기화 |
| `debug-show-log` | 전체 로그 출력 |

## 🔧 개발 환경

- **언어**: AutoLISP
- **타겟**: AutoCAD 2020 이상
- **인코딩**: UTF-8
- **파일 경로**: `/home/user/webapp/`

## 📌 참고 사항

1. **경계선 선택**: LWPOLYLINE 또는 LINE만 지원
2. **C.T.C 단위**: 미터(m)로 입력, 내부적으로 mm로 변환
3. **좌표계**: AutoCAD WCS(World Coordinate System) 기준
4. **H-Pile 방향**: 현재 수평 방향으로 고정 (향후 회전 기능 추가 예정)
5. **경계선 옵셋**: 항상 내부 방향으로 생성

## 🎓 학습 포인트

### UTF-8 문자 처리
- `×` (곱하기 기호)는 3바이트 UTF-8 문자
- `substr`로 정확한 위치 계산 어려움
- 해결: 문자 단위 ASCII 검사 방식

### AutoCAD 명령 순서
- `-BHATCH`: 비대화형 해치
- 매개변수 순서 중요: 패턴 → 축척 → 각도 → 선택 → 완료

### 레이어명 규칙
- 특수문자 제한: `/`, `\`, `<`, `>`, `:`, `"`, `|`, `?`, `*` 불가
- 공백 허용되나 권장하지 않음
- 숫자와 알파벳, 하이픈(`-`), 언더스코어(`_`) 권장

### 좌표 계산
- 중심 기준 대칭 도형: `half-value` 사용
- 회전 변환: 수직 벡터 = (-dy, dx)
- 정규화: `dx / length`, `dy / length`

## 📞 문의 및 지원

문제가 발생하거나 개선 사항이 있으시면:

1. **디버그 로그 확인**: `(debug-show-log)` 실행
2. **에러 메시지 복사**: AutoCAD 명령줄 메시지 전체
3. **작업 환경 정보**: AutoCAD 버전, 경계선 형태, 선택한 규격

---

**마지막 업데이트**: 2026-01-14  
**커밋**: 4355370  
**개발자**: TSP Development Team
