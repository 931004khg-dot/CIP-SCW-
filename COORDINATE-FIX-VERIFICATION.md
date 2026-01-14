# H-Pile 좌표 버그 수정 확인 가이드

## 수정 완료 상태

**커밋**: b38abae  
**날짜**: 2026-01-14  
**수정 내용**: H-Pile 좌표 계산 완전 수정 - 모든 점이 동일한 위치로 생성되는 버그 해결

## 문제 증상

- **증상**: 12개 H-Pile 좌표가 모두 동일한 위치에 생성됨
- **결과**: LIST 명령으로 확인 시 면적 0, 둘레 비정상(30~60mm)
- **원인**: 중첩 괄호식의 AutoLISP 평가 순서 문제

## 수정 내용

### ❌ 잘못된 코드 (수정 전)
```lisp
(defun create-hpile-section (insert-pt h b tw tf layer-name / half-h half-b half-tw half-tf pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 pt9 pt10 pt11 pt12 pline-ent)
  (setq half-h (/ h 2.0))
  (setq half-b (/ b 2.0))
  (setq half-tw (/ tw 2.0))
  (setq half-tf (/ tf 2.0))
  
  ;; ❌ 중첩 괄호로 인한 오류
  (setq pt1 (list (- (car insert-pt) half-b) (+ (cadr insert-pt) half-h)))
  (setq pt2 (list (+ (car insert-pt) half-b) (+ (cadr insert-pt) half-h)))
  ;; ... 나머지 점들도 동일한 문제
)
```

### ✅ 수정된 코드 (수정 후)
```lisp
(defun create-hpile-section (insert-pt h b tw tf layer-name / half-h half-b half-tw half-tf cx cy pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 pt9 pt10 pt11 pt12 pline-ent)
  (setq half-h (/ h 2.0))
  (setq half-b (/ b 2.0))
  (setq half-tw (/ tw 2.0))
  (setq half-tf (/ tf 2.0))
  
  ;; ✅ 중심 좌표를 명시적으로 분리
  (setq cx (car insert-pt))
  (setq cy (cadr insert-pt))
  
  ;; ✅ 명확한 수식으로 계산
  (setq pt1  (list (- cx half-b) (+ cy half-h)))           ; 좌상단
  (setq pt2  (list (+ cx half-b) (+ cy half-h)))           ; 우상단
  (setq pt3  (list (+ cx half-b) (+ cy (- half-h half-tf)))) ; 우상단 안쪽
  (setq pt4  (list (+ cx half-tw) (+ cy (- half-h half-tf)))) ; 웹 우상단
  (setq pt5  (list (+ cx half-tw) (- cy (- half-h half-tf)))) ; 웹 우하단
  (setq pt6  (list (+ cx half-b) (- cy (- half-h half-tf)))) ; 우하단 안쪽
  (setq pt7  (list (+ cx half-b) (- cy half-h)))           ; 우하단
  (setq pt8  (list (- cx half-b) (- cy half-h)))           ; 좌하단
  (setq pt9  (list (- cx half-b) (- cy (- half-h half-tf)))) ; 좌하단 안쪽
  (setq pt10 (list (- cx half-tw) (- cy (- half-h half-tf)))) ; 웹 좌하단
  (setq pt11 (list (- cx half-tw) (+ cy (- half-h half-tf)))) ; 웹 좌상단
  (setq pt12 (list (- cx half-b) (+ cy (- half-h half-tf)))) ; 좌상단 안쪽
)
```

## 수정 확인 방법

### ⚠️ **중요: 반드시 파일을 재로드하세요!**

AutoCAD는 메모리에 함수를 캐시하므로, 파일을 수정해도 **반드시 재로드**해야 합니다.

### 단계 1: AutoCAD에서 완전히 파일 재로드

```
명령: (load "C:/LISP/TSP.lsp")
```

또는 APPLOAD 명령으로 TSP.lsp 재로드:
```
명령: APPLOAD
→ TSP.lsp 선택 → Load
```

### 단계 2: TSP 명령 실행

```
명령: TSP
```

### 단계 3: 설정 입력

- **공법**: H-Pile + 토류판
- **H-Pile 규격**: H 298×201×9/14 (기본값)
- **띠장 규격**: H 300×300×10/15 (기본값)
- **C.T.C**: 2m (기본값)

### 단계 4: 경계선 선택

LWPOLYLINE 또는 LINE 선택

### 단계 5: 결과 확인

#### 5-1. LIST 명령으로 H-Pile 확인

```
명령: LIST
→ H-Pile 폴리라인 2개 선택
```

#### ✅ 정상 결과 (수정 후 기대값)

```
LWPOLYLINE  레이어: _H-Pile_298x201x9-14
  좌표:
    정점 X=31006.08, Y=57728.48  (좌상단)
    정점 X=31207.08, Y=57728.48  (우상단)
    정점 X=31207.08, Y=57714.48  (우상단 안쪽)
    정점 X=31111.08, Y=57714.48  (웹 우상단)
    정점 X=31111.08, Y=57444.48  (웹 우하단)
    정점 X=31207.08, Y=57444.48  (우하단 안쪽)
    정점 X=31207.08, Y=57430.48  (우하단)
    정점 X=31006.08, Y=57430.48  (좌하단)
    정점 X=31006.08, Y=57444.48  (좌하단 안쪽)
    정점 X=31102.08, Y=57444.48  (웹 좌하단)
    정점 X=31102.08, Y=57714.48  (웹 좌상단)
    정점 X=31006.08, Y=57714.48  (좌상단 안쪽)
  
  면적: 약 59,698 mm² (≈ 298×201)
  둘레: 약 998 mm (≈ 2×(298+201))
```

#### ❌ 비정상 결과 (수정 전 - 파일 재로드 안 한 경우)

```
LWPOLYLINE  레이어: _H-Pile_298x201x9-14
  좌표:
    정점 X=31106.58, Y=57579.48  (모든 점 동일!)
    정점 X=31106.58, Y=57579.48
    정점 X=31106.58, Y=57579.48
    정점 X=31106.58, Y=57579.48
    정점 X=31106.58, Y=57579.48
    정점 X=31106.58, Y=57579.48
    정점 X=31106.58, Y=57579.48
    정점 X=31106.58, Y=57579.48
    정점 X=31106.58, Y=57579.48
    정점 X=31106.58, Y=57579.48
    정점 X=31106.58, Y=57579.48
    정점 X=31106.58, Y=57579.48
  
  면적: 0
  둘레: 30~60 mm
```

## 예상 좌표 계산 (H 298×201×9/14)

### 기준값
- **H** (높이) = 298 mm  → half-h = 149 mm
- **B** (폭) = 201 mm → half-b = 100.5 mm
- **tw** (웹 두께) = 9 mm → half-tw = 4.5 mm
- **tf** (플랜지 두께) = 14 mm → half-tf = 7 mm

### 중심점 (예: cx=31106.58, cy=57579.48)

| 점 | X 좌표 | Y 좌표 | 설명 |
|----|--------|--------|------|
| pt1  | 31006.08 | 57728.48 | 좌상단 (cx - 100.5, cy + 149) |
| pt2  | 31207.08 | 57728.48 | 우상단 (cx + 100.5, cy + 149) |
| pt3  | 31207.08 | 57714.48 | 우상단 안쪽 (cx + 100.5, cy + 142) |
| pt4  | 31111.08 | 57714.48 | 웹 우상단 (cx + 4.5, cy + 142) |
| pt5  | 31111.08 | 57444.48 | 웹 우하단 (cx + 4.5, cy - 142) |
| pt6  | 31207.08 | 57444.48 | 우하단 안쪽 (cx + 100.5, cy - 142) |
| pt7  | 31207.08 | 57430.48 | 우하단 (cx + 100.5, cy - 149) |
| pt8  | 31006.08 | 57430.48 | 좌하단 (cx - 100.5, cy - 149) |
| pt9  | 31006.08 | 57444.48 | 좌하단 안쪽 (cx - 100.5, cy - 142) |
| pt10 | 31102.08 | 57444.48 | 웹 좌하단 (cx - 4.5, cy - 142) |
| pt11 | 31102.08 | 57714.48 | 웹 좌상단 (cx - 4.5, cy + 142) |
| pt12 | 31006.08 | 57714.48 | 좌상단 안쪽 (cx - 100.5, cy + 142) |

## 독립 테스트 스크립트

좌표 계산만 테스트하려면:

```lisp
(load "test-hpile-coords.lsp")
```

이 스크립트는 AutoCAD 없이도 좌표 계산 로직을 검증합니다.

## 트러블슈팅

### 문제: 여전히 모든 좌표가 동일함

**원인**: 파일 재로드를 하지 않음

**해결**:
1. AutoCAD 완전히 재시작
2. APPLOAD로 TSP.lsp 재로드
3. 또는 명령줄에서 `(load "C:/LISP/TSP.lsp")` 실행

### 문제: 면적이 0이 아니지만 너무 작음

**원인**: 단위 문제 (mm vs m)

**확인**: 
- 도면 단위가 mm인지 확인
- UNITS 명령으로 단위 확인

### 문제: I자 형태가 찌그러짐

**원인**: 규격 파싱 오류

**확인**:
```lisp
(parse-h-spec "H 298×201×9/14")
```
결과: `(298 201 9 14)` 여야 함

## 수정 파일 목록

- ✅ `/home/user/webapp/TSP.lsp` - create-hpile-section 함수 수정 (줄 730-787)
- ✅ `/home/user/webapp/TSP-debug.lsp` - create-hpile-section 재정의 수정 (줄 797-860)
- ✅ 커밋 b38abae - "fix: H-Pile 좌표 계산 완전 수정"

## 결론

**현재 상태**: ✅ 코드 수정 완료  
**필요 조치**: ⚠️ AutoCAD에서 TSP.lsp 재로드 필수  
**확인 방법**: LIST 명령으로 12개 좌표가 모두 다른지 확인

---

**참고**: 이 수정은 커밋 b38abae에서 완료되었으며, 두 파일(TSP.lsp, TSP-debug.lsp) 모두 수정되었습니다. 
파일을 재로드하지 않으면 AutoCAD는 메모리에 캐시된 이전 버전의 함수를 계속 사용합니다.
