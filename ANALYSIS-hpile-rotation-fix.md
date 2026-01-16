# H-Pile 회전 문제 분석 및 해결 방안

## 문제 요약
ㄷ자 경계선에서 직선 구간 H-Pile이 모두 90도 회전되어 옆으로 누워있는 문제 발생

## 근본 원인 분석

### 1. H-Pile 블록 구조 이해
```lisp
;; create-hpile-section-block 함수 (TSP-debug.lsp 라인 1566)
;; H-Pile 블록 기본 방향:
;; - 웹(Web): Y축 방향 (수직, 높이 h)
;; - 플랜지(Flange): X축 방향 (수평, 폭 b)
;; - 기준점(Base Point): 하단 플랜지 중심 (0, -h/2)
;;
;; 회전 0도일 때:
;;   ^  Y축
;;   |
;;   |  ┌─┐  ← 상단 플랜지
;;   |  │웹│
;;   |  │ │
;;   |  └─┘  ← 하단 플랜지 (기준점)
;;   +-------> X축
```

### 2. 잘못된 회전 로직 (수정 전)
```lisp
;; 라인 2208-2211 (잘못된 버전)
(setq outward-normal (+ seg-angle (* boundary-orient (/ pi 2.0))))
(setq hpile-rotation outward-normal)  ; ❌ 잘못됨!

;; 예시: 수평 경계선 (seg-angle = 0°, boundary-orient = 1 for CCW)
;; outward-normal = 0 + 1×90° = 90°
;; hpile-rotation = 90°
;; 결과: H-Pile이 90도 회전 → 웹이 수평으로 누움 ❌
```

**문제점:**
- `outward-normal`은 경계선의 **외부 방향**(웹이 향할 방향)
- `hpile-rotation`은 H-Pile 블록의 **회전 각도**
- 이 둘을 같게 설정하면 웹이 경계선과 수직이 아닌 경계선과 평행하게 됨

### 3. 올바른 회전 로직 (수정 후)
```lisp
;; 라인 2206-2213 (올바른 버전)
;; 외부 법선 방향 계산 (웹이 향할 방향)
(setq outward-normal (+ seg-angle (* boundary-orient (/ pi 2.0))))

;; H-Pile 회전 = 경계선 방향 (웹이 수직을 유지하도록)
(setq hpile-rotation seg-angle)  ; ✅ 올바름!

;; 예시: 수평 경계선 (seg-angle = 0°)
;; outward-normal = 90° (위쪽)
;; hpile-rotation = 0° (회전 없음)
;; 결과: H-Pile이 서있는 상태 유지 ✅
```

**해결 원리:**
- H-Pile 블록은 기본적으로 웹이 **수직** (Y축 방향)
- 경계선 방향으로 회전시키면 웹도 **경계선과 평행**하게 회전
- 그런 다음 `outward-normal` 방향으로 `tf`만큼 오프셋하여 하단 플랜지가 경계선에 닿게 함

## 수정 사항

### 변경 1: H-Pile 회전 로직
```lisp
;; TSP-debug.lsp 라인 2206-2213
;; 이전:
;; (setq hpile-rotation outward-normal)

;; 수정:
(setq hpile-rotation seg-angle)
```

### 변경 2: 삽입점 계산 (변경 없음, 올바름)
```lisp
;; 라인 2215-2217
(foreach hpile-pt hpile-positions
  ;; 하부 플랜지가 경계선에 닿도록 tf만큼 바깥으로 오프셋
  (setq insert-pt (polar hpile-pt outward-normal tf))
  ...
)
```

### 변경 3: DCL × 기호 복원
```lisp
;; 라인 729, 734
;; 이전: "H 298x201x9/14" (ASCII x)
;; 수정: "H 298×201×9/14" (유니코드 ×)
```

## 검증 시나리오

### 시나리오 1: 수평 경계선 (seg-angle = 0°)
| 변수 | 값 | 설명 |
|------|-------|------|
| seg-angle | 0° | 경계선 방향 (동쪽) |
| boundary-orient | 1 (CCW) | 반시계 방향 |
| outward-normal | 90° | 위쪽 (북쪽) |
| hpile-rotation | 0° | 회전 없음 → 웹이 수직 유지 ✅ |
| insert-pt | hpile-pt + (0, tf) | tf만큼 위로 오프셋 |

### 시나리오 2: 수직 경계선 (seg-angle = 90°)
| 변수 | 값 | 설명 |
|------|-------|------|
| seg-angle | 90° | 경계선 방향 (북쪽) |
| boundary-orient | 1 (CCW) | 반시계 방향 |
| outward-normal | 180° | 왼쪽 (서쪽) |
| hpile-rotation | 90° | 90도 회전 → 웹이 수평 → 경계선과 평행 ✅ |
| insert-pt | hpile-pt + (-tf, 0) | tf만큼 왼쪽으로 오프셋 |

### 시나리오 3: ㄷ자 경계선 (복합)
```
      ②──────③
      │      │
      │      │
      ①──────④

세그먼트 분석:
- ①→②: seg-angle = 90°, hpile-rotation = 90°
- ②→③: seg-angle = 0°, hpile-rotation = 0°
- ③→④: seg-angle = 270°, hpile-rotation = 270° (= -90°)
- ④→①: (바닥, H-Pile 없음)

모든 경우에 웹이 경계선과 평행하게 배치 ✅
```

## 테스트 파일

### test-boundary-detection.lsp
```lisp
;; 명령어: TEST-BOUNDARY
;; 기능:
;; 1. DXF 코드 70 비트 확인 (폐합 플래그)
;; 2. 폐합선/열린선 판별
;; 3. 꼭지점 추출 및 방향 판단 (CCW/CW)
```

## 기대 효과

1. **직선 구간 H-Pile 정상 배치**: 웹이 수직으로 서있는 올바른 자세
2. **ㄷ자/ㄴ자 경계 대응**: 복잡한 다각형에서도 일관된 회전 적용
3. **DCL 표시 개선**: 규격 문자열에 올바른 × 기호 사용

## 다음 단계

1. ✅ 직선 구간 H-Pile 회전 수정 (완료)
2. ✅ DCL × 기호 복원 (완료)
3. ⏳ 폐합선 판별 검증 (ㄷ자 경계가 열린선으로 인식되는 문제)
4. ⏳ 모서리 H-Pile 배치 로직 검증
5. ⏳ 직사각형 케이스 회귀 테스트

## 참고

- 커밋: 4237d58
- 파일: TSP-debug.lsp
- 라인: 2206-2217 (직선 구간 배치)
- 관련 함수:
  - `place-hpile-timber-along-boundary` (라인 1851)
  - `create-hpile-section-block` (라인 1566)
  - `is-closed-polyline` (라인 154)
  - `get-polygon-orientation` (라인 116)
