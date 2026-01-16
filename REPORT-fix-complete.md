# TSP H-Pile 배치 문제 해결 완료 보고서

## 📋 작업 요약

**날짜**: 2026-01-16  
**버전**: TSP 1.0.0  
**커밋**: 4237d58, 1280e6b  
**상태**: ✅ 완료 및 원격 저장소 푸시 완료

## 🎯 해결된 핵심 문제

### 1. ❌ 문제: H-Pile이 모두 옆으로 누워있음 (90도 회전)
- **증상**: ㄷ자 경계선에서 직선 구간 H-Pile 18개가 모두 회전 90°로 배치
- **위치**: 레이어 '_측면말뚝'에 잘못 배치됨
- **영향**: 의도한 '레이어 0'이 아닌 측면 배치로 인식

### 2. ❌ 문제: ㄷ자 경계선에서 외부 방향 클릭 요구
- **증상**: 폐합된 다각형임에도 열린선으로 오인
- **원인**: DXF 70 비트만으로 판별하여 기하학적 폐합 미인식

### 3. ❌ 문제: DCL에서 × 기호가 x로 표시됨
- **증상**: "H 298x201x9/14" (ASCII x)
- **영향**: 규격 표시의 가독성 저하

## ✅ 해결 방안

### 해결 1: H-Pile 회전 로직 수정

#### 근본 원인 분석
```
H-Pile 블록 구조:
- 웹(Web): Y축 방향 (수직, 높이 h)
- 플랜지(Flange): X축 방향 (수평, 폭 b)
- 기준점: 하단 플랜지 중심 (0, -h/2)

잘못된 로직:
  hpile-rotation = outward-normal
  → 웹이 경계선과 수직 방향으로 회전
  → H-Pile이 누움 ❌

올바른 로직:
  hpile-rotation = seg-angle
  → 웹이 경계선과 평행 방향으로 회전
  → H-Pile이 서있음 ✅
```

#### 코드 변경 (TSP-debug.lsp 라인 2206-2213)
```lisp
;; 이전 (잘못됨):
(setq outward-normal (+ seg-angle (* boundary-orient (/ pi 2.0))))
(setq hpile-rotation outward-normal)  ; ❌

;; 수정 (올바름):
(setq outward-normal (+ seg-angle (* boundary-orient (/ pi 2.0))))
(setq hpile-rotation seg-angle)  ; ✅
```

#### 검증 시나리오
| 경계 방향 | seg-angle | outward-normal | hpile-rotation | 결과 |
|-----------|-----------|----------------|----------------|------|
| 수평 (→) | 0° | 90° (↑) | 0° | 웹이 수직 유지 ✅ |
| 수직 (↑) | 90° | 180° (←) | 90° | 웹이 수평 (경계와 평행) ✅ |
| 대각선 | 45° | 135° | 45° | 웹이 45° 회전 ✅ |

### 해결 2: 폐합선 판별 로직 개선

#### 문제 상황
- ㄷ자/ㄴ자 경계를 PLINE Close 없이 그린 경우
- 첫/마지막 점이 일치하지만 DXF 70 비트는 0

#### 개선 방법 (TSP-debug.lsp 라인 154-180)
```lisp
(defun is-closed-polyline (ent / ent-data closed-flag vertices first-pt last-pt)
  ;; 방법 1: DXF 코드 70 bit 0 확인
  (setq closed-flag (cdr (assoc 70 ent-data)))
  (if (and closed-flag (= 1 (logand 1 closed-flag)))
    T  ; DXF 플래그로 폐합 확인
    ;; 방법 2: 첫/마지막 꼭지점 비교
    (progn
      ;; 꼭지점 추출
      (setq vertices '())
      (foreach item ent-data
        (if (= (car item) 10)
          (setq vertices (append vertices (list (cdr item))))
        )
      )
      ;; 0.1mm 허용 오차로 일치 여부 확인
      (if (>= (length vertices) 2)
        (equal (car vertices) (last vertices) 0.1)
        nil
      )
    )
  )
)
```

#### 효과
- ✅ DXF 플래그 우선 확인 (빠름)
- ✅ 기하학적 폐합도 자동 인식 (정확함)
- ✅ 사용자 클릭 불필요 (UX 개선)

### 해결 3: DCL × 기호 복원

#### 변경 위치
- 라인 729: 드롭다운 리스트 초기화
- 라인 734: 띠장 규격 리스트

#### 변경 내용
```lisp
;; 이전: "H 298x201x9/14" (ASCII x)
;; 수정: "H 298×201×9/14" (유니코드 ×)
```

## 📦 추가 작업

### 1. 분석 문서 작성
- **파일**: `ANALYSIS-hpile-rotation-fix.md`
- **내용**: 문제 분석, 원인, 해결 방안, 검증 시나리오

### 2. 테스트 파일 생성
- **파일**: `test-boundary-detection.lsp`
- **기능**: 
  - 명령어: `TEST-BOUNDARY`
  - DXF 70 비트 확인
  - 폐합선/열린선 판별
  - 꼭지점 추출 및 방향 판단

## 🔍 테스트 가이드

### AutoCAD에서 테스트 방법

#### 1단계: 파일 로드
```
Command: (load "TSP-debug.lsp")
Command: (load "test-boundary-detection.lsp")
```

#### 2단계: 경계선 테스트
```
Command: TEST-BOUNDARY
경계선 폴리라인 선택: [ㄷ자 폴리라인 선택]

예상 출력:
=== 경계선 분석 ===
엔티티 타입: LWPOLYLINE
DXF 70 코드 값: 0 (또는 1)
Bit 0 (폐합): 1 (폐합) 또는 0 (열림) → 첫/마지막 점 일치로 판별
결과: 폐합 다각형 (Closed) ✅
꼭지점 개수: 6
다각형 방향: CCW (반시계) 또는 CW (시계)
```

#### 3단계: TSP 실행
```
Command: TSP
[경계선 선택] → ㄷ자 폴리라인 선택
[H-Pile 규격 선택] → H 298×201×9/14
[C.T.C 입력] → 2
[확인]

예상 결과:
- 사용자 클릭 없이 자동으로 외부 방향 계산 ✅
- H-Pile이 서있는 자세로 배치 ✅
- 레이어 '_측면말뚝'에 올바른 회전으로 배치 ✅
```

## 📊 커밋 내역

### 커밋 1: 4237d58
```
fix: 직선 구간 H-Pile 회전 수정 및 × 복원

핵심 변경사항:
1. H-Pile 회전 로직 수정
   - hpile-rotation = seg-angle (경계 평행)
2. DCL × 기호 복원
3. 테스트 파일 추가
4. 삽입점 계산 유지

기대 효과:
- 직선 구간 H-Pile 정상 배치
- ㄷ자/ㄴ자 경계 올바른 회전
- DCL 규격 표시 개선
```

### 커밋 2: 1280e6b
```
fix: 폐합선 판별 로직 개선

해결:
1. is-closed-polyline 함수 개선
   - DXF 70 bit 0 확인
   - 첫/마지막 꼭지점 비교
   - 0.1mm 허용 오차

기대 효과:
- ㄷ자/ㄴ자 경계 자동 인식
- 사용자 클릭 불필요
- UX 개선
```

## 🚀 배포 상태

✅ **원격 저장소 푸시 완료**
- Repository: https://github.com/931004khg-dot/CIP-SCW-.git
- Branch: main
- Commits: 06b5810 → 1280e6b

## ⏭️ 다음 단계

### 필수 테스트 (사용자 확인 필요)
1. ✅ 직선 구간 H-Pile 회전 (완료 예상)
2. ✅ ㄷ자 경계 자동 인식 (완료 예상)
3. ⏳ ㄴ자 경계 테스트
4. ⏳ 정육각형 경계 테스트
5. ⏳ 직사각형 회귀 테스트 (기존 기능 유지 확인)

### 향후 개선 사항
1. 모서리 H-Pile 배치 로직 검증
2. 복합 다각형 (5+ 꼭지점) 테스트
3. 자기 교차 감지 및 대응 로직 강화
4. 옵셋 방향 수학적 확정 (trial-and-error 제거)

## 📝 참고 파일

- **TSP-debug.lsp**: 메인 코드 (라인 154-180, 2206-2217)
- **test-boundary-detection.lsp**: 테스트 유틸리티
- **ANALYSIS-hpile-rotation-fix.md**: 상세 분석 문서
- **이 문서**: 작업 완료 보고서

## ✅ 체크리스트

- [x] H-Pile 회전 로직 수정
- [x] DCL × 기호 복원
- [x] 폐합선 판별 로직 개선
- [x] 테스트 파일 생성
- [x] 분석 문서 작성
- [x] 커밋 및 푸시 완료
- [ ] AutoCAD 실제 테스트 (사용자)
- [ ] 직사각형 회귀 테스트 (사용자)
- [ ] 복합 다각형 테스트 (사용자)

## 🙏 사용자 확인 요청

**AutoCAD에서 다음 경계선으로 테스트 부탁드립니다:**

1. **ㄷ자 경계**: 폐합선 자동 인식 및 H-Pile 올바른 회전 확인
2. **ㄴ자 경계**: 폐합선 자동 인식 및 방향 일관성 확인
3. **직사각형**: 기존 기능 정상 동작 확인 (회귀 테스트)
4. **정육각형**: CW/CCW 상관없이 외부 배치 확인

**테스트 후 피드백 부탁드립니다:**
- H-Pile 회전이 올바른가? (서있는 자세)
- 외부 방향이 올바른가? (경계 바깥쪽)
- 하부 플랜지가 경계선에 닿아있는가?
- 사용자 클릭 없이 자동으로 진행되는가?

---

**작성자**: Claude Code (AI Assistant)  
**날짜**: 2026-01-16  
**버전**: 1.0.0
