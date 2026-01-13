# 가시설 흙막이 벽체 자동 작도 - 기술 사양서

## 문서 정보

- **버전**: 1.0.0
- **작성일**: 2026-01-13
- **작성자**: 지반사업부 설계팀
- **언어**: AutoLISP
- **대상 플랫폼**: AutoCAD 2010 이상

---

## 1. 시스템 개요

### 1.1 목적

건축물 지하벽체 경계선을 기준으로 가시설 흙막이 벽체 구조물을 자동으로 작도하여 설계 업무 효율성을 향상시킨다.

### 1.2 범위

- 3가지 흙막이 공법 지원 (H-Pile+토류판, CIP, S.C.W)
- 엄지말뚝/벽체, 띠장, 버팀보 자동 배치
- 레이어 자동 관리 및 문자 표기

### 1.3 제약사항

- AutoCAD 2D 환경 기준
- WCS (World Coordinate System) 사용
- Metric 단위 (mm) 기준
- Polyline 또는 Line 객체만 경계선으로 인식

---

## 2. 시스템 아키텍처

### 2.1 파일 구조

```
retaining-wall-system/
│
├── lisp/
│   ├── retaining-utils.lsp      (공통 유틸리티 모듈)
│   ├── hpile-retaining.lsp      (H-Pile+토류판 공법 모듈)
│   ├── cip-retaining.lsp        (CIP 공법 모듈)
│   ├── scw-retaining.lsp        (S.C.W 공법 모듈)
│   └── retaining-main.lsp       (메인 진입점 및 메뉴)
│
├── docs/
│   ├── user-guide.md            (사용자 가이드)
│   └── technical-spec.md        (기술 사양서)
│
├── examples/
│   └── sample-drawings.dwg      (예제 도면)
│
└── README.md                     (프로젝트 개요)
```

### 2.2 모듈 의존성

```
retaining-main.lsp
    │
    ├─→ retaining-utils.lsp (공통 함수)
    │
    ├─→ hpile-retaining.lsp
    │       └─→ retaining-utils.lsp
    │
    ├─→ cip-retaining.lsp
    │       └─→ retaining-utils.lsp
    │
    └─→ scw-retaining.lsp
            └─→ retaining-utils.lsp
```

---

## 3. 모듈별 상세 사양

### 3.1 retaining-utils.lsp (공통 유틸리티)

#### 3.1.1 레이어 관리 함수

##### `create-layer`
```lisp
(defun create-layer (layer-name color linetype))
```
- **기능**: 레이어 생성 및 설정
- **입력**: 레이어명, 색상 코드, 선종류
- **출력**: 없음 (레이어 생성 및 활성화)

##### `init-retaining-layers`
```lisp
(defun init-retaining-layers ())
```
- **기능**: 가시설 전용 레이어 일괄 생성
- **생성 레이어**:
  - PILE (Red, 1)
  - WALE (Blue, 5)
  - STRUT (Green, 3)
  - RETAINING-DIM (Yellow, 2)
  - RETAINING-TEXT (White, 7)
  - EARTH-ANCHOR (Magenta, 6)

#### 3.1.2 기하학 계산 함수

##### `dist-2p`
```lisp
(defun dist-2p (p1 p2))
```
- **기능**: 두 점 사이 거리 계산
- **입력**: 점1 (x y z), 점2 (x y z)
- **출력**: 거리 (실수)

##### `angle-2p`
```lisp
(defun angle-2p (p1 p2))
```
- **기능**: 두 점을 잇는 선의 각도 계산
- **입력**: 점1, 점2
- **출력**: 각도 (라디안)

##### `mid-point`
```lisp
(defun mid-point (p1 p2))
```
- **기능**: 두 점의 중점 계산
- **입력**: 점1, 점2
- **출력**: 중점 좌표

##### `perpendicular-angle`
```lisp
(defun perpendicular-angle (ang))
```
- **기능**: 수직 방향 각도 계산
- **입력**: 각도 (라디안)
- **출력**: 수직 각도 (원래 각도 + π/2)

#### 3.1.3 선/폴리라인 처리 함수

##### `get-line-points`
```lisp
(defun get-line-points (ent))
```
- **기능**: 선형 객체의 좌표 리스트 추출
- **지원 객체**: LINE, LWPOLYLINE, POLYLINE
- **입력**: 엔티티 이름
- **출력**: 좌표 리스트 ((x1 y1 z1) (x2 y2 z2) ...)

##### `divide-segments`
```lisp
(defun divide-segments (pt-list))
```
- **기능**: 좌표 리스트를 세그먼트로 분할
- **입력**: 좌표 리스트
- **출력**: 세그먼트 리스트 (((p1) (p2)) ((p2) (p3)) ...)

##### `divide-segment`
```lisp
(defun divide-segment (p1 p2 spacing))
```
- **기능**: 선분을 등간격으로 분할
- **입력**: 시작점, 끝점, 간격
- **출력**: 분할점 리스트

#### 3.1.4 도형 그리기 함수

##### `draw-hpile`
```lisp
(defun draw-hpile (center-pt ang size))
```
- **기능**: H-Pile 단면 그리기
- **입력**: 중심점, 각도, 크기
- **도면 요소**: Line, Circle

##### `draw-cip-circle`
```lisp
(defun draw-cip-circle (center-pt diameter))
```
- **기능**: CIP 원형 말뚝 그리기
- **입력**: 중심점, 직경
- **도면 요소**: Circle

##### `draw-scw-wall`
```lisp
(defun draw-scw-wall (p1 p2 thickness))
```
- **기능**: SCW 벽체 그리기
- **입력**: 시작점, 끝점, 두께
- **도면 요소**: Polyline (사각형)

##### `draw-wale`
```lisp
(defun draw-wale (p1 p2 size))
```
- **기능**: 띠장 그리기
- **입력**: 시작점, 끝점, 크기
- **도면 요소**: Line, Circle (마커)

##### `draw-strut`
```lisp
(defun draw-strut (p1 p2 size))
```
- **기능**: 버팀보 그리기
- **입력**: 시작점, 끝점, 크기
- **도면 요소**: Line, Circle (마커)

#### 3.1.5 사용자 입력 함수

##### `get-real-input`
```lisp
(defun get-real-input (prompt default))
```
- **기능**: 실수형 입력 받기 (기본값 제공)
- **입력**: 프롬프트 문자열, 기본값
- **출력**: 사용자 입력 또는 기본값

##### `get-int-input`
```lisp
(defun get-int-input (prompt default))
```
- **기능**: 정수형 입력 받기 (기본값 제공)

---

### 3.2 hpile-retaining.lsp (H-Pile+토류판 공법)

#### 3.2.1 메인 함수

##### `C:HPILE-RT`
```lisp
(defun C:HPILE-RT ())
```

**작업 흐름:**

1. 레이어 초기화
2. 경계선 선택 및 좌표 추출
3. 설계 파라미터 입력
   - 엄지말뚝 간격
   - H-Pile 크기
   - 띠장 단수 및 크기
   - 버팀보 간격 및 크기
4. H-Pile 배치
   - 세그먼트별 등간격 분할
   - H-Pile 단면 및 마커 그리기
5. 토류판 표시
   - 이중선으로 표현
6. 띠장 배치
   - 레벨별 수평 배치
   - H-Beam 마커 표시
7. 버팀보 배치
   - 대각선 또는 교차 배치
8. 완료 보고

**입력 파라미터:**

| 파라미터 | 타입 | 기본값 | 단위 |
|---------|------|--------|------|
| pile-spacing | Real | 1.8 | m |
| pile-size | Real | 300.0 | mm |
| wale-levels | Integer | 3 | 단 |
| wale-size | Real | 400.0 | mm |
| strut-spacing | Real | 6.0 | m |
| strut-size | Real | 500.0 | mm |

---

### 3.3 cip-retaining.lsp (CIP 공법)

#### 3.3.1 메인 함수

##### `C:CIP-RT`
```lisp
(defun C:CIP-RT ())
```

**작업 흐름:**

1. 레이어 초기화
2. 경계선 선택
3. CIP 타입 선택
   - 1: 원형 CIP
   - 2: 사각형 CIP
   - 3: 연속벽체
4. 설계 파라미터 입력
5. CIP 말뚝 배치
   - 타입별 도형 생성
   - 철근 마커 배치
6. 띠장 배치
7. 버팀보 배치
8. 완료 보고

**입력 파라미터:**

| 파라미터 | 타입 | 기본값 | 단위 |
|---------|------|--------|------|
| pile-type | Integer | 1 | - |
| pile-spacing | Real | 1.5 | m |
| pile-diameter | Real | 800.0 | mm |
| rebar-dia | Real | 32.0 | mm |
| rebar-count | Integer | 12 | 개 |
| wale-levels | Integer | 3 | 단 |
| wale-size | Real | 400.0 | mm |
| strut-spacing | Real | 6.0 | m |
| strut-size | Real | 600.0 | mm |

**특수 기능:**

- **원형 CIP**: 철근을 원주 상에 균등 배치
- **사각형 CIP**: 철근을 4개 모서리에 배치
- **연속벽체**: 철근을 수평 방향으로 균등 배치

---

### 3.4 scw-retaining.lsp (S.C.W 공법)

#### 3.4.1 메인 함수

##### `C:SCW-RT`
```lisp
(defun C:SCW-RT ())
```

**작업 흐름:**

1. 레이어 초기화
2. 경계선 선택
3. 설계 파라미터 입력
   - 벽체 두께
   - 벽체 중첩 길이
   - 시멘트 설계강도
   - H-Beam 보강재 정보
4. 소일시멘트 연속벽체 그리기
   - 이중선 표시
   - 중첩부 해칭
5. H-Beam 보강재 배치
   - 십자 형태 마커
   - 규격 문자 표기
6. 띠장 배치
   - H-Beam에서 띠장으로 연결선
7. 버팀보 배치
8. 완료 보고

**입력 파라미터:**

| 파라미터 | 타입 | 기본값 | 단위 |
|---------|------|--------|------|
| wall-thickness | Real | 800.0 | mm |
| overlap-length | Real | 200.0 | mm |
| cement-strength | Real | 1.0 | MPa |
| hbeam-spacing | Real | 2.0 | m |
| hbeam-size | Real | 400.0 | mm |
| wale-levels | Integer | 3 | 단 |
| wale-size | Real | 500.0 | mm |
| strut-spacing | Real | 6.0 | m |
| strut-size | Real | 600.0 | mm |

**특수 기능:**

- **중첩부 표시**: HATCH 명령으로 ANSI31 패턴 적용
- **벽체 규격 표기**: 두께 및 강도 자동 문자 배치
- **H-Beam 연결**: 보강재에서 띠장으로 연결선 자동 생성

---

### 3.5 retaining-main.lsp (메인 프로그램)

#### 3.5.1 파일 로드 함수

##### `load-retaining-files`
```lisp
(defun load-retaining-files ())
```
- **기능**: 필수 LISP 파일 일괄 로드
- **로드 순서**:
  1. retaining-utils.lsp
  2. hpile-retaining.lsp
  3. cip-retaining.lsp
  4. scw-retaining.lsp

#### 3.5.2 메인 메뉴 함수

##### `C:RETAINING`
```lisp
(defun C:RETAINING ())
```
- **기능**: 공법 선택 메뉴 표시 및 실행
- **메뉴 옵션**:
  - 1: H-Pile + 토류판
  - 2: CIP
  - 3: S.C.W
  - 0: 종료

##### `C:RT`
```lisp
(defun C:RT ())
```
- **기능**: RETAINING 단축 명령어

#### 3.5.3 유틸리티 명령어

##### `C:RETAINING-INIT`
- **기능**: 레이어 초기화

##### `C:RETAINING-HELP`
- **기능**: 도움말 표시

##### `C:RETAINING-VERSION`
- **기능**: 버전 정보 표시

---

## 4. 데이터 구조

### 4.1 좌표 표현

```lisp
;; 2D 점
(x y 0.0)

;; 예시
(1000.0 2000.0 0.0)
```

### 4.2 세그먼트 리스트

```lisp
;; 세그먼트 = 두 점의 리스트
((p1) (p2))

;; 세그먼트 리스트
(
  ((p1) (p2))
  ((p2) (p3))
  ((p3) (p4))
)
```

### 4.3 엔티티 데이터

```lisp
;; LINE
(
  (-1 . <Entity name>)
  (0 . "LINE")
  (10 . (x1 y1 z1))  ; 시작점
  (11 . (x2 y2 z2))  ; 끝점
)

;; LWPOLYLINE
(
  (-1 . <Entity name>)
  (0 . "LWPOLYLINE")
  (10 . (x1 y1))  ; 버텍스 1
  (10 . (x2 y2))  ; 버텍스 2
  ...
)
```

---

## 5. 알고리즘

### 5.1 선분 등분할 알고리즘

```
입력: p1 (시작점), p2 (끝점), spacing (간격)
출력: 분할점 리스트

1. total_dist = distance(p1, p2)
2. num_divs = floor(total_dist / spacing)
3. result = [p1]
4. for i = 1 to num_divs:
     ratio = (i * spacing) / total_dist
     pt = p1 + ratio * (p2 - p1)
     result.append(pt)
5. return result
```

### 5.2 버팀보 자동 배치 알고리즘

```
입력: seg_list (세그먼트 리스트)
출력: 버팀보 좌표 쌍

전제: 세그먼트가 4개 이상 (사각형 경계)

1. seg1 = seg_list[0]
2. seg2 = seg_list[2]  (대향 세그먼트)
3. p1_mid = midpoint(seg1)
4. p2_mid = midpoint(seg2)
5. draw_strut(p1_mid, p2_mid)

6. if len(seg_list) >= 4:
     seg3 = seg_list[1]
     seg4 = seg_list[3]
     p3_mid = midpoint(seg3)
     p4_mid = midpoint(seg4)
     draw_strut(p3_mid, p4_mid)
```

---

## 6. 성능 고려사항

### 6.1 시간 복잡도

| 작업 | 복잡도 | 비고 |
|------|--------|------|
| 경계선 좌표 추출 | O(n) | n = 버텍스 개수 |
| 세그먼트 분할 | O(n) | n = 세그먼트 개수 |
| 엄지말뚝 배치 | O(n*m) | n = 세그먼트, m = 분할점 |
| 띠장/버팀보 배치 | O(n) | n = 세그먼트 |

### 6.2 최적화 기법

1. **좌표 리스트 캐싱**: 경계선 좌표를 한 번만 추출
2. **지역 변수 사용**: 글로벌 변수 최소화
3. **불필요한 연산 회피**: 조건문으로 선행 검사

### 6.3 메모리 사용

- 좌표 리스트: O(n) - n = 버텍스 개수
- 세그먼트 리스트: O(n) - n = 세그먼트 개수
- 말뚝 좌표 리스트: O(m) - m = 말뚝 개수

---

## 7. 오류 처리

### 7.1 입력 검증

```lisp
;; 경계선 선택 검증
(if (not boundary-ent)
  (progn
    (princ "\n경계선 선택 취소됨")
    (exit)
  )
)

;; 좌표 리스트 검증
(if (< (length pt-list) 2)
  (progn
    (princ "\n유효하지 않은 경계선")
    (exit)
  )
)
```

### 7.2 예외 상황

| 상황 | 처리 방법 |
|------|-----------|
| 경계선 미선택 | 함수 종료, 안내 메시지 |
| 유효하지 않은 객체 | 재선택 요청 |
| 좌표 추출 실패 | 오류 메시지, 함수 종료 |
| 입력값 범위 초과 | 경고 메시지 (현재 미구현) |

---

## 8. 테스트 계획

### 8.1 단위 테스트

#### 8.1.1 유틸리티 함수 테스트

```lisp
;; dist-2p 테스트
(setq p1 '(0 0 0))
(setq p2 '(3 4 0))
(princ (dist-2p p1 p2))  ; 예상: 5.0

;; mid-point 테스트
(princ (mid-point p1 p2))  ; 예상: (1.5 2.0 0.0)
```

#### 8.1.2 도형 그리기 테스트

각 공법별로 간단한 테스트 경계선 작성 후 실행

### 8.2 통합 테스트

#### 테스트 케이스

1. **직사각형 경계**
   - 4개 세그먼트
   - 폐합 Polyline
   - 모든 공법 적용

2. **L자 형태 경계**
   - 6개 세그먼트
   - 개방 Polyline
   - H-Pile, CIP 공법 적용

3. **복잡한 형태**
   - 8개 이상 세그먼트
   - 폐합 Polyline
   - 모든 공법 적용

---

## 9. 향후 개선 계획

### 9.1 기능 추가

- [ ] 3D 입체 모델 생성
- [ ] 굴착 단계별 단면도 자동 생성
- [ ] 물량 자동 산출 기능
- [ ] Excel 연동 물량표 출력
- [ ] 계측기 위치 자동 배치
- [ ] Earth Anchor 배치 기능
- [ ] 지층 단면도 통합

### 9.2 성능 개선

- [ ] 대규모 경계선 처리 최적화
- [ ] 메모리 사용량 감소
- [ ] 입력 검증 강화

### 9.3 사용성 개선

- [ ] 대화상자(Dialog Box) UI
- [ ] 설정 저장/불러오기
- [ ] 템플릿 기능
- [ ] Undo/Redo 지원

---

## 10. 참고 자료

### 10.1 AutoLISP 레퍼런스

- AutoCAD Developer Documentation
- AutoLISP Reference Guide

### 10.2 가시설 설계 기준

- 건축구조기준 (KDS 41)
- 가시설 설계 및 시공 지침
- 지반공학 설계기준

### 10.3 관련 표준

- KS F 4602: 강재말뚝
- KS F 2573: 토류판
- KS D 3503: 일반 구조용 압연강재

---

## 11. 버전 히스토리

### v1.0.0 (2026-01-13)

- ✅ 초기 버전 릴리스
- ✅ H-Pile + 토류판 공법 구현
- ✅ CIP 공법 구현 (3가지 타입)
- ✅ S.C.W 공법 구현
- ✅ 통합 메뉴 시스템
- ✅ 사용자 가이드 작성
- ✅ 기술 사양서 작성

---

## 문의

기술적인 질문이나 버그 리포트는 지반사업부 설계팀으로 연락 바랍니다.

**작성자**: 지반사업부 설계팀  
**최종 업데이트**: 2026-01-13
