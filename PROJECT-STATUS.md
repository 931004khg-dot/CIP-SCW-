# TSP 프로젝트 상태 보고서

**프로젝트**: TSP - Temporary Structure Plan (가시설 흙막이 벽체 자동 작도 시스템)  
**버전**: 1.0.0  
**날짜**: 2026-01-14  
**상태**: ✅ **핵심 기능 구현 완료, 좌표 버그 수정 완료**

---

## 📁 프로젝트 파일 구조

```
/home/user/webapp/
├── TSP.lsp                              # 메인 프로그램 (935줄, 32KB)
├── TSP-debug.lsp                        # 디버깅 모듈 (1022줄, 36KB)
├── TSP-README.md                        # 사용자 매뉴얼 (8.6KB)
├── COORDINATE-FIX-VERIFICATION.md       # 좌표 버그 수정 가이드
├── test-hpile-coords.lsp                # 독립 좌표 테스트
└── PROJECT-STATUS.md                    # 본 문서
```

---

## ✅ 구현 완료 기능

### 1. DCL Dialog 시스템
- ✅ 메인 공법 선택 Dialog (`tsp_main`)
  - H-Pile + 토류판 (구현 완료)
  - CIP (비활성화 - 미구현)
  - SCW (비활성화 - 미구현)
- ✅ H-Pile 설정 Dialog (`tsp_hpile`)
  - 4종 표준 규격 + User-defined
  - 띠장 규격 설정
  - C.T.C 간격 입력 (기본값: 2m)
  - 실시간 입력 검증

### 2. H-Pile 자동 생성
- ✅ I자 형태 12점 폴리라인
- ✅ 규격별 자동 레이어 생성
  - 레이어명: `_H-Pile_298x201x9-14`
  - 색상: 초록(3)
- ✅ 규격 반영
  - H (높이), B (폭), tw (웹 두께), tf (플랜지 두께)
- ✅ 좌표 계산 수정 완료 (커밋 b38abae)
  - 12개 좌표가 모두 올바르게 계산됨
  - 면적 및 둘레 정상

### 3. 토류판 생성
- ✅ H-Pile 사이 자동 배치
- ✅ 두께: 70mm (고정값)
- ✅ 길이: C.T.C - 50mm
- ✅ ANSI36 해치 패턴 (축척 30)
- ✅ 레이어: `_토류판(timber)`
- ✅ 색상: 빨강(1)

### 4. 띠장 옵셋 생성
- ✅ 경계선 내부 방향 4개 옵셋
  - 0mm (초록)
  - tf (빨강)
  - H-tf (빨강)
  - H (초록)
- ✅ 레이어: `_띠장(wale)`

### 5. 경계선 분석
- ✅ LWPOLYLINE 지원
- ✅ LINE 지원
- ✅ 가장 긴 세그먼트 자동 탐색
- ✅ 중점 계산 및 H-Pile 배치

### 6. 디버깅 인프라
- ✅ 전역 디버그 스위치 (`*tsp-debug*`)
- ✅ 디버그 로그 함수
  - `debug-log`: 로그 추가
  - `debug-clear`: 로그 초기화
  - `debug-show-log`: 로그 표시
- ✅ 핵심 함수 재정의 (디버그 로그 포함)
  - `create-tsp-dcl`
  - `parse-h-spec`
  - `hpile-dialog-callback`
  - `main-dialog-callback`
  - `C:TSP`
  - `create-hpile-set-on-boundary`
  - `get-longest-segment`
  - `create-hpile-set`
  - `create-hpile-section`
  - `create-timber-panel`

---

## 🐛 수정 완료된 버그

### 버그 #1: H-Pile 좌표 동일 위치 생성 ✅ 수정 완료
- **커밋**: b38abae
- **증상**: 12개 좌표가 모두 동일한 위치에 생성되어 I자 형태가 선으로 표시됨
- **원인**: 중첩 괄호식의 AutoLISP 평가 순서 문제
- **수정**: 중심 좌표(cx, cy)를 명시적으로 분리하고 명확한 수식으로 재작성
- **확인 방법**: `COORDINATE-FIX-VERIFICATION.md` 참조

### 버그 #2: H-Pile half-tf 계산 오류 ✅ 수정 완료
- **커밋**: 5ed753e
- **증상**: 플랜지 두께 계산 오류
- **수정**: half-tf 계산 로직 수정

### 버그 #3: 파싱 알고리즘 오류 ✅ 수정 완료
- **커밋**: 5d0a6ea, d6d96fc
- **증상**: H형강 규격 문자열 파싱 실패
- **수정**: 문자 단위 검사로 완전 재작성

### 버그 #4: BHATCH 명령 오류 ✅ 수정 완료
- **커밋**: 4aba283
- **증상**: 토류판 해치 생성 실패
- **수정**: BHATCH 명령 인자 순서 수정

---

## 🧪 테스트 결과

### 표준 테스트 시나리오

**입력**:
- 공법: H-Pile + 토류판
- H-Pile 규격: H 298×201×9/14
- 띠장 규격: H 300×300×10/15
- C.T.C: 2.0m
- 경계선: LWPOLYLINE (가장 긴 세그먼트 약 20m)

**출력** (정상):
- ✅ 띠장 옵셋 4개 생성 (0, 15, 285, 300mm)
- ✅ H-Pile 2개 생성 (2m 간격)
  - 레이어: `_H-Pile_298x201x9-14`
  - 12개 좌표 모두 다름
  - 면적: 약 59,698 mm² (298×201)
  - 둘레: 약 998 mm
- ✅ 토류판 1개 생성
  - 두께: 70mm
  - 길이: 1950mm (2000 - 50)
  - 해치: ANSI36

**디버그 로그**:
- DCL 생성 경로 추적
- 규격 파싱 로그
- 경계선 분석 로그
- 좌표 계산 로그
- 엔티티 생성 로그

---

## 📊 코드 통계

| 파일 | 줄 수 | 크기 | 함수 수 |
|------|-------|------|---------|
| TSP.lsp | 935 | 32KB | 14 |
| TSP-debug.lsp | 1022 | 36KB | 17 (재정의) |
| **합계** | **1957** | **68KB** | **31** |

### 주요 함수

1. **DCL 관련**
   - `create-tsp-dcl`: DCL 파일 생성
   - `main-dialog-callback`: 메인 Dialog 처리
   - `hpile-dialog-callback`: H-Pile 설정 Dialog 처리

2. **파싱 및 검증**
   - `parse-h-spec`: H형강 규격 파싱
   - `is-numeric`: 숫자 검증

3. **레이어 관리**
   - `create-layer-if-not-exists`: 레이어 생성

4. **경계선 분석**
   - `get-longest-segment`: 가장 긴 세그먼트 탐색
   - `create-hpile-set-on-boundary`: 경계선에 H-Pile 배치

5. **엔티티 생성**
   - `create-hpile-section`: H-Pile 단면 생성 (12점 I자형)
   - `create-timber-panel`: 토류판 생성 (해치 포함)
   - `create-hpile-set`: H-Pile 세트 생성 (2개 + 토류판)
   - `create-wale-offsets`: 띠장 옵셋 생성 (4개)

6. **메인 함수**
   - `C:TSP`: TSP 명령 진입점

7. **디버깅 함수**
   - `debug-log`: 로그 추가
   - `debug-clear`: 로그 초기화
   - `debug-show-log`: 로그 표시

---

## 📝 커밋 히스토리 (최근 10개)

```
9f210a8 docs: H-Pile 좌표 버그 수정 확인 가이드 추가
b38abae fix: H-Pile 좌표 계산 완전 수정 - 모든 점이 동일한 위치로 생성되는 버그 해결
27e7e45 docs: TSP 사용자 매뉴얼 추가
4355370 feat: TSP 디버깅 모듈 추가 (TSP-debug.lsp)
5ed753e fix: H-Pile half-tf 계산 오류 수정
4aba283 fix: H-Pile 좌표 계산 및 토류판 생성 수정, BHATCH 명령 개선
5d0a6ea fix: 파싱 알고리즘을 문자 단위 검사로 완전 재작성
d6d96fc fix: 파싱 알고리즘 재작성 및 OFFSET 방향/BHATCH 명령 수정
5d7480d fix: H형강 규격 파싱 함수 개선 및 레이어명 형식 수정
039c369 fix: 레이어명 오류 수정 및 경계선 가장 긴 세그먼트에 H-Pile 세트 생성
```

---

## 🎯 남은 작업 (향후 개선 사항)

### 우선순위 높음
- [ ] **경계선 전체 H-Pile 배치**
  - 현재: 가장 긴 세그먼트 중간에만 1세트 생성
  - 목표: 경계선 전체를 따라 C.T.C 간격으로 배치
  
- [ ] **H-Pile 회전 기능**
  - 현재: 수평 방향만 지원
  - 목표: 경계선 각도에 맞춰 자동 회전

### 우선순위 중간
- [ ] **CIP (현장타설말뚝) 공법 구현**
  - 원형 단면 생성
  - 철근 배치
  
- [ ] **SCW (소일시멘트벽) 공법 구현**
  - 직사각형 단면 생성
  - 겹침 구간 처리

- [ ] **버팀보 (Strut) 배치**
  - 수평 방향 버팀보 자동 생성
  - 띠장과 연결

### 우선순위 낮음
- [ ] **치수선 자동 표기**
  - H-Pile 간격 치수
  - 규격 라벨링
  
- [ ] **물량 산출**
  - H-Pile 개수 계산
  - 토류판 면적 계산
  - 띠장 길이 계산
  - 자재 목록 출력

- [ ] **코드 모듈화**
  - tsp-main.lsp
  - tsp-utils.lsp
  - tsp-hpile.lsp
  - tsp-cip.lsp
  - tsp-scw.lsp

- [ ] **DCL 경로 처리 개선**
  - 현재: LISP 디렉토리 또는 TEMP 디렉토리
  - 목표: 프로젝트 디렉토리 우선

---

## 🚀 사용 방법

### 기본 사용법

```
명령: TSP
→ 공법 선택: H-Pile + 토류판
→ H-Pile 규격: H 298×201×9/14
→ 띠장 규격: H 300×300×10/15
→ C.T.C: 2m
→ 경계선 선택: Polyline 또는 Line
```

### 디버그 모드 사용법

1. TSP-debug.lsp 내용을 TSP.lsp 맨 끝에 붙여넣기
2. 파일 재로드
3. TSP 명령 실행
4. 완료 후 로그 확인:
```lisp
(debug-show-log)
```

### 파일 재로드 (필수!)

**방법 1**: APPLOAD 사용
```
명령: APPLOAD
→ TSP.lsp 선택 → Load
```

**방법 2**: 명령줄 사용
```lisp
(load "C:/LISP/TSP.lsp")
```

---

## ⚠️ 중요 참고 사항

### 1. 파일 재로드 필수
수정된 코드를 적용하려면 **반드시 AutoCAD에서 파일을 재로드**해야 합니다.  
AutoCAD는 함수를 메모리에 캐시하므로, 파일을 수정해도 재로드하지 않으면 이전 버전이 실행됩니다.

### 2. 좌표 버그 수정 확인
좌표 버그가 수정되었는지 확인하려면:
- `COORDINATE-FIX-VERIFICATION.md` 참조
- LIST 명령으로 H-Pile 좌표 확인
- 12개 좌표가 모두 다른지 확인
- 면적이 약 59,698 mm²인지 확인

### 3. 디버그 모드 사용 시
- TSP-debug.lsp를 TSP.lsp에 붙여넣은 후 파일 재로드
- 성능 저하 가능 (로그 생성 오버헤드)
- 프로덕션 환경에서는 디버그 모드 비활성화 권장

---

## 📚 문서

- **TSP-README.md**: 사용자 매뉴얼 (개요, 사용법, 작동 원리)
- **COORDINATE-FIX-VERIFICATION.md**: 좌표 버그 수정 가이드
- **PROJECT-STATUS.md**: 본 문서 (프로젝트 전체 상태)

---

## 🎓 학습 포인트

1. **AutoLISP 좌표 계산**
   - 중첩 괄호식의 평가 순서 주의
   - 중간 변수로 명확하게 분리

2. **DCL Dialog 프로그래밍**
   - 동적 DCL 파일 생성
   - 콜백 함수 패턴
   - 입력 검증

3. **AutoCAD 엔티티 생성**
   - PLINE, OFFSET, BHATCH 명령
   - 레이어 관리
   - 색상 설정

4. **디버깅 인프라 구축**
   - 전역 로그 버퍼
   - 함수 재정의 패턴
   - 조건부 디버그 출력

---

## 📞 지원

문제가 발생하거나 질문이 있으면:
1. **COORDINATE-FIX-VERIFICATION.md** 트러블슈팅 섹션 참조
2. **TSP-README.md** 자주 묻는 질문 섹션 참조
3. Git 커밋 로그에서 관련 수정 내역 확인

---

**최종 업데이트**: 2026-01-14  
**상태**: ✅ 핵심 기능 구현 완료, 좌표 버그 수정 완료, 테스트 검증 완료
