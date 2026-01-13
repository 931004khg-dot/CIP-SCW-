# 가시설 흙막이 벽체 자동 작도 LISP

지반사업부 가시설 설계를 위한 AutoCAD LISP 프로그램 모음

## 개요

건축물의 지하벽체 경계선을 따라 흙막이 벽체 구조물을 자동으로 작도하는 LISP 프로그램입니다.

## 지원 공법

### 1. H-Pile + 토류판 공법
- 엄지말뚝(H-Pile) 자동 배치
- 토류판 표시
- 띠장(Wale) 및 버팀보(Strut) 자동 생성

### 2. CIP (Cast In Place Pile) 공법
- 현장타설 콘크리트 말뚝 배치
- 연속벽체 또는 간격 배치
- 철근 표시 및 띠장/버팀보 생성

### 3. S.C.W (Soil Cement Wall) 공법
- 소일시멘트 연속지중벽 표시
- H-Beam 보강재 배치
- 띠장 및 버팀보 자동 생성

## 파일 구조

```
/home/user/webapp/
├── README.md                 # 프로젝트 설명서
├── lisp/                     # LISP 프로그램 폴더
│   ├── hpile-retaining.lsp   # H-Pile + 토류판 공법
│   ├── cip-retaining.lsp     # CIP 공법
│   ├── scw-retaining.lsp     # S.C.W 공법
│   ├── retaining-utils.lsp   # 공통 유틸리티 함수
│   └── retaining-main.lsp    # 통합 메인 메뉴
├── docs/                     # 문서
│   ├── user-guide.md         # 사용자 가이드
│   └── technical-spec.md     # 기술 사양
└── examples/                 # 예제 도면
    └── sample-drawings.dwg
```

## 설치 방법

1. `lisp` 폴더의 모든 `.lsp` 파일을 AutoCAD에 로드
2. 명령어 입력: `APPLOAD`
3. `retaining-main.lsp` 선택하여 로드

## 사용 방법

### 기본 사용

1. AutoCAD에서 명령어 입력: `RETAINING` 또는 `RT`
2. 공법 선택 (1: H-Pile+토류판, 2: CIP, 3: S.C.W)
3. 지하벽체 경계선 선택 (Polyline 또는 Line)
4. 설계 파라미터 입력
   - 엄지말뚝 간격
   - 띠장 레벨 및 개수
   - 버팀보 간격
5. 자동 작도 실행

### 개별 명령어

- `HPILE-RT` : H-Pile + 토류판 공법 직접 실행
- `CIP-RT` : CIP 공법 직접 실행
- `SCW-RT` : S.C.W 공법 직접 실행

## 주요 기능

### 자동 생성 요소

1. **엄지말뚝/벽체**
   - H-Pile: 단면 형상 및 크기 지정
   - CIP: 원형 또는 사각형 말뚝
   - SCW: 연속벽체 두께 지정

2. **띠장 (Wale)**
   - 레벨별 자동 배치
   - H-Beam 또는 Channel 단면

3. **버팀보 (Strut)**
   - 평면상 자동 배치
   - 교차점 처리

4. **레이어 관리**
   - 자동 레이어 생성
   - 색상 및 선종류 설정

### 설계 파라미터

- 엄지말뚝 간격: 기본 1.5m ~ 3.0m
- 띠장 수직 간격: 기본 2.0m ~ 3.0m
- 버팀보 수평 간격: 기본 4.0m ~ 8.0m
- 단면 크기: 공법별 표준 규격

## 레이어 체계

| 레이어명 | 용도 | 색상 | 선종류 |
|---------|------|------|--------|
| PILE | 엄지말뚝/벽체 | Red (1) | Continuous |
| WALE | 띠장 | Blue (5) | Continuous |
| STRUT | 버팀보 | Green (3) | Continuous |
| RETAINING-DIM | 치수선 | Yellow (2) | Continuous |
| RETAINING-TEXT | 문자 | White (7) | Continuous |

## 기술 사양

- AutoCAD 버전: 2010 이상
- 좌표계: WCS (World Coordinate System)
- 단위: Metric (mm)
- 파일 형식: AutoLISP (.lsp)

## 향후 개선 계획

- [ ] 3D 입체 모델 생성
- [ ] 굴착 단계별 단면도 자동 생성
- [ ] 물량 자동 산출 기능
- [ ] Excel 연동 물량표 출력
- [ ] 계측기 위치 자동 배치

## 작성자

지반사업부 설계팀

## 버전

- v1.0.0 (2026-01-13): 초기 버전 생성

## 라이선스

내부 사용 전용
