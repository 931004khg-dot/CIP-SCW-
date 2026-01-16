#!/usr/bin/env python3
# -*- coding: utf-8 -*-

# 제공된 로그에서 직선 H-Pile 회전 각도 재확인
# 로그 원본 데이터:
log_data = """
경계선 분석 완료: 꼭지점 12개, 볼록 모서리 4개, 오목 모서리 8개.
선분 0: angle=180.0°, current rotation=180.0°, expected=90.0° ❌
선분 1: angle=270.0°, current rotation=270.0°, expected=180.0° ❌
선분 2: angle=180.0°, current rotation=180.0°, expected=90.0° ❌
선분 3: angle=270.0°, current rotation=270.0°, expected=180.0° ❌
선분 4: angle=180.0°, current rotation=180.0°, expected=90.0° ❌
선분 5: angle=270.0°, current rotation=270.0°, expected=180.0° ❌
선분 6: angle=0.0°, current rotation=0.0°, expected=270.0° ❌
선분 7: angle=90.0°, current rotation=90.0°, expected=0.0° ❌
선분 8: angle=0.0°, current rotation=0.0°, expected=270.0° ❌
"""

print("=" * 60)
print("로그 분석 보고서: 직선 구간 H-Pile 회전 각도")
print("=" * 60)
print()

# 현재 코드 (되돌린 후):
print("📍 현재 코드 상태 (bf09a5a):")
print("  hpile-rotation = seg-angle")
print("  boundary-direction = seg-angle - 90°")
print()

# 로그에서 나타난 문제:
print("❌ 로그에서 발견된 문제:")
print("  - 현재 회전(current rotation) = seg-angle")
print("  - 기대 회전(expected) = seg-angle - 90°")
print("  - 결론: 모든 직선 구간에서 90° 차이 발생")
print()

# 원인 분석:
print("🔍 원인 분석:")
print("  1. 현재 코드: hpile-rotation = seg-angle")
print("     → H-Pile 웹이 경계선과 평행하게 배치됨")
print()
print("  2. 올바른 배치를 위해서는:")
print("     → H-Pile 웹이 경계선에 수직(안쪽 향함)")
print("     → 플랜지가 경계선 바깥을 향함")
print("     → 플랜지 아래면이 경계선에 닿음")
print()
print("  3. 따라서 필요한 수정:")
print("     hpile-rotation = seg-angle - 90°")
print()

# 예시로 확인:
print("📊 구체적 예시:")
examples = [
    {"seg": 0, "angle": 180, "dir": "←(왼쪽)"},
    {"seg": 1, "angle": 270, "dir": "↓(아래)"},
    {"seg": 6, "angle": 0, "dir": "→(오른쪽)"},
    {"seg": 7, "angle": 90, "dir": "↑(위)"},
]

for ex in examples:
    seg = ex["seg"]
    angle = ex["angle"]
    direction = ex["dir"]
    current = angle
    expected = (angle - 90) % 360
    
    print(f"\n  선분 {seg}: {direction}")
    print(f"    선분 각도: {angle}°")
    print(f"    현재 회전: {current}° ❌")
    print(f"    올바른 회전: {expected}° ✓")
    print(f"    → 웹이 경계선 안쪽을 향하려면 {expected}°로 회전 필요")

print()
print("=" * 60)
print("결론:")
print("  현재 코드(bf09a5a)는 틀렸습니다!")
print("  이전 커밋(cbe1f07)이 올바른 수정이었습니다!")
print("  다시 cbe1f07 코드로 되돌려야 합니다!")
print("=" * 60)
