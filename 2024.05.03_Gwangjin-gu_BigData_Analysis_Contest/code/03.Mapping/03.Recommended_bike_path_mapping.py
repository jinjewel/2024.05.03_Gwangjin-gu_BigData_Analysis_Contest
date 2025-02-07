import folium

# 광진구의 중심 위치
latitude = 37.5385
longitude = 127.0820

# 지도의 중심을 정하기 위한 위도와 경도 좌표
map_center = [latitude, longitude]

# Folium 지도 객체 생성
m = folium.Map(location=map_center, zoom_start=13)

# 선을 그릴 좌표 리스트
coordinates1 = [
    [37.5451, 127.0853],
    [37.539986, 127.0832],
    [37.5366, 127.08355],
    # ST-255
]

coordinates2 = [
    [37.5483, 127.0676],
    [37.5482, 127.0689],
    [37.548012, 127.070461],
    [37.54785, 127.0718],
    [37.5473, 127.0741]
]

coordinates3 = [
    [37.53247833,127.0850906], # ST-257
    [37.5345, 127.0917],
    [37.5344, 127.0927],
    [37.53596878,127.0946732] # ST-255
]

coords_1 = [(37.5573,127.07955),(37.5599, 127.081), (37.5623, 127.0823), (37.5664, 127.084585)]
coords_2 = [(37.5642, 127.07655),(37.56402,127.0772), (37.5623, 127.082283)]
coords_3 = [(37.5623, 127.082283), (37.56112,127.08444),(37.5594, 127.0877)]

# Folium PolyLine 객체를 이용해서 선 그리기, weight로 선의 두께 조정
folium.PolyLine(coordinates1, color='purple', weight=7).add_to(m)  # 여기에서 weight을 조정
folium.PolyLine(coordinates2, color='purple', weight=7).add_to(m)  # 여기에서 weight을 조정
folium.PolyLine(coordinates3, color='deeppink', weight=7).add_to(m)  # 여기에서 weight을 조정
folium.PolyLine(coords_1, color="purple", weight=8, opacity=1).add_to(m)
folium.PolyLine(coords_2, color="purple", weight=8, opacity=1).add_to(m)
folium.PolyLine(coords_3, color="deeppink", weight=8, opacity=1).add_to(m)

# HTML 파일로 저장
m.save('C:/Users/User/Desktop/2024_05_2024년광진구빅데이터분석공모전_본선/예선 준비/code/광진구_내_보수를_추천하는_자전거도로.html')
