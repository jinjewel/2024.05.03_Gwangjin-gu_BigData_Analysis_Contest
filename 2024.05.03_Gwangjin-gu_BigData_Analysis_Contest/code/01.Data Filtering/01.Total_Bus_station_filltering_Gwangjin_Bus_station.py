### 실제 서울시 버스정류장 데이터에서 광진구 버스정류장 데이터를 뽑아내기

from shapely.geometry import Point, Polygon
import folium
import pandas as pd
import numpy as np

### 광진구 지역을 지도에 표시
# 광진구의 중심 좌표
gwangjin_center = [37.5386, 127.0827]

# 지도 객체 생성
m = folium.Map(location=gwangjin_center, zoom_start=13)

# OpenStreetMap의 URL을 사용하여 지도에 도로 레이어 추가
folium.TileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', attr='OpenStreetMap').add_to(m)

# 광진구 경계 좌표
gwangjin_boundary = [
    [37.5718, 127.0785],
    [37.5318, 127.0581],
    [37.5264, 127.0715],
    [37.5263, 127.09],
    [37.5436, 127.1099],
    [37.5584, 127.1139],
    [37.5589, 127.1122],
    [37.5567, 127.1057],
    [37.5609, 127.1011],
    [37.5718, 127.1039],
    [37.5724, 127.1019],
    [37.573, 127.1013],
    [37.5707, 127.0883],
    [37.5718, 127.0785]
]
# 광진구 경계를 지도에 추가
folium.Polygon(gwangjin_boundary, color='blue', fill=True, fill_color='#3186cc').add_to(m)

# 광진구 Polygon 객체 생성
gwangjin_poly = Polygon(gwangjin_boundary)

### 서울시 버스 정류장 위치를 지도에 표시
# 엑셀 파일 경로
excel_file_path = "C:/Users/User/Desktop/2024_05_2024년광진구빅데이터분석공모전_본선/예선 준비/code/서울시_내_버스정류장_데이터.xlsx"

# 엑셀 파일 불러오기
df = pd.read_excel(excel_file_path)

# DataFrame에서 필요한 컬럼 선택
bus_stations = df[['ID', 'Name', 'Latitude', 'longitude']]

# 광진구 내 버스 정류장 필터링
filtered_stations = bus_stations[bus_stations.apply(lambda row: gwangjin_poly.contains(Point(row['Latitude'], row['longitude'])), axis=1)]

# 엑셀 파일로 저장
filtered_stations.to_excel("C:/Users/User/Desktop/2024_05_2024년광진구빅데이터분석공모전_본선/예선 준비/code/서울시_내_버스정류장에서_필터링한_광진구_내_버스정류장_데이터.xlsx" , index=False)

# 필터링된 버스 정류장을 지도에 표시
for index, row in filtered_stations.iterrows():
    folium.Marker([row['Latitude'], row['longitude']], popup=row['Name']).add_to(m)

# HTML 파일로 저장
m.save('C:/Users/User/Desktop/2024_05_2024년광진구빅데이터분석공모전_본선/예선 준비/code/서울시_내_버스정류장에서_필터링한_광진구_내_버스정류장.html')
