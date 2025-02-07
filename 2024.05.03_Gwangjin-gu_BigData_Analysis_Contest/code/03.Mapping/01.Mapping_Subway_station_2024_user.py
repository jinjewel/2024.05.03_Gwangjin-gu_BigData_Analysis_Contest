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

# # 광진구 경계 좌표
# gwangjin_boundary = [
#     [37.5718, 127.0785],
#     [37.5318, 127.0581],
#     [37.5264, 127.0715],
#     [37.5263, 127.09],
#     [37.5436, 127.1099],
#     [37.5584, 127.1139],
#     [37.5589, 127.1122],
#     [37.5567, 127.1057],
#     [37.5609, 127.1011],
#     [37.5718, 127.1039],
#     [37.5724, 127.1019],
#     [37.573, 127.1013],
#     [37.5707, 127.0883],
#     [37.5718, 127.0785]
# ]
# # 광진구 경계를 지도에 추가
# folium.Polygon(gwangjin_boundary, color='blue', fill=True, fill_color='#3186cc').add_to(m)

# # 광진구 Polygon 객체 생성
# gwangjin_poly = Polygon(gwangjin_boundary)

### 서울시 지하철 역 위치를 지도에 표시
# 엑셀 파일 경로
excel_file_path = "C:/Users/User/Desktop/2024_05_2024년광진구빅데이터분석공모전_본선/예선 준비/code/광진구_내_지하철역별_2024사용자수_데이터.xlsx"

# 엑셀 파일 불러오기
df = pd.read_excel(excel_file_path)

# DataFrame에서 필요한 컬럼 선택
subway_stations = df[['station_name', 'user_number', 'latitude', 'longitude']]

# 데이터의 절대값에 따른 반지름 조정하기 위한 스케일링 팩터 설정
max_abs_diff = subway_stations['user_number'].max()
scale_factor = 50 / max_abs_diff

# 필터링된 버스 정류장을 지도에 표시
for index, row in subway_stations.iterrows():

    if int(row['user_number']) > 600000 :
        color = 'red'
    elif int(row['user_number']) > 500000 :
        color = 'orange'
    elif int(row['user_number']) > 400000 :
        color = 'green'    
    else :
        color = 'blue'    
        
    radius = row['user_number'] * scale_factor

    folium.CircleMarker([row['latitude'], row['longitude']],radius=radius,color=color, fill = True, fill_color=color, fill_opacity=0.7).add_to(m)

# HTML 파일로 저장
m.save('C:/Users/User/Desktop/2024_05_2024년광진구빅데이터분석공모전_본선/예선 준비/code/광진구_내_지하철역별_평균사용자수_표시.html')
