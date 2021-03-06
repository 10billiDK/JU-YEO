# Ju-Yeo 전통주 추천 어플리케이션/시스템

<h2>Project Introduction</h2><br>

[![Watch the video](https://i.imgur.com/vKb2F1B.png)](https://youtu.be/Qwf9D3z0fys)<br>

본 프로젝트는 농림축산식품부 빅데이터 창업 경진대회 출품작이며, 우수상을 수상한 프로젝트입니다. 외면 받는 전통주를 대중화하고자 하는 목표로 프로젝트를 진행했습니다.<br>

<h2>File description</h2><br>

main.py - 파이썬의 KivyMD Library를 이용한 어플리케이션 프론트엔드 소스코드<br>

Jurecom.py - TF-IDF를 이용해 키워드 분석을 하고, 그 결과물을 코사인 유사도 측정을 하여 컨텐츠기반의 전통주 추천 시스템을 만든 코드<br>

association.py - Apriori 분석을 통해서 날씨별, 상황별, 안주별 추천 술들을 정리해 두었고, 그 CSV파일들을 이용해서 챗봇 작동시 해당 술들을 추천해주는 기능을 하는 코드<br>

R-SHINY_ggplot최종 - EDA를 하며 유의미하다고 생각된 통계 자료를 시각화하였고, 이 자료는 R-SHINY를 통해 웹에 구현<br>

R-SHINY_map1 - 전통주 양조장의 주소를 GGMAP을 활용해 시각화 했고, 이 자료는 R-SHINY를 통해 웹에 구현<br>

image 폴더 - 각 전통주의 이미지를 크롤링해서 모아둔 폴더<br>

Juyeo.png - 이 어플리케이션의 인트로 화면에 쓰이는 로고 이미지<br>

graph.png, jido.png - RShiny를 이용한 양조주 위치검색 페이지나, 통계분석 자료들을 모아놓은 페이지로 이동하기 위한 클릭용 이미지<br>

kivy-env.zip - 이 어플리케이션의 폰트와 테마등을 조정해 둔 환경 파일들<br>

Reference_materials.pdf - 이 프로젝트 내용이 담긴 PPT<br>

Proposal.docx - 이 프로젝트의 기획서이고, 전반적인 흐름, 디테일을 다 포함하고 있음<br>

<h2>Data</h2><br>

전통주 주류 관련 데이터 - 전통주 갤러리 사이트에서 주류 관련 정보를 크롤링함. 이는 컨텐츠 기반 추천시스템을 만드는 데 활용됐음<br>

설문지 데이터 - 설문을 통해 사람들이 전통주를 어떤 상황에, 어떤 날씨에, 어느 안주와 함께 마시는지 조사. 이는 전통주 추천 챗봇의 장바구니 분석에 활용 됐음<br>

전통주 주류 관련 데이터, 설문지 데이터는 전처리를 하고 모델링을 한 뒤, 로컬 데이터베이스에 저장해 두었음<br>


<h2>To do List</h2><br>
데이터베이스가 로컬로 형성되어있음 -> 추후 서버를 임대하거나, 리눅스로 서버를 구축하여 데이터베이스를 올려볼 계획<br>

이달의 전통주를 하나하나 직접 입력함 -> 데이터베이스에 this-month 테이블을 추가하여 더술닷컴 이달의 전통주 세션과 연결해볼 계획<br>

챗봇의 구현이 미흡함 -> 추후 KoGPT를 이용한 자연어처리를 이용한 언어학습도 생각해봤으나, 우선순위가 떨어짐<br>

추천의 정확도가 떨어짐 -> 서버를 연결하면 로그인기능을 더할계획. 사용자가 로그인을 해서 각각 전통주를 검색한 기록이나, 즐겨찾기한 목록등을 이용하여 협력기반 필터링 시스템을 도입하는것도 구상중<br>

라벨인식 기능 미구현 -> openCV for python을 이용해서 구현할 계획. 최우선적으로 만들것이고, 모아놓은 크롤링 이미지와 그 이미지들의 변형을 이용해서 학습을 시킬 계획<br>
