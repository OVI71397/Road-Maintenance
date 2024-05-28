# Road-Maintenance

## Introduction to the Problem
According to [the Global Economy](https://www.theglobaleconomy.com/Kenya/roads_quality/), the road quality index (**RQI**) in Kenya in 2019 was **4.1**, which put this country in 7th place right after South Africa (the comparison of these countries could be found [here](https://public.tableau.com/app/profile/viktoriia8163/viz/Kenyaroadcondition/Dashboard3)). However, taking into account the fact that **RQI varies from 1 to 7**, the Kenya road condition cannot be considered outstanding. Unfortunately, there is no open source recently updated data regarding current road conditions ([the latest update](https://datacatalog.worldbank.org/search/dataset/0042061) was in 2017) but, judging by the tendency of the last few years, the road quality was slowly declining. In view of this, the authorities should consider a reactive maintenance course of action. The designed **Kenya Roads** app will assist them in doing so. It provides detailed information on where the particularly problematic road sections are located and recommends specific works to improve the current situation.

## Features
- **Interactive Web Application**: Developed using the Shiny R package to provide an easy-to-use interface.
- **Interactive Maps**: Utilizes Leaflet and Plotly for displaying dynamic and interactive maps highlighting problematic road sections.
- **Image Classification**: Employs TensorFlow and Keras to train image classifiers using CNN models (MobileNetv2, ResNet152v2, Inceptionv3, VGG16) to identify road defects.
- **API Deployment**: The trained model is deployed as an API with Flask on AWS EC2, enabling real-time road condition assessments.

## Requirements
- **R Packages**:
  - Shiny
  - Leaflet
  - Plotly
  - maps
  - dplyr

- **Python Packages**:
  - TensorFlow
  - Keras
  - OpenCV
  - Flask
  - matplotlib
  - seaborn
  - numpy
  - pandas


