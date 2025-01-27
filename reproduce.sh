#!/bin/bash

# Download the data
if [ ! -d covid-policy-dataset ];
then
  git clone git@github.com:OxCGRT/covid-policy-dataset.git
fi

if [ ! -d aggregatesUMD ];
then
  git clone git@github.com:daniele-baccega/aggregatesUMD.git
fi

if [ ! -d Region_Mobility_Report_CSVs ];
then
  wget -nc https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip
  unzip Region_Mobility_Report_CSVs.zip -d Region_Mobility_Report_CSVs
  rm Region_Mobility_Report_CSVs.zip
fi

if [ ! -d Sybil ];
then
  git clone git@github.com:daniele-baccega/sybil-forecasting.git
  mv sybil-forecasting Sybil
  cd Sybil
  git checkout sybil-v3.0
  cd ..
fi

docker build -t danielebaccega/reproduce-sybil .
docker run -it --user $UID:$UID --rm -v $(pwd):/home/docker/sybil-forecasting danielebaccega/reproduce-sybil
