#!/usr/bin/bash
python3 -m venv env
source ./env/bin/activate
./env/bin/python3 -m pip install --upgrade pip
pip install -r ./requirements.txt
