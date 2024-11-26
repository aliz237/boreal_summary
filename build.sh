#!/bin/bash
set -x
basedir=$( cd "$(dirname "$0")" ; pwd -P )
pushd ${HOME}

pip install git+https://github.com/MAAP-Project/maap-py.git@v4.1.0

source activate r
conda install -c conda-forge r-optparse -y
pip3 install pyOpenSSL --upgrade
