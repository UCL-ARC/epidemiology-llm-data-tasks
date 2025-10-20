#!/bin/bash

set -e  # exit on any error

echo "Checking if R is installed..."

if command -v R &> /dev/null; then
    echo "R is already installed."
    R --version | head -n 1
else
    echo "R is not installed. Starting installation process..."
    
    # ensure we're on ubuntu
    if ! command -v lsb_release &> /dev/null; then
        echo "Error: This script is designed for Ubuntu systems. Could not find lsb_release."
        exit 1
    fi
    
    # get ubuntu codename
    UBUNTU_VERSION=$(lsb_release -cs)
    echo "Detected Ubuntu version: $UBUNTU_VERSION"
    
    echo "Installing dependencies..."
    sudo apt update
    sudo apt install -y software-properties-common dirmngr gnupg curl
    
    # key
    echo "Adding R repository key..."
    wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
    
    # add repo
    echo "Adding R repository for $UBUNTU_VERSION..."
    
    case $UBUNTU_VERSION in
        noble|jammy|focal|bionic)
            sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu ${UBUNTU_VERSION}-cran40/"
            ;;
        *)
            echo "Unsupported Ubuntu version: $UBUNTU_VERSION"
            echo "Please add the appropriate repository manually and run the script again."
            exit 1
            ;;
    esac
    
    echo "Installing R..."
    sudo apt update
    sudo apt install -y r-base r-base-dev
    
    echo "R installation completed."
    R --version | head -n 1
fi

echo "Installing renv package for R..."
sudo Rscript -e 'install.packages("renv", repos="https://cloud.r-project.org")'

echo "R and renv setup complete."