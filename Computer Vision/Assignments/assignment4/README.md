# Assignment 4 - Object detection and document recognition

Make sure you have python installed on your machine

The python version which was used in development is Python 3.9

Make sure you have installed packages that are required for this project. Also make sure you have installed Tesseract OCR engine on your machine.

On mac you can use homebrew to install tesseract:

`brew install tesseract`

### To run the code from the terminal, change the directory to the root of the project and run:

`python3 main.py`

## Program Logic:

First I show the image and let user crop the image.
Then I show the cropped image on top of the original image, at this stage user can decide if it is a good crop or not.
If the user decides that the crop is good, the by clicking any key on the keyboard, the program will close both images and start
the recognition process. Otherwise, user can focus on the original image and make a new crop, until the crop is good.

The recognition process is done by using the Tesseract OCR engine. The program will first convert the image to grayscale,
then run Tessaract OCR on the image. The result will be printed on the terminal. Afterwards the regular expression is used to
detect ID number of ADA Student Card, if the result is found, it will be printed on the terminal.