[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-24ddc0f5d75046c5622901739e7c5dd533143b0c8e959d652212380cedb1ea36.svg)](https://classroom.github.com/a/Q7E_7-bO)

# Assignment 3 - Feature Extraction

Make sure you have python installed on your machine

The python version which was used in development is Python 3.9

Make sure you have installed packages that are required for this project

### To run the code from the terminal, change the directory to the root of the project and run:

`python3 main.py`

Tasks are executed and generated files are saved in a [Output](output) directory according to the task number and the feature type

## Folder structure

[Assets](assets) folder contains photos that are used for the assignment, these are the original photos that are used for the assignment

[Output](output) folder contains the generated files for each task

[Src](src) folder contains the source code for the assignment


## Code structure

`main.py` serves as the entry point for the program and calls the functions for the respective tasks. For each task, there is a function that is called from the main function. Each function represents a task from the assignment

### Task 1

Task 1 consist of 3 subtasks and has corresponding functions for each subtask. 

#### Edge detection
Edge detection uses Sobel and Canny method to detect edges in the image. Afterward, uses edge photos to find contours and draw them on the original image. Canny performs better than Sobel in my case.

#### Corner detection
Corner detection uses Harris method to detect corners in the image.

### Line and Circle detection
Line and Circle detection uses Hough transform to detect lines and circles in the image from skimage package.

### Task 2
To find active contours, I used the snake algorithm from skimage package. First User can draw circle on the image and then the snake algorithm is used to find the contour of the circle. 

### Task 3
To find interest points of the image, I used the ORB and SIFT algorithm from opencv package. I also save the keypoints in the file for each image. After that I used Brute Force Matcher to match the descriptors of the images, and draw matches on the images.