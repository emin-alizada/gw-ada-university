import cv2
import numpy
import math

img = cv2.imread('assets/img4.jpg', cv2.IMREAD_GRAYSCALE)
imgColor = cv2.imread('assets/img4.jpg')

imgBlur = cv2.GaussianBlur(img, (3, 3), 0)
imgCanny = cv2.Canny(imgBlur, 100, 200)

sobelX = cv2.Sobel(img, cv2.CV_64F, 1, 0, ksize=5)
sobelY = cv2.Sobel(img, cv2.CV_64F, 0, 1, ksize=5)
imgSobel = numpy.sqrt(sobelY**2 + sobelX**2)
imgSobelNorm = (imgSobel * 255 / imgSobel.max()).astype(numpy.uint8)

cv2.imshow('img', imgSobelNorm)

imgLines = imgCanny
imgLinesP = imgCanny
lines = cv2.HoughLines(imgLines, 1, numpy.pi / 180, 150, None, 0, 0)

imgColoredLines = imgColor
imgColoredLinesP = imgColor

if lines is not None:
    for i in range(0, len(lines)):
        rho = lines[i][0][0]
        theta = lines[i][0][1]
        a = math.cos(theta)
        b = math.sin(theta)
        x0 = a * rho
        y0 = b * rho
        pt1 = (int(x0 + 1000 * (-b)), int(y0 + 1000 * (a)))
        pt2 = (int(x0 - 1000 * (-b)), int(y0 - 1000 * (a)))
        cv2.line(imgColoredLines, pt1, pt2, (0, 0, 255), 3, cv2.LINE_AA)

linesP = cv2.HoughLinesP(imgLinesP, 1, numpy.pi / 180, 50, None, 50, 10)

if linesP is not None:
    for i in range(0, len(linesP)):
        l = linesP[i][0]
        cv2.line(imgColoredLinesP, (l[0], l[1]), (l[2], l[3]), (0, 0, 255), 3, cv2.LINE_AA)

cv2.imshow("Source", img)
cv2.imshow("Detected Lines (in red) - Standard Hough Line Transform", imgColoredLines)
cv2.imshow("Detected Lines (in red) - Probabilistic Line Transform", imgColoredLinesP)

cv2.waitKey(0)
cv2.destroyAllWindows()