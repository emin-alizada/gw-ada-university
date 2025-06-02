import cv2
import matplotlib.pyplot as plt
import os
import numpy as np

from skimage import color
from skimage.transform import hough_circle, hough_circle_peaks, probabilistic_hough_line
from skimage.feature import canny
from skimage.draw import circle_perimeter


def task1():
    edgeDetection()
    cornerDetection()
    lineAndCircleDetection()


def edgeDetection():
    imgArray = ['img1', 'img5', 'img10']

    def edgeDetectionProcess(img_name):
        img = cv2.imread(f'assets/{img_name}.jpg')
        img = cv2.cvtColor(img, cv2.COLOR_RGB2BGR)
        img_gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
        img_blur = cv2.GaussianBlur(img_gray, (3, 3), 0)

        # Sobel Edge Detection
        sobelx = cv2.Sobel(src=img_blur, ddepth=cv2.CV_64F, dx=1, dy=0, ksize=5)

        sobely = cv2.Sobel(src=img_blur, ddepth=cv2.CV_64F, dx=0, dy=1, ksize=5)

        sobelxy = cv2.Sobel(src=img_blur, ddepth=cv2.CV_64F, dx=1, dy=1, ksize=5)

        # Canny Edge Detection
        edges = canny(img_gray, sigma=3, low_threshold=10, high_threshold=50)
        edges = np.array((edges * 255), dtype=np.uint8)

        contours, hierarchy = cv2.findContours(edges, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        imgContour = cv2.drawContours(img, contours, -1, (255, 0, 0), 5)

        path = f'./output/task1/edgeDetection/{img_name}'
        if not os.path.exists(os.path.dirname(f'{path}/')):
            os.makedirs(os.path.dirname(f'{path}/'))

        # Display images
        plt.subplot(3, 3, 1)
        plt.imshow(img)
        plt.title('Original Image')
        plt.xticks([])
        plt.yticks([])

        plt.subplot(3, 3, 2)
        plt.imshow(img_gray, cmap='gray')
        plt.title('Grayscale Image')
        plt.xticks([])
        plt.yticks([])

        plt.subplot(3, 3, 3)
        plt.imshow(img_blur, cmap='gray')
        plt.title('Blurred Grayscale Image')
        plt.xticks([])
        plt.yticks([])

        plt.subplot(3, 3, 4)
        plt.imshow(sobelx, cmap='gray', vmin=0, vmax=255)
        plt.title('Sobel X')
        plt.xticks([])
        plt.yticks([])
        cv2.imwrite(f'{path}/sobelx.jpg', sobelx)

        plt.subplot(3, 3, 5)
        plt.imshow(sobely, cmap='gray', vmin=0, vmax=255)
        plt.title('Sobel Y')
        plt.xticks([])
        plt.yticks([])
        cv2.imwrite(f'{path}/sobely.jpg', sobely)

        plt.subplot(3, 3, 6)
        plt.imshow(sobelxy, cmap='gray', vmin=0, vmax=255)
        plt.title('Sobel XY')
        plt.xticks([])
        plt.yticks([])
        cv2.imwrite(f'{path}/sobelxy.jpg', sobelxy)

        plt.subplot(3, 3, 7)
        plt.imshow(edges, cmap='gray', vmin=0, vmax=255)
        plt.title('Canny Edge Detection')
        plt.xticks([])
        plt.yticks([])
        cv2.imwrite(f'{path}/canny.jpg', edges)

        plt.subplot(3, 3, 8)
        plt.imshow(imgContour)
        plt.title('Contours Detection')
        plt.xticks([])
        plt.yticks([])
        imgContour = cv2.cvtColor(imgContour, cv2.COLOR_BGR2RGB)
        cv2.imwrite(f'{path}/contours.jpg', imgContour)

        plt.tight_layout()
        plt.savefig(f'{path}/edgeDetection.jpg', dpi=900)
        plt.show()

    for i in imgArray:
        edgeDetectionProcess(i)


def cornerDetection():
    imgArray = ['img1', 'img5', 'img10']

    def cornerDetectionProcess(img_name):
        img = cv2.imread(f'assets/{img_name}.jpg')
        img = cv2.cvtColor(img, cv2.COLOR_RGB2BGR)
        imgGray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
        imgGray = cv2.GaussianBlur(imgGray, (3, 3), 0)
        imgGray = np.float32(imgGray)

        dst = cv2.cornerHarris(imgGray, 2, 3, 0.04)
        # result is dilated for marking the corners
        dst = cv2.dilate(dst, None)

        imgWithCorners = img.copy()
        imgWithCorners[dst > 0.001 * dst.max()] = [255, 0, 0]

        path = f'./output/task1/cornerDetection/{img_name}'
        if not os.path.exists(os.path.dirname(f'{path}/')):
            os.makedirs(os.path.dirname(f'{path}/'))

        plt.subplot(1, 2, 1)
        plt.imshow(img)
        plt.title('Original Image')
        plt.xticks([])
        plt.yticks([])

        plt.subplot(1, 2, 2)
        plt.imshow(imgWithCorners)
        plt.title('Image with corners')
        plt.xticks([])
        plt.yticks([])

        imgWithCorners = cv2.cvtColor(imgWithCorners, cv2.COLOR_BGR2RGB)
        cv2.imwrite(f'{path}/cornerDetection.jpg', imgWithCorners)

        plt.tight_layout()
        plt.savefig(f'{path}/cornerDetectionPlot.jpg', dpi=900)
        plt.show()

    for i in imgArray:
        cornerDetectionProcess(i)


def lineAndCircleDetection():
    imgArray = ['img1', 'img5', 'img10']

    def circleDetectionProcess(img_name):
        img = cv2.imread(f'assets/{img_name}.jpg', cv2.IMREAD_GRAYSCALE)
        edges = canny(img, sigma=3, low_threshold=10, high_threshold=50)

        hough_radii = np.arange(45, 250, 2)
        hough_res = hough_circle(edges, hough_radii)

        accums, cx, cy, radii = hough_circle_peaks(hough_res, hough_radii,
                                                   total_num_peaks=10)

        imgWithCircle = color.gray2rgb(img)
        for center_y, center_x, radius in zip(cy, cx, radii):
            circy, circx = circle_perimeter(center_y, center_x, radius,
                                            shape=imgWithCircle.shape)
            imgWithCircle[circy, circx] = (220, 20, 20)

        plt.subplot(1, 2, 1)
        plt.imshow(img, cmap='gray')
        plt.title('Original Image Grayscale')
        plt.xticks([])
        plt.yticks([])

        plt.subplot(1, 2, 2)
        plt.imshow(imgWithCircle, cmap='gray')
        plt.title('Image with circles')
        plt.xticks([])
        plt.yticks([])

        path = f'./output/task1/circleDetection/{img_name}'
        if not os.path.exists(os.path.dirname(f'{path}/')):
            os.makedirs(os.path.dirname(f'{path}/'))

        plt.tight_layout()
        plt.savefig(f'{path}/circleDetection.jpg', dpi=900)
        plt.show()

    def lineDetectionProcess(img_name):
        img = cv2.imread(f'assets/{img_name}.jpg', cv2.IMREAD_GRAYSCALE)
        edges = canny(img, sigma=3, low_threshold=10, high_threshold=50)

        lines = probabilistic_hough_line(edges, threshold=10, line_length=50, line_gap=15)

        linesImg = color.gray2rgb(img)

        for line in lines:
            start_point, end_point = line
            line_width = 10
            cv2.line(linesImg, start_point, end_point, (255, 0, 0), line_width)

        plt.subplot(2, 2, 1)
        plt.imshow(img, cmap='gray')
        plt.title('Original Image Grayscale')
        plt.xticks([])
        plt.yticks([])

        plt.subplot(2, 2, 2)
        plt.imshow(edges, cmap='gray')
        plt.title('Canny Edge Detection')
        plt.xticks([])
        plt.yticks([])

        plt.subplot(2, 2, 3)
        plt.imshow(linesImg, cmap='gray')
        plt.title('Image with lines')
        plt.xticks([])
        plt.yticks([])

        path = f'./output/task1/lineDetection/{img_name}'
        if not os.path.exists(os.path.dirname(f'{path}/')):
            os.makedirs(os.path.dirname(f'{path}/'))

        plt.tight_layout()
        plt.savefig(f'{path}/lineDetection.jpg', dpi=900)
        plt.show()

    for i in imgArray:
        circleDetectionProcess(i)
        lineDetectionProcess(i)
