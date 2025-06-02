import math
import cv2
import matplotlib.pyplot as plt
import numpy as np
from skimage.filters import gaussian
from skimage.segmentation import active_contour
from skimage import draw

coordinates = []
img = None


def task2():
    global img
    img = cv2.imread("assets/img1.jpg")
    newSize = (img.shape[1] // 5, img.shape[0] // 5)
    img = cv2.resize(img, newSize, interpolation=cv2.INTER_AREA)
    cv2.imshow("window", img)
    cv2.setMouseCallback('window', mouseCallback)
    cv2.waitKey(0)
    cv2.destroyAllWindows()


def calcCenterAndRadius():
    global coordinates
    cord1 = coordinates[0]
    cord2 = coordinates[1]

    x1 = cord1[0]
    y1 = cord1[1]

    x2 = cord2[0]
    y2 = cord2[1]

    xCenter = (x1 + x2) // 2
    yCenter = (y1 + y2) // 2
    radius = math.sqrt((x2 - xCenter) ** 2 + (y2 - yCenter) ** 2)
    return int(xCenter), int(yCenter), int(radius)


def runActiveContour(xCenter, yCenter, radius):
    global img, coordinates
    s = np.linspace(0, 2 * np.pi, 360)
    r = yCenter + radius * np.sin(s)
    c = xCenter + radius * np.cos(s)
    init = np.array([r, c]).T

    snake = active_contour(gaussian(img, 3), init, alpha=0.015, beta=0.01, gamma=0.001)

    # Display the original image and the active contour
    fig, ax = plt.subplots(figsize=(15, 15))
    img = cv2.cvtColor(img, cv2.COLOR_RGB2BGR)
    ax.imshow(img, cmap='gray')
    ax.plot(init[:, 1], init[:, 0], '--r', lw=3)
    ax.plot(snake[:, 1], snake[:, 0], '-b', lw=3)
    ax.set_xticks([]), ax.set_yticks([])
    plt.savefig("output/task2/task3.jpg", dpi=900)
    plt.show()


def mouseCallback(event, x, y, _, __):
    global coordinates
    if event == cv2.EVENT_LBUTTONDOWN:
        coordinates = [(x, y)]
    elif event == cv2.EVENT_LBUTTONUP:
        coordinates.append((x, y))
        xCenter, yCenter, radius = calcCenterAndRadius()
        coordinates.append((xCenter, yCenter))
        # cv2.circle(img, coordinates[2], radius, (255, 0, 0), 2)
        cv2.imshow("window", img)
        runActiveContour(xCenter, yCenter, radius)
