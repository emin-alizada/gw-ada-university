import numpy as np
import cv2 as cv
from matplotlib import pyplot as plt

imgPath = 'img/water_coins.jpg'

img = cv.imread(imgPath)
assert img is not None, "file could not be read, check with os.path.exists()"
img = cv.cvtColor(img, cv.COLOR_BGR2RGB)
gray = cv.cvtColor(img, cv.COLOR_BGR2GRAY)
ret, thresh = cv.threshold(gray,0,255,cv.THRESH_BINARY_INV+cv.THRESH_OTSU)

plt.subplot(1, 2, 1)
plt.imshow(img, cmap='gray')
plt.subplot(1, 2, 2)
plt.imshow(thresh, cmap='gray')
plt.show()

# ========================

# noise removal
kernel = np.ones((3,3),np.uint8)
opening = cv.morphologyEx(thresh,cv.MORPH_OPEN,kernel, iterations = 2)
# sure background area
sure_bg = cv.dilate(opening,kernel,iterations=3)
# Finding sure foreground area
dist_transform = cv.distanceTransform(opening,cv.DIST_L2,5)
ret, sure_fg = cv.threshold(dist_transform,0.7*dist_transform.max(),255,0)
# Finding unknown region
sure_fg = np.uint8(sure_fg)
unknown = cv.subtract(sure_bg,sure_fg)

plt.subplot(2, 3, 1)
plt.imshow(opening, cmap='gray')
plt.subplot(2, 3, 2)
plt.imshow(sure_bg, cmap='gray')
plt.subplot(2, 3, 3)
plt.imshow(dist_transform, cmap='gray')
plt.subplot(2, 3, 4)
plt.imshow(unknown, cmap='gray')
plt.subplot(2, 3, 5)
plt.imshow(sure_fg, cmap='gray')
plt.show()

# ========================

# Marker labelling
ret, markers = cv.connectedComponents(sure_fg)
# Add one to all labels so that sure background is not 0, but 1
markers = markers+1
# Now, mark the region of unknown with zero
markers[unknown==255] = 0

plt.imshow(markers, cmap='gray')
plt.show()