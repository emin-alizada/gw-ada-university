import math

import cv2
import matplotlib.pyplot as plt

imgpath = "img2.jpg"
img = cv2.imread(imgpath, 0)
print(len(img[0]))

plt.subplot(1, 3, 1)
plt.imshow(img, cmap='gray')
plt.title('image')

plt.subplot(1, 3, 2)
plt.hist(img.ravel(), 256, [0, 255])
plt.title('histogram')

maxval = 255

im_bin = (img > 128) * maxval

plt.subplot(1, 3, 3)
plt.imshow(im_bin, cmap='gray')
plt.title('image')

plt.show()

ret, th1 = cv2.threshold(img, 127, 255, cv2.THRESH_BINARY)
th2 = cv2.adaptiveThreshold(img, 255, cv2.ADAPTIVE_THRESH_MEAN_C, cv2.THRESH_BINARY, 11, 6)
th3 = cv2.adaptiveThreshold(img, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY, 11, 6)
titles = ['Original Image', 'Global Thresholding (v = 127)',
          'Adaptive Mean Thresholding', 'Adaptive Gaussian Thresholding']

images = [img, th1, th2, th3]

for i in range(4):
    plt.subplot(2, 2, i + 1), plt.imshow(images[i], 'gray')
    plt.title(titles[i])
    plt.xticks([]), plt.yticks([])
plt.show()

# ==================================
# Color discretization

# plt.subplot(1, 3, 1)
# plt.imshow(img, cmap='gray')
# plt.title('image')
#
# plt.subplot(1, 3, 2)
# plt.hist(img.ravel(), 256, [0, 255])
# plt.title('histogram')
#
# maxval = 255
# colorNumbers = 4
#
# im_bin = img % (maxval / colorNumbers) * colorNumbers
#
# plt.subplot(1, 3, 3)
# plt.imshow(im_bin, cmap='gray')
# plt.title('image')
#
# plt.show()

# ==================================
