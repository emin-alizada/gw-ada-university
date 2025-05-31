import cv2
import matplotlib.pyplot as plt
import numpy

imgpath = "img1.jpg"
img = cv2.cvtColor(cv2.imread(imgpath), cv2.COLOR_RGB2BGR)

# print(img[0])

readyMatrix = [0.114, 0.587, 0.299]

newImage = img @ readyMatrix

plt.subplot(2, 3, 1)
plt.imshow(img, cmap='gray')
plt.title('original image')

plt.subplot(2, 3, 2)
plt.imshow(newImage, cmap='gray')
plt.title('with formula')

newImageTwo = numpy.mean(img, 2)
# print(newImageTwo.shape)

plt.subplot(2, 3, 3)
plt.imshow(newImageTwo, cmap='gray')
plt.title('with average')


imgHSLWithChanges = cv2.cvtColor(img, cv2.COLOR_BGR2HLS)
imgHSLWithChanges[:, :, 0:2] += 15
imgHSLWithChanges = cv2.cvtColor(imgHSLWithChanges, cv2.COLOR_HLS2BGR)

plt.subplot(2, 3, 4)
plt.imshow(imgHSLWithChanges)
plt.title('changed hue and light by 15 for every pixel')


imgHSL = cv2.cvtColor(img, cv2.COLOR_BGR2HLS)
imgHSL = cv2.cvtColor(imgHSL, cv2.COLOR_HLS2BGR)

plt.subplot(2, 3, 5)
plt.imshow(imgHSL)
plt.title('convert to hsl and back')

difference = img - imgHSL
print(difference)

plt.show()