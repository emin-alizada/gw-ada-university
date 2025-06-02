import cv2 as cv
import numpy as np
import matplotlib.pyplot as plt


def task2():
    imageArray = [
        '1.jpeg',
        '2.jpeg',
        '3.png',
        '4.png',
        '5.png',
        '6.jpeg'
    ]
    for imageName in imageArray:
        img = cv.imread(f'assets/noisy/speckle/{imageName}', cv.IMREAD_GRAYSCALE)
        fileName, fileExtension = imageName.split('.')

        # Using median blur with different kernel sizes
        usingMedianBlur(img, 5, fileName, fileExtension)
        usingMedianBlur(img, 7, fileName, fileExtension)
        usingMedianBlur(img, 9, fileName, fileExtension)

        # Using Crimmins speckle removal with different kernel sizes
        usingCrimminsSpeckleRemoval(img, 5, fileName, fileExtension)
        usingCrimminsSpeckleRemoval(img, 7, fileName, fileExtension)
        usingCrimminsSpeckleRemoval(img, 9, fileName, fileExtension)


def usingMedianBlur(img, kSize, fileName, fileExtension):
    changedImg = cv.medianBlur(img, kSize)

    plotAndSaveImage(img, changedImg, f'Using median blur \n with ksize = {kSize}',
                     f'{fileName}-medianBlur-kernel-{kSize}', fileExtension)


def usingCrimminsSpeckleRemoval(img, kSize, fileName, fileExtension):
    changedImg = img.copy()

    # Apply Crimmins speckle removal
    for i in range(kSize // 2, changedImg.shape[0] - kSize // 2):
        for j in range(kSize // 2, changedImg.shape[1] - kSize // 2):
            # Find the median value of the surrounding pixels
            neighbors = changedImg[
                            i - kSize // 2:i + kSize // 2 + 1,
                            j - kSize // 2:j + kSize // 2 + 1
                        ]
            median = np.median(neighbors)

            # Replace the pixel value if it is suspected to be noisy
            if changedImg[i, j] > median:
                changedImg[i, j] = median

    plotAndSaveImage(img, changedImg, f'Using Crimmins \n speckle removal \n with ksize = {kSize}',
                     f'{fileName}-crimminsSpeckleRemoval-kernel-{kSize}', fileExtension)


def plotAndSaveImage(originalImg, changedImage, title, fileName, fileExtension):
    plt.subplot(1, 2, 1)
    plt.imshow(originalImg, cmap='gray')
    plt.title('Original image')

    plt.subplot(1, 2, 2)
    plt.imshow(changedImage, cmap='gray')
    plt.title(title)

    cv.imwrite(f'./generated/task2/{fileName}.{fileExtension}', changedImage)

    plt.subplots_adjust(
        left=0.125,
        right=0.9,
        bottom=0.1,
        top=0.8,
        wspace=0.5,
        hspace=0.7,
    )

    plt.savefig(f'./generated/task2/{fileName}Plot.png', dpi=300)
    plt.show()
