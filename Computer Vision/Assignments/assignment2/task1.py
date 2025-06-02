import cv2 as cv
import matplotlib.pyplot as plt
import numpy as np


def task1():
    img = cv.imread('./assets/noisy/chemical/inchi1.png', cv.IMREAD_GRAYSCALE)
    img = cv.adaptiveThreshold(img, 255, cv.ADAPTIVE_THRESH_GAUSSIAN_C, cv.THRESH_BINARY, 15, 2)

    usingMorphology(img)
    usingSpatialFilters(img)
    usingFrequencyFilters(img)

    # Gets rid of salt and pepper noise the best, but at the same time letters are no longer readable
    gettingRidOfSaltAndPepper(img)

    # In my opinion, this combination of filters gets rid of salt and pepper noise the best
    # and at the same time letters are still readable
    bestPerformant(img)

    addNoiseAndRestore()


def usingMorphology(img):
    kernel = np.ones((3, 3), np.uint8)
    changedImage = cv.erode(img, kernel, iterations=1)

    kernel = np.ones((2, 2), np.uint8)
    changedImage = cv.dilate(changedImage, kernel, iterations=2)

    description = 'One iteration of erosion and two iterations of dilation.' \
                  'Although we could not get rid of pepper noise, ' \
                  'lines and letters are reconstructed better'
    plotAndSaveImage(img, changedImage, 'Morphology applied', 'usingMorphology', description)
    return changedImage


def usingSpatialFilters(img):
    usingMeanFilter(img)
    usingMedianFilter(img)
    usingGaussianFilter(img)
    usingBilateralFilter(img)


def usingFrequencyFilters(img):
    # High pass filter
    genericFrequencyFilter(img, createHighPassFilter, 'High pass filter applied',
                           'usingHighPassFilter')

    # Low pass filter
    genericFrequencyFilter(img, createLowPassFilter, 'Low pass filter applied',
                           'usingLowPassFilter')

    # Band pass filter
    genericFrequencyFilter(img, createBandPassFilter, 'Band pass filter applied',
                           'usingBandPassFilter')


def usingMeanFilter(img):
    changedImage = cv.blur(img, (5, 5))

    plotAndSaveImage(img, changedImage, 'Mean filter applied', 'usingMeanFilter')

    return changedImage


def usingMedianFilter(img):
    # Higher values of ksize result in an almost white image
    changedImage = cv.medianBlur(img, 3)

    plotAndSaveImage(img, changedImage, 'Median filter applied', 'usingMedianFilter')

    return changedImage


def usingGaussianFilter(img):
    changedImage = cv.GaussianBlur(img, (7, 7), 0)

    plotAndSaveImage(img, changedImage, 'Gaussian filter applied', 'usingGaussianFilter')

    return changedImage


def usingBilateralFilter(img):
    changedImage = cv.bilateralFilter(img, 10, 100, 75)

    plotAndSaveImage(img, changedImage, 'Bilateral filter applied', 'usingBilateralFilter')

    return changedImage


def createHighPassFilter(img, fshift, high_cutoff=30, factor=2, **kwargs):
    rows, cols = img.shape
    crow, ccol = rows // 2, cols // 2

    # Create a high-pass filter with a default cutoff frequency of 30
    d = high_cutoff
    n = factor
    highpass = 1 / (1 + (d / np.sqrt((cols - ccol) ** 2 + (rows - crow) ** 2)) ** (2 * n))
    # Apply the filter to the Fourier spectrum
    filtered_spectrum = fshift * highpass
    return filtered_spectrum


def createLowPassFilter(img, fshift, low_cutoff=30, factor=2, **kwargs):
    rows, cols = img.shape
    crow, ccol = rows // 2, cols // 2

    # Create a low-pass filter with a default cutoff frequency of 30
    d = low_cutoff
    n = factor
    lowpass = 1 / (1 + ((np.sqrt((cols - ccol) ** 2 + (rows - crow) ** 2)) / d) ** (2 * n))
    # Apply the filter to the Fourier spectrum
    filtered_spectrum = fshift * lowpass
    return filtered_spectrum


def createBandPassFilter(img, fshift, low_cutoff=10, high_cutoff=80, factor=2):
    rows, cols = img.shape
    crow, ccol = rows // 2, cols // 2

    # Create a bandpass filter with a default lower cutoff frequency of 10 and default upper cutoff frequency of 80
    n = factor
    bandpass = 1 / (1 + (((cols - ccol) ** 2 + (rows - crow) ** 2) / high_cutoff ** 2) ** n) - 1 / (
            1 + (((cols - ccol) ** 2 + (rows - crow) ** 2) / low_cutoff ** 2) ** n)
    # Apply the filter to the Fourier spectrum
    filtered_spectrum = fshift * bandpass
    return filtered_spectrum


def genericFrequencyFilter(img, createFilterFunction, imageTitle, imageFileName, **kwargs):
    f = np.fft.fft2(img)
    fshift = np.fft.fftshift(f)

    filtered_spectrum = createFilterFunction(img, fshift, **kwargs)
    filtered_spectrum_img = np.log(1 + np.abs(filtered_spectrum))

    filtered_shift = np.fft.ifftshift(filtered_spectrum)
    filtered_img = np.fft.ifft2(filtered_shift)
    filtered_img = np.abs(filtered_img)

    plotAndSaveImageFrequencyFilter(img, filtered_spectrum_img, filtered_img, imageTitle,
                                    imageFileName)

    return filtered_img


def gettingRidOfSaltAndPepper(img):
    # Morphology
    kernel = np.ones((3, 3), np.uint8)
    erodeImgae = cv.erode(img, kernel, iterations=1)

    # Median filter
    medianBlur = cv.medianBlur(erodeImgae, 5)

    # Morphology
    diluteKernel = np.ones((2, 2), np.uint8)
    diluteImage = cv.dilate(medianBlur, diluteKernel, iterations=2)

    plt.subplot(1, 4, 1)
    plt.imshow(img, cmap='gray')
    plt.title('Original image')

    plt.subplot(1, 4, 2)
    plt.imshow(erodeImgae, cmap='gray')
    plt.title('Eroded image \n with kernel 3x3')

    plt.subplot(1, 4, 3)
    plt.imshow(medianBlur, cmap='gray')
    plt.title('Median blur \n with kernel 5x5')

    plt.subplot(1, 4, 4)
    plt.imshow(diluteImage, cmap='gray')
    plt.title('Diluted image \n with kernel 2x2 \n twice')

    cv.imwrite(f'generated/task1/gettingRidOfSaltAndPepper.png', diluteImage)

    plt.subplots_adjust(
        left=0.125,
        right=0.9,
        bottom=0.1,
        top=0.9,
        wspace=1,
        hspace=0.7,
    )

    plt.savefig(f'./generated/task1/gettingRidOfSaltAndPepperPlot.png', dpi=300)
    plt.show()


def bestPerformant(img):
    bilateralFilterImg = cv.bilateralFilter(img, 10, 100, 75)

    kernel = np.ones((3, 3), np.uint8)
    erodeImage = cv.erode(bilateralFilterImg, kernel, iterations=1)

    diluteKernel = np.ones((2, 2), np.uint8)
    diluteImage = cv.dilate(erodeImage, diluteKernel, iterations=2)

    plt.subplot(1, 4, 1)
    plt.imshow(img, cmap='gray')
    plt.title('Original image')

    plt.subplot(1, 4, 2)
    plt.imshow(bilateralFilterImg, cmap='gray')
    plt.title('Bilateral filter \n applied')

    plt.subplot(1, 4, 3)
    plt.imshow(erodeImage, cmap='gray')
    plt.title('Eroded image \n with kernel \n 3x3')

    plt.subplot(1, 4, 4)
    plt.imshow(diluteImage, cmap='gray')
    plt.title('Diluted image \n with kernel \n 2x2 twice')

    cv.imwrite(f'generated/task1/bestPerformant.png', diluteImage)

    plt.subplots_adjust(
        left=0.125,
        right=0.9,
        bottom=0.1,
        top=0.9,
        wspace=1,
        hspace=0.7,
    )

    plt.savefig(f'./generated/task1/bestPerformantPlot.png', dpi=300)
    plt.show()


def addNoiseAndRestore():
    img = cv.imread('assets/IMG_4111.jpg')
    img = cv.cvtColor(img, cv.COLOR_RGB2BGR)

    # Add Gaussian noise
    mean = 0
    sigma = 15
    noise = (np.random.normal(mean, sigma, img.shape)).astype('uint8')
    noisy_img = img + noise

    restoreImageBy9 = cv.medianBlur(noisy_img, 9)
    restoreImageBy15 = cv.medianBlur(noisy_img, 15)
    restoreImageBy25 = cv.medianBlur(noisy_img, 25)

    plt.subplot(2, 3, 1)
    plt.imshow(img)
    plt.title('Original image')

    plt.subplot(2, 3, 2)
    plt.imshow(noise)
    plt.title('Noise')

    plt.subplot(2, 3, 3)
    plt.imshow(noisy_img)
    plt.title('Noisy image')

    plt.subplot(2, 3, 4)
    plt.imshow(restoreImageBy9)
    plt.title('Restored image with \n Median Blur \n by 9x9 kernel', wrap=True)

    plt.subplot(2, 3, 5)
    plt.title('Restored image with \n Median Blur \n by 15x15 kernel', wrap=True)
    plt.imshow(restoreImageBy15)

    plt.subplot(2, 3, 6)
    plt.imshow(restoreImageBy25)
    plt.title('Restored image with \n Median Blur \n by 25x25 kernel', wrap=True)

    noisy_img = cv.cvtColor(noisy_img, cv.COLOR_BGR2RGB)
    noise = cv.cvtColor(noise, cv.COLOR_BGR2RGB)
    cv.imwrite(f'generated/task1/noisyImage.jpg', noisy_img)
    cv.imwrite(f'generated/task1/noise.jpg', noise)

    restoreImageBy9 = cv.cvtColor(restoreImageBy9, cv.COLOR_BGR2RGB)
    restoreImageBy15 = cv.cvtColor(restoreImageBy15, cv.COLOR_BGR2RGB)
    restoreImageBy25 = cv.cvtColor(restoreImageBy25, cv.COLOR_BGR2RGB)

    cv.imwrite(f'generated/task1/restoreImageBy9.jpg', restoreImageBy9)
    cv.imwrite(f'generated/task1/restoreImageBy15.jpg', restoreImageBy15)
    cv.imwrite(f'generated/task1/restoreImageBy25.jpg', restoreImageBy25)

    plt.subplots_adjust(
        left=0.125,
        right=0.9,
        bottom=0.1,
        top=0.9,
        wspace=1,
        hspace=0.7,
    )
    plt.savefig(f'./generated/task1/noisyImagePlot.png', dpi=300)
    plt.show()


def plotAndSaveImage(originalImg, changedImage, title, fileName, description=None):
    # Plotting
    plt.subplot(1, 2, 1)
    plt.imshow(originalImg, cmap='gray')
    plt.title('Original image')

    plt.subplot(1, 2, 2)
    plt.imshow(changedImage, cmap='gray')
    plt.title(title)

    if description:
        plt.figtext(0.5, 0.01, description, wrap=True, horizontalalignment='center', fontsize=12)

    cv.imwrite(f'generated/task1/{fileName}.png', changedImage)

    plt.subplots_adjust(
        left=0.125,
        right=0.9,
        bottom=0.1,
        top=0.9,
        wspace=1,
        hspace=0.7,
    )

    plt.savefig(f'./generated/task1/{fileName}Plot.png', dpi=300)
    plt.show()


def plotAndSaveImageFrequencyFilter(originalImg, filterImage, changedImage, title, fileName):
    # Plotting
    plt.subplot(1, 3, 1)
    plt.imshow(originalImg, cmap='gray')
    plt.title('Original image')

    plt.subplot(1, 3, 2)
    plt.imshow(filterImage, cmap='gray')
    plt.title('Filter Visualization')

    plt.subplot(1, 3, 3)
    plt.imshow(changedImage, cmap='gray')
    plt.title(title)

    plt.subplots_adjust(
        left=0.125,
        right=0.9,
        bottom=0.1,
        top=0.9,
        wspace=1,
        hspace=0.7,
    )

    plt.savefig(f'./generated/task1/{fileName}Plot.png', dpi=300)
    plt.show()
