import cv2
from skimage.color import deltaE_ciede2000, rgb2lab
import os

# imgPath = 'assets/img1.jpg'
# imgPath = 'assets/img2.jpg'
imgPath = 'assignmentPhotos/IMG_4111.jpg'
# imgPath = 'assignmentPhotos/IMG_4120.jpg'
# imgPath = 'assignmentPhotos/IMG_4188.jpg'
# imgPath = 'assignmentPhotos/IMG_4191.jpg'
imgName = (os.path.splitext(os.path.basename(imgPath))[0])


def rgbToGrayScale():
    imgRGB = cv2.imread(imgPath)  # reads as RGB
    imgBGR = cv2.cvtColor(imgRGB, cv2.COLOR_RGB2BGR)  # To be able to plot it, need to convert it to BGR
    toGrayScaleMatrixBGR = [0.114, 0.587, 0.299]
    grayScaleImage = imgBGR @ toGrayScaleMatrixBGR  # Matrix multiplication to convert image into grayscale
    generatedFileName = f'generatedAssets/{imgName}-rgbToGrayScale.jpg'
    cv2.imwrite(generatedFileName, grayScaleImage)
    print('Generated GrayScale File is saved at:')
    print(generatedFileName)
    print('\n')
    return grayScaleImage


def dicretizeIMG(grayScaleImg):
    maxVal = 255
    colorNumbers = 5  # After a couple of experiments from test images, I think 5 colors describes image the best
    discretedIMG = grayScaleImg // (maxVal // colorNumbers) * (maxVal // colorNumbers)
    generatedFileName = f'generatedAssets/{imgName}-discretized.jpg'
    cv2.imwrite(generatedFileName, discretedIMG)
    print('Generated discretized image into five colors file is saved at:')
    print(generatedFileName)
    print('\n')
    return discretedIMG


def grayScaleToBlackAndWhite(grayScaleImg):
    intGrayScaleImg = grayScaleImg.astype('uint8')  # convert to integer array
    blackAndWhiteImg = cv2.adaptiveThreshold(intGrayScaleImg, 255, cv2.ADAPTIVE_THRESH_GAUSSIAN_C, cv2.THRESH_BINARY,
                                             15, 2)  # adaptive threshold for the best results
    generatedFileName = f'generatedAssets/{imgName}-blackAndWhite.jpg'

    cv2.imwrite(generatedFileName, blackAndWhiteImg)
    print('Generated black and white image file is saved at:')
    print(generatedFileName)
    print('\n')
    return blackAndWhiteImg


def changeHueSaturationLight(changeBy=20):
    imgRGB = cv2.imread(imgPath)  # reads as RGB
    imgHLS = cv2.cvtColor(imgRGB, cv2.COLOR_RGB2HLS)
    imgHLS[:, :, 0:2] += changeBy
    imgHLS[:, :, 0] = imgHLS[:, :, 0] % 360  # Hue range is from 0 to 360
    generatedFileName = f"generatedAssets/{imgName}-HLSChangedImageChangedBy{changeBy}.jpg"
    cv2.imwrite(generatedFileName, imgHLS)
    print('Generated black and white image file is saved at:')
    print(generatedFileName)
    print('\n')
    return imgHLS


def colorCloseness():
    imgRGB = cv2.imread(imgPath)
    # As the photos that are taken by smartphone are way too big, we need to resize it to a
    # smaller resolution preserving aspect ratio
    newResolution = (480, int(imgRGB.shape[0] // (imgRGB.shape[1] / 480)))
    imgRGB = cv2.resize(imgRGB, newResolution)
    imgLAB = rgb2lab(imgRGB)
    height, width, _ = imgLAB.shape
    threshold = 12
    closenessIndicatorColor = [255, 0, 0]

    def clickHandler(event, clickedX, clickedY, _, __):
        if event != cv2.EVENT_LBUTTONDOWN:
            return

        print(f"Started looking for close colors for the pixel - [{clickedX}, {clickedY}]")
        print("Please be patient, it can take up to couple of minutes")

        targetPixel = imgLAB[clickedY, clickedX]
        closeColorCoordinates = []
        for y in range(0, height):
            for x in range(0, width):
                de = deltaE_ciede2000(targetPixel, imgLAB[y, x])
                if de <= threshold:
                    closeColorCoordinates.append((y, x))

        for y, x in closeColorCoordinates:
            imgRGB[y, x] = closenessIndicatorColor

        generatedFileName = f"generatedAssets/{imgName}-colorCloseness[{clickedX}, {clickedY}].jpg"
        cv2.imwrite(generatedFileName, imgRGB)
        print('Generated color closeness file is saved at:')
        print(generatedFileName)
        print('\n')
        exit(0)

    cv2.imshow('Untouched Image', imgRGB)
    cv2.setMouseCallback('Untouched Image', clickHandler)  # setting mouse handler for the image
    cv2.waitKey(0)  # waiting for any key press
    cv2.destroyAllWindows()  # closing all windows


# ====================================================================

#  Task 3
grayScaleIMG = rgbToGrayScale()

#  Task 4
discretedIMG = dicretizeIMG(grayScaleIMG)

#  Task 5
blackAndWhiteIMG = grayScaleToBlackAndWhite(grayScaleIMG)

#  Task 6 with different values
changeHueSaturationLight(5)
changeHueSaturationLight(10)
hueSaturationLightChangedIMG = changeHueSaturationLight()
changeHueSaturationLight(30)

#  Task 7 Color closeness
colorCloseness()
