import cv2
import pytesseract
import re

idImgPath = './assets/id.jpeg'
idImg = cv2.imread(idImgPath)
idImgClone = idImgCropped = idImg.copy()

mouseCoordinates = []


def mouseCallback(event, x, y, _, __):
    global idImgCropped

    if event == cv2.EVENT_LBUTTONDOWN:
        mouseCoordinates.append((x, y))

    elif event == cv2.EVENT_LBUTTONUP:
        mouseCoordinates.append((x, y))
        cv2.rectangle(idImg, mouseCoordinates[0], mouseCoordinates[1], (0, 0, 255), 3)
        cv2.imshow('id', idImg)
        idImgCropped = idImgClone[mouseCoordinates[0][1]:mouseCoordinates[1][1], mouseCoordinates[0][0]:mouseCoordinates[1][0]]
        cv2.imshow('idCropped', idImgCropped)
        mouseCoordinates.clear()


cv2.imshow('id', idImg)
cv2.setMouseCallback('id', mouseCallback)
cv2.waitKey(0)
cv2.destroyAllWindows()


def detectIdCardNumber(image):
    # Perform OCR using Tesseract
    text = pytesseract.image_to_string(image)
    print("Detected text on cropped image:")
    print(text)
    print('\n')

    pattern = r"ID:\s*\w{9}"
    matches = re.findall(pattern, text)

    if len(matches) > 0:
        id_card_number = matches[0].split(":")[1].strip()
        print("Detected ID card number:", id_card_number)
    else:
        print("Could not detect ID card number")


def preProcessImage(image):
    # Convert the image to grayscale
    image = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)

    # Apply image preprocessing (if required) like thresholding, noise removal, etc.
    detectIdCardNumber(image)


preProcessImage(idImgCropped)

