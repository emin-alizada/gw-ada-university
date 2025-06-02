import cv2
import matplotlib.pyplot as plt
import os


def task3():
    imgAngle1 = cv2.imread('assets/angle3.jpg')
    imgAngle2 = cv2.imread('assets/angle4.jpg')
    imgAngle1 = cv2.cvtColor(imgAngle1, cv2.COLOR_RGB2BGR)
    imgAngle2 = cv2.cvtColor(imgAngle2, cv2.COLOR_RGB2BGR)

    orb = cv2.ORB_create()
    sift = cv2.SIFT_create()

    keyPoints1, descriptors1 = orb.detectAndCompute(imgAngle1, None)
    keyPoints2, descriptors2 = orb.detectAndCompute(imgAngle2, None)
    siftKeyPoints1, siftDescriptors1 = sift.detectAndCompute(imgAngle1, None)
    siftKeyPoints2, siftDescriptors2 = sift.detectAndCompute(imgAngle2, None)

    imgAngle1KeyPoints = cv2.drawKeypoints(imgAngle1, keyPoints1, None, color=(255, 0, 0), flags=cv2.DRAW_MATCHES_FLAGS_DRAW_RICH_KEYPOINTS)
    imgAngle2KeyPoints = cv2.drawKeypoints(imgAngle2, keyPoints2, None, color=(255, 0, 0), flags=cv2.DRAW_MATCHES_FLAGS_DRAW_RICH_KEYPOINTS)
    imgAngle1SiftKeyPoints = cv2.drawKeypoints(imgAngle1, siftKeyPoints1, None, color=(255, 0, 0), flags=cv2.DRAW_MATCHES_FLAGS_DRAW_RICH_KEYPOINTS)
    imgAngle2SiftKeyPoints = cv2.drawKeypoints(imgAngle2, siftKeyPoints2, None, color=(255, 0, 0), flags=cv2.DRAW_MATCHES_FLAGS_DRAW_RICH_KEYPOINTS)

    bf = cv2.BFMatcher(cv2.NORM_HAMMING, crossCheck=True)
    matches = bf.match(descriptors1, descriptors2)
    topMatches = sorted(matches, key=lambda x: x.distance)[:40]

    bfSift = cv2.BFMatcher(cv2.NORM_L2, crossCheck=True)
    siftMatches = bfSift.match(siftDescriptors1, siftDescriptors2)
    topSiftMatches = sorted(siftMatches, key=lambda x: x.distance)[:40]

    imgMatches = cv2.drawMatches(imgAngle1, keyPoints1, imgAngle2, keyPoints2, topMatches, None, flags=cv2.DrawMatchesFlags_DEFAULT, matchesThickness=10)
    imgSiftMatches = cv2.drawMatches(imgAngle1, siftKeyPoints1, imgAngle2, siftKeyPoints2, topSiftMatches, None, flags=cv2.DrawMatchesFlags_DEFAULT, matchesThickness=10)

    path = f'./output/task3/'
    if not os.path.exists(os.path.dirname(f'{path}/')):
        os.makedirs(os.path.dirname(f'{path}/'))

    plt.subplot(4, 2, 1)
    plt.imshow(imgAngle1)
    plt.title('Angle 1')
    plt.xticks([])
    plt.yticks([])

    plt.subplot(4, 2, 2)
    plt.imshow(imgAngle2)
    plt.title('Angle 2')
    plt.xticks([])
    plt.yticks([])

    plt.subplot(4, 2, 3)
    plt.imshow(imgAngle1KeyPoints)
    plt.title('Angle 1 Key Points')
    plt.xticks([])
    plt.yticks([])
    imgAngle1KeyPoints = cv2.cvtColor(imgAngle1KeyPoints, cv2.COLOR_BGR2RGB)
    cv2.imwrite(f'{path}/angle1ORBKeyPoints.jpg', imgAngle1KeyPoints)

    plt.subplot(4, 2, 4)
    plt.imshow(imgAngle2KeyPoints)
    plt.title('Angle 2 Key Points')
    plt.xticks([])
    plt.yticks([])
    imgAngle2KeyPoints = cv2.cvtColor(imgAngle2KeyPoints, cv2.COLOR_BGR2RGB)
    cv2.imwrite(f'{path}/angle2ORBKeyPoints.jpg', imgAngle2KeyPoints)

    plt.subplot(4, 2, 5)
    plt.imshow(imgAngle1SiftKeyPoints)
    plt.title('Angle 1 SIFT Key Points')
    plt.xticks([])
    plt.yticks([])
    imgAngle1SiftKeyPoints = cv2.cvtColor(imgAngle1SiftKeyPoints, cv2.COLOR_BGR2RGB)
    cv2.imwrite(f'{path}/angle1SIFTKeyPoints.jpg', imgAngle1SiftKeyPoints)

    plt.subplot(4, 2, 6)
    plt.imshow(imgAngle2SiftKeyPoints)
    plt.title('Angle 2 SIFT Key Points')
    plt.xticks([])
    plt.yticks([])
    imgAngle2SiftKeyPoints = cv2.cvtColor(imgAngle2SiftKeyPoints, cv2.COLOR_BGR2RGB)
    cv2.imwrite(f'{path}/angle2SIFTKeyPoints.jpg', imgAngle2SiftKeyPoints)

    plt.subplot(4, 2, 7)
    plt.imshow(imgMatches)
    plt.title('Matches')
    plt.xticks([])
    plt.yticks([])
    imgMatches = cv2.cvtColor(imgMatches, cv2.COLOR_BGR2RGB)
    cv2.imwrite(f'{path}/matchesORB.jpg', imgMatches)

    plt.subplot(4, 2, 8)
    plt.imshow(imgSiftMatches)
    plt.title('SIFT Matches')
    plt.xticks([])
    plt.yticks([])
    imgSiftMatches = cv2.cvtColor(imgSiftMatches, cv2.COLOR_BGR2RGB)
    cv2.imwrite(f'{path}/matchesSIFT.jpg', imgSiftMatches)

    plt.subplots_adjust(
        left=0.125,
        right=0.9,
        bottom=0.1,
        top=0.9,
        wspace=1,
        hspace=0.7,
    )

    plt.savefig(f'{path}/matchesPlot.jpg', dpi=1800)
    plt.show()
