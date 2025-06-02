import os
import pydicom
import imageio


def task3():
    testFolder = '00013'
    innerFolders = ['FLAIR', 'T1w', 'T1wCE', 'T2w']
    # All DCM files will be stored in this array
    dcmArray = []

    for innerFolder in innerFolders:
        path = f'./test/{testFolder}/{innerFolder}'
        # Sorting files to get the images in correct order
        files = sorted(os.listdir(path), key=lambda x: int(x.split('-')[1].split('.')[0]))
        dcmImagesArray = []

        # Reading DCM files with pydicom and adding them to arrays
        for file in files:
            dcm = pydicom.dcmread(f'{path}/{file}')
            dcmArray.append(dcm)
            dcmImagesArray.append(dcm.pixel_array)

        # Creating GIFs path
        gifPath = f'./generated/task3/{testFolder}/{innerFolder}.gif'

        # check if the path exists
        if not os.path.exists(os.path.dirname(gifPath)):
            os.makedirs(os.path.dirname(gifPath))

        # Saving GIFs
        imageio.mimsave(gifPath, dcmImagesArray, fps=20)

    # Print metadata of first image in Flair folder
    print(dcmArray[0])

# Regarding the metadata files contain infotmation patient, like pation ID, MR acquisition type, Slice Thickness,
# Spacing Between Slices, etc.
