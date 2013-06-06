import cv2
import glob
p='E:/myRproj/trunk/face/leader'
#p='E:/myRproj/trunk/face/test'
cascade=cv2.CascadeClassifier("haarcascade_frontalface_alt.xml")
files=glob.glob(p+'/*.png')
for i in range(0,len(files)):
    image=cv2.imread(files[i])
    print image
    gray=cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)
    gray=cv2.equalizeHist(gray)
    #gray=cv2.resize(gray,(96,128))
    rects=cascade.detectMultiScale(image,scaleFactor=1.115,minNeighbors=8,minSize=(4,4))
    #print rects
    x=rects[0][0]
    y=rects[0][1]
    width=rects[0][2]
    height=rects[0][3]
    im2=image[y:(y+width),x:(x+height),:]
    im3=cv2.resize(im2,(48,48))
    #s=files[i].split("\\")[0]
    cv2.imwrite(str(i+10)+'.png',im3)
    #cv2.imwrite(s,im3)