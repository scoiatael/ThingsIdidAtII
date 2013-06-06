import pygame, sys
width=672
height=480
kx=32
ky=48
ekran = pygame.display.set_mode((width,height))
names = {}
przybornikRectT={}
offset = 640
for x in range(int((width-offset)/kx)):
    for y in range(int(height/ky)):
        a,b,c,d=offset+x*kx,y*ky,kx,ky
        names[(a,b,c,d)]="kaf{0}{1}.bmp".format(x+1,y+1)
        przybornikRectT[(a,b,c,d)]=pygame.image.load(names[(a,b,c,d)])
a,b,c,d=0,0,kx,ky
BKaf=(a,b,c,d)
names[BKaf]="kaf0.bmp"
przybornikRectT[BKaf]=pygame.image.load(names[BKaf])

tabRectT = {}
zapiszT = {}
for x in range(int(offset/kx)):
    for y in range(int(height/ky)):
        a,b,c,d = x*kx,y*ky,kx,ky
        tabRectT[(a,b,c,d)]=pygame.image.load("kaf0.bmp")
        zapiszT[(a,b,c,d)]="kaf0.bmp"
moving=0
movingN=""
dx=0
dy=0
xo=0
y0=0


def zapisz(tabRectT):
    hashT={"kaf11.bmp":"p 1","kaf12.bmp":"p 2","kaf13.bmp":"p 3","kaf14.bmp":"t","kaf15.bmp":"pup",}
    zapis = open("poziom.txt", "w")
    for key in tabRectT:
        a,b,c,d = key
        if tabRectT[key]!= "kaf0.bmp":
            zapis.write("{0} {1} {2}\n".format(hashT[tabRectT[key]],a*2,b))

def inRect(A, tabRectT, przybornikRectT):
    x,y=A
    for key in tabRectT:
        a,b,c,d=key
        if x>=a and x<a+c and y>=b and y<b+d:
            return key
    for key in przybornikRectT:
        a,b,c,d=key
        if x>=a and x<a+c and y>=b and y<b+d:
            return key
    return (0,0,0,0)


itx=offset
ity=0
big=1
old=0
tabRectTSave=tabRectT
zapiszTSave=zapiszT

while 1:
    zapisz(zapiszT)
    for event in pygame.event.get():
        if event.type==pygame.QUIT:
            sys.exit()
        if event.type==pygame.MOUSEBUTTONDOWN and event.button==1:
            x,y=event.pos
            temp1=inRect((x,y), tabRectT, przybornikRectT)
            if moving==0 and temp1 in przybornikRectT:
                moving = przybornikRectT[temp1]
                movingN = names[temp1]
                a,b,c,d = temp1
                itx=a
                ity=b
                xo=x-a
                yo=y-b
            else:
                if temp1 in tabRectT and moving!=0:
                    a,b,c,d=temp1
                    tabRectTSave=dict(tabRectT)
                    zapiszTSave=dict(zapiszT)
                    for x in range(big):
                        for y in range(big):
                                if (a+x*kx,b+y*ky,c,d) in tabRectT:
                                    tabRectT[(a+x*kx,b+y*ky,c,d)]=moving
                                    zapiszT[(a+x*kx,b+y*ky,c,d)]=movingN
                    big=1
                if temp1 in przybornikRectT:
                    moving=0    
        if event.type==pygame.MOUSEMOTION:
            dx,dy=event.pos
            if dx<0:
                dx=0
            if dx>width:
                dx=width
            if dy<0:
                dy=0
            if dy>height:
                dy=height
        if event.type==pygame.MOUSEBUTTONDOWN and event.button==3:
            x,y=event.pos
            a,b,c,d=inRect((x,y), tabRectT, przybornikRectT)
            if moving!=0:
                moving=0
                big=1
            elif (a,b,c,d) in tabRectT:
                tabRectTSave=dict(tabRectT)
                zapiszTSave=dict(zapiszT)
                tabRectT[(a,b,c,d)]=przybornikRectT[BKaf]
                zapiszT[(a,b,c,d)]=names[BKaf]
                big=1
        if event.type==pygame.KEYDOWN:
            print(event.key)
            if event.key==275:
                if moving!=0:
                    itx+=kx
                    if itx==width:
                        itx=width-kx
                    moving=przybornikRectT[(itx,ity,kx,ky)]
                    movingN=names[(itx,ity,kx,ky)]
                    
            if event.key==276:
                if moving!=0:
                    itx-=kx
                    if itx<offset:
                        itx=offset
                    moving=przybornikRectT[(itx,ity,kx,ky)]
                    movingN=names[(itx,ity,kx,ky)]
            if event.key==274:
                if moving!=0:
                    ity+=ky
                    if ity==height:
                        ity=height-ky
                    moving=przybornikRectT[(itx,ity,kx,ky)]
                    movingN=names[(itx,ity,kx,ky)]
                    
            if event.key==273:
                if moving!=0:
                    ity-=ky
                    if ity<0:
                        ity=0
                    moving=przybornikRectT[(itx,ity,kx,ky)]
                    movingN=names[(itx,ity,kx,ky)]
            if event.key==32:
                obrazek=pygame.Surface((offset/kx, height/ky))
                for x in range(int(offset/kx)):
                    for y in range(int(height/ky)):
                        obrazek.set_at((x,y), tabRectT[(x*kx, y*ky, kx, ky)].get_at((1,1)))
                pygame.image.save(obrazek, "efekt.bmp")
                #ekran.blit(obrazek, (int(width/2),int(height/2)))
                #print(1)
                #pygame.time.wait(3000)
            if event.key==304:
                big=3
            if event.key==107:
                if moving!=0:
                    tabRectTSave=dict(tabRectT)
                    zapiszTSave=dict(zapiszT)
                    x,y = pygame.mouse.get_pos()
                    temp1=a,b,c,d = inRect((x,y),tabRectT,przybornikRectT)
                    if temp1 in tabRectT:
                        kolejka=[temp1]
                        old=zapiszT[temp1]
                        if old!=movingN:
                            while len(kolejka)>0:
                                a,b,c,d=key=kolejka[0]
                                kolejka=kolejka[1:]
                                if key in tabRectT:
                                    if zapiszT[key]==old:
                                        kolejka=kolejka+[(a-kx,b,c,d),(a+kx,b,c,d),(a,b-ky,c,d),(a,b+ky,c,d)]
                                        tabRectT[key]=moving
                                        zapiszT[key]=movingN
            if event.key==122:
                RT1=dict(tabRectT)
                ZT1=dict(zapiszT)
                tabRectT=dict(tabRectTSave)
                zapiszT=dict(zapiszTSave)
                tabRectTSave=RT1        
                zapiszTSave=ZT1        
                                
                            
                
    for x in range(int(offset/kx)):
        for y in range(int(height/ky)):
            ekran.blit(tabRectT[(x*kx,y*ky,kx,ky)], (x*kx, y*ky))
    for x in range(int((width-offset)/kx)):
        for y in range(int(height/ky)):
            ekran.blit(przybornikRectT[(offset+x*kx,y*ky,kx,ky)], (offset+x*kx, y*ky))
    for x in range(int(width/kx)):
        pygame.draw.line(ekran,(0,0,0), ((x+1)*kx,0), ((x+1)*kx,height), 4)
    for y in range(int(height/ky)):
        pygame.draw.line(ekran,(0,0,0), (0,(y+1)*ky), (width,(y+1)*ky), 4)
    pygame.draw.line(ekran,(255,255,255), (offset,0), (offset,height), 4)
    if moving!=0:
        a,b,c,d = inRect((dx,dy), tabRectT, przybornikRectT)
        if (a,b,c,d) in tabRectT:
            pygame.draw.line(ekran, (100,0,0), (a,b), (a+c*big,b+d*big), 2)
            pygame.draw.line(ekran, (100,0,0), (a+c*big,b), (a,b+d*big), 2) 
        ekran.blit(moving, (dx-xo,dy-yo))
    pygame.display.flip()