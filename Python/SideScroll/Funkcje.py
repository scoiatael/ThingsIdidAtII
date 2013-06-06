from Sprite import przeciwnik,statek,teren,powerup
from pygame.locals import QUIT,K_SPACE,K_UP,K_DOWN,K_LEFT,K_RIGHT,K_LCTRL, K_z
from sys import exit
import pygame

def wczytaj(nrPoz,wszystko,wrog_images,moving):
    poziom=[[] for i in range(1280)]
    f = open("gra1/Images/Poziom{0}/poziom.txt".format(nrPoz),"r")
    for Linia in f.readlines():
        Linia = Linia.split(' ')
        if Linia[0]=='p':
            pos = (620,int(Linia[3]))
            images=[]
            for x in wrog_images[8*(int(Linia[1])-1):8*(int(Linia[1]))]:
                images.append(x)
            poziom[int(Linia[2])].append(przeciwnik(pos,images,wszystko,moving,5*int(Linia[1]), int(Linia[1])-1, (-10,0,pygame.image.load("gra1/Images/pocisk/prz1.png").convert_alpha(),1)))
        elif Linia[0]=='t':
            pos = (int(Linia[1]),int(Linia[2]))
            images = pygame.image.load("gra1/Images/teren.png")
            poziom[int(Linia[1])].append(teren(pos,images,moving))
        elif Linia[0]=='pup':
            pos = (int(Linia[1]),int(Linia[2]))
            images = pygame.image.load("gra1/Images/powerup.png")
            poziom[int(Linia[1])].append(powerup(pos,images,moving))
    return poziom;  

def game_over(screen,Str):
    cos = pygame.Surface((640,480))
    cos.fill((20,20,200))
    font1= pygame.font.SysFont("Arial", 32)
    napis = font1.render(Str, 0, (0,0,0))
    screen.blit(cos,(0,0))
    screen.blit(napis,(320-int(napis.get_width()/2),240-int(napis.get_height())))
    pygame.display.update()
    pygame.time.wait(3000)
        
    

def start_level(nrPoziomu,screen,statek_images,typy_pociskow,wrog_images):
    pygame.display.set_caption("Gra Scroll level nr{0}".format(nrPoziomu))
    font1=pygame.font.SysFont("arial", 16)
    tlo = pygame.image.load("gra1/Images/Poziom{0}/tlo.jpg".format(nrPoziomu))
    clock = pygame.time.Clock()
    wszystko = pygame.sprite.Group()
    boost=1
    moving=[1]
    Poziom = wczytaj(nrPoziomu,wszystko, wrog_images,moving)
    shoot=0
    shootR=0
    powerup=0
    powertime=0
    Pts=0
    statek_gr=statek((60,210),statek_images,wszystko,1000)
    statek_gr.type="gracza"
    kont =True
    max_offset=1280
    
    for i in range(640):
        for x in Poziom[i]:
            wszystko.add(x)
    game_offset=640
    
    while kont:
        clock.tick(30)
        game_offset+=moving[0]
        print(game_offset)
        if game_offset>=max_offset-1:
            game_offset=max_offset-2
            moving[0]=0
        if moving[0]==0:
            kont=False
        for x in wszystko:
            T=x.type.split(' ')
            if T[0]=="wrogi": 
                if T[1]=="przeciwnik":
                    if T[2]=="2":
                        kont=True
        for events in Poziom[game_offset]:
            wszystko.add(events)
        for x in pygame.sprite.spritecollide(statek_gr, wszystko,False,False):
            if x.type.split(' ')[0] == "wrogi" or x.type=="teren":
                statek_gr.dmg -= x.dmg
                x.trafiony(statek_gr,1,statek_gr.dmg)
                if statek_gr.dmg<=0:
                    game_over(screen,"Przegrales!")
                    kont=False
            elif x.type=="przyjazny":
                powertime+=x.dmg
                x.trafiony(1,1,1)
        for x in wszystko:
            for y in pygame.sprite.spritecollide(x,wszystko,False,False):
                if x!=y:
                    T1= x.type.split(' ')[0]=="gracza" and y.type.split(' ')[0]!="teren" 
                    T2= y.type.split(' ')[0]=="gracza" and x.type.split(' ')[0]!="teren"
                    if T1 or T2:
                        if not (T1 and T2):
                            Pts+=1
                    dmg1=x.dmg
                    dmg2=y.dmg
                    x.trafiony(y,1,dmg2)
                    y.trafiony(x,2,dmg1)
        for obj in wszystko:
            if obj.x<-obj.rect.width/2:
                obj.exists=False
            if obj.type.split(' ')[0]=="wrogi":
                if obj.y<obj.rect.height/2:
                    obj.move_b()
                    if obj.vy<0:
                        obj.vy=-obj.vy
                if obj.y>480-obj.rect.height/2:
                    obj.move_b()
                    if obj.vy>0:
                        obj.vy=-obj.vy
                if moving[0]==0 and obj.type.split(' ')[1]!="pocisk":
                    obj.vx=0
                if moving[0]==0 and obj.x > 640-obj.rect.width/2:
                    obj.x=600-obj.rect.width/2
                    #obj.trafiony(0,0,-1)
                    obj.vx=-2
                    obj.rect.centerx=obj.x
            if not obj.exists:
                wszystko.remove(obj)
        shoot+=1
        if shoot>5:
            shoot=5
        shootR+=1
        if shootR>10:
            shootR=10
        
        if powertime >0:
            powerup=1
        else:
            powerup=0
        
        for event in pygame.event.get():
            if event.type == QUIT:
                exit()
    
        Klawisze = pygame.key.get_pressed()
        speed=[0,0]
        if moving[0]!=0:
            if Klawisze[K_z]:
                boost=5
                moving[0]=5
            else: 
                boost=1
                moving[0]=1
        if Klawisze[K_UP]:
            speed[1]-=boost                    
        if Klawisze[K_DOWN]:
            speed[1]+=boost
        if Klawisze[K_LEFT]:
            speed[0]-=boost
        if Klawisze[K_RIGHT]:
            speed[0]+=boost
        if Klawisze[K_SPACE] and shoot==5:
            statek_gr.shoot(typy_pociskow[0+powerup],"gracza")
            shoot=0
            powertime-=1
        if Klawisze[K_LCTRL] and shootR==10:
            statek_gr.shoot(typy_pociskow[2],"gracza")
            shootR=0
        statek_gr.modspeed(speed)
        if statek_gr.x<int(statek_gr.rect.width/2) or  statek_gr.x>640-statek_gr.rect.width/2:
            temp=statek_gr.y
            statek_gr.move_b()
            statek_gr.y=temp
            statek_gr.vx=0
        if statek_gr.y<int(statek_gr.rect.height/2) or statek_gr.y>480-statek_gr.rect.height/2:
            temp=statek_gr.x
            statek_gr.move_b()
            statek_gr.x=temp
            statek_gr.vy=0
        screen.blit(tlo, (640-(game_offset%640),0))
        screen.blit(tlo, (-(game_offset%640),0))
        if powertime<=0:
            powertime=0    
        wszystko.update()
        wszystko.draw(screen)
        info = font1.render("Punkty:"+str(Pts)+" PoweredUp shots left: "+str(powertime),0,(0,0,0))
        screen.blit(info,(0,0))
        statek_gr.update()
        pygame.sprite.Group(statek_gr).draw(screen)
        pygame.display.update()
    if statek_gr.dmg>0:
        game_over(screen, "Level completed")
        return 1
    else:
        return 0