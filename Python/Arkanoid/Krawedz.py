import pygame
from math import sqrt
from random import randint

class Brzeg(pygame.sprite.Sprite):
    def __init__(self, image, pos):
        pygame.sprite.Sprite.__init__(self)
        self.x,self.y = pos
        self.image = image
        self.rect = self.image.get_rect()
        self.rect.left = self.x
        self.rect.top = self.y
        self.exists=True
    def update(self):
        return
    
class BrzegGora(Brzeg):
    def __init__(self, nrPoziomu):
        Brzeg.__init__(self, pygame.image.load("Images/{0}/BrzegGora.png".format(nrPoziomu)), (0,0))
        

class BrzegLewy(Brzeg):
    def __init__(self, nrPoziomu):
        Brzeg.__init__(self, pygame.image.load("Images/{0}/BrzegLewy.png".format(nrPoziomu)), (0,0))
        
class BrzegPrawy(Brzeg):
    def __init__(self, nrPoziomu):
        Brzeg.__init__(self, pygame.image.load("Images/{0}/BrzegPrawy.png".format(nrPoziomu)), (620,0))
        
class klocek(pygame.sprite.Sprite):
    def __init__(self, typ, nrPoziomu, wiersz, kol):
        pygame.sprite.Sprite.__init__(self)
        nazwy = []
        self.images=[]
        nazwyN=[]
        self.typ=typ
        self.niszczony=False
        if typ!="nor" and typ!="inv" and typ!="big":
            self.exists=False
            return
        if typ=="nor":
            self.hp=3
            nazwy.extend({"Images/{0}/klocek_".format(nrPoziomu)+typ+"_{0}.png".format(x) for x in range(4)})
            nazwyN.extend({"Images/{0}/klocekN_".format(nrPoziomu)+"{0}.png".format(x) for x in range(4)})
        if typ=="big":
            self.hp=5
            nazwy.extend({"Images/{0}/klocek_".format(nrPoziomu)+typ+"_{0}.png".format(x) for x in range(6)})
            nazwyN.extend({"Images/{0}/klocekN_".format(nrPoziomu)+"{0}.png".format(x) for x in range(2,4)})
        if typ=="inv":
            self.hp=9
            nazwy.extend({"Images/{0}/klocek_".format(nrPoziomu)+typ+"_{0}.png".format(x) for x in range(1)})
        nazwy.sort()
        nazwyN.sort()
        for n in nazwy+nazwyN:
            self.images.append(pygame.image.load(n).convert_alpha())
        self.cur_img = 0
        self.image = self.images[0]
        self.rect=self.image.get_rect()
        self.x = kol*self.rect.width+self.rect.width*2
        self.y = wiersz*self.rect.height+self.rect.height*2
        self.rect.centerx=self.x
        self.rect.centery=self.y
        self.exists=True
        if typ=="inv":
            self.hp=9
        if typ=="big":
            self.hp=5
        self.radius=10
        self.destr_timer=0
        
    def update(self):
        if self.typ=="inv":
            return
        if self.cur_img>self.hp:
            self.niszczony=True
            if self.destr_timer==0:
                self.cur_img+=1
                self.destr_timer=10
            else: 
                self.destr_timer-=1
            if self.cur_img>=8:
                self.exists=False
                return
        self.image=self.images[self.cur_img]
            
    def trafiony(self):
        self.cur_img+=1
        
    def kulka_nad(self, kulka):
        return self.y > kulka.y and self.rect.top < kulka.rect.bottom and not kulka.y>self.rect.top

    def kulka_pod(self, kulka):
        return self.y < kulka.y and self.rect.bottom > kulka.rect.top and not kulka.y<self.rect.bottom
    
    def kulka_prawa(self,kulka):
        return self.x < kulka.x and self.rect.right > kulka.rect.left and not kulka.x<self.rect.right
    
    def kulka_lewa(self,kulka):
        return self.x < kulka.x and self.rect.left < kulka.rect.right and not kulka.x>self.rect.left
        
class pileczka(pygame.sprite.Sprite):
    def __init__(self, pos):
        pygame.sprite.Sprite.__init__(self)
        self.image=pygame.image.load("Images/pilka.png")
        self.rect=self.image.get_rect()
        self.x,self.y=pos
        self.rect.centerx=self.x
        self.rect.centery=self.y
        self.vx = 0
        self.vy = -10
        self.prev=pos
        self.exists=True
        self.radius=10
        self.zbija=0
        
    def update(self):
        if self.zbija>0:
            self.zbija-=1
        self.prev=self.x,self.y
        self.x+=self.vx
        if self.y>480:
            self.exists=False
        if self.vy==0:
            self.vy=-10
        self.y+=self.vy
        self.rect.centerx=self.x
        self.rect.centery=self.y
        self.vx,self.vy=normalize_speed((self.vx,self.vy),10)
        
    def cofnij(self):
        x,y=self.prev
        self.x=x
        self.rect.centerx=self.x
        self.y=y
        self.rect.centery=self.y
        
    def trafiony(self,obj):
        if isinstance(obj, Brzeg):
            if isinstance(obj, BrzegGora):
                self.vy=-self.vy
            if isinstance(obj,BrzegLewy) or isinstance(obj, BrzegPrawy):
                self.vx=-self.vx
        if isinstance(obj, klocek):
            if obj.niszczony:
                return
            G=obj.kulka_nad(self)
            D=obj.kulka_pod(self)
            P=obj.kulka_prawa(self)
            L=obj.kulka_lewa(self)
            #print((G,D,P,L))
            count = 0
            if ((G or D) and (P or L)) or not (G or D or P or L):
                sgnx=sgn(-self.x+obj.x)
                sgny=sgn(-self.y+obj.y)
                while (sgn(self.vx)==sgnx or sgn(self.vy)==sgny) or (pygame.sprite.collide_rect(self, obj) and count<30):
                    count+=1
                    if count==30:
                        if isinstance(obj, Tarcza):
                            self.y-=40
                    self.cofnij()
                    self.vx=randint(-5,5)
                    self.vy=randint(-5,5)
                    self.vx,self.vy=normalize_speed((self.vx,self.vy),10)
                    self.update()
                return
            while pygame.sprite.collide_rect(self,obj) and count<30:
                count+=1
                if count==30:
                        if isinstance(obj, Tarcza):
                            self.y-=40
                self.cofnij()
                if G or D:
                    self.vy=-self.vy
                    self.y+=self.vy
                if P or L:
                    self.vx=-self.vx
                    self.x+=self.vx
                if isinstance(obj,Tarcza):
                    self.vx-=int((-self.x+obj.x)/2)
                self.vx+=randint(-1,1)
                self.vy+=randint(-1,1)
                self.vx,self.vy = normalize_speed((self.vx,self.vy),10)
        
        
        
def sgn(x):
    if x==0:
        return 0
    if x<0:
        return -1
    return 1

class Tarcza(klocek):
    def __init__(self):
        pygame.sprite.Sprite.__init__(self)
        self.image = pygame.image.load("Images/tarcza.png")
        self.rect = self.image.get_rect()
        self.x,self.y = (320,440)
        self.rect.centerx = self.x
        self.rect.centery = self.y
        self.vx = 0
        self.vy = 0
        self.exists=True
        self.niszczony=False
        self.radius=10
        
    def update(self):
        self.prev=self.x,self.y
        tempx  = abs(self.vx) - 1
        self.vx = tempx * sgn(self.vx)
        tempy  = abs(self.vy) - 1
        self.vy = tempy * sgn(self.vy) 
        if abs(self.vy)<0:
            self.vy=0
        if abs(self.vx)<0:
            self.vx=0
        self.x+=self.vx
        if self.x < 50:
            self.x=50
        if  self.x>590:
            self.x=590
        self.rect.centerx=self.x
        
    def modspeed(self, vx):
        self.vx=vx
    def modpos(self,x):
        self.x+=x
        self.rect.centerx=self.x

    def trafiony(self):
        x,y=self.prev
        self.x=x
        self.y=y
    
def collision(pilka,klocek):
    G = klocek.kulka_nad(pilka)
    D = klocek.kulka_pod(pilka)
    P = klocek.kulka_prawa(pilka)
    L = klocek.kulka_lewa(pilka)
    if G:
        if P:
            length = sqrt((pilka.x-klocek.rect.right)**2+(pilka.y-klocek.rect.top)**2)
            return length<=pilka.radius
        if L:
            length = sqrt((pilka.x-klocek.rect.left)**2+(pilka.y-klocek.rect.top)**2)
            return length<=pilka.radius
        return abs(pilka.x-klocek.x)<=(pilka.radius+klocek.rect.width/2)
    if D:
        if P:
            length = sqrt((pilka.x-klocek.rect.right)**2+(pilka.y-klocek.rect.bottom)**2)
            return length<=pilka.radius
        if L:
            length = sqrt((pilka.x-klocek.rect.left)**2+(pilka.y-klocek.rect.bottom)**2)
            return length<=pilka.radius
        return abs(pilka.x-klocek.x)<=(pilka.radius+klocek.rect.width/2)
    if P:
        return abs(pilka.y-klocek.y)<=(pilka.radius+klocek.rect.height/2)
    if L:
        return abs(pilka.x-klocek.y)<=(pilka.radius+klocek.rect.height/2)
    return False

def normalize_speed(V,max_V):
    vx,vy=V
    dV = sqrt(vx**2+vy**2)
    if dV==0:
        return (0,0)
    k = float(max_V)/dV
    vx=int(vx*k)
    vy=int(k*vy)
    return (vx,vy)

def sin(a,b):
    return b/sqrt(a**2+b**2)

def cos(a,b):
    return a/sqrt(a**2+b**2)

def sina(A,B):
    a1,b1=A
    a2,b2=B
    return sin(a1,b1)*cos(a2,b2)-cos(a1,b1)*sin(a2,b2)

def cosa(A,B):
    a1,b1=A
    a2,b2=B
    return cos(a1,b1)*cos(a2,b2)+sin(a1,b1)*sin(a2,b2)

class powerup(pygame.sprite.Sprite):
    def __init__(self,pos):
        pygame.sprite.Sprite.__init__(self)
        self.typ=["boost","slow", "pilki","-1hp"][randint(0,3)]
        self.image=pygame.image.load("Images/"+self.typ+".png").convert_alpha()
        self.rect=self.image.get_rect()
        self.x,self.y=pos
        self.rect.centerx=self.x
        self.rect.centery=self.y
        self.exists=True
    def update(self):
        if self.y>480:
            self.exists=False
        self.y+=1
        self.rect.centery=self.y
        