import pygame
from random import randint, choice

def sgn(x):
    if x<0:
        return -1
    return 1

def abs(x):
    return x*sgn(x)

class pocisk(pygame.sprite.Sprite):
    def __init__(self, Typ_P, pos, typ, pr):
        pygame.sprite.Sprite.__init__(self)
        vx,vy,image,dmg=Typ_P
        self.type=typ+" pocisk"
        self.x,self.y = pos
        self.image=image
        self.dmg = dmg
        self.rect = self.image.get_rect()
        self.vx=vx
        if sgn(pr)==sgn(vx):
            self.vx+=pr
        self.x+=40
        self.vy=vy
        self.prev=(self.x,self.y)
        self.exists=True
        self.rect.centerx=self.x
        self.rect.centery=self.y
    def update(self):
        self.prev=(self.x,self.y)
        self.x+=self.vx
        self.y+=self.vy
        if abs(self.vx)<5:
            self.vx+=sgn(self.vx)
        self.rect.centerx=self.x
        self.rect.centery=self.y
    def modspeed(self,vx,vy):
        self.vx=vx
        self.vy=vy
    def getspeed(self):
        return (self.vx,self.vy)
    def getpos(self):
        return (self.x,self.y)
    def move_b(self):
        self.x,self.y=self.prev
    def trafiony(self,x,p,dmg2):
        if dmg2==-1 or self.type.split(' ')[0]!=x.type.split(' ')[0]:
            self.exists=False

class statek(pygame.sprite.Sprite):
    def __init__(self, pos, images, pociski, max_hp):
        pygame.sprite.Sprite.__init__(self)
        self.max_hp=max_hp
        self.dmg=max_hp
        self.type="wrogi"
        self.x,self.y = pos
        self.images = images
        self.pociski = pociski
        self.cur_img=0
        self.image=self.images[self.cur_img]
        self.rect = self.image.get_rect()
        self.rect.centerx=self.x
        self.rect.centery=self.y
        self.vx=0
        self.vy=0
        self.prev=(self.x,self.y)
        self.exists=True
    def update(self):
        self.cur_img+=1
        self.cur_img=self.cur_img%8
        self.image=(self.images[self.cur_img]).copy()
        self.prev=(self.x,self.y)
        self.x+=self.vx
        self.y+=self.vy
        self.rect.centerx=self.x
        self.rect.centery=self.y
        Prostokat = self.image.get_rect()
        pygame.draw.rect(self.image, (20,200,20), pygame.Rect(Prostokat.left,Prostokat.top,Prostokat.width,10))
        pygame.draw.rect(self.image, (160,40,40), pygame.Rect(Prostokat.left,Prostokat.top,int(Prostokat.width*(1-self.dmg/self.max_hp)),10))
        
    def modspeed(self,A):
        vx,vy=A
        self.vx+=vx
        self.vy+=vy
    def modpos(self,A):
        x,y=A
        self.prev=self.x,self.y
        self.x+=x
        self.y+=y
    def getspeed(self):
        return (self.vx,self.vy)
    def getpos(self):
        return (self.x,self.y)
    def move_b(self):
        self.x,self.y=self.prev
    def shoot(self, Typ_P,y):
        self.pociski.add(pocisk(Typ_P, (self.x,self.y),y,self.vx))

class przeciwnik(statek):
    def __init__(self,pos,images,pociski,moving, max_hp, typ_prz, typ_po):
        statek.__init__(self,pos, images,pociski,max_hp)
        if typ_prz==1:
            self.states=[(-1,2),(-2,-1),(1,1),(2,-2)]
        if typ_prz==0:
            self.states=[(1,1),(0,1),(1,0),(-1,-1)]
            self.vx=-2
            self.vy=-2
        if typ_prz==2:
            self.states=[(1,-1),(-1,-1),(-1,1),(1,1)]
            self.max_hp=100
            self.dmg=100
        self.st_ct=0;
        self.type+=" przeciwnik"
        self.type+=" "+str(typ_prz)
        self.moving=moving
        self.typ_po=typ_po
    def update(self):
        self.prev=(self.x,self.y)
        statek.modpos(self,self.states[int(self.st_ct/100)])
        self.cur_img+=1
        self.cur_img=self.cur_img%8
        self.image=(self.images[self.cur_img]).copy()
        self.x+=self.vx
        self.y+=self.vy
        self.rect.centerx=self.x
        self.rect.centery=self.y
        Prostokat = self.image.get_rect()
        pygame.draw.rect(self.image, (20,200,20), pygame.Rect(Prostokat.left,Prostokat.top,Prostokat.width,10))
        pygame.draw.rect(self.image, (160,40,40), pygame.Rect(Prostokat.left,Prostokat.top,int(Prostokat.width*(1-self.dmg/self.max_hp)),10))
        #if randint(0,50)==0:
         #   statek.modspeed(self,(randint(-4,4),randint(-4,4)))
        if abs(self.vx)>5:
            self.vx=5*sgn(self.vx)
        if abs(self.vy)>5:
            self.vy=5*sgn(self.vy)
        self.st_ct+=1;
        self.st_ct=self.st_ct%400
        if randint(0,100)==0:
            self.shoot(self.typ_po,"wrogi")
        self.x-=self.moving[0]
        self.rect.centerx=self.x
        
    def trafiony(self,obj,prio,dmg):
        if dmg==-1:
            self.move_b()
            self.vy=randint(-1,1)
            self.st_ct=randint(0,399)
            return
        typ = obj.type.split(' ')
        
        if typ[0]=="teren":
            self.move_b()
            self.vx=randint(-1,1)
            self.vy=randint(-1,1)
            self.st_ct=randint(0,399)
            return
        if self.type.split(' ')[0] == typ[0]:
            return
        self.dmg-=dmg
        if self.dmg<=0:
            self.exists=False

class teren(pygame.sprite.Sprite):
    def __init__(self,pos,image,moving):
        pygame.sprite.Sprite.__init__(self)
        self.type="teren"
        self.dmg=100
        self.x,self.y=pos
        self.image=image
        self.rect = self.image.get_rect()
        self.rect.centerx=self.x
        self.rect.centery=self.y        
        self.exists=True
        self.moving=moving
    def update(self):
        self.x-=self.moving[0]
        self.rect.centerx=self.x
    def trafiony(self,x,p,dmg):
        return
    
class powerup(pygame.sprite.Sprite):
    def __init__(self,pos,image,moving):
        pygame.sprite.Sprite.__init__(self)
        self.type="przyjazny"
        self.dmg=10
        self.x,self.y=pos
        self.image=image
        self.rect = self.image.get_rect()
        self.rect.centerx=self.x
        self.rect.centery=self.y
        self.exists=True
        self.moving=moving
    def update(self):
        self.x-=self.moving[0]
        self.rect.centerx=self.x
    def trafiony(self,dmg,p,dmg2):
        self.exists=False
