'''
Created on Apr 14, 2012

@author: Mobile
'''
import pygame
from random import randint
from math import sin,cos,radians,hypot,ceil

def sgn(x):
    if x==0:
        return 0
    if x<0:
        return -1
    return 1

def mrandint(a):
    return randint(0,a)

def iloczyn(A,B):
    ax,ay=A
    bx,by=B
    return ax*bx+ay*by

class Rakieta(pygame.sprite.Sprite):
    def __init__(self):
        pygame.sprite.Sprite.__init__(self)
        self.image_base = pygame.transform.scale(pygame.image.load("Images/rocketship2.png"),(80,48))
        self.image = self.image_base
        self.rect = self.image.get_rect()
        self.rect.centerx,self.rect.centery = (320,240)
        self.vx = 0
        self.vy = 0
        self.kat = 0
        self.leci = False
        self.max_hp = 100
        self.max_paliwo = 10000
        self.hp = self.max_hp
        self.paliwo = self.max_paliwo
        self.prev = self.rect.centerx,self.rect.centery
        self.speed = 2
        self.radius = 40
        
    def update(self):
        if self.hp <= 0:
            self.kill()
            return
        self.image = (self.image_base).copy()
        if self.leci:
            if self.paliwo==0:
                self.leci = False
            else:
                self.vx += int(cos(radians(self.kat))*self.speed)
                self.vy += int(-sin(radians(self.kat))*self.speed)
                self.paliwo -= 1 
                for i in range(mrandint(15)+4):
                    pygame.draw.circle(self.image, (120+mrandint(40),10+mrandint(25),mrandint(20)), (mrandint(5)+i,int(self.rect.height/2)+mrandint(10)-i), mrandint(4))
        
        self.prev = self.rect.centerx,self.rect.centery
        self.rect.centerx += self.vx
        self.rect.centery += self.vy
        d  = hypot(self.vx,self.vy)
        if d>5:
            self.vx = 5*self.vx/d
            self.vy = 5*self.vy/d
        if self.rect.left < 0:
            self.rect.left = 0
            self.vx=0
        if self.rect.right > 640:
            self.rect.right = 640
            self.vx=0
        if self.rect.top < 0:
            self.rect.top = 0
            self.vy=0
        if self.rect.bottom > 480:
            self.rect.bottom = 480
            self.vy=0
            
        temp = (self.rect.centerx, self.rect.centery)
        self.image = pygame.transform.rotate(self.image,self.kat)
        self.rect = self.image.get_rect()
        self.rect.centerx,self.rect.centery = temp
        Prostokat = self.image_base.get_rect()
        pygame.draw.rect(self.image, (20,200,20), pygame.Rect(int(self.rect.width/2)-40,int(self.rect.height/2)-24,Prostokat.width,3))
        pygame.draw.rect(self.image, (160,40,40), pygame.Rect(int(self.rect.width/2)-40,int(self.rect.height/2)-24,int(Prostokat.width*(1-self.hp/self.max_hp)),3))
        
        Prostokat = self.image_base.get_rect()
        pygame.draw.rect(self.image, (255,255,20), pygame.Rect(int(self.rect.width/2)-40,int(self.rect.height/2)-14,Prostokat.width,3))
        pygame.draw.rect(self.image, (200,200,40), pygame.Rect(int(self.rect.width/2)-40,int(self.rect.height/2)-14,int(Prostokat.width*(1-self.paliwo/self.max_paliwo)),3))
        
    
    def cofnij(self):
        self.rect.centerx,self.rect.centery = self.prev
    
    def obroc(self,x):
        self.kat+=x
        self.kat=self.kat%360
        
    def wlacz_silnik(self):
        self.leci = True
        
    def wylacz_silnik(self):
        self.leci = False
        
    def trafiony(self,obj):
        if isinstance(obj,powerup):
            if obj.typ == "healthheart":
                self.hp+=10
                if self.hp>self.max_hp:
                    self.hp=self.max_hp
                    return False
            if obj.typ == "star":
                self.paliwo+=10
            if obj.typ == "minus":
                self.hp-=10
                self.paliwo-=10
        if isinstance(obj,planeta):
            self.cofnij()
            (X,Y) = (self.rect.centerx-obj.rect.centerx,-self.rect.centery+obj.rect.centery)
            self.hp-=int(5+0.1*hypot(self.vx,self.vy)*abs(iloczyn((self.vx,self.vy),(X,Y)))/hypot(X,Y))
            print(self.hp)
            self.vx*=-1/2
            self.vy*=-1/2
        return True
        
class powerup(pygame.sprite.Sprite):
    def __init__(self,pos,typ):
        pygame.sprite.Sprite.__init__(self)
        self.typ=typ
        self.image=pygame.transform.scale(pygame.image.load("Images/"+typ+".png"),(40,40))
        self.rect = self.image.get_rect()
        self.rect.centerx,self.rect.centery = pos
        self.radius = 20
    def trafiony(self,obj):
        self.kill()
        
class planeta(pygame.sprite.Sprite):
    def __init__(self, pos, statek,big):
        pygame.sprite.Sprite.__init__(self)
        self.image = pygame.transform.scale(pygame.image.load("Images/planet.png"),(40*big,40*big))
        self.rect = self.image.get_rect()
        self.rect.centerx,self.rect.centery = pos
        self.big = big
        self.statek = statek
        self.radius = 20*big
        self.bonus=1
        
    def update(self):
        dx = (self.rect.centerx-self.statek.rect.centerx)
        dy = (self.rect.centery-self.statek.rect.centery)
        d = hypot(dx,dy)
        if d != 0:
            F = self.big**4*1000/(d**2)
            self.statek.vx += (abs(dx*F/d))*sgn(self.bonus*dx)
            self.statek.vy += (abs(dy*F/d))*sgn(self.bonus*dy)
            
    def bonus(self,x):
        self.bonus=x

class PlGroup(pygame.sprite.Group):
    def bonus(self,x):
        for obj in self.sprites():
            planeta.bonus(obj,x)