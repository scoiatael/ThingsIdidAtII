'''
Created on Apr 19, 2012

@author: Mobile
'''
from random import randint
import pygame
from math import hypot


def dist(p1,p2):
    x1,y1 = p1
    x2,y2 = p2
    return hypot(x1-x2,y1-y2)
def dot(w1,w2):
    return w1[0]*w2[0] + w1[1]*w2[1]

def skaluj(L,alpha):
    for i in range(len(L)):
        L[i] *= alpha
    return L

class platforma:
    def __init__(self, wys, x, startowa=False):
        self.vx = 0
        self.vy = 0
        self.startowa=startowa
        self.rad = randint(6,20)
        self.kolor = (randint(2,100), randint(2,100), randint(2,100))
        if startowa:
            self.x = 320
            self.y = -2*self.rad
        else:
            self.x = x+self.rad
            self.y = wys
        
    def get_wys(self):
        return self.y - self.rad
    
    def center(self):
        return (self.x,self.y)
    
    def center_norm(self,offset):
        return (self.x,self.y-offset)
    
    def v(self):
        return [0,0]
        
class Plansza(pygame.sprite.Group):
    def usun_p(self):
        self.Tabela = self.Tabela[1:]
        self.wys_min = self.Tabela[0].get_wys()
    
    def gen_k(self):
        self.Tabela += [platforma(self.wys_max,randint(40,450))]
        for i in range(randint(2,6)):
            self.Tabela+=[platforma(self.wys_max, self.Tabela[-1].x+self.Tabela[-1].rad)]
        self.wys_max-=randint(80,150) 
    
    def __init__(self):
        self.Tabela = []
        self.wys_max = -200
        self.Tabela += [platforma(0,0,True)]
        while self.wys_max >= -480:
            self.gen_k()
        self.wys_min = self.Tabela[0].get_wys()
            
    def assure(self, wys_min, wys_max):
        while self.wys_min > wys_min:
            self.usun_p()
        
        while self.wys_max > wys_max:
            self.gen_k()
            
    def toScreen(self,screen, offset):
        for obj in self.Tabela:
            x,y = obj.center()
            pygame.draw.circle(screen, obj.kolor, (x,y-int(offset)), obj.rad)
            
    def collide(self,obj,offset):
        for kol in self.Tabela:
            if abs(kol.y-offset - obj.y) < 100:
                if dist(kol.center_norm(offset), obj.center()) < (kol.rad+obj.rad):
                    obj.collide(kol,offset)
            
    
class Pilka:
    
    def __init__(self):
        self.x = 320
        self.y = 420
        self.vx = 0
        self.vy = 0
        self.prev = (self.x,self.y)
        self.w_grze = True
        self.rad = 16
        self.damping = 0.9
        self.images = {}
        for x in ["left","right"]:
            for y in ["stand","jump"]:
                self.images[x+" "+y]=pygame.image.load("Images/gripe."+y+"_"+x+".png")
        self.color = (240,200,220)
        
    def get_pos(self):
        return (self.x,self.y)
    
    def add_v(self, A):
        vx,vy = A
        self.vx += vx
        self.vy += vy
        
    def set_col(self,bool):
        if bool:
            self.color = (140,200,120)
        else:
            self.color = (240,100,120)
        
        
    def toScreen(self,screen):
        pygame.draw.circle(screen, self.color, self.center_int(), self.rad)
        if abs(self.vy)<2:
            y = "stand"
        else:
            y = "jump"
        if self.vx<0:
            x = "left"
        else:
            x = "right"
        screen.blit(self.images[x+" "+y],(self.x-self.rad,self.y-self.rad))
        
    
    def update(self):
        self.prev = (self.x,self.y)
        self.x+=self.vx
        self.y+=self.vy
        self.vy+=0.20
        if self.y > 480:
            self.w_grze = False
        if self.x < 10+self.rad:
            self.x = 10+self.rad
            self.vx = -self.vx
        if self.x>630-self.rad:
            self.x = 630-self.rad
            self.vx = -self.vx
        
    def cofnij(self):
        self.x,self.y = self.prev
        
    def ok(self):
        return self.w_grze
    
    def center(self):
        return (self.x,self.y)
    
    def center_int(self):
        return (int(self.x), int(self.y))
    
    def v(self):
        return [self.vx,self.vy]
    
    def collide(self, obj, offset):
        [self.x,self.y],[self.vx,self.vy] = zderzenie([self.x,self.y],[obj.x,obj.y-offset],[self.vx,self.vy],[obj.vx,obj.vy],self.rad,obj.rad,self.damping)

   
def zderzenie(p1,p2,v1,v2,R1,R2,damping):  
    """ wg: http://en.wikipedia.org/wiki/Elastic_collision """
    dx = p1[0]-p2[0]
    dy = p1[1]-p2[1]
    
    delta = hypot(dx,dy)
    
    if delta == 0:
        return
    
    dx /= delta
    dy /= delta
    
    
    dtx,dty = dy,-dx
      
    mtx,mty = dx * ((R1+R2) - delta), dy * ((R1+R2) - delta)
    
    p1[0],p1[1] = p1[0]+2* mtx, p1[1] + 2*mty
    p2[0],p2[1] = p2[0]-0.5 * mtx, p2[1] - 0.5*mty
    
    dot1n = dot(v1, (dx,dy))
    dot1t = dot(v1, (dtx,dty))
    dot2n = dot(v2, (dx,dy))
    dot2t = dot(v2, (dtx,dty))
      
    v1nx,v1ny = dot1n * dx, dot1n *dy
    v1tx,v1ty = dot1t * dtx, dot1t *dty
    
    v2nx,v2ny = dot2n * dx, dot2n *dy
    v2tx,v2ty = dot2t * dtx, dot2t *dty
    
    v1[0] = v1tx + dx * hypot(v2nx,v2ny)
    v1[1] = v1ty + dy * hypot(v2nx,v2ny)
    
    v2[0] = v2tx - dx * hypot(v1nx,v1ny)
    v2[1] = v2ty - dy * hypot(v1nx,v1ny)
    
    skaluj(v1,damping)
    skaluj(v2,damping)
    
    return (p1,v1)
