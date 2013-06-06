import pygame
from Krawedz import klocek, BrzegGora,BrzegLewy,BrzegPrawy, pileczka,Tarcza,powerup
from pygame.locals import *
from sys import exit
from random import randint


def start_level(nrPoziomu,screen):
    pygame.mixer.init()
    S = pygame.mixer.Sound("Images/zderzenie.ogg")
    wszystko = pygame.sprite.Group()
    tarcza = Tarcza()
    tlo = pygame.image.load("Images/{0}/tlo.jpg".format(nrPoziomu))
    wszystko.add(tarcza)
    wiersz=0
    klocki=pygame.sprite.Group()
    for line in open("Images/{0}/poziom.txt".format(nrPoziomu)):
        kol=0
        for arg in line.split(' '):
            temp=klocek(arg.strip('\n'), nrPoziomu, wiersz,kol)
            if temp.exists:
                klocki.add(temp)
            kol+=1
        wiersz+=1
    wszystko.add(klocki)
    klocki.add(tarcza)
    brzegi=pygame.sprite.Group()
    brzegi.add(BrzegGora(nrPoziomu))
    brzegi.add(BrzegPrawy(nrPoziomu))
    brzegi.add(BrzegLewy(nrPoziomu))
    wszystko.add(brzegi)
    HP = 5
    skonczony = False
    pilki=pygame.sprite.Group()
    font1= pygame.font.SysFont("Arial", 32)
    clock=pygame.time.Clock()
    czas=0
    powerupy=pygame.sprite.Group()
    wszystko.add(powerupy)
    boost=10
    time=0
    while not skonczony and HP>0:
        clock.tick(30)
        skonczony = True
        time-=1
        if time<0:
            time=0
        if time==0:
            boost=10
        for obj in pygame.sprite.spritecollide(tarcza,powerupy,False):
            if obj.typ=="-1hp":
                HP-=1
            if obj.typ=="boost":
                boost=20
                time=300
            if obj.typ=="slow":
                boost=1
                time=300
            if obj.typ=="pilki":
                HP+=1
            obj.exists=False
        for a in wszystko:
            if not a.exists:
                if isinstance(a,pileczka):
                    pilki.remove(a)
                if isinstance(a,klocek):
                    #if randint(-2,2)==0:
                    temp=powerup((a.x,a.y))
                    powerupy.add(temp)
                    wszystko.add(temp)
                    klocki.remove(a)
                    S.play()
                if isinstance(a,powerup):
                    powerupy.remove(a)
                wszystko.remove(a)
            if isinstance(a, klocek) and a!=tarcza:
                skonczony=False
        if len(pilki)==0:
            HP-=1
            pil = pileczka((tarcza.x, tarcza.y-20))
            pilki.add(pil)
            wszystko.add(pil)
        for pil in pilki:
            for obj in pygame.sprite.spritecollide(pil, klocki, False):
                if pil.zbija==0:
                    pil.zbija=10
                    obj.trafiony()
                pil.trafiony(obj)
            for obj in pygame.sprite.spritecollide(pil,brzegi,False):
                pil.trafiony(obj)
        for event in pygame.event.get():
            if event.type==QUIT:
                exit()
        Klawisze = pygame.key.get_pressed()
        speed=0
        if Klawisze[K_LEFT]:
            speed-=1
        if Klawisze[K_RIGHT]:
            speed+=1
        tarcza.modpos(boost*speed)
        wszystko.update()
        screen.blit(tlo,(0,0))
        wszystko.draw(screen)
        napis = font1.render("Pilki: "+str(HP), 0, (0,0,0))
        screen.blit(napis,(0,0))
        napis2 = font1.render("Time: "+str(czas), 0, (0,0,0))
        screen.blit(napis2, (napis.get_width(),0))
        czas+=1
        pygame.display.update()
        
    if HP>0:
        return 1
    return 0

def game_over(screen,Str):
    cos = pygame.Surface((640,480))
    cos.fill((20,20,200))
    font1= pygame.font.SysFont("Arial", 32)
    napis = font1.render(Str, 0, (0,0,0))
    screen.blit(cos,(0,0))
    screen.blit(napis,(320-int(napis.get_width()/2),240-int(napis.get_height())))
    pygame.display.update()
    pygame.time.wait(3000)
        
#nrPoziomu = int(input("Podaj nr poziomu:\n"))
nrPoziomu=2
pygame.init()
screen = pygame.display.set_mode((640,480))
nxt=0
while nrPoziomu<3 or nxt==0:
    nrPoziomu+=nxt
    nxt=start_level(nrPoziomu,screen)
game_over(screen, "Gratulacje, wygrales!")

        
        