'''
Created on Apr 14, 2012

@author: Mobile
'''
from pickle import load
import pygame
from Spritey import Rakieta, powerup, planeta, PlGroup
from random import randint

def WczytajPlansze(nrPlanszy):
    F = open("Poziomy/Plansza"+str(nrPlanszy)+".dump",'rb')
    statek = Rakieta()
    n = load(F)
    Planety = PlGroup()
    for i in range(n):
        pos, big = load(F)
        Planety.add(planeta(pos,statek,big))
    n = load(F)
    Powerupy = pygame.sprite.Group()
    for i in range(n):
        pos,typ = load(F)
        Powerupy.add(powerup(pos,typ))
    return (statek, Planety, Powerupy)

def graj(nrPlanszy):
    global powerup
    (statek,Planety,Powerupy) = WczytajPlansze(nrPlanszy)
    pygame.init()
    screen = pygame.display.set_mode((640,480))
    tlo = pygame.image.load("Images/background.png")
    kont = True
    Punkty = 0
    font = pygame.font.SysFont("Arial", 20, False, False)
    Plusy = []

    for i in range(8):
        pos = (64*i+randint(0,64)+40, 48*i+randint(0,48)+40)
        obj = powerup(pos,"plus")
        Plusy.append(obj)
        
    Plusy = pygame.sprite.Group(Plusy)
    temp = pygame.sprite.Group()
    temp.add(Planety)
    temp.add(Powerupy)
    print(len(pygame.sprite.groupcollide(Plusy, temp, True, False, pygame.sprite.collide_circle)))
    clock = pygame.time.Clock()
    
    screen.blit(tlo,(0,0))
    Planety.draw(screen)
    Powerupy.draw(screen)
    Plusy.draw(screen)
    pygame.sprite.Group([statek]).draw(screen)
    napis = font.render("Punkty: "+ str(Punkty),False,(0,0,0))
    screen.blit(napis,(0,0))
    pygame.display.update()
    
    while True:
        clock.tick(5)
        pygame.event.get()
        klawisze = pygame.key.get_pressed()
        if klawisze[pygame.K_SPACE]:
            break
        
    while kont:
        clock.tick(30)
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                exit()
        klawisze = pygame.key.get_pressed()
        if klawisze[pygame.K_LEFT]:
            statek.obroc(2)
        if klawisze[pygame.K_RIGHT]:
            statek.obroc(-2)
        if klawisze[pygame.K_UP]:
            statek.wlacz_silnik()
        if klawisze[pygame.K_DOWN]:
            statek.wylacz_silnik()
        if klawisze[pygame.K_LCTRL]:
            Planety.bonus(-1)
        else:
            Planety.bonus(1)
        if klawisze[pygame.K_SPACE]:
            statek.vx=0
            statek.vy=0
        if klawisze[pygame.K_ESCAPE]:
            exit()
        if statek.paliwo <= 0 or statek.hp <= 0 or len(Plusy) == 0:
            kont = False
        Planety.update()
        statek.update()
        
        for planeta in pygame.sprite.spritecollide(statek, Planety, False, pygame.sprite.collide_circle):
            statek.trafiony(planeta)
        for powerup in pygame.sprite.spritecollide(statek, Powerupy, False, pygame.sprite.collide_circle):
            if statek.trafiony(powerup):
                powerup.trafiony(statek)
        for x in pygame.sprite.spritecollide(statek, Plusy, True, pygame.sprite.collide_circle):
            Punkty+=1
            
        screen.blit(tlo,(0,0))
        Planety.draw(screen)
        Powerupy.draw(screen)
        Plusy.draw(screen)
        pygame.sprite.Group([statek]).draw(screen)
        napis = font.render("Punkty: "+ str(Punkty),False,(0,0,0))
        screen.blit(napis,(0,0))
        pygame.display.update()


nrPlanszy = int(input("Ktory poziom? "))
graj(nrPlanszy)