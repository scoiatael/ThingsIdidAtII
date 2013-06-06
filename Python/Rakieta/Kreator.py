'''
Created on Apr 14, 2012

@author: Mobile
'''
import pygame
from sys import exit
from Spritey import planeta,Rakieta,powerup
from pickle import dump
from random import randint

def ZapiszPlansze(T, nrPlanszy):
    Planety,Powerupy = T
    F = open('Poziomy/Plansza'+nrPlanszy+'.dump','wb')
    dump(len(Planety),F)
    for pl in Planety:
        dump(((pl.rect.centerx,pl.rect.centery),pl.big),F)
    dump(len(Powerupy),F)
    for py in Powerupy:
        dump(((py.rect.centerx,py.rect.centery),py.typ),F)
    
def TworzPlansze():
    nrPlanszy = str(int(input("Podaj nr planszy: ").rstrip('\n')))
    pygame.init()
    screen = pygame.display.set_mode((640,480))
    Planety = pygame.sprite.Group()
    Powerupy = pygame.sprite.Group()
    statek = Rakieta()
    tlo = pygame.image.load("Images/background.png")
    kont = True
    while kont:
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                exit()
        klawisze = pygame.key.get_pressed()
        if klawisze[pygame.K_0]:
            Planety.add(planeta(pygame.mouse.get_pos(),statek, randint(1,3)))
        if klawisze[pygame.K_1]:
            Powerupy.add(powerup(pygame.mouse.get_pos(),"healthheart"))
        if klawisze[pygame.K_2]:
            Powerupy.add(powerup(pygame.mouse.get_pos(),"minus"))
        if klawisze[pygame.K_3]:
            Powerupy.add(powerup(pygame.mouse.get_pos(),"star"))
        if klawisze[pygame.K_SPACE]:
            ZapiszPlansze((Planety,Powerupy), nrPlanszy)
            exit()
        screen.blit(tlo,(0,0))
        Planety.draw(screen)
        Powerupy.draw(screen)
        pygame.sprite.Group([statek]).draw(screen)
        pygame.display.update()

TworzPlansze()