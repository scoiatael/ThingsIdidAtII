'''
Created on Apr 19, 2012

@author: Mobile
'''
import pygame
from Plansza import Plansza, Pilka
from sys import exit

def sgn(i):
    if i<0:
        return -1
    return 1

def my_max(A,B):
    if abs(A)>abs(B):
        return B*sgn(A)
    return A

def start_game(screen,tlo):
    offset = -480
    pilka = Pilka()
    plansza = Plansza()
    kontrola = False
    
    clock = pygame.time.Clock()
    
    counter = 0
    start = False
    font = pygame.font.SysFont("Arial", 20, False, False)
    while True:
        counter-=1
        if counter < 0:
            counter=0
        clock.tick(30)
        if start:
            offset-=1
            pilka.y+=1
            while pilka.y < 100:
                offset-=1
                pilka.y+=1
            plansza.assure(offset+480,offset)
            
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                exit()
                
        klawisze = pygame.mouse.get_pressed()
        if kontrola != False:
            kontrola = pygame.mouse.get_pos()
            if klawisze[0] and counter==0:
                xk,yk = kontrola
                xs,ys = pilka.center_int()
                pilka.add_v((my_max((xs-xk)/5,10), my_max((ys-yk)/5,10)))
                counter = 30
                kontrola = False
                start = True
        else:
            if klawisze[0] and counter ==0:
                kontrola = pygame.mouse.get_pos()
                counter = 10
        pilka.update()
        pilka.set_col(counter==0)
        if not pilka.ok():
            game_over(screen, "Przegrales", int((-pilka.y-offset-480)/10+42))
            return
        plansza.collide(pilka,offset)
        screen.blit(tlo, (0, ((-offset) % 480)))
        screen.blit(tlo, (0, -480+((-offset) % 480)))
        pygame.draw.rect(screen, (0,100,0), pygame.Rect((0,0),(10,480)))
        pygame.draw.rect(screen, (0,100,0), pygame.Rect((630,0),(10,480)))
        screen.blit(font.render(str(int((-pilka.y-offset-480)/10+42)),False,(100,20,100)), (10,0))
        plansza.toScreen(screen,offset)
        pilka.toScreen(screen)
        if kontrola != False:
            pygame.draw.line(screen, (120,100,20), pilka.center_int(), kontrola)
        pygame.display.update()
    
def game_over(screen, string,punkty):
    font = pygame.font.SysFont("Arial", 40)
    font2 = pygame.font.SysFont("Arial", 30)
    napis = font.render(string,False,(0,0,0))
    napis2 = font2.render("Punkty: "+str(punkty),False,(0,0,0))
    screen.fill((200,200,255))
    screen.blit(napis,(320-int(napis.get_width()/2),240 - int(napis.get_height()/2)))
    screen.blit(napis2,(320-int(napis2.get_width()/2),240 - int(napis.get_height()/2)-napis2.get_height()))
    pygame.display.update()
    pygame.time.wait(2000)
    
    
    
    
    
    
    
    
    
    
    
    
    
def main():
    pygame.init()
    screen = pygame.display.set_mode((640,480))
    tlo = pygame.image.load("Images/tlo.png")
    tlo = pygame.transform.scale(tlo, (640,480))
    start_game(screen,tlo)
    
main()