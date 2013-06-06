import pygame
from Funkcje import start_level,game_over
pygame.init()
statek_images = []

nazwy = [ "gra1/Images/" + str(i) + ".png" for i in range(1,9)]


wrog_images = []


v_poc = 10
nrPoziomu = int(input("Podaj nr poziomu:\n"))

screen = pygame.display.set_mode((640, 480))
tlo = pygame.Surface((640,480))
for n in nazwy:
    statek_images.append(pygame.image.load(n).convert_alpha())
nazwy = [ "gra1/Images/wrogowie/" + str(i) + ".png" for i in range(1,25)]
print(nazwy)
for n in nazwy:
    wrog_images.append(pygame.image.load(n).convert_alpha())
typy_pociskow=[(v_poc,0,pygame.image.load("gra1/Images/pocisk/1.png").convert_alpha(),1),(2*v_poc,0,pygame.image.load
("gra1/Images/pocisk/2.png").convert_alpha(),2),(int(v_poc/2),0,pygame.image.load("gra1/Images/pocisk/3.png").convert_alpha(),10)]


nxt=0
while nrPoziomu<3 or nxt==0:
    nrPoziomu+=nxt
    nxt=start_level(nrPoziomu,screen,statek_images,typy_pociskow,wrog_images)
game_over(screen, "Gratulacje, wygrales!")
