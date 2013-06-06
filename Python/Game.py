import pygame, sys, math, time

def move(P, S):
	screen, col = S
	for A in P:
		img, rect, speed = A
		screen.fill(col)
		rect=rect.move(speed)
		screen.blit(img, rect)
		pygame.display.flip()

height = 480
width = 640
rectwidth = 20
rectheight = 20
screen = pygame.display.set_mode( (height, width) )
col = (255,255,255)
P1 = pygame.image.load("P1.jpg")
P1rect = P1.get_rect()
P1speed=[2,0]
P2 = pygame.image.load("P2.jpg")
P2rect = P2.get_rect()
P1speed=[-2,0]
toprect = pygame.Rect((0,0), (width, rectheight)
bottomrect = pygame.Rect((0,height-rectheight), (width, rectheight)
leftrect = pygame.Rect((0,0), (rectwidth, height)
rightrect = pygame.Rect((width-rectwidth,0), (rectwidth, height)

P=[(P1,P1rect,P1speed),(P2,P2rect,P2speed)]

cont = 1

while cont:
	time.sleep(0.02)
	move(P, (screen, col))
	for A in P:
		col = 0
		img, rect, speed = A
		if rect.colliderect(toprect):
			col = 1
		if rect.colliderect(bottomrect):
			col = 1
		if rect.colliderect(rightrect):
			col = 1
		if rect.colliderect(leftrect):	 
			col = 1