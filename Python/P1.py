import pygame, sys, math

screen = pygame.display.set_mode( (640,480) )
kol1 = (255,20,20)
P = []
S = 0
	
while 1:
	time.sleep(0.02)
	for event in pygame.event.get():
		if event == pygame.QUIT: sys.exit()
		if event == pygame.MOUSEMOTION: 
			P.append(pygame.MOUSEMOTION.get_pos())
			S = S + math.sqrt(P[-1][0]^2 + P[-1][1]^2)
			while(S > 100):
				S = S - math.sqrt(P[0][0]^2 + P[0][1]^2)
				P = P[1:]
	screen.fill(kol1)
	pygame.draw.lines(screen, kol2, 0, P)  
	pygame.display.flip()
	