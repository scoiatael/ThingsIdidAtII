import sys, pygame, time, math
pygame.init()

size = width, height = 640, 480
speed = [2, 2]
black = 0, 0, 0
poz = [0, 0]
speed = [2,2]
screen = pygame.display.set_mode(size)

ball = pygame.image.load("ball.bmp")
ballrect = ball.get_rect()
Punkty=[(0,0),(0,0)]

def abs(n):
	if n<0:
		return -n
	return n

def mod(A, n):
	x,y = A
	if x==0 or y==0:
		return [0,0]
	if x^2 + y^2<n^2:
		time.sleep(0.05)
		return [x,y]
	c = float(x)/float(y)
	y1 = float(n)/math.sqrt(1+c*c)
	x1 = y1*c
	x1=abs(x1)
	y1=abs(y1)
	if y<0:
		y1=-y1
	if x<0:
		x1=-x1
	return [math.ceil(x1), math.ceil(y1)]

def move(P, screen,ball, ballrect, poz):
	ballrect = ballrect.move(poz)
	while len(P)>1:
		x,y = P[0]
		while abs(x-poz[0])>0 and abs(y-poz[1])>0:
			time.sleep(0.01)
			print(x-poz[0])
			speed = mod( ((x-poz[0]) , (y-poz[1])), 2 )
			print(speed)
			poz = [poz[0]+speed[0] , poz[1]+speed[1] ]
			ballrect = ballrect.move(speed)
			screen.fill(black)
			screen.blit(ball, ballrect)
			P[0]=poz
			pygame.draw.lines(screen, (250,20,20), 0, P)
			pygame.display.flip()
		P = P[1:]
	return poz

poz=[0,0]
while 1:
	for event in pygame.event.get():
		if event.type == pygame.QUIT: sys.exit()
		if event.type == pygame.KEYDOWN: 
			poz=move(Punkty, screen, ball, ballrect, poz)
			Punkty = [pygame.mouse.get_pos()]
	time.sleep(0.02)
	Punkty.append( 	pygame.mouse.get_pos() )
	screen.fill(black)
	pygame.draw.lines(screen, (250,20,20), 0, Punkty)
	pygame.display.flip()