from utilities import *

class GameStage:
    def __init__(self, screen):
        self._time_step = 0.01
        self._screen = screen
        self._next_stage = None
        self._show_fps = True
        self._sysfont = pygame.font.SysFont("Consolas", 16)
        self._previous = [10] * 32

    def run(self):
        old_time = pygame.time.get_ticks() * 0.001 # milisecs to secs
        new_time = pygame.time.get_ticks() * 0.001 # milisecs to secs
        delta = 0.0
        quit = False
        ms = 20
        while not quit:
            new_time = pygame.time.get_ticks() * 0.001
            delta += new_time - old_time
            old_time = new_time

            for event in pygame.event.get():
                if event.type == pygame.QUIT:
                    quit = True
                    self._next_stage = None
                elif event.type == pygame.KEYDOWN and event.key == K_F1:
                    self._show_fps = not self._show_fps
                else:
                    quit = self.on_event(event)

            temp = delta
            while delta > self._time_step and not quit:
                quit = self.on_update(self._time_step, new_time)
                delta -= self._time_step

            self._screen.fill((0, 0, 0))
            self.on_redraw(self._screen, temp, new_time)
            if self._show_fps and delta != 0:
                # smooth fps
                average = 0
                for i in range(len(self._previous) - 1):
                    self._previous[i] = self._previous[i + 1]
                    average += self._previous[i]
                self._previous[-1] = temp
                average += temp
                average /= len(self._previous)
                counter = self._sysfont.render("ms: " + str(int(average * 1000)) + " fps: " + str(int(1.0 / average)), False, (255, 0, 0))
                self._screen.blit(counter, (0, 0))
            pygame.display.update()
            
        return self._next_stage

    def on_event(self, event):
        return False

    def on_update(self, delta, current):
        return False

    def on_redraw(self, surface, delta, current):
        pass

    def set_next_stage(next_stage):
        self._next_stage = next_stage

    def get_next_stage():
        return self._next_stage
