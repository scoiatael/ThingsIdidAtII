from GameStage import *
from Environment import *

class Gameplay(GameStage):
    def __init__(self, screen):
        super(Gameplay, self).__init__(screen)
        loader = TxtLevelLoader()
        loader.load("data/level0.txt")
        self._environment = Environment()
        self._environment.load(loader)
        self._player = self._environment.get_player(0)

    def on_event(self, event):
        return False

    def on_update(self, delta, current):
        # self._environment.update(delta, current)
        pass

    def on_redraw(self, surface, delta, current):
        self._environment.redraw(surface, vec2(0, 0), current, True)