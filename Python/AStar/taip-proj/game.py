from utilities import *
from Gameplay import *
from Environment import *

screen = initialize_pygame(vec2(800, 600), False)

# TileSprite.prepare_mask_set()
gameplay = Gameplay(screen)
gameplay.run()
