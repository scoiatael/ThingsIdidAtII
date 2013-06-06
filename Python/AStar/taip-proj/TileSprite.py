from utilities import *

TILE_SIZE = vec2(TILE_WIDTH, TILE_HEIGHT)

class TileSprite:
    def __init__(self, file_name):
        if file_name == "<empty>":
            self._size = vec2(1, 1)
            self._time = 1
            self._priority = 0
            self._frames = [[[create_color_mask(TILE_SIZE, (255, 255, 0, 255))]]]
            self._file_name = file_name
            self._icon = None
        else:
            surface = pygame.image.load(file_name)

            tile_size = vec2()
            tile_size.x, tile_size.y, frame_time, priority = surface.get_at((0, 0))
            frame_time = frame_time / 255.0
            surface.set_at((0, 0), surface.get_at((0, 1)))

            width, height = surface.get_size()
            frame_width = width
            frame_height = floor(frame_width * (tile_size.y / tile_size.x)) // 2

            num_width = frame_width // tile_size.x
            num_height = frame_height // tile_size.y
            num_frames = height // frame_height

            frames = [None] * num_frames
            for i in range(num_frames):
                frames[i] = [[surface.subsurface((x * tile_size.x, y * tile_size.y + i * frame_height) + tile_size.intcpl()) for x in range(num_width)] for y in range(num_height)]

            self._size = vec2(num_width, num_height)
            self._time = frame_time
            self._priority = priority
            self._frames = frames
            self._file_name = file_name
            self._icon = None

    def get_frame(self, position, time):
        anim_frame = floor(time / self._time) % len(self._frames)
        x = (position.x + (position.y // self._size.y) % 2 * self._size.y) % self._size.x
        y = position.y % self._size.y
        return self._frames[anim_frame][y][x]

    def get_icon(self, size):
        if self._icon == None or vec2(self._icon.get_size()) != size:
            self._icon = transform.scale(self._frames[0][0][0], size.intcpl())
        return self._icon

    def get_priority(self):
        return self._priority

    def set_priority(self, priority):
        self._priority = priority

    def get_file_name(self):
        return self._file_name

    def is_animated(self):
        return len(self._frames) > 1
