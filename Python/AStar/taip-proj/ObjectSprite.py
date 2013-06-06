from utilities import *

class ObjectSprite:
    PI2 = 2 * pi
    INV_2PI = 1.0 / (2 * pi)

    def __init__(self, file_name):
        surface = pygame.image.load(file_name)
        frame_size = vec2()
        frame_size.x, frame_size.y, frame_time, _ = surface.get_at((0, 0))
        self._vertical_offset, _, _, _ = surface.get_at((0, 1))
        frame_time = frame_time / 255.0
        color_key = surface.get_at((1, 0))
        
        surface.set_at((0, 0), color_key)
        surface.set_at((0, 1), color_key)
        surface.set_colorkey(color_key)
        surface_size = vec2(surface.get_size())
        self._frame_size = frame_size
        self._size = surface_size // frame_size
        self._time = frame_time
        self._angle_offset = pi / self._size.y

        def clip(subsurface):
            clip = subsurface.get_bounding_rect()
            return subsurface, clip

        self._frames = [[clip(surface.subsurface((x * frame_size.x, y * frame_size.y) + frame_size.intcpl())) for x in range(self._size.x)] for y in range(self._size.y)]
        self._file_name = file_name
        self._icon = None

    def get_frame(self, angle, time):
        """Zwraca ramkę animacji które powinna być wyświetlona na podstawie czasu i kąta obrotu."""
        anim_frame = floor(time / self._time % self._size.x)
        direction = floor(((angle + self._angle_offset) % ObjectSprite.PI2 * ObjectSprite.INV_2PI) * self._size.y)
        return self._frames[direction][anim_frame]

    def get_n_frame(self, index):
        return self._frames[0][index]

    def get_frame_size(self):
        return self._frame_size

    def get_frame_num(self):
        return len(self._frames[0])

    def get_icon(self, size):
        """Zwraca ikone sprite'a dla edytora."""
        if self._icon == None or vec2(self._icon.get_size()) != size:
            self._icon = transform.scale(self._frames[0][0][0], size.intcpl())
        return self._icon

    def get_time(self):
        """Zwraca czas animacji."""
        return self._time * len(self._frames[0])

    def get_file_name(self):
        return self._file_name

    def get_offset(self):
        return self._vertical_offset