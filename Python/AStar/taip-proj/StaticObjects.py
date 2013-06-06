from ObjectSprite import *
from TileSprite import *
SECTOR_SIZE = 4
NUM_SAMPLES = 16

class StaticObjects:
    def __init__(self, tile_size = TILE_SIZE):
        self._sectors = []
        self._size = vec2(0, 0)
        self._sprites = set()
        self._tile_size = tile_size

    def redraw_to_cache(self, surface, position, frames):
        start_sector = screen_to_world(position) / SECTOR_SIZE
        start_sector.x = floor(start_sector.x)
        start_sector.y = floor(start_sector.y) - 2

        size_in_pixels = vec2(surface.get_size())
        size_in_sectors = ((size_in_pixels / self._tile_size) / SECTOR_SIZE + vec2(2, 4)).floor()
        surface_center = size_in_pixels // 2

        offset_x, offset_y = (-position).intcpl()
        basexx, basexy = (BASE_X).intcpl() 
        baseyx, baseyy = (BASE_Y).intcpl()

        for y in reversed(range(size_in_sectors.y)):
            for x in range(size_in_sectors.x):
                cursor_x = (start_sector.x + x - y)
                cursor_y = (start_sector.y + x + y) + 1
                count = 0
                while count < 2:
                    if cursor_x >= 0 and cursor_y >= 0 and cursor_x < self._size.x and cursor_y < self._size.y:
                        sector = self._sectors[cursor_y][cursor_x]
                        for object in sector:
                            frame = object[2].get_n_frame(object[1])
                            frame_size = vec2(frame[0].get_size()) // 2
                            depth = size_in_pixels.y - basexy * object[0].x - baseyy * object[0].y - offset_y
                            dest_x = int(basexx * object[0].x + baseyx * object[0].y + offset_x - frame_size.x)
                            dest_y = int(depth - object[2].get_offset())
                            frame_center = vec2(dest_x + frame_size.x, dest_y + frame_size.y)
                            delta = (frame_center - surface_center).abs()
                            if delta.x < (frame_size.x + surface_center.x) or delta.y < (frame_size.y + surface_center.y):
                                frames.append((frame, depth, (dest_x, dest_y)))
                    cursor_y -= 1
                    count += 1

    def add_objects(self, position, size, sprite, radius, number = 0xffffffff):
        radius = max(radius, 0.1) # aby zapobiec dzieleniu przez zero
        number = int(min(floor((size.x / radius * 0.5) * (size.y / radius * 0.5)), number))
        for i in range(number):
            x, y = random() * size.x + position.x, random() * size.y + position.y
            for i in range(NUM_SAMPLES):
                sector = vec2(int(x / SECTOR_SIZE), int(y / SECTOR_SIZE))
                if sector.x >= 0 and sector.y >= 0 and sector.x < self._size.x and sector.y < self._size.y and self._check(vec2(x, y), radius, sprite):
                    self._sectors[sector.y][sector.x].append((vec2(x, y), randint(0, sprite.get_frame_num() - 1), sprite))
                    self._sectors[sector.y][sector.x].sort(key = lambda x: -x[0].y)
                    self._sprites.add(sprite)
                    break

    def del_objects(self, position, size):
        pos0, pos1 = position, position + size
        first, last = position.floor() // SECTOR_SIZE, (position + size).floor() // SECTOR_SIZE + vec2(1, 1)
        
        for y in range(first.y, last.y):
            for x in range(first.x, last.x):
                if x >= 0 and y >= 0 and x < self._size.x and y < self._size.y:
                    sector = self._sectors[y][x]
                    i, s = 0, len(sector)
                    while i < s:
                        if pos0.x <= sector[i][0].x and pos0.y <= sector[i][0].y and sector[i][0].x < pos1.x and sector[i][0].y < pos1.y:
                            del sector[i]
                            s -= 1
                        else:
                            i += 1

    def set_sprites(self, sprites):
        self._sprites = set(sprites)

    def get_sprites(self):
        return list(self._sprites)

    def set_sector(self, position, sector):
        self._sectors[position.y][position.x] = sector
    
    def get_sector(self, position):
        return self._sectors[position.y][position.x]

    def set_size(self, size):
        new_size = (size / SECTOR_SIZE).ceil()
        sectors = [[ [] for x in range(new_size.x)] for y in range(new_size.y)]

        for y in range(min(new_size.y, self._size.y)):
            for x in range(min(new_size.x, self._size.x)):
                sectors[y][x] = self._sectors[y][x]

        self._sectors = sectors
        self._size = new_size

    def get_size(self):
        return self._size

    def _check(self, position, radius, sprite):
        first = vec2(int(position.x - radius), int(position.y - radius)) // SECTOR_SIZE
        last = vec2(int(position.x + radius) + 1, int(position.y + radius) + 1) // SECTOR_SIZE
        sqradius = radius * radius
        empty = True

        for y in range(first.y, last.y):
            for x in range(first.x, last.x):
                for i in self._sectors[y][x]:
                    delta = i[0] - position
                    if delta.lengthsq() < sqradius:
                        empty = False
                        break

        return empty