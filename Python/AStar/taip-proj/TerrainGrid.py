from TileSprite import *

class TerrainGrid:
    EMPTY_FIELD = -1
    BLEND_MASKS = load_blen_mask_set(TILE_SIZE)
    RED_TINT_MASK = create_color_mask(TILE_SIZE, (255, 0, 0, 128))

    def __init__(self, tile_size = TILE_SIZE):
        self._auxiliary = pygame.Surface(tile_size.intcpl(), SRCALPHA)
        self._draw_flags = None
        self._counter = -1
        self._grid_size = vec2(0, 0)
        self._tile_size = tile_size
        self._fields = []
        self._counters = []

    def redraw(self, surface, position, time, draw_flags):
        """Odrysowuje teren."""
        self._counter += 1

        shifted = vec2(int(position.x), int(position.y) - floor(self._tile_size.y * 0.5))

        start_tile = screen_to_world(shifted)
        start_tile.x = floor(start_tile.x) - 1
        start_tile.y = floor(start_tile.y) + 4
        surface_size = vec2(surface.get_size())

        width = int(surface_size.x / self._tile_size.x) - 2
        height = int(surface_size.y / self._tile_size.y) - 2

        start_tile_x, start_tile_y = start_tile.intcpl()
        viewport_size_x, viewport_size_y = surface_size.intcpl()
        offset_x, offset_y = (-shifted).intcpl()
        basex_x, basex_y = BASE_X.intcpl()
        basey_x, basey_y = BASE_Y.intcpl()

        auxiliary = self._auxiliary
        grid_masks = TerrainGrid.BLEND_MASKS
        blit = pygame.Surface.blit
        frame_number = self._counter
        if self._draw_flags != draw_flags:
            frame_number = self._counter + 1
        self._draw_flags = draw_flags

        for y in range(height):
            for x in range(width):
                cursor_x = start_tile_x + x - y
                cursor_y = start_tile_y + x + y
                if cursor_x >= 0 and cursor_y >= 0 and cursor_x < self._grid_size.x and cursor_y < self._grid_size.y:
                    game_frame = self._counters[cursor_y][cursor_x]
                    primary, adjacents, flags = self._fields[cursor_y][cursor_x]
                    self._counters[cursor_y][cursor_x] = frame_number
                    if game_frame + 1 != frame_number or primary.is_animated():
                        dest_x = basex_x * cursor_x + basey_x * cursor_y + offset_x
                        dest_y = viewport_size_y - basex_y * cursor_x - basey_y * cursor_y - offset_y
                        primary_tile = primary.get_frame(vec2(cursor_x, cursor_y), time)
                        blit(surface, primary_tile, (dest_x, dest_y))
                        for n in adjacents:
                            secondary_tile = n[0].get_frame(vec2(cursor_x, cursor_y), time)
                            blit(auxiliary, secondary_tile, (0, 0))
                            blit(auxiliary, grid_masks[n[1]], (0, 0), None, BLEND_RGBA_MULT)
                            blit(surface, auxiliary, (dest_x, dest_y))
                        if draw_flags and flags:
                            blit(surface, TerrainGrid.RED_TINT_MASK, (dest_x, dest_y))

                cursor_y += 1
                if cursor_x >= 0 and cursor_y >= 0 and cursor_x < self._grid_size.x and cursor_y < self._grid_size.y:
                    game_frame = self._counters[cursor_y][cursor_x]
                    primary, adjacents, flags = self._fields[cursor_y][cursor_x]
                    self._counters[cursor_y][cursor_x] = frame_number
                    if game_frame + 1 != frame_number or primary.is_animated():
                        dest_x = basex_x * cursor_x + basey_x * cursor_y + offset_x
                        dest_y = viewport_size_y - basex_y * cursor_x - basey_y * cursor_y - offset_y
                        primary_tile = primary.get_frame(vec2(cursor_x, cursor_y), time)
                        blit(surface, primary_tile, (dest_x, dest_y))
                        for n in adjacents:
                            secondary_tile = n[0].get_frame(vec2(cursor_x, cursor_y), time)
                            blit(auxiliary, secondary_tile, (0, 0))
                            blit(auxiliary, grid_masks[n[1]], (0, 0), None, BLEND_RGBA_MULT)
                            blit(surface, auxiliary, (dest_x, dest_y))
                        if draw_flags and flags:
                            blit(surface, TerrainGrid.RED_TINT_MASK, (dest_x, dest_y))

    def set_tile(self, position, tile_sprite):
        """Ustawia sprite kafla."""
        __, adjacents, flags = self._fields[position.y][position.x]
        self._fields[position.y][position.x] = tile_sprite, adjacents, flags
        for x in range(position.x - 1, position.x + 2):
            for y in range(position.y - 1, position.y + 2):
                self._update(x, y)
                
    def get_tile(self, position):
        """Zwraca sprite kafla."""
        return self._fields[position.y][position.x][0]

    def set_flags(self, position, flags):
        """Ustawia flagi kafla, to znaczy czy jest osiągalny przez jednostki."""
        tile, adjacents, __ = self._fields[position.y][position.x]
        self._fields[position.y][position.x] = tile, adjacents, flags
        self._counters[position.y][position.x] = 0

    def get_flags(self, position):
        """Zwraca flagi kafla, to znaczy czy jest osiągalny przez jednostki."""
        return self._fields[position.y][position.x][2]

    def get_tile_size(self):
        """Zwraca rozmiar kafli."""
        return self._tile_size

    def set_size(self, size):
        """Zmienia rozmiar siatki terenu, zachowując poprzednią zawartość."""
        empty = TileSprite("<empty>")
        fields = [[(empty, [], False) for x in range(size.x)] for y in range(size.y)]
        counters = [[0 for x in range(size.x)] for y in range(size.y)]
        for y in range(min(size.y, self._grid_size.y)):
            for x in range(min(size.x, self._grid_size.x)):
                fields[y][x] = self._fields[y][x]
        self._fields = fields
        self._counters = counters
        self._grid_size = size
        self._counter = -1
        for y in range(size.y):
            for x in range(size.x):
                self._update(x, y)

    def get_size(self):
        """Zwraca rozmiar siatki terenu."""
        return  self._grid_size

    def _update(self, x, y):
        def bitor(x, y, z):
            if x in y: 
                y[x] |= z 
            else: 
                y[x] = z

        def priority(tile):
            if tile == None:
                return 0
            else:
                return tile.get_priority()

        FIELDS = [(-1, +1, 1), (-1, 0, 1|2|4), (-1, -1, 4), (0, -1, 4|8|16), (+1, -1, 16), (+1, 0, 16|32|64), (+1, +1, 64), (0, +1, 1|64|128)]
        if 0 < x and x < (self._grid_size.x - 1) and 0 < y and y < (self._grid_size.y - 1):
            tile, __, collision = self._fields[y][x]
            adjacents = dict()
            for i in FIELDS:
                adjacent_tile = self._fields[y + i[1]][x + i[0]][0]
                if tile != adjacent_tile:
                    bitor(adjacent_tile, adjacents, i[2])
            tile_priority = priority(tile)
            adjacents = [x for x in adjacents.items() if (priority(x[0]) > tile_priority)]
            adjacents.sort(key = lambda x: priority(x[0]))
            self._fields[y][x] = tile, adjacents, collision
            self._counters[y][x] = 0
