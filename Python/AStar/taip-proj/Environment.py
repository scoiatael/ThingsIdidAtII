from ObjectSprite import *
from TerrainGrid import *
from StaticObjects import *
from DynamicObject import *
from LevelLoader import *
from IMGUI import *
from queue import PriorityQueue

EMPTY_TILE = 0
MARGIN_SIZE = vec2(TILE_SIZE.x * 2, TILE_SIZE.y * 2 + 10)
MAX_OBJECT_SIZE = 128
UPDATE_OBJECT_RANGE = 25
REDRAW_OBJECT_RADIUS = 20

class Environment:
    def __init__(self, tile_size = TILE_SIZE):
        # environment
        self._terrain_grid = TerrainGrid(tile_size)
        self._static_objects = StaticObjects()
        self._dynamic_objects = []

        # buffers for drawing
        self._previous_buffer = None
        self._current_buffer = None
        self._prev_viewport_pos = vec2(-0xfffffff, -0xfffffff)

        # surface cache
        self._PAGE_SIZE = vec2(320, 240)
        self._cache_size = 0
        self._cache_pages = []
        self._frame_number = 0

        self.resize(vec2(50, 50))
        
    def load(self, loader):
        self._terrain_grid = loader.get_terrain_grid()
        self._static_objects = loader.get_static_objects()
        self._dynamic_objects = loader.get_dynamic_objects()
        for x in self._dynamic_objects:
            x.set_environment(self)

    def save(self, loader):
        loader.set_terrain_grid(self._terrain_grid)
        loader.set_static_objects(self._static_objects)
        loader.set_dynamic_objects(self._dynamic_objects)

    def resize(self, size):
        self._terrain_grid.set_size(size)
        self._static_objects.set_size(size)
        i, s = 0, len(self._dynamic_objects)
        while i < s:
            position = self._dynamic_objects[i].get_position()
            if position.x < 0 or position.y < 0 or size.x <= position.x or size.y <= position.y:
                del self._dynamic_objects[i]
                s -= 1
            else:
                i += 1

    def notify(self, message):
        for object in self._dynamic_objects:
            object.notify(message)
            
    def reachable(self, point):
        return not self._terrain_grid.get_flags(point)

    def update(self, delta, current):
        i, s = 0, len(self._dynamic_objects)
        while i < s:
            if self._dynamic_objects[i].update(delta, current):
                self._dynamic_objects[i] = self._dynamic_objects[-1]
                self._dynamic_objects.pop()
                s -= 1
            else:
                i += 1

    def redraw(self, surface, position, time, collisions = False):
        self._create_buffers(surface)
        
        surface_size = vec2(surface.get_size())
        viewport_pos = (world_to_screen(position) - surface_size / 2).floor()
        displacement = self._prev_viewport_pos - viewport_pos
        displacement.y = -displacement.y

        self._current_buffer.fill((0, 0, 255, 0))
        self._current_buffer.blit(self._previous_buffer, displacement.intcpl())

        self._terrain_grid.redraw(self._current_buffer, viewport_pos - MARGIN_SIZE, time, collisions)

        surface.blit(self._current_buffer, (-MARGIN_SIZE).intcpl())

        first = viewport_pos // self._PAGE_SIZE
        last = (viewport_pos + surface_size) // self._PAGE_SIZE + vec2(1, 1)

        visible_objects = []
        self._static_objects.redraw_to_cache(surface, viewport_pos, visible_objects)

        self._prev_viewport_pos = viewport_pos

        # cull invisible objects
        redraw_radius_sq = REDRAW_OBJECT_RADIUS * REDRAW_OBJECT_RADIUS
        for object in self._dynamic_objects:
            d_pos = position - object.get_position()
            radius = d_pos.lengthsq()
            if radius < redraw_radius_sq:
                object.redraw(surface, viewport_pos, time, visible_objects)
        
        visible_objects.sort(key = lambda x: x[1])
        for object in visible_objects:
            surface.blit(object[0][0], object[2], object[0][1])

        self.redraw_path(surface, viewport_pos, self.findPath((3, 3), (40, 30)))
        self._swap_buffers()

    def get_players(self):
        return []

    def _create_buffers(self, surface):
        surface_size = vec2(surface.get_size())
        required_size = surface_size + MARGIN_SIZE * 2
        if self._previous_buffer == None:
            self._previous_buffer = pygame.Surface(required_size.intcpl())
            self._previous_position = vec2(-0xfffffff, -0xfffffff)
        if self._current_buffer == None:
            self._current_buffer = pygame.Surface(required_size.intcpl())
    
    def _swap_buffers(self):
        tmp = self._current_buffer
        self._current_buffer = self._previous_buffer
        self._previous_buffer = tmp

    #returns table with points (nodes) to go through
    def findPath(self, startPoint, endPoint):
        def pairPlus(pair1, pair2):
            a1,b1 = pair1
            a2,b2 = pair2
            return (a1+a2, b1+b2)
    
        def manhattanDis(pair1, pair2):
            a1,b1 = pair1
            a2,b2 = pair2
            t0 = a2-a1
            t1 = b2-b1
            return ((t0*t0+t1*t1))
        
        def seqPlus(table, pair):
            tableNew = table[:]
            tableNew.append(pair)
            return tableNew
         
        def findPathAux(self, startPoint, endPoint):
            reached = set()
            unchecked = PriorityQueue()
            
            moves = {(i,j) for i in [-1,0,1] for j in [-1,0,1]}
            moves.remove((0,0))
            
            count=0
            unchecked.put((0,startPoint,[]))
            while not unchecked.empty():
                _, point, seq = unchecked.get()
      
                if point == endPoint:
                    return seq

                if not point in reached:
                    reached.add(point)
                    for move in moves:
                        pointNew = pairPlus(point, move)
                        if self.reachable(vec2(pointNew)):
                            seqNew = seqPlus(seq, move)
                            unchecked.put((manhattanDis(pointNew, endPoint), pointNew, seqNew))
        
        seq = findPathAux(self, startPoint, endPoint)
        if seq == None:
            return []

        seqNew = []
        acc = last = startPoint
        for move in seq:
            if move == last:
                acc = pairPlus(acc, last)
            else:
                seqNew = seqPlus(seqNew, acc)
                last = move
                acc = pairPlus(acc,move)
        seqNew = seqPlus(seqNew, endPoint)
        return seqNew

    def redraw_path(self, surface, position, path):
        surface_size = vec2(surface.get_size())
        i, s = 0, len(path) - 1
        while i < s:
            first = world_to_screen(vec2(path[i]) + vec2(0.5, 0.5))
            second = world_to_screen(vec2(path[i + 1]) + vec2(0.5, 0.5))
            first -= position
            second -= position
            first.y = surface_size.y - first.y
            second.y = surface_size.y - second.y
            pygame.draw.line(surface, (255, 0, 0), first.intcpl(), second.intcpl())
            i += 1
        
        
        
    
    