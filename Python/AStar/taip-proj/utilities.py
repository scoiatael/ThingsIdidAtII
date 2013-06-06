from math import *
from random import randint
from random import random
from sys import modules
from inspect import getmembers
from inspect import isclass
import pygame
import pygame.gfxdraw
from pygame.locals import *
from pygame import transform
from collections import namedtuple

class vec2:
    def __init__(self, x = 0, y = 0):
        if isinstance(x, tuple):
            self.x = x[0]
            self.y = x[1]
        else:
            self.x = x
            self.y = y

    def __add__(a, b):
        return vec2(a.x + b.x, a.y + b.y)

    def __sub__(a, b):
        return vec2(a.x - b.x, a.y - b.y)

    def __mul__(a, b):
        if isinstance(b, vec2):
            return vec2(a.x * b.x, a.y * b.y)
        else:
            return vec2(a.x * b, a.y * b)

    def __truediv__(a, b):
        if isinstance(b, vec2):
            return vec2(a.x / b.x, a.y / b.y)
        else:
            return vec2(a.x / b, a.y / b)

    def __floordiv__(a, b):
        if isinstance(b, vec2):
            return vec2(a.x // b.x, a.y // b.y)
        else:
            return vec2(a.x // b, a.y // b)

    def __pos__(a):
        return vec2(+a.x, +a.y)

    def __neg__(a):
        return vec2(-a.x, -a.y)

    def __eq__(a, b):
        return a.x == b.x and a.y == b.y

    def __ne__(a, b):
        return a.x != b.x or a.y != b.y

    def __setitem__(self, index, value):
        if index == 0:
            self.x = value
        else:
            self.y = value

    def __getitem__(self, index):
        if index == 0:
            return self.x
        else:
            return self.y

    def __str__(self):
        return "[" + str(self.x) + ", " + str(self.y) + "]"

    def __repr__(self):
        return "[" + str(self.x) + ", " + str(self.y) + "]"

    def couple(self):
        return self.x, self.y

    def lengthsq(self):
        return self.x * self.x + self.y * self.y

    def length(self):
        return sqrt(self.x * self.x + self.y * self.y)

    def normal(self):
        inv = 1.0 / self.length()
        return vec2(self.x * inv, self.y * inv)

    def intcpl(self):
        return int(self.x), int(self.y)

    def floor(self):
        return vec2(floor(self.x), floor(self.y))

    def ceil(self):
        return vec2(ceil(self.x), ceil(self.y))

    def abs(self):
        return vec2(abs(self.x), abs(self.y))

    def copy(self):
        return vec2(self.x, self.y)

def rotate2(vector, angle):
    s = sin(angle)
    c = cos(angle)
    x = c * vector.x - s * vector.y
    y = s * vector.x + c * vector.y
    return vec2(x, y)

def reflect2(vector, normal):
    proj = normal * dot2(vector, normal)
    return -vector + proj * 2

def dot2(vec_a, vec_b):
    return vec_a.x * vec_b.x + vec_a.y * vec_b.y

def angle2(normal):
    angle = acos(normal.x)
    if normal.y < 0:
        return angle - pi * 0.5
    else:
        return 2 * pi - angle - pi * 0.5

def clamp(value, min = 0.0, max = 1.0):
    if value < min:
        value = min
    if value > max:
        value = max
    return value

def area(size):
    for y in range(size.y):
        for x in range(size.x):
            yield vec2(x, y)

rect = namedtuple('rect', ['x', 'y', 'w', 'h'])

def region_hit(region, position):
    return not (position.x < region.x or position.y < region.y or position.x >= region.x + region.w or position.y >= region.y + region.h)

def mul_color(a, b):
    return (a[0] * b[0] // 255, a[1] * b[1] // 255, a[2] * b[2] // 255, a[3] * b[3] // 255)

TILE_ASPECT = 0.75   # height / width
TILE_WIDTH = 64
TILE_HEIGHT = int(TILE_WIDTH * TILE_ASPECT)

BASE_X = vec2(TILE_WIDTH * 0.5, TILE_HEIGHT * -0.5)
BASE_Y = vec2(TILE_WIDTH * 0.5, TILE_HEIGHT * 0.5)
INV_DET = 1.0 / (BASE_X.x * BASE_Y.y - BASE_Y.x * BASE_X.y)

INV_BASE_X = vec2(INV_DET * BASE_Y.y, -INV_DET * BASE_X.y)
INV_BASE_Y = vec2(-INV_DET * BASE_Y.x, INV_DET * BASE_X.x)

def world_to_screen(world):
    return BASE_X * world.x + BASE_Y * world.y

def screen_to_world(screen):
    return INV_BASE_X * screen.x + INV_BASE_Y * screen.y

def initialize_pygame(resolution = None, fullscreen = False):
    pygame.init()
    pygame.font.init()
    pygame.mixer.init(buffer = 64)
    if pygame.font.get_init() == False:
        raise Exception("Cannot initialize pygame.font module...")
    if pygame.mixer.get_init() == False:
        raise Exception("Cannot initialize pygame.mixer module...")
    if resolution is None:
        resolution = vec2(0, 0)
        fullscreen = True
    if fullscreen:
        return pygame.display.set_mode((0, 0), FULLSCREEN | HWSURFACE)
    else:
        return pygame.display.set_mode(resolution.intcpl())

def draw_text_center(surface, font, text, color, region, offset = (0, 0)):
    source = font.render(text, True, color)
    src_size = source.get_size()
    offset_x = (region.w - src_size[0]) // 2 + offset[0]
    offset_y = (region.h - src_size[1]) // 2 + offset[1]
    surface.blit(source, (region.x + offset_x, region.y + offset_y))

def create_tile_mask(tile_size):
    def uv_lerp(X, A, B, C, D):
        V = B - A
        U = D - A
        u = (V.y * (X.x - A.x) - V.x * (X.y - A.y)) / (U.x * V.y - U.y * V.x)
        v = (X.y - A.y - u * U.y) / V.y
        return u, v
    mask = pygame.Surface(tile_size.intcpl(), SRCALPHA)
    mask.fill((0, 0, 0, 0))
    half = int(tile_size.y / 2)
    A = vec2(0.0, tile_size.y * 0.5)
    B = vec2(tile_size.x * 0.5, 0.0)
    C = vec2(tile_size.x , tile_size.y * 0.5)
    D = vec2(tile_size.x * 0.5, tile_size.y)
    mask.lock()
    for y in range(tile_size.y):
        for x in range(tile_size.x):
            position = vec2(x + 0.5, y + 0.5)
            u, v = uv_lerp(position, A, B, C, D)
            if 0.0 < u < 1.0 and 0.0 < v < 1.0:
                mask.set_at((x, y), (255, 255, 255, 255))
    mask.unlock()
    return mask
        
def create_blend_mask(number, tile_size):
    def uv_lerp(X, A, B, C, D):
            V = B - A
            U = D - A
            u = (V.y * (X.x - A.x) - V.x * (X.y - A.y)) / (U.x * V.y - U.y * V.x)
            v = (X.y - A.y - u * U.y) / V.y
            return u, v

    def gradient(A, B, C, D, a, b, c, d, surface):
        center = tile_size * 0.5
        for y in range(int(tile_size.y)):
            for x in range(int(tile_size.x)):
                point = vec2(x + 0.5, y + 0.5)
                u, v = uv_lerp(point, A, B, C, D)
                if -0.05 <= u and u <= 1.05 and -0.05 <= v and v <= 1.05:
                    m = (1.0 - v) * a + v * b
                    n = (1.0 - v) * d + v * c
                    value = (1.0 - u) * m + u * n
                    alpha = clamp(int(255.0 * value), 0, 255)
                    surface.set_at((x, y), (255, 255, 255, alpha))

    def int_to_list(x):
        return [(x >> i) & 1 for i in range(8)]

    VECTORS = [vec2(tile_size.x * 0.5, 0), tile_size * 0.25, vec2(0.0, tile_size.y * 0.5), vec2(tile_size.x * 0.25, tile_size.y * 0.75),
                vec2(tile_size.x * 0.5, tile_size.y), tile_size * 0.75, vec2(tile_size.x, tile_size.y * 0.5), vec2(tile_size.x * 0.75, tile_size.y * 0.25)]

    factors = int_to_list(number)
    surface = pygame.Surface(tile_size.intcpl(), SRCALPHA)
    center = tile_size * 0.5
    gradient(VECTORS[2], VECTORS[1], center, VECTORS[3], factors[2], factors[1], 0.0, factors[3], surface)
    gradient(VECTORS[3], center, VECTORS[5], VECTORS[4], factors[3], 0.0, factors[5], factors[4], surface)
    gradient(center, VECTORS[7], VECTORS[6], VECTORS[5], 0.0, factors[7], factors[6], factors[5], surface)
    gradient(VECTORS[1], VECTORS[0], VECTORS[7], center, factors[1], factors[0], factors[7], 0.0, surface)
    mask = create_tile_mask(tile_size)
    surface.blit(mask, (0, 0), None, BLEND_RGBA_MULT)

    return surface

def create_color_mask(tile_size, color):
    def uv_lerp(X, A, B, C, D):
        V = B - A
        U = D - A
        u = (V.y * (X.x - A.x) - V.x * (X.y - A.y)) / (U.x * V.y - U.y * V.x)
        v = (X.y - A.y - u * U.y) / V.y
        return u, v
    mask = pygame.Surface(tile_size.intcpl(), SRCALPHA)
    mask.fill((0, 0, 0, 0))
    half = int(tile_size.y / 2)
    A = vec2(0.0, tile_size.y * 0.5)
    B = vec2(tile_size.x * 0.5, 0.0)
    C = vec2(tile_size.x, tile_size.y * 0.5)
    D = vec2(tile_size.x * 0.5, tile_size.y)
    mask.lock()
    for y in range(tile_size.y):
        for x in range(tile_size.x):
            position = vec2(x + 0.5, y + 0.5)
            u, v = uv_lerp(position, A, B, C, D)
            if 0.0 < u < 1.0 and 0.0 < v < 1.0:
                mask.set_at((x, y), color)
    mask.unlock()
    return mask

def prepare_blend_mask_set(tile_size, file_name = "data/masks.png"):
    MASK_NUMBER = 256
    masks = [create_blend_mask(i, tile_size) for i in range(MASK_NUMBER)]
    surface = pygame.Surface((tile_size.x, tile_size.y * MASK_NUMBER), SRCALPHA)
    for i in range(len(masks)):
        surface.blit(masks[i], (0, i * tile_size.y))
    pygame.image.save(surface, file_name)

def load_blen_mask_set(tile_size, file_name = "data/masks.png"):
    # prepare_blend_mask_set(tile_size, "data/masks.png")
    surface = None
    try:
        surface = pygame.image.load(file_name)
    except:
        prepare_blend_mask_set(tile_size, file_name)
    surface = pygame.image.load(file_name)
    return [surface.subsurface((0, x * tile_size.y) + tile_size.intcpl()) for x in range(256)]

def extrude_paths(x):
    first = 0
    last = 0
    temp = 0
    result = []
    while first < len(x):
        while first < len(x):
            if x[first] == '"':
                first += 1
                last = first
                break
            first += 1
        while last < len(x):
            if(x[last] == '"'):
                temp = last
                break
            last += 1
        if temp > first:
            result.append(x[first:last])
            first = last
        first += 1
    return result