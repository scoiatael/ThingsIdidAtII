from utilities import *
from os import listdir
import pygame
from pygame.locals import *
from pygame import transform

pygame.init()

def prepare_tile_sprite(paths, dest, tile_size, frame_time, priority):
    """Pomocnicza funkcja statyczna, wycina kafle."""
    surfaces = [pygame.image.load(path) for path in paths]
    dest_width, dest_height = surfaces[0].get_size()
    for surface in surfaces:
        width, height = surface.get_size()
        dest_width = min(dest_width, width)
        dest_height = min(dest_height, height)

    dest_width = dest_width // tile_size.x * tile_size.x
    dest_height = floor(dest_width * (tile_size.y / tile_size.x)) // tile_size.y * tile_size.y
    num_width = dest_width // tile_size.x * 2
    num_height = dest_height // tile_size.y

    surface = pygame.Surface((dest_width * 2, dest_height * len(surfaces)), SRCALPHA)
    x_axis = tile_size // 2
    y_axis = vec2(x_axis.x, -x_axis.y)
    xoffset = vec2(dest_width, 0)
    yoffset = vec2(0, dest_height)
    tile_mask = create_tile_mask(tile_size)
    for i in range(len(surfaces)):
        if (dest_width, dest_height) != surface.get_size():
            surfaces[i] = pygame.transform.smoothscale(surfaces[i].convert(32, SRCALPHA), (dest_width, dest_height))

        doubled = pygame.Surface((dest_width * 2, dest_height * 2), SRCALPHA)
        doubled.blit(surfaces[i], (0, 0))
        doubled.blit(surfaces[i], yoffset.intcpl())
        doubled.blit(surfaces[i], xoffset.intcpl())
        doubled.blit(surfaces[i], (xoffset + yoffset).intcpl())

        for x in range(num_width):
            for y in range(num_height):
                position = x_axis * x + y_axis * y + yoffset - vec2(0, tile_size.x // 2)
                tile = pygame.Surface(tile_size.intcpl(), SRCALPHA)
                tile.blit(doubled, (0, 0), position.intcpl() + tile_size.intcpl())
                tile.blit(tile_mask, (0, 0), None, BLEND_RGBA_MULT)
                surface.blit(tile, (x * tile_size.x, i * dest_height + y * tile_size.y))

    surface.set_at((0, 0), (tile_size.x, tile_size.y, floor(255.0 * frame_time), priority))
    pygame.image.save(surface, dest)


def prepare_object_sprite(path, offset, frame_time = 1.0):
    """Pomocnicza funkcja statyczna, tworzy tablice klatek i ustawia kanał alfa."""
    DIR = ["nw", "sw", "se", "ne", "n", "s", "w", "e"]
    MAP = { "n" : 0, "nw" : 1, "w" : 2, "sw" : 3, "s" : 4, "se" : 5, "e" : 6, "ne" : 7 }
    def decompose(path):
        a, b, c = 0, 0, 0
        while not path[a].isdigit() and a < len(path):
            a += 1
        b = a
        while path[b].isdigit() and b < len(path):
            b += 1
        for x in DIR:
            l = len(x)
            if path[a - l:a] == x:
                c = a - l
                break
        else:
            return path[:a], (int(path[a:b]) - 5) % 8, 0
        return path[:c].strip(), MAP[path[c:a]], int(path[a:b])

    groups = dict()
    for x in listdir(path):
        name, dir, num = decompose(x)
        if name in groups:
            groups[name].append((x, dir, num))
        else:
            groups[name] = [(x, dir, num)]
        
    for item in groups.items():
        surface_width, surface_height = 0, 0
        sprite_width, sprite_height = pygame.image.load(path + "/" + item[1][0][0]).get_size()
        for x in item[1]:
            surface_width = max(surface_width, x[2])
            surface_height = max(surface_height, x[1])
        surface_width = (surface_width + 1) * sprite_width
        surface_height = (surface_height + 1) * sprite_height
        surface = pygame.Surface((surface_width, surface_height))
        for x in item[1]:
            temp = pygame.image.load(path + "/" + x[0])
            surface.blit(temp, (x[2] * sprite_width, x[1] * sprite_height))
        transparent = surface.get_at((0, 0))
        for y in range(surface_height):
            for x in range(surface_width):
                if surface.get_at((x, y)) == transparent:
                    surface.set_at((x, y), (255, 0, 255))
        surface.set_at((0, 0), (sprite_width, sprite_height, floor(255.0 * frame_time)))
        surface.set_at((0, 1), (offset, 0, 0))
        pygame.image.save(surface, item[0] + ".png")

print("RapingOrcsQithGreatMagic conversion tool v1")
quit = False
while not quit:
    command = input()
    if command.startswith("help"):
        print('If an error can be ignored, it will be.')
        print('Ranges: priority 0...255, width 16...255, height 16...255, frame time 1...1000.')
        print('Defaults: frame_time 1000, width 64, height 48.')
        print('tile "<src paths>" "<dest path>" <priority> [frame_time (ms)] [tile_width] [tile_height] - converts tile sprite')
        print('object "<folder path>" [vertical offset] [frame_time] - converts object sprite')
        print('exit - exits')
    elif command.startswith("tile"):
        paths = extrude_paths(command)
        if len(paths) < 2:
            print("At least source and destination path have to be specified!")
        else:
            split = [x.strip() for x in command[command.rfind('"') + 1:].split()]
            priority, frame_time, width, height = 0, 1.0, 64, 48
            if len(split) == 0 or not split[0].isdigit():
                print("Priority has to be specified!")
            else:
                priority = clamp(int(split[0]), 0, 255)

            if len(split) > 1 and split[1].isdigit():
                frame_time = clamp(float(split[1]), 1.0, 1000.0) / 1000.0

            if len(split) > 3 and split[2].isdigit() and split[3].isdigit():
                width = clamp(int(split[2]), 16, 255)
                height = clamp(int(split[3]), 16, 255)

            print("Preparing tile sprite, frame time: " + str(frame_time) + ", width: " + str(width) + ", height: " + str(height) + "... ", end = "")
            succeded = True
            try:
                prepare_tile_sprite(paths[0:-1], paths[-1], vec2(width, height), frame_time, priority) 
            except:
                print("FAILED")
                succeded = False
            if succeded:
                print("SUCCEDED")

    elif command.startswith("object"):
        paths = extrude_paths(command)
        if len(paths) != 1:
            print("Folder path required!")
        else:
            split = [x.strip() for x in command[command.rfind('"') + 1:].split()]
            frame_time = 1.0
            offset = 0
            if len(split) > 0 and split[0].isdigit():
                frame_time = clamp(float(split[0]), 1.0, 1000.0) / 1000.0
            
            if len(split) > 1 and split[1].isdigit():
                offset = clamp(float(split[1]), 1, 1000)

            print("Preparing object sprite, frame time: " + str(frame_time) + " offset: " + str(offset) + "...", end = "")
            succeded = True
            try:
                prepare_object_sprite(paths[0], offset, frame_time)
            except:
                print("FAILED")
                succeded = False
            if succeded:
                print("SUCCEDED")
    elif command.startswith("exit"):
        quit = True
    else:
        print("Invalid command!")
