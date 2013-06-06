from utilities import *
from Gameplay import *
from Environment import *

EDIT_MODE_STATIC = 0
EDIT_MODE_DYNAMIC = 1
EDIT_MODE_TERRAIN = 2
EDIT_MODE_COLLISIONS = 3

MAX_LEVEL_SIZE = 1024
MIN_LEVEL_SIZE = 32
MAX_BRUSH_SIZE = 32

class EditorGUI:
    GRID_SIZE = 32
    SIDE_HEIGHT = 12
    BOTTOM_HEIGHT = 2

    def __init__(self, size):
        self._imgui = IMGUI(pygame.font.SysFont("Consolas", 16))
        self._edit_mode = EDIT_MODE_TERRAIN
        self._collisions = False
        self._left_slider = 0.0
        self._right_slider = 0.0
        self._left_index = 0
        self._right_index = 0
        self._left_delete = False
        self._right_delete = False
        self._command_line = ""
        self._command_history = ['static "data/tree0.png"', 'load "data\level0.txt"']
        self._command_index = 0
        self._current_command = ""

        self._left_area = rect(0, 0, EditorGUI.GRID_SIZE + TILE_SIZE.x, EditorGUI.GRID_SIZE * EditorGUI.SIDE_HEIGHT)
        self._right_area = rect(size.x - EditorGUI.GRID_SIZE - TILE_SIZE.x, 0, EditorGUI.GRID_SIZE + TILE_SIZE.x, EditorGUI.GRID_SIZE * EditorGUI.SIDE_HEIGHT)
        self._bottom_area = rect(0, size.y - EditorGUI.GRID_SIZE * EditorGUI.BOTTOM_HEIGHT, size.x, EditorGUI.GRID_SIZE * EditorGUI.BOTTOM_HEIGHT)
        
        self._left_static_icons = []
        self._left_dynamic_icons = []
        self._right_icons = []

        self._message = False

    def redraw(self, surface):
        GRID_SIZE = EditorGUI.GRID_SIZE
        imgui = self._imgui
        imgui.begin(surface)
        
        self._left_slider = imgui.slider(1, self._left_slider, rect(self._left_area.x, self._left_area.y, GRID_SIZE, self._left_area.h - GRID_SIZE * 3), GRID_SIZE)
        self._right_slider = imgui.slider(2, self._right_slider, rect(self._right_area.x + TILE_SIZE.x, self._right_area.y, GRID_SIZE, self._right_area.h - GRID_SIZE * 3), GRID_SIZE)
        
        static = imgui.switch(3, "static", rect(self._left_area.x, self._left_area.y + self._left_area.h - GRID_SIZE * 3, self._left_area.w, GRID_SIZE), self._edit_mode == EDIT_MODE_STATIC)
        if static: self._edit_mode = EDIT_MODE_STATIC
        dynamic = imgui.switch(4, "dynamic", rect(self._left_area.x, self._left_area.y + self._left_area.h - GRID_SIZE * 2, self._left_area.w, GRID_SIZE), self._edit_mode == EDIT_MODE_DYNAMIC)
        if dynamic: self._edit_mode = EDIT_MODE_DYNAMIC
        self._left_delete = imgui.switch(5, "delete", rect(self._left_area.x, self._left_area.y + self._left_area.h - GRID_SIZE * 1, self._left_area.w, GRID_SIZE), self._left_delete)

        grid_tile = imgui.switch(6, "grid tile", rect(self._right_area.x, self._right_area.y + self._right_area.h - GRID_SIZE * 3, self._right_area.w, GRID_SIZE), self._edit_mode == EDIT_MODE_TERRAIN)
        if grid_tile: self._edit_mode = EDIT_MODE_TERRAIN
        elif self._edit_mode == EDIT_MODE_TERRAIN:
            self._edit_mode = EDIT_MODE_COLLISIONS

        if self._edit_mode == EDIT_MODE_TERRAIN:
            self._collisions = imgui.switch(7, "collision", rect(self._right_area.x, self._right_area.y + self._right_area.h - GRID_SIZE * 2, self._right_area.w, GRID_SIZE), self._collisions)
        else:
            collision = imgui.switch(7, "collision", rect(self._right_area.x, self._right_area.y + self._right_area.h - GRID_SIZE * 2, self._right_area.w, GRID_SIZE), self._edit_mode == EDIT_MODE_COLLISIONS)
            if collision: self._edit_mode = EDIT_MODE_COLLISIONS

        self._right_delete = imgui.switch(8, "delete", rect(self._right_area.x, self._right_area.y + self._right_area.h - GRID_SIZE * 1, self._right_area.w, GRID_SIZE), self._right_delete)

        imgui.set_focus(9)
        self._command_line = imgui.editbox(9, self._command_line, rect(self._bottom_area.x, self._bottom_area.y + GRID_SIZE, self._bottom_area.w, GRID_SIZE))
        self._right_index = imgui.item_list(10, self._right_icons, self._right_index, self._right_slider, rect(self._right_area.x, self._right_area.y, self._right_area.w - GRID_SIZE, self._right_area.h - GRID_SIZE * 3), TILE_SIZE)
        if self._edit_mode == EDIT_MODE_STATIC:
            self._left_index = clamp(self._left_index, 0, len(self._left_static_icons) - 1)
            self._left_index = imgui.item_list(11, self._left_static_icons, self._left_index, self._left_slider, rect(self._left_area.x + self._left_area.w - GRID_SIZE * 2, self._left_area.y, self._left_area.w - GRID_SIZE, self._left_area.h - GRID_SIZE * 3), TILE_SIZE)
        else:
            self._left_index = clamp(self._left_index, 0, len(self._left_dynamic_icons) - 1)
            self._left_index = imgui.item_list(11, self._left_dynamic_icons, self._left_index, self._left_slider, rect(self._left_area.x + self._left_area.w - GRID_SIZE * 2, self._left_area.y, self._left_area.w - GRID_SIZE, self._left_area.h - GRID_SIZE * 3), TILE_SIZE)
        imgui.end()

    def handle(self, event):
        if (event.type == MOUSEBUTTONUP or event.type == MOUSEBUTTONDOWN or event.type == MOUSEMOTION) and self._gui_hit(vec2(event.pos)):
            self._imgui.on_event(event)
        elif event.type == KEYDOWN or event.type == KEYUP:
            if event.type == KEYDOWN:
                if self._message:
                    self._command_line = ""
                    self._message = False
                if event.key == K_RETURN:
                    self._current_command = self._command_line
                    self._command_index = 0
                    self._command_line = ""
                elif event.key == K_UP and self._command_history != []:
                    self._command_index = min(self._command_index + 1, len(self._command_history))
                    self._command_line = self._command_history[-self._command_index]
                elif event.key == K_DOWN and self._command_history != []:
                    self._command_index = max(self._command_index - 1, 1)
                    self._command_line = self._command_history[-self._command_index]
            self._imgui.on_event(event)
        else:
            return False
        return True

    def get_edit_mode(self):
        return self._edit_mode

    def get_left_delete(self):
        return self._left_delete

    def get_right_delete(self):
        return self._right_delete

    def get_collision(self):
        return self._collisions or self._edit_mode == EDIT_MODE_COLLISIONS

    def get_left_index(self):
        return self._left_index

    def get_right_index(self):
        return self._right_index

    def get_command(self):
        if self._current_command != "":
            command = self._current_command
            self._command_history.append(command)
            self._current_command = ""
            return command
        else:
            return ""

    def set_left_static_icons(self, icons):
        self._left_static_icons = icons

    def set_left_dynamic_icons(self, icons):
        self._left_dynamic_icons = icons

    def set_right_icons(self, icons):
        self._right_icons = icons

    def set_message(self, message):
        self._command_line = message
        self._message = True

    def _gui_hit(self, position):
        return region_hit(self._left_area, position) or region_hit(self._right_area, position) or region_hit(self._bottom_area, position)

class Editor(GameStage):
    GUI_GRID_SIZE = 32
    SIDE_AREA_HEIGHT = 12
    BOTTOM_AREA_HEIGHT = 2

    def __init__(self, screen):
        super(Editor, self).__init__(screen)
        
        self._editor_gui = EditorGUI(vec2(screen.get_size()))
        if self._editor_gui.get_edit_mode() != EDIT_MODE_STATIC:
            self._editor_gui.set_left_dynamic_icons(DYNAMIC_OBJECTS)

        self._viewport = vec2(screen.get_size())
        self._environment = Environment()
        self._position = vec2(10, 10)
        self._rmb_drag = False
        self._lmb_drag = False
        self._exit = False

        self._brush_visible = False
        self._brush_screen_pos = vec2(0, 0)
        self._brush_world_pos = vec2(0, 0)
        self._brush_size = 3
        self._brush_tint = create_color_mask(TILE_SIZE, (0, 0, 255, 127))

        self._terrain_grid_sprites = []
        self._static_object_sprites = []

    def on_event(self, event):
        if not self._rmb_drag and not self._lmb_drag and self._editor_gui.handle(event):
            self._brush_visible = False
            command = self._editor_gui.get_command()
            if command != "":
                self._exec(command)
        else:
            self._brush_visible = True
            if event.type == MOUSEBUTTONDOWN:
                if event.button == 1:
                    self._lmb_drag = True
                    self._edit()
                elif event.button == 3:
                    self._rmb_drag = True
                elif event.button == 4:
                    self._brush_size = min(self._brush_size + 1, MAX_BRUSH_SIZE)
                elif event.button == 5:
                    self._brush_size = max(self._brush_size - 1, 1)
            elif event.type == MOUSEBUTTONUP:
                if event.button == 1:
                    self._lmb_drag = False
                elif event.button == 3:
                    self._rmb_drag = False
            elif event.type == MOUSEMOTION:
                self._update_brush_position(event.pos)
                if self._rmb_drag:
                    relative = vec2(event.rel)
                    relative.x = -relative.x
                    self._position += screen_to_world(relative)
                else:
                    self._brush_visible = True
                    if self._lmb_drag:
                        self._edit()
        
        return self._exit

    def on_update(self, delta, current):
        self._environment.update(delta, current)
        return self._exit

    def on_redraw(self, surface, delta, current):
        self._environment.redraw(surface, self._position, current, self._editor_gui.get_collision())
        
        if self._brush_visible and not self._rmb_drag and self._editor_gui.get_edit_mode() != EDIT_MODE_DYNAMIC:
            first = -self._brush_size // 2 + 1
            last = first + self._brush_size
            for v in self._brush_viewport():
                surface.blit(self._brush_tint, v.intcpl())

        self._editor_gui.redraw(surface)

    def _edit(self):
        edit_mode = self._editor_gui.get_edit_mode()
        if edit_mode == EDIT_MODE_STATIC and len(self._static_object_sprites) > 0:
            size = vec2(self._brush_size, self._brush_size)
            position = self._brush_world_pos - size * 0.5
            if self._editor_gui.get_left_delete():
                self._environment._static_objects.del_objects(position, size)
            else:
                self._environment._static_objects.add_objects(position, size, self._static_object_sprites[self._editor_gui.get_left_index()], 1)
        elif edit_mode == EDIT_MODE_DYNAMIC:
            if self._editor_gui.get_left_delete():
                i, s = 0, len(self._environment._dynamic_objects)
                while i < s:
                    if self._environment._dynamic_objects[i].hit_test(self._brush_world_pos):
                        del self._environment._dynamic_objects[i]
                        s -= 1
                    else:
                        i += 1
            else:
                object = DYNAMIC_OBJECTS[self._editor_gui.get_left_index()]()
                object.set_position(self._brush_world_pos.copy())
                object.set_environment(self._environment)
                self._environment._dynamic_objects.append(object)
        elif edit_mode == EDIT_MODE_TERRAIN and self._terrain_grid_sprites != []:
            terrain_grid_size = self._environment._terrain_grid.get_size()
            for v in self._brush_world():
                if 0 <= v.x and v.x < terrain_grid_size.x and 0 <= v.y and v.y < terrain_grid_size.y:
                    self._environment._terrain_grid.set_tile(v, self._terrain_grid_sprites[self._editor_gui.get_right_index()])
        if self._editor_gui.get_collision():
            terrain_grid_size = self._environment._terrain_grid.get_size()
            for v in self._brush_world():
                if 0 <= v.x and v.x < terrain_grid_size.x and 0 <= v.y and v.y < terrain_grid_size.y:
                    self._environment._terrain_grid.set_flags(v, not self._editor_gui.get_right_delete())

    def _exec(self, command):
        if command.startswith("load "):
            paths = extrude_paths(command)
            loader = TxtLevelLoader()
            result = True
            try:
                loader.load(paths[0])
            except:
                result = False
            
            if result:
                self._environment.load(loader)
                self._terrain_grid_sprites = loader.get_terrain_grid_sprites()
                self._editor_gui.set_right_icons(self._terrain_grid_sprites)
                self._static_object_sprites = loader.get_static_object_sprites()
                self._editor_gui.set_left_static_icons(self._static_object_sprites)
                self._editor_gui.set_message("DONE")
            else:
                self._editor_gui.set_message("FAIL")

        elif command.startswith("save "):
            paths = extrude_paths(command)
            loader = TxtLevelLoader()
            self._environment.save(loader)
            result = True
            try:
                loader.save(paths[0])
            except:
                result = False
            if result:
                self._editor_gui.set_message("DONE")
            else:
                self._editor_gui.set_message("FAIL")

        elif command.startswith("static "):
            paths = extrude_paths(command)
            objects = []
            try:
                for path in paths:
                    objects.append(ObjectSprite(path))
            except:
                objects = []

            if objects != []:
                self._static_object_sprites += objects
                self._editor_gui.set_left_static_icons(self._static_object_sprites)
                self._editor_gui.set_message("DONE")
            else:
                self._editor_gui.set_message("FAIL")

        elif command.startswith("tile "):
            paths = extrude_paths(command)
            tiles = []
            try:
                for path in paths:
                    tiles.append(TileSprite(path))
            except:
                tiles = []

            if tiles != []:
                self._terrain_grid_sprites += tiles
                self._editor_gui.set_right_icons(self._terrain_grid_sprites)
                self._editor_gui.set_message("DONE")
            else:
                self._editor_gui.set_message("FAIL")

        elif command.startswith("resize "):
            split = [x.strip() for x in command.split()]
            if len(split) == 3 and split[1].isdigit() and split[2]:
                width = int(split[1])
                height = int(split[2])
                self._environment.resize(vec2(width, height))
                self._editor_gui.set_message("DONE")
            else:
                self._editor_gui.set_message("FAIL")

        elif command.startswith("exit"):
            self._exit = True

        else:
            self._editor_gui.set_message("UNKNOWN COMMAND")

    def _update_brush_position(self, position):
        screen_position = vec2(position)
        screen_position.y = self._viewport.y - screen_position.y
        screen_position -= self._viewport * 0.5

        world_position = screen_to_world(screen_position) + self._position
        self._brush_world_pos.x = world_position.x
        self._brush_world_pos.y = world_position.y

        screen_position = world_to_screen(self._brush_world_pos.floor() - self._position)
        self._brush_screen_pos.x = int(screen_position.x + self._viewport.x * 0.5)
        self._brush_screen_pos.y = int(screen_position.y + self._viewport.y * 0.5)

    def _brush_world(self):
        first = -self._brush_size // 2 + 1
        last = first + self._brush_size
        for x in range(first, last):
            for y in range(first, last):
                yield vec2(x, y) + self._brush_world_pos.floor()

    def _brush_viewport(self):
        first = -self._brush_size // 2 + 1
        last = first + self._brush_size
        for x in range(first, last):
            for y in range(first, last):
                position = self._brush_screen_pos + world_to_screen(vec2(x, y))
                position.y = self._viewport.y - position.y - TILE_SIZE.y * 0.5
                yield position

screen = initialize_pygame(vec2(800, 600), False)
# screen = initialize_pygame(vec2(0, 0), True)
editor = Editor(screen)
editor.run()
