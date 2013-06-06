from utilities import *

GUI_FRAME_COLOR = (255, 0, 0, 255)

class IMGUI:
    FRAME_COLORS = [mul_color(GUI_FRAME_COLOR, (x * 51, x * 51, x * 51, 128)) for x in range(0, 6)]
    TEXT_COLOR = (255, 255, 255, 255)

    def __init__(self, font, background = (127, 0, 0, 255)):
        self._background = background
        self._surface = None
        self._font = font

        self._mouse_pos = vec2(0, 0)
        self._mouse_down = False
        self._enter_down = False

        self._hot_item = 0
        self._active_item = 0
        self._active_param0 = 0
        self._active_param1 = 0
        self._having_focus = 0

        self._chars = []

    def on_event(self, event):
        def is_allowed(letter):
            if letter != "":
                code = ord(letter[0])
                x = event.unicode == "\n" or event.unicode == "\b"
                y = 32 <= code and code < 127
                return x or y
            return False

        if event.type == MOUSEMOTION:
            self._mouse_pos = vec2(event.pos)
        elif event.type == MOUSEBUTTONUP and event.button == 1:
            self._mouse_down = False
        elif event.type == MOUSEBUTTONDOWN and event.button == 1:
            self._mouse_down = True
        elif event.type == KEYDOWN:
            if event.key == K_RETURN:
                self._enter_down = True
            if is_allowed(event.unicode):
                self._chars += event.unicode
        elif event.type == KEYUP and event.key == K_RETURN:
            self._enter_down = False

    def begin(self, surface):
        self._surface = surface

    def end(self):
        if not self._mouse_down:
            self._active_item = 0
            self._active_param0 = 0
        elif self._active_item == 0:
            self._active_item = -1

        self._chars = []
    
    def set_focus(self, id):
        self._having_focus = id

    def has_focus(self, id):
        return self._having_focus == id

    def button(self, id, text, region):
        # check region hit
        if region_hit(region, self._mouse_pos):
            self._hot_item = id
            if self._active_item == 0 and self._mouse_down:
                self._active_item = id

        # render button
        if self._active_item != id:
            self._draw_convex_frame(region)
            draw_text_center(self._surface, self._font, text, IMGUI.TEXT_COLOR, region)
        else:
            self._draw_concave_frame(region)
            draw_text_center(self._surface, self._font, text, IMGUI.TEXT_COLOR, region, (2, 2))

        # has button been clicked
        return self._mouse_down == 0 and self._hot_item == id and self._active_item == id and region_hit(region, self._mouse_pos)

    def switch(self, id, text, region, state):
        # check region hit
        if region_hit(region, self._mouse_pos):
            self._hot_item = id
            if self._active_item == 0 and self._mouse_down:
                self._active_item = id

        # render button
        if not state:
            self._draw_convex_frame(region)
            draw_text_center(self._surface, self._font, text, IMGUI.TEXT_COLOR, region)
        else:
            self._draw_concave_frame(region)
            draw_text_center(self._surface, self._font, text, IMGUI.TEXT_COLOR, region, (2, 2))

        # has button been clicked
        if self._mouse_down == 0 and self._hot_item == id and self._active_item == id and region_hit(region, self._mouse_pos):
            state = not state
        return state

    def slider(self, id, value, region, size = 32):
        UPPER = -1
        THUMB = 0
        LOWER = +1

        # some precomputing
        range = region.h - size * 3
        upper_region = rect(region.x, region.y, region.w, size)
        thumb_region = rect(region.x, region.y + int(range * value) + size, region.w, size)
        lower_region = rect(region.x, region.y + region.h - size, region.w, size)
        middle_region = rect(region.x, region.y + size, region.w, region.h - 2 * size)

        # check region hit
        if region_hit(region, self._mouse_pos):
            self._hot_item = id
            if self._active_item == 0 and self._mouse_down:
                if region_hit(upper_region, self._mouse_pos):
                    self._active_item = id
                    self._active_param0 = UPPER
                elif region_hit(thumb_region, self._mouse_pos):
                    self._active_item = id
                    self._active_param0 = THUMB
                    self._active_param1 = self._mouse_pos.y - thumb_region.y
                elif region_hit(lower_region, self._mouse_pos):
                    self._active_item = id
                    self._active_param0 = LOWER

        # update value
        if self._active_item == id:
            if self._active_param0 == UPPER and self._mouse_down == 0 and region_hit(upper_region, self._mouse_pos):
                value -= 0.05
            elif self._active_param0 == LOWER and self._mouse_down == 0 and region_hit(lower_region, self._mouse_pos):
                value += 0.05
            elif self._active_param0 == THUMB:
                value = (self._mouse_pos.y - region.y - size - self._active_param1) / range
            value = clamp(value)

        # redraw slider
        thumb_region = rect(region.x, region.y + int(range * value) + size, region.w, size)
        self._draw_flat_frame(middle_region)
        self._draw_convex_frame(thumb_region)
        if self._active_item != id or self._active_param0 == THUMB:
            self._draw_convex_frame(upper_region)
            self._draw_convex_frame(lower_region)
        elif self._active_param0 == UPPER:
            self._draw_concave_frame(upper_region)
            self._draw_convex_frame(lower_region)
        elif self._active_param0 == LOWER:
            self._draw_convex_frame(upper_region)
            self._draw_concave_frame(lower_region)

        return value

    def editbox(self, id, buffer, region):
        # update buffer
        if self._having_focus == id:
            for c in self._chars:
                if c == '\b':
                    buffer = buffer[0:-1]
                else:
                    buffer = buffer + c
            

        # redraw editbox
        self._draw_flat_frame(region)
        source = self._font.render(buffer, True, IMGUI.TEXT_COLOR)
        src_size = source.get_size()
        offset_x = 2
        offset_y = (region.h - src_size[1]) // 2
        self._surface.blit(source, (region.x + offset_x, region.y + offset_y), (0, 0, region.w, region.h))

        return buffer

    def item_list(self, id, items, index, position, region, item_size):
        # check region hit
        if region_hit(region, self._mouse_pos):
            self._hot_item = id
            if self._active_item == 0 and self._mouse_down:
                self._active_item = id

        # render item list
        item_number = region.h // int(item_size.y)
        first_item = floor((max(0, len(items) - item_number)) * position)
        for i in range(min(item_number, len(items))):
            item_region = rect(region.x, region.y + i * item_size.y, item_size.x, item_size.y)
            if i + first_item == index: pygame.gfxdraw.box(self._surface, item_region, (0, 255, 0, 127))
            self._surface.blit(items[i + first_item].get_icon(item_size), item_region)

        # has button been clicked
        if self._mouse_down == 0 and self._hot_item == id and self._active_item == id and region_hit(region, self._mouse_pos):
            relative_y = self._mouse_pos.y - region.y
            possible = int(relative_y) // int(item_size.y)
            if possible < min(item_number, len(items)):
                index = possible + first_item
        return index

    def _draw_convex_frame(self, region):
        if isinstance(self._background, pygame.Surface):
            self._surface.blit(self._background, region, region)
        else:
            self._surface.fill(self._background, region)
        pygame.gfxdraw.line(self._surface, region.x + region.w - 1, region.y + region.h - 1, region.x, region.y + region.h - 1, IMGUI.FRAME_COLORS[0])
        pygame.gfxdraw.line(self._surface, region.x + region.w - 1, region.y + region.h - 2, region.x + region.w - 1, region.y, IMGUI.FRAME_COLORS[0])
        pygame.gfxdraw.line(self._surface, region.x + region.w - 2, region.y + region.h - 2, region.x + 1, region.y + region.h - 2, IMGUI.FRAME_COLORS[1])
        pygame.gfxdraw.line(self._surface, region.x + region.w - 2, region.y + region.h - 3, region.x + region.w - 2, region.y + 1, IMGUI.FRAME_COLORS[1])
        pygame.gfxdraw.line(self._surface, region.x + region.w - 3, region.y + region.h - 3, region.x + 2, region.y + region.h - 3, IMGUI.FRAME_COLORS[2])
        pygame.gfxdraw.line(self._surface, region.x + region.w - 3, region.y + region.h - 4, region.x + region.w - 3, region.y + 2, IMGUI.FRAME_COLORS[2])
        pygame.gfxdraw.line(self._surface, region.x + 1, region.y, region.x + region.w - 1, region.y, IMGUI.FRAME_COLORS[5])
        pygame.gfxdraw.line(self._surface, region.x, region.y, region.x, region.y + region.h - 1, IMGUI.FRAME_COLORS[5])
        pygame.gfxdraw.line(self._surface, region.x + 2, region.y + 1, region.x + region.w - 2, region.y + 1, IMGUI.FRAME_COLORS[4])
        pygame.gfxdraw.line(self._surface, region.x + 1, region.y + 1, region.x + 1, region.y + region.h - 2, IMGUI.FRAME_COLORS[4])
        pygame.gfxdraw.line(self._surface, region.x + 3, region.y + 2, region.x + region.w - 3, region.y + 2, IMGUI.FRAME_COLORS[3])
        pygame.gfxdraw.line(self._surface, region.x + 2, region.y + 2, region.x + 2, region.y + region.h - 3, IMGUI.FRAME_COLORS[3])

    def _draw_concave_frame(self, region):
        if isinstance(self._background, pygame.Surface):
            self._surface.blit(self._background, region, region)
        else:
            self._surface.fill(self._background, region)
        pygame.gfxdraw.line(self._surface, region.x + region.w - 1, region.y + region.h - 1, region.x, region.y + region.h - 1, IMGUI.FRAME_COLORS[5])
        pygame.gfxdraw.line(self._surface, region.x + region.w - 1, region.y + region.h - 2, region.x + region.w - 1, region.y, IMGUI.FRAME_COLORS[5])
        pygame.gfxdraw.line(self._surface, region.x + region.w - 2, region.y + region.h - 2, region.x + 1, region.y + region.h - 2, IMGUI.FRAME_COLORS[4])
        pygame.gfxdraw.line(self._surface, region.x + region.w - 2, region.y + region.h - 3, region.x + region.w - 2, region.y + 1, IMGUI.FRAME_COLORS[4])
        pygame.gfxdraw.line(self._surface, region.x + region.w - 3, region.y + region.h - 3, region.x + 2, region.y + region.h - 3, IMGUI.FRAME_COLORS[3])
        pygame.gfxdraw.line(self._surface, region.x + region.w - 3, region.y + region.h - 4, region.x + region.w - 3, region.y + 2, IMGUI.FRAME_COLORS[3])
        pygame.gfxdraw.line(self._surface, region.x + 1, region.y, region.x + region.w - 1, region.y, IMGUI.FRAME_COLORS[0])
        pygame.gfxdraw.line(self._surface, region.x, region.y, region.x, region.y + region.h - 1, IMGUI.FRAME_COLORS[0])
        pygame.gfxdraw.line(self._surface, region.x + 2, region.y + 1, region.x + region.w - 2, region.y + 1, IMGUI.FRAME_COLORS[1])
        pygame.gfxdraw.line(self._surface, region.x + 1, region.y + 1, region.x + 1, region.y + region.h - 2, IMGUI.FRAME_COLORS[1])
        pygame.gfxdraw.line(self._surface, region.x + 3, region.y + 2, region.x + region.w - 3, region.y + 2, IMGUI.FRAME_COLORS[2])
        pygame.gfxdraw.line(self._surface, region.x + 2, region.y + 2, region.x + 2, region.y + region.h - 3, IMGUI.FRAME_COLORS[2])

    def _draw_flat_frame(self, region):
        if isinstance(self._background, pygame.Surface):
            self._surface.blit(self._background, region, region)
        else:
            self._surface.fill(self._background, region)
        pygame.gfxdraw.box(self._surface, region, IMGUI.FRAME_COLORS[4])
