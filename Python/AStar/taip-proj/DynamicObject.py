from utilities import *
from ObjectSprite import *
from AIStates import *

class DynamicObject:
    DEFAULT_HP = 1
    DELTA = 0.05
    KNOCKBACK = 0.5
    
    def __init__(self):
        self._environment = None
        self._position = vec2()
        self._direction = vec2(0, 1).normal()
        self._velocity = 2.0
        self._sq_radius = 0.5

        self._anim_sprite = None
        self._anim_time = 0.0
        self._anim_loop = False
        self._finished = False

        self.hp = DynamicObject.DEFAULT_HP

    def update(self, delta, current):
        self._position += self._direction * self._velocity * delta

    def redraw(self, surface, position, current, frames):
        if(self._anim_sprite != None):
            delta = current - self._anim_time
            if self._anim_loop or delta < self._anim_sprite.get_time():
                self._finished = False
            else:
                delta = self._anim_sprite.get_time() - 0.01
                self._finished = True
            screen_direction = world_to_screen(self._direction).normal()
            screen_direction.y = - screen_direction.y
            angle = angle2(screen_direction)
            frame = self._anim_sprite.get_frame(angle, delta)
            dest = world_to_screen(self._position).floor() - position
            surface_size = vec2(surface.get_size())
            dest.y = surface_size.y - dest.y
            frame_size = self._anim_sprite.get_frame_size()
            depth = dest.y
            # dest -= vec2(floor(frame_size.x * 0.5), floor(frame_size.y * 0.70))
            p1 = dest
            p2 = dest + vec2(frame[0].get_size())
            if not (p2.x < 0 or p2.y < 0 or surface_size.x < p1.x or surface_size.y < p1.y):
                frames.append((frame, depth, dest.intcpl()))

    def animate(self, sprite, current, loop = True):
        self._anim_sprite = sprite
        self._anim_time = current
        self._anim_loop = loop

    def finished(self):
        return self._finished

    def hit_test(self, position):
        delta_x = position.x - self._position.x
        delta_y = position.y - self._position.y
        sq_dist = delta_x * delta_x + delta_y * delta_y
        return sq_dist < self._sq_radius

    def get_position(self):
        return self._position

    def set_position(self, position):
        self._position = position

    def get_environment(self):
        return self._environment

    def set_environment(self, environment):
        self._environment = environment

    def get_icon(size):
        return pygame.Surface(size.intcpl(), SRCALPHA)

    def attack(self, object):
        object.suffer_dmg(DAMAGE_ON_HIT)
        
    def suffer_dmg(self, amount):
        self.hp -= amount
        if self.hp < 0:
            self.hp = 0
            self.die()
            
    def die(self):
        self.dead = True            
        
    def move_to(self, point, delta):
        pos = self.get_position()
        olddir = self._direction
        oldpos = self._position
        self._direction = (-pos + point)
        if self._direction == vec2(0,0):
            self._direction == olddir
        else:
            self._direction = self._direction.normal()
        if dist(pos, point) < DynamicObject.DELTA:
            self.set_position(point)
            return True
        self.set_position(pos + self._direction * self._velocity * delta)
        
        for obj in self.get_environment()._dynamic_objects:
            t = self._sq_radius + obj._sq_radius
            if dist(obj.get_position(), self.get_position()) < t*t and self != obj:
                dest_point = oldpos + (self._position - obj._position)*DynamicObject.KNOCKBACK
                if self.get_environment().reachable(dest_point):
                    self.set_position(oldpos)
                else:
                    self.set_position(dest_point)
                break
                
        return False
        
    def notify(self, message):
        pass

class PlayerObject(DynamicObject):
    ATTACK = ObjectSprite("data/black mage/attack.png")
    INJURED = ObjectSprite("data/black mage/injured.png")
    CASTING = ObjectSprite("data/black mage/casting.png")
    RUNNING = ObjectSprite("data/black mage/running.png")
    #TIPPING = ObjectSprite("data/black mage/tipping over.png")
    STOPPED = ObjectSprite("data/black mage/stopped.png")

    DEFAULT_HP = 100

    def __init__(self):
        super(PlayerObject, self).__init__()
        self.animate(PlayerObject.RUNNING, 0.0)
        self.hp = PlayerObject.DEFAULT_HP
        self.dead = False

    def update(self, delta, current):
        self._direction = rotate2(vec2(1.0, 0.0), current)
        super(PlayerObject, self).update(delta, current)

    def goto(self, position):
        pass

    def cast(self, spell, enemy):
        pass

    def get_icon(size):
        return PlayerObject.STOPPED.get_icon(size)

class MeleeOrc(DynamicObject):
    ATTACK = ObjectSprite("data/ice troll/attack.png")
    INJURED = ObjectSprite("data/ice troll/been hit.png")
    RUNNING = ObjectSprite("data/ice troll/walking.png")
    STOPPED = ObjectSprite("data/ice troll/stopped.png")

    DAMAGE_ON_HIT = 10
    HP = 30
    
    def __init__(self, state = AIPatrol([(1, 1), (2, 3), (5, 6)], 10.0)):
        super(MeleeOrc, self).__init__()
        self._state = state
        self.hp = MeleeOrc.HP
        self.dead = False
        
    def update(self, delta, current):
        self._state = self._state.execute(self, current, delta)
        return self.dead
    
    def get_icon(size):
        return MeleeOrc.STOPPED.get_icon(size)

    def set_State(self,state):
        self._state = state
        
    def get_State(self):
        return self._state
    
    def notify(self, message):
        self.set_State(AIAttack(message, self.get_State()))

def create_ball_sprites(number, radius):
    def lerp(a, b, t):
        return ((1.0 - t) * a[0] + t * b[0], (1.0 - t) * a[1] + t * b[1], (1.0 - t) * a[2] + t * b[2], (1.0 - t) * a[3] + t * b[3])

    FIRST_COLOR = (255, 200, 0, 255)
    LAST_COLOR = (255, 0, 0, 150)
    double_radius = radius + radius
    surface = pygame.Surface((double_radius * number, double_radius), SRCALPHA)
    for i in range(number):
        pygame.gfxdraw.filled_circle(surface, i * double_radius + radius, radius, radius, lerp(FIRST_COLOR, LAST_COLOR, i / (number - 1)))
        pygame.gfxdraw.aacircle(surface, i * double_radius + radius, radius, radius, lerp(FIRST_COLOR, LAST_COLOR, i / (number - 1)))
    return [surface.subsurface((x * double_radius, 0, double_radius, double_radius)) for x in range(number)]

class BallEffect(DynamicObject):
    SPRITES = create_ball_sprites(24, 3)
    TIMEOUT = 0.1
    VELOCITY = 0.5

    class Particle:
        def __init__(self):
            self.reset(-BallEffect.TIMEOUT)

        def reset(self, current):
            self.time = current + (random() - 0.5)
            self.dir = rotate2(vec2(0.0, 1.0), random() * pi * 2) * BallEffect.VELOCITY
            self.pos = vec2(0.0, 0.0)

    def __init__(self, current = 0.0, number = 48):
        super(BallEffect, self).__init__()
        self._particles = [BallEffect.Particle() for x in range(number)]
        self._old_time = current

    def update(self, delta, current):
        super(BallEffect, self).update(delta, current)

    def redraw(self, surface, position, current, frames):
        delta = current - self._old_time
        self._old_time = current
        real_size = BallEffect.SPRITES[0].get_size()
        half_size = vec2(real_size) // 2
        i, s = 0, len(self._particles)
        while i < s:
            if current - self._particles[i].time > BallEffect.TIMEOUT:
                self._particles[i].reset(current)
            else:
                self._particles[i].pos += self._particles[i].dir * delta
            dest = world_to_screen(self._particles[i].pos + self._position) - position - half_size 
            index = int(clamp((current - self._particles[i].time) / BallEffect.TIMEOUT, 0.0, 0.99) * len(BallEffect.SPRITES))
            dest.y = surface.get_size()[1] - dest.y
            frames.append(((BallEffect.SPRITES[index], (0, 0) + real_size), dest.y + 16, dest.intcpl()))
            i += 1

DYNAMIC_OBJECTS = [x[1] for x in getmembers(modules[__name__], lambda member: isclass(member) and member.__module__ == __name__) if x[0] != "DynamicObject" and x[0] != "ParticleEffect"]
