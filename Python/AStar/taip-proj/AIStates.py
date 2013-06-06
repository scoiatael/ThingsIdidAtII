from utilities import *

def dist(a,b):
    return (a-b).length()
    
class MonsterTypes:
    Melee = "Melee"
    Ranged = "Ranged"
    Shaman = "Shaman"
   
    
class _AIStatePrototype:
    def __init__(self, monster_type = MonsterTypes.Melee, params = []):
        pass
    #returns next state and changes object     
    def execute(self, dynamic_object, current, delta):
        pass

        
class AIIdle(_AIStatePrototype):
    def __init__(self, timeout = 100, monster_type = MonsterTypes.Melee, next_state = None):
        self.timeout = timeout
        self.time = 0.0
        self.next_state = next_state
        
    def execute(self, dynamic_object, current, delta):
        if dynamic_object._anim_sprite != dynamic_object.STOPPED:
            dynamic_object.animate(dynamic_object.STOPPED, current)
        self.time+=delta
        if self.time == self.timeout:
            if next_state == None:
                self.time = 0.0
                return self
            return self.next_state
        return self
        
class AIAttack(_AIStatePrototype):    
    SEARCH_PATH_DIST = 4
    SEARCH_RADIUS = 100
    ATTACK_RADIUS = 3
    
    def __init__(self, player, monster_type = MonsterTypes.Melee, previous_state = AIIdle()):
        self.player = player
        self.path = []
        self.last_path_point = None
        self.previous_state = previous_state
        
    def execute(self, dynamic_object, current, delta):
        if dist(self.player.get_position(), dynamic_object.get_position()) > SEARCH_RADIUS:
            return self.previous_state
        if self.path == [] or dist(self.player.get_position(), self.last_path_point) > SEARCH_PATH_DIST:
            self.path = dynamic_object.get_environment().findPath(dynamic_object.get_position(), self.player.get_position())
            self.last_path_point = self.path[-1]
        got_there = dynamic_object.move_to(vec2(self.path[0]), delta)
        if got_there:
            self.path = self.path[1:]
        if len(self.path) < ATTACK_RADIUS:
            dynamic_object.attack(player)
            if dynamic_object._anim_sprite != dynamic_object.ATTACK:
                dynamic_object.animate(dynamic_object.ATTACK, current)
        elif dynamic_object._anim_sprite != dynamic_object.RUNNING:
            dynamic_object.animate(dynamic_object.RUNNING, current)
        return self;
        
class AIPatrol(_AIStatePrototype):
    SPOT_RADIUS = 100
    IDLE_TIMEOUT = 0
    SIGHTSEEING_CHANCE = 100000000000
    SIGHTSEEING_TIMEOUT = 5
    
    def __init__(self, path, timeout = 100, monster_type = MonsterTypes.Melee,):
        self.path = path
        self.cur_seg = 0
        self.timeout = timeout
        self.time = 0.0
        
    def execute(self, dynamic_object, current, delta):
        if dynamic_object._anim_sprite != dynamic_object.RUNNING:
            dynamic_object.animate(dynamic_object.RUNNING, current)
        self.time+=delta
        if self.time == self.timeout:
            return AIIdle(AIPatrol.IDLE_TIMEOUT)
        
        if randint(0, AIPatrol.SIGHTSEEING_CHANCE) == 0:
            return AIIdle(AIPatrol.SIGHTSEEING_TIMEOUT)
            
        for player in dynamic_object.get_environment().get_players():
            if dist(player.get_position(), dynamic_object.get_position()) > AIPatrol.SPOT_RADIUS:
                return AIWarnOthers(self)

        got_there = dynamic_object.move_to(vec2(self.path[self.cur_seg]), delta)
        
        if got_there:
            self.cur_seg+=1
            if self.cur_seg == len(self.path):
                self.cur_seg = 0
        return self
        
class AIWarnOthers(_AIStatePrototype):
    WARNING_TIMEOUT = 20
    WARNING_CHANCE = 10
    
    def __init__(self, player, monster_type = MonsterTypes.Melee, previous_state = None):
        self.player = player
        self.time = 0
        self.previous_state = previous_state
        
    def execute(self, dynamic_object, current, delta):
        if dynamic_object._anim_sprite != dynamic_object.STOPPED:
            dynamic_object.animate(dynamic_object.STOPPED, current)
        self.time+=delta
        if self.time == AIWarnOthers.WARNING_TIMEOUT:
            return AIAttack(self.player, self.previous_state)
        
        if randint(0, AIWarnOthers.WARNING_CHANCE) == 0:
            dynamic_object.get_environment().notify(player)
        return self
     