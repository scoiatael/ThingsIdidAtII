from random import randint

def dist(a,b):
    return (a-b).length()
    
class _AIStatePrototype:
    #returns next state and changes object     
    def execute(self, dynamic_object, current, delta):
        pass

        
class AIIdle(_AIStatePrototype):
    def __init__(self, timeout, next_state = None):
        self.timeout = timeout
        self.time = 0.0
        self.next_state = next_state
        
    def execute(self, dynamic_object, current, delta):
        dynamic_object.animate(dynamic_object.STANDING, current)
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
    
    def __init__(self, player, previous_state = AIIdle(100)):
        self.player = player
        self.path = None
        self.last_path_point = None
        self.previous_state = previous_state
        
    def execute(self, dynamic_object, current, delta):
        if dist(self.player.get_position(), dynamic_object.get_position()) > SEARCH_RADIUS:
            return self.previous_state
        if self.path == None or dist(self.player.get_position(), self.last_path_point) > SEARCH_PATH_DIST:
            self.path = dynamic_object.get_environment().findPath(dynamic_object.get_position(), self.player.get_position())
            self.last_path_point = self.path[-1]
        got_there = dynamic_object.move_to(self.path[0])
        if got_there:
            self.path = self.path[1:]
        if len(self.path) < ATTACK_RADIUS:
            dynamic_object.attack(player)
            dynamic_object.animate(dynamic_object.ATTACK, current)
        else:
            dynamic_object.animate(dynamic_object.RUNNING, current)
        return self;
        
class AIPatrol(_AIStatePrototype):
    SPOT_RADIUS = 100
    IDLE_TIMEOUT = 100
    SIGHTSEEING_CHANCE = 5
    SIGHTSEEING_TIMEOUT = 5
    
    def __init__(self, path, timeout):
        self.path = path
        self.cur_seg = 0
        self.timeout = timeout
        self.time = 0.0
        
    def execute(self, dynamic_object,current, delta):
        dynamic_object.animate(dynamic_object.RUNNING, current)
        self.time+=delta
        if self.time == self.timeout:
            return AIIdle(IDLE_TIMEOUT)
        
        if randint(0,SIGHTSEEING_CHANCE) == 0:
            return AIIdle(SIGHTSEEING_TIMEOUT)
            
        for player in dynamic_object.get_envionment().get_player():
            if dist(player.get_position(), dynamic_object.get_position()) > SPOT_RADIUS:
                return AIWarnOthers(self)
        got_there = dynamic_object.move_to(self.path[self.cur_seg])
        
        if got_there:
            self.cur_seg+=1
            if self.cur_seg == len(self.path):
                self.cur_seg = 0
        return self
        
class AIWarnOthers(_AIStatePrototype):
    WARNING_TIMEOUT = 20
    WARNING_CHANCE = 10
    
    def __init__(self, player, previous_state = None):
        self.player = player
        self.time = 0
        self.previous_state = previous_state
        
    def execute(self, dynamic_object, current, delta):
        dynamic_object.animate(dynamic_object.STANDING, current)
        self.time+=delta
        if self.time == WARNING_TIMEOUT:
            return AIAttack(self.player, self.previous_state)
        
        if randint(0,WARNING_CHANCE) == 0:
            dynamic_object.get_environment().notify(player)
        return self
     