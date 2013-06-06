'''
Created on Jun 4, 2012

@author: Mobile
'''
from queue import PriorityQueue

class AStarTest:
    def __init__(self):
        self.terrainGrid = [[1,1,1,1,1,0,1,1,1,0],
                            [1,1,1,1,1,0,1,1,1,0],
                            [1,1,1,1,1,0,1,1,1,0],
                            [1,1,1,1,1,0,1,1,1,0],
                            [1,0,1,0,0,0,1,1,1,0],
                            [1,0,1,1,1,0,1,1,1,0],
                            [1,0,1,1,0,1,1,1,1,0],
                            [1,0,1,1,0,1,1,1,1,0],
                            [1,0,1,1,1,0,1,1,1,0],
                            [1,1,1,1,1,0,1,1,1,1]]
        
    def findPath(self, startPoint, endPoint):
        
        def pairPlus(pair1, pair2):
            a1,b1 = pair1
            a2,b2 = pair2
            return (a1+a2, b1+b2)
    
        def dist(pair1, pair2):
            a1,b1 = pair1
            a2,b2 = pair2
            return ((abs(a2-a1)+abs(b2-b1)))
        
        def seqPlus(table, pair):
            tableNew = table[:]
            tableNew.append(pair)
            return tableNew
         
        def findPathAux(self, startPoint, endPoint):
       
            def reachable(point, mapping):
                x,y = point
                if x >=0 and y>=0 and y<len(mapping) and x< len(mapping[y]):
                    return mapping[y][x]
                return False
            
            reached = set()
            unchecked = PriorityQueue()
            
            moves = {(i,j) for i in [-1,0,1] for j in [-1,0,1]}
            moves.remove((0,0))
            
            count=0
            unchecked.put((0,startPoint,[]))
            while not unchecked.empty():
                _, point, seq = unchecked.get()
                count+=1
                if point == endPoint:
                    print(count)
                    return seq
                if not point in reached:
                    reached.add(point)
                    for move in moves:
                        pointNew = pairPlus(point, move)
                        if reachable(pointNew, self.terrainGrid):
                            seqNew = seqPlus(seq, move)
                            unchecked.put((dist(pointNew, endPoint), pointNew, seqNew))
                            
        seq = findPathAux(self, startPoint, endPoint)
        print(seq)
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
        
t = AStarTest()                    
print(t.findPath((0,0), (9,9)))