class BSTNode
  
  def initialize(wartosc,numer)
    @value = wartosc
    @left = nil
    @right = nil
    @it = numer
  end
   
  def wstaw(wartosc,numer)
    if wartosc >= @value
      if @right == nil
        @right = BSTNode.new(wartosc,numer)
      else
        @right.wstaw(wartosc,numer)
      end
    else
      if @left == nil
        @left = BSTNode.new(wartosc,numer)
      else
        @left.wstaw(wartosc,numer)
      end
    end
  end
  
  def drukuj
    temp = []
    if @left != nil
      temp += @left.drukuj
    end
    temp += [@it]
    if @right != nil
      temp+=@right.drukuj
    end
    return temp
  end
  
end


class Node
  @next = nil
  def initialize(wart)
    @wartosc = wart
  end
  
  def wartosc(wart)
    @wartosc = wart
  end

  def next(point)
    @next = point
  end

  def get_val
    return @wartosc
  end
  
  def get_next
    return @next
  end
end
    
class Kolekcja
  def initialize
    @last = nil
    @begin = nil
    @size = 0
  end
    
  def dodaj_el(el)
    if @begin != nil
      @last.next(Node.new(el))
      @last = @last.get_next()
    else
      @last = @begin = Node.new(el)
    end
    @size+=1
  end
  
  def length
    return @size
  end
  
  def get_el(i)
    if i.class != Fixnum or i<0
      print "Error: podaj poprawny iterator. Podano"
      puts i
      return
    end
    temp = @begin
    while temp != nil
      if i == 0
        return temp
      end
      i-=1
      temp = temp.get_next
    end
    print "Error: podaj poprawny iterator. Podano: "
    puts i
    self.drukuj
    return 
  end
  
  def get(i)
    return get_el(i).get_val
  end
  
  def swap(i,j)
    el1 = self.get_el(i)
    el2 = self.get_el(j)
    temp = el1.get_val
    el1.wartosc(el2.get_val)
    el2.wartosc(temp)
  end
  
  def drukuj
    print "["
    it = @begin
    while it.get_next != nil
      print it.get_val
      print ", "
      it = it.get_next
    end
    print it.get_val
    puts "]"
  end
end
    

class Sortowanie
  
  def Sortowanie.bubble_sort(kol)
    if !kol.respond_to?("get") or !kol.respond_to?("swap")
      puts "Error: zla kolekcja"
      return
    end
    ok = false
    while !ok
      ok = true
      (kol.length-2).times do |x|   
                              if kol.get(x)> kol.get(x+1) 
                                ok = false 
                                kol.swap(x,(x+1)) 
                              end 
                           end                                                       
    end
  end
  
  def Sortowanie.heap_sort(kol)
    if !kol.respond_to?("get") or !kol.respond_to?("swap")
      puts "Error: zla kolekcja"
    end
    tree = BSTNode.new(kol.get(0),0)
    it = 1
    koniec = kol.length-1
    it.upto(koniec) do |x|
      tree.wstaw(kol.get(x),x)
    end
    efekt = tree.drukuj
    0.upto(koniec) do |x|
       if efekt[x]>x
        kol.swap(efekt[x], x)
       end 
       end
  end 
end


k = Kolekcja.new
0.upto(100) {|x| k.dodaj_el(Random.rand(100))}
k2 = k.clone
t = Time.now.to_f
Sortowanie.heap_sort(k)
puts Time.now.to_f-t
t = Time.now.to_f
Sortowanie.bubble_sort(k2)
puts Time.now.to_f-t
k.drukuj
k2.drukuj