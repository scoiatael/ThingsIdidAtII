class Node
  @next = nil
  @prev = nil
  def initialize(wart)
    @wartosc = wart
  end
  
  def wartosc(wart)
    @wartosc = wart
  end

  def prev(point)
    @prev = point
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
  
  def get_prev
    return @prev
  end
end

class Kolekcja
  def initialize
    @first = nil
    @last = nil
    @size = 0
    @last_search_iter = 0
    @last_search_point = nil
  end
  
  def dodaj_el(element)
    @size+=1
    if @size==1
      @first = @last = Node.new(element)
      return true
    end
    if element < @first.get_val
      element = Node.new(element)
      element.next(@first)
      @first.prev(element)
      @first = element
      return true
    else
      it  = @first.get_next
      while it != nil
        if it.get_val > element
          temp = Node.new(element)
          temp.prev(it.get_prev)
          temp.get_prev.next(temp)
          temp.next(it)
          it.prev(temp)
          return true
        else 
          it = it.get_next
        end
      end
    end
    @last.next(Node.new(element))
    @last.get_next.prev(@last)
    @last = @last.get_next
    return true
  end
  
  def get_el(i)
    if i.class != Fixnum or i<0
      print "Error: podaj poprawny iterator. Podano"
      puts i.class.name + " " + i.to_s
      return
    end
    old = i
    temp = @first
    while temp != nil
      if i == 0
        return temp
      end
      i-=1
      temp = temp.get_next
    end
    print "Error: podaj poprawny iterator. Podano: "
    puts old
    self.drukuj
    return 
  end
  
  def get(i)
    temp = get_el(i)
    if temp!=nil
      return temp.get_val
    end
  end
  
  def length
    return @size
  end
  
  def drukuj
    0.upto(self.length-1) do |x| 
      print x.to_s + ": " + self.get(x).to_s
      print ", " 
    end
    puts ""
  end
end

class Tablica
  def initialize(tab)
    @T = tab.sort
  end
  
  def get(i)
    return @T[i]
  end
  
  def length
    return @T.length
  end
  
  def to_s
    return @T.to_s
  end
end

class Wyszukiwanie
  def Wyszukiwanie.bin_search(kol,szukana)
    if !kol.respond_to?("get") or !kol.respond_to?("length")
      print "Error: podaj poprawna kolekcje. Podano"
      puts kol
      return
    end
    pocz = 0
    kon= kol.length-1
    op = 0
    while pocz < kon-5
      op+=1
      temp = ((kon + pocz)/2).truncate
      if kol.get(temp) == szukana
        puts op
        return temp
      end
      if kol.get(temp) > szukana
        kon = temp
      else
        pocz = temp 
      end
    end
    pocz.upto(kon) {|x| if kol.get(x)==szukana
                              op+=1
                             puts op 
                            return x 
                        end}
    puts "Error: not found"
  end
  
  def Wyszukiwanie.interpol_search(kol, szukana)
    if !kol.respond_to?("get") or !kol.respond_to?("length")
      print "Error: podaj poprawna kolekcje. Podano"
      puts kol
      return
    end
    pocz = 0
    kon= kol.length-1
    op = 0
    while pocz<kon-5
      op+=1
      temp = pocz + (((szukana - kol.get(pocz))*(kon-pocz)).to_f / (kol.get(kon)-kol.get(pocz)))
      temp = temp.truncate
      if temp < pocz or temp > kon
        break
      end
      if kol.get(temp) == szukana
        puts op
        return temp
      end
      if kol.get(temp) > szukana
        kon = temp
      end
      if kol.get(temp) < szukana
        pocz = temp 
      end
    end
    pocz.upto(kon){|x| if kol.get(x)==szukana
                          op+=1
                          puts op
                         return x
                       end}
    puts "Error: not found"
  end
end

y=[]
0.upto(100) {y+=[Random.rand(100)]}
x = Random.rand(100)
kol = Kolekcja.new()
kol2 = Tablica.new(y)
y.each {|x| kol.dodaj_el(x)}
puts "Test na liscie dwukierunkowej"
kol.drukuj
temp1 = Wyszukiwanie.interpol_search(kol,x)
if temp1!=nil
  puts "Interpol Search: kol[" + temp1.to_s + "]=" + kol.get(temp1).to_s
end
temp2 = Wyszukiwanie.bin_search(kol,x)
if temp2!=nil
  puts "Bin Search: kol[" + temp2.to_s + "]=" + kol.get(temp2).to_s
end
puts "Test na tablicy"
puts kol2
temp1 = Wyszukiwanie.interpol_search(kol2,x)
if temp1!=nil
  puts "Interpol Search: kol2[" + temp1.to_s + "]=" + kol2.get(temp1).to_s
end
temp2 = Wyszukiwanie.bin_search(kol2,x)
if temp2!=nil
  puts "Bin Search: kol2[" + temp2.to_s + "]=" + kol2.get(temp2).to_s
end