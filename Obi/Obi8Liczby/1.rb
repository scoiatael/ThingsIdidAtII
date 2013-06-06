class Fixnum
  def prime
    a=2
    tab=[1]
    while a<Math.sqrt(self)
      if self%a==0
        tab+=[a, self/a]
      end
      a+=1
    end
    if self%Math.sqrt(self)==0
      tab+=[Math.sqrt(self)]
    end
    return tab
  end
  
  def prime?
    a=2
    while a<=Math.sqrt(self)
      if self%a==0
        return false
      end
      a+=1
    end
    return true
  end
  
  def ack(liczba)
    if self==0
      return liczba+1
    end
    if liczba==0
      return (self-1).ack(1)
    end
    return (self-1).ack(self.ack(liczba-1))
  end
  
  def doskonala?
    temp=0
    self.prime.each{|x| temp+=x}
    if temp==self
      return true
    end
    return false
  end
  
  def to_s
    if self<0
      return "-"+((-1)*self).to_s
    end
    if self<10
      return ["zero","jeden","dwa","trzy","cztery","piec","szesc","siedem","osiem","dziewiec"][self]
    end
    head = (self%10).to_s
    tail = (self/10).to_s
    return tail+" "+head
  end
end


puts(6.prime?)
puts(7.prime?)
puts(144.prime)
puts(6.prime)
puts(123.to_s)
puts(6.doskonala?)