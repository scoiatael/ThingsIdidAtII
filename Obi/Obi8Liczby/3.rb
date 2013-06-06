class Jawna < String
  def zaszyfruj(klucz)
    return Zaszyfrowane.new(self.split("").map{|x| ((x.ord+klucz)%128).chr }.join("") )
  end
end

class Zaszyfrowane < String
  def odszyfruj(klucz)
    return Jawna.new(Jawna.new(self).zaszyfruj(-klucz)) 
  end
end

puts Jawna.new("abc").zaszyfruj(3).odszyfruj(3)
