def sgn(x)
  if x==0
    return 0
  end
  if x<0
    return -1
  else
    return 1
  end
end

def abs(x)
  return x*sgn(x)
end

class Funkcja
  def initialize(a)
    if a.class != Proc
      puts "Blad 'initialize': podaj funkcje. Podano "+a.class.name
      return nil
    end
    @funkcja = a
  end
  
  def value(x)
    if x.class != Float
      puts "Blad 'value': podaj liczbe(float). Podano "+x.class.name
    end
    return @funkcja.call( x)
  end
  
  
  def zerowe(a,b,e)
    if e.class==Fixnum
      e = Float.new e
    end   
    if a.class != Fixnum or b.class != Fixnum or e.class != Float
      puts "Blad 'zerowe': podaj 3 liczby (int,int,float). Podano "+a.class.name+", "+b.class.name+", "+e.class.name
      return
    end
    if a>b
      puts "Blad: podaj poprawny przedzial"
      return
    end
    m_zerowe = []
    sgna = sgn(@funkcja.call(a))
    x = a
    (abs(a-b)/e).to_i.times do
      temp = @funkcja.call( x)
      if sgn(temp)!=sgna 
        m_zerowe +=[x]
        sgna = sgn(temp)
      else
        x+=e
      end
    end
    return m_zerowe
  end
  
  def pole(a,b)
    if a.class != Fixnum or b.class != Fixnum
      puts "Blad 'pole': podaj 2 liczby (int,int). Podano "+a.class.name+", "+b.class.name
      return
    end
    if a>b
      puts "Blad: podaj poprawny przedzial"
      return
    end
    pole = 0
     (abs(a-b)*100).times do
       pole+=(@funkcja.call(a)/100)
       a+=0.01
       end
    return pole.round(2)  
  end
  
  def poch(x)
    if x.class == Fixnum
      x = x.to_f
    end
    if x.class!=Float
      puts "Blad 'poch': podaj liczbe(float). Podano "+ x.class.name
      return
    end
   granica = 0.00001
   return ((@funkcja.call(x+granica) - @funkcja.call(x) )/(granica)).round(2)
  end
  
  def toFile(pathname, przedzialD, przedzialG)
    if pathname.class != String
      puts "Blad: podaj nazwe pliku. Podano "+pathname.class.name
    end
    if przedzialD.class != Fixnum or przedzialG.class != Fixnum
      puts "Blad: podaj przedzial (int,int). Podano " + przedzialD.class.name + ", "+ przedzialG.class.name
    end
    if przedzialD>przedzialG
      puts "Blad: podaj poprawny przedzial"
      return
    end
    pathname+=".txt"
    plik = File.new(pathname, "w")
    wartosci = []
    it = przedzialD
    ymin=0
    ymax=0
    while it < przedzialG do
      val = @funkcja.call(it)
      wartosci+= [val]
      if val < ymin
        ymin = val
      end
      if val > ymax
        ymax = val
      end
      it+=1
    end
    obraz = []
    (ymax-ymin+1).to_i.times {obraz +=[[]]}
    it = przedzialD
    while it<przedzialG do
      obraz[(wartosci[(it-przedzialD)]-ymin)] += [it]
      it+=1
    end
    it = ymin
    while it < ymax do
      obraz[it-ymin].sort!
      it+=1
    end
    ity = ymax
    while ity >= ymin do
      it = przedzialD
      zapis = ""
      licz = 0
      while it <= przedzialG do
        if obraz[ity-ymin][licz] == it
          zapis+="."
          licz+=1
        else 
          zapis+=" "
        end
        it+=1
      end
      plik.puts(zapis)
      ity-=1
    end
  end
end


a_func = Funkcja.new Proc.new {|x| x*x*x}

puts "Poch w 4 "+a_func.poch(4).to_s
puts "Pole miedzy -3 a 3 "+a_func.pole(-3,3).to_s
puts "miejsca zerowe miedzy -1 a 1 z dokl 0.5 "+a_func.zerowe(-1,1,0.5).to_s

a_func.toFile("test",-3,3)
