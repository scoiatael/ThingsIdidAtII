Kod napisany do tej pracowni mozna podzielic na 2 grupy:
I. Plik StawyCli.hs zawierajacy zgodne ze specyfikacja rozwiazanie problemu: wczytuje dane z zadanego pliku i wypisuje liste rozwiazan do standardowego wyjscia. Obsluguje zarowno podanie sciezki jako argumentu, jak i wpisanie jej do standardowego wejscia. W czasie rozwiazywania wypisuje '.' co krok (aby pokazac ze pracuje i nie jest zawieszony). Wszystkie komunikaty sa wypisywania do stderr, a lista rozwiazan do stdout: tak wiec mozna go sprawdzac jak wszystkie inne programy. Tylko ten plik zostal szczegolowo okomentowany.

II. Pozostale pliki (stawyGUI.hs, stawy.cabal, stawy.glade, Setup.hs): pozwalaja one skompliowac i uruchomic program w trybie GUI, jednak wymaga to dodatkowych blibliotek (gtk i glade). Instalacja i uruchomienie przebiega przy uzyciu narzedzia cabal, ktore standardowo jest dodawane do dystrybucji ghci. nalezy wydac polecenia:
$ runghc Setup configure 
--tutaj bedzie lista pakietow do pobrania, prawdopodobnie gtk i glade
$ cabal install gtk glade (i ewentualnie inne pakiety)
$ runghc Setup configure {--user} 
--user jesli pakiety zostaly zainstalowane jako zwykly uzytkownik
$ runghc Setup build
$ runghc Setup install
--nastepnie mozna uruchomic program ze sciezki do ktorej zostal zainstalowany


**Kod daje sie latwo przyspieszyc, jednak wymagaloby to uzycia dodatkowych bibliotek (ktore trzeba by sciagac, podobnie jak gtk i glade), np vector. Stwierdzilem ze bazowa czesc rozwiazania nie powinna z nich korzystac wiec ich nie uzylem. Z kolei GUI jest bardzo lekka nakladka na modul rozwiazujacy, wiec nie bylo sensu tylko dla niego przepisywac polowy kodu. 
