Elektrownia wiatrowa
=====

Prosty model elektrowni wiatrowej na uczelniany przedmiot. Realizuje wzorzec projektowy producent-konsument.

Użycie
-----

    $ rebar3 shell
    1> application:start(powerhouse).

Opis projektu
-----

Projekt składa się z czterch modułów: windmill, powerhouse, powerhouse_sup i client. Każdy z nich udostępnia swoje api.

**windmill.erl** - moduł stworzony na bazie zachowania gen_server. Udostępnia włączanie i wyłączanie wiatraków. Znajdują się w nim funkcje, które w pętli co 2 sekundy produkują nowy stan, który następnie jest niejako "zbierany" przez elektrownię.

**powerhouse.erl** - moduł stworzony na bazie zachownia gen_server. Jest odpowiedzialny za zbieranie produkowanej energii i przechowywanie jej w swoim stanie.

**powerhouse_sup.erl** - moduł stworzony na bazie zachowanie supervisor. Odpowiada za stworzenie wiatraków. W niej można zmienić ich ilość w funkcji windmills() (domyślnie 10).

**client.erl** - moduł symulujący klienta. Pobiera z elektrowni losową ilość energii


API dla elektrowni
-----
Zachęcam do otworzenia dokumentacji z folder /doc

Wyświetlanie aktualnego stanu elektrowni w formacie {Zgromadzona energia, ilość wiatraków}

    powerhouse:state().

Odblokowywanie wiatraków (automatycznie po włączeniu)

    powerhouse:windmills_on().

Zatrzymywanie wiatraków (brak produkcji energii)

    powerhouse:windmills_off().

Tworzenie N nowych klientów - zwraca listę pidów

    Client = powerhouse:new_clients(N).

Polecam również stosowanie observer:start() w celu rozwiania wszelakich wątpliwości.

