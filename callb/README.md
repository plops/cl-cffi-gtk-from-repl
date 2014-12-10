In `calib.c` befindet sich eine kleine Library, die eine Information
`val` in einem `struct T` abspeichert. Gleichzeitig enthaelt diese
Datenstruktur noch Zeiger auf zwei Methoden-Funktionen `set` und
`callb`.

Das ungefaehr so, wie auch GObject seine Daten vorhaelt. Mein Ziel ist
mit minimalen Aufwand die Methoden-Funktionen von Common Lisp aus
mittels CFFI aufzurufen. Dafuer muss der Datentyp `struct t` auch in
Lisp definiert werden. In der Callbackfunktion `my-callb` kann auf die
Variable innerhalb der Datenstruktur mit `foreign-slot-value`
zugegriffen werden.

