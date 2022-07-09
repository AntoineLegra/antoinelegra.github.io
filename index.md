# Die 2 Pieces MTools

Anton Legra, vom 2022 07 02.

Die ›2 Pieces MTools‹ waren ursprünglich vier kleine Stücke 64 Bit-Software, die die Möglichkeiten der Programmentwicklung mit der freien Software Lazarus und FreePascal ausloten sollten.

Ein herausragendes Merkmal von Lazarus und FreePascal ist, dass sie nicht nur unter MS Windows X, sondern auch unter anderen Betriebssystemen laufen oder dass man mit ihnen per Cross-Compiling Code für andere Betriebssysteme erzeugen kann. In Anwendung wurde hier allerdings nur die Möglichkeit gebracht, den selben Quelltext sowohl unter MS Windows X und Linux — in diesem Fall seit Fedora 31 — sowie in verschiedenen Monitor-Auflösungen (Stichwort HiDPI) einzusetzen und zum Laufen zu bringen.

Seit Anfang 2022 liegt der Schwerpunkt der Entwicklung auf Linux — WinX läuft nur noch so nebenher. Meist kompiliere ich die WinX-Version einfach aus den eins zu eins übertragenen Linux-Sources. Die ehemaligen ›4 Pieces MTools‹ bestehen daher auch nur noch aus den Programmen:

Die ›2 Pieces MTools‹ bestehen nur noch aus den Programmen:

- Compare & Backup
- Kalender mit Datenbank

Der Download erfolgt über GitHub -- siehe den Link oben.

## Compare & Backup
Compare & Backup, kurz MCmpBack, ist ein reines Backup-Programm, das in einer Zwei-Fenster-Ansicht ein halbautomatisches Backup von der Festplatte auf Sicherungsmedien wie externe Festplatten, SDcards oder USBsticks bietet. Basis dafür sind Profile, in denen Quell- und Ziel-Laufwerk sowie Dateifilter vorab festgelegt werden.

## Kalender mit Datenbank
Der Kalender mit Datenbank, kurz MKalender, ist ein kleines Kalender-Tool mit Datenbank-Anbindung. Hierfür wurde u.a. eine eigene Kalender-Komponente entwickelt und mit der SQLite-Unterstützung von Lazarus experimentiert. Es wurden Feiertagsberechnungen implementiert, Suchmöglichkeiten sowie Kontakt eingebaut.

