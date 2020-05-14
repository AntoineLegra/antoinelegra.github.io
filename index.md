# Die 4 Pieces MTools

Anton Legra, vom 2020 05 14.

Die ›4 Pieces MTools‹ sind vier kleine Stücke Software, die die Möglichkeiten der freien Software Lazarus und FreePascal ausloten sollen.

Ein herausragendes Merkmal von Lazarus und FreePascal ist, dass sie nicht nur unter MS Windows X, sondern auch unter anderen Betriebssystemen laufen oder man mit ihnen per Cross-Compiling Code für andere Betriebssysteme erzeugen kann. In Anwendung wurde hier allerdings nur die Möglichkeit gebracht, den selben Quelltext sowohl unter MS Windows X und Linux — in diesem Fall Fedora 31/32 — sowie in verschiedenen Monitor-Auflösungen (Stichwort HiDPI) einzusetzen und zum Laufen zu bringen.

Die ›4 Pieces MTools‹ bestehen aus den Programmen:

- Find & Backup
- Compare & Backup
- Kalender mit Datenbank
- Bilderalbum mit Datenbank

## Find & Backup 
Find & Backup, oder kurz [MFndBack](_include/MFndBack.7z), ist eine reine Studie aus der Zeit, als der Autor anfing, sich mit Lazarus und FreePascal zu beschäftigen. Aus dieser Studie heraus entstand dann das Zwei-Fenster-Tool Compare & Backup, das der Autor als Backup-Programm auf seinen PCs unter MS Windows X und Fedora Linux einsetzt.

## Compare & Backup
Compare & Backup, kurz [MCmpBack](_include/MCmpBack.7z), ist ein reines Backup-Programm, das in einer Zwei-Fenster-Ansicht ein halbautomatisches Backup von der Festplatte auf Sicherungsmedien wie externe Festplatten, SDcards oder USBsticks bietet. Basis dafür sind Profile, in denen Quell- und Ziel-Laufwerk sowie Dateifilter vorab festgelegt werden.

## Kalender mit Datenbank
Der Kalender mit Datenbank, kurz MKalender, ist ein kleines Kalender-Tool mit Datenbank-Anbindung. Hierfür wurde u.a. eine eigene Kalender-Komponente für die Lazarus ToolPalette entwickelt und mit der SQLite-Unterstützung von Lazarus experimentiert. Es wurden Feiertagsberechnungen implementiert, Suchmöglichkeiten sowie Kontakt eingebaut.

## Bilderalbum mit Datenbank
Das Bilderalbum mit Datenbank, kurz [MPicAlb](https://github.com/AntoineLegra/antoinelegra.github.io/_index/MPicAlb.7z), ist nicht nur der Versuch, mit Bildern zu arbeiten, sondern auch, die Bilder nachträglich in eine gewisse Ordnung zu bekommen, sie mit Kategorien und Notizen zu versehen, um dann danach suchen oder sie gruppieren zu können.
