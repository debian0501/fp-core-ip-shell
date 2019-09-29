# Eta (Imperative shell /functional core)

## Einleitung
Die funktionale Programmierung verbessert die Software-Qualität durch die Vermeidung von Seiteneffekten. Eine Funktion tut nicht mehr als Parameter entgegenzunehmen einen Algorithmus ausführen und das Ergebnis zurückgeben. So eine Funktion bezeichnet man als pure function. Doch sind in jedem realen Programm auch Seiteneffekte notwendig, wie z.B. Lesen aus der Datenbank, Logging, Ausgaben auf die Konsole usw. Diese Kommunikation mit der Außenwelt wird in der funktionalen Programmierung mittels Monaden gekapselt, damit der Code weiterhin pure bleibt. Das geht soweit, dass für jedes Feature, wie z.B. Lesen, Schreiben, Zustand eine eigene Monade genutzt wird und diese dann aufeinander geschichtet werden. Hört sich kompliziert an, ist es auch. Was in der imperativen Programmierung einfache Anweisungen sind, sind in der funktionalen Programmierung komplexe Strukturen mit oft auch schlechter Laufzeit[^1]. 

So stellt sich die Frage: Wieso nicht die Vorteile beider Paradigmen nutzen und durch eine geeignete Architektur funktionalen von imperativen Code trennen.

## Imperative Shell functional core
Wie könnte eine solche Architektur aussehen? Überlegen wir uns nochmal was die Stärken der beiden Paradigmen sind: 

* Imperative Programmierung
	* Kommunikation mit der Außenwelt, wie z.B. Datenbank, Fremdsysteme, Benutzer
* Funktionale Programmierung
	* Algorithmen, Datentransformation

Der imperative Code ist also unser Draht zur Außenwelt. Er nimmt Informationen entgegen bzw. gibt sie an die Außenwelt weiter. Die Logik unserer Applikation findet sich dagegen im funktionalen Code wieder. Dieser hat keine Verbindung zur Außenwelt. Ist somit in sich gekapselt und kommuniziert mit dem imperativen Code um seine Arbeit verrichten zu können. Bildlich kann man sich dies als einen funktionalen Kern vorstellen, der von einer imperativen Hülle umgeben ist.

## Technologie

## Beispiel 

## Auffälligkeiten


## Referenzen
[^1]: https://wiki.haskell.org/Performance/Monads
https://gist.github.com/kbilsted/abdc017858cad68c3e7926b03646554e
