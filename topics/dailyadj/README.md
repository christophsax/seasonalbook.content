# seasadj

Experiments with daily seasonal adjustment.


Chirstoph Meyer:

In der Anlage findest Du eine csv-Datei, welche sechs Zeitreihen mit Tagesdaten zum Zahlungsverkehr im SIC-System enthält.
Das Datenset umfasst die zehn Jahre 2005-2014 und enthält die Volumen- bzw. Wertebetrachtung («trx_count» bzw. «trx_CHF_sum») jeweils für das Total der SIC-Transaktionen einerseits und für die Interbanken- und Retail-Zahlungen andererseits.

Diese Daten sind bisher höchstens auf Monatsfrequenz und noch nie auf Tagesfrequenz veröffentlicht worden.

Wie besprochen kannst Du aber diese Tagesreihen verwenden, um sie unter Anwendung Deiner Filtermethoden in einen periodischen und unerklärten Teil aufzuteilen. Die beiden original Zeitreihen des Totals (PaymentCategory == "all") wirst Du auch veröffentlichen können. Bevor Du allerdings die vier anderen original Zeitreihen zu den Interbanken- und Retail-Zahlungen veröffentlichst (PaymentCategory %in% c("Interbank", "Retail")), möchten wir die Ergebnisse gerne mit Dir diskutieren.


