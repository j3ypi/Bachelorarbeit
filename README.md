# Varianz der Power pseudo-exakter oder konditionaler Tests von Annahmen des Rasch Modells
## Vergleich des Rasch Samplers mit dem Exact Sampler

### Zusammenfassung:
Draxler & Zessin (2015) haben eine Klasse pseudo-exakter oder konditionaler Tests
zur Power-Berechnung von Annahmen des Rasch Modells vorgeschlagen. Zum Simulieren
der für die Power-Berechnung notwendigen Daten bedarf es Sampling-
Algorithmen. Verhelst (2008) hat mit dem Rasch Sampler einen relativ schnellen
Algorithmus entworfen, der die wahre Verteilung mithilfe von Markov Chain Monte
Carlo Prozeduren approximiert. Miller & Harrison (2013) haben mit dem Exact
Sampler einen Algorithmus entwickelt, der die exakte Verteilung abzählen und daraus
ziehen kann. Die Genauigkeit der beiden Sampler wird verglichen, indem potentielle
Einflüsse der Stichprobengröße, DIF-Parameter und Itemschwierigkeit auf die
Genauigkeit der Power-Berechnung untersucht werden. Darüber hinaus werden die
Burn-In Phase und der Step-Parameter als Einflussfaktoren auf den Rasch Sampler
überprüft. Die Genauigkeit der Sampler unterscheidet sich nicht wesentlich. Bei steigender
Stichprobengröße steigt die Power an. Auch bei größeren Modellabweichung
im positiven wie im negativen kann eine höhere Power beobachtet werden. Bei moderater
Itemschwierigkeit ist die Power bei positivem und negativem DIF-Parameter
nahezu gleich groß. Bei Modellabweichung eines leichten Items ist die Power bei positiver
Abweichung größer als bei negativer. Mit einem schwierigen Item ist mit dem
Unterschied, dass die Streeung deutlich höher ausfällt, ein gegensätzlicher Trend zu
beobachten. Weder die Burn-In Phase noch der Step-Parameter hat einen Einfluss
auf die Genauigkeit des Rasch Samplers. Aufgrund von effizienterer Berechnung
sollte in jedem Fall der Rasch Sampler verwendet werden. Die Ergebnisse bezüglich
des Verhaltens der Power unter Variation verschiedener Parameter entsprechen
den Beobachtungen von Draxler & Zessin (2015).


### Abstract:
Draxler & Zessin (2015) have proposed a class of pseudo-exact or conditional tests
for power calculation of assumptions of the Rasch model. Sampling algorithms are
required to simulate the data required for power calculation. Verhelst (2008) has
designed a relatively fast algorithm called the Rasch Sampler, which approximates
the true distribution using Markov Chain Monte Carlo procedures. Miller & Harrison
(2013) have developed an algorithm called the Exact Sampler, which can count the
exact distribution and draw from it. The accuracy of the two samplers is compared by
examining potential influences of sample size, DIF-parameters and item difficulty on
the accuracy of the power calculation. Furthermore, the burn-in phase and the step
parameter are checked as influencing factors on the Rasch Sampler. The accuracy
of the samplers does not differ meaningfully. The power increases with higher sample
size. Also the power increases with larger positive and negative model deviations.
With moderate item difficulty, the power for positive and negative DIF parameters is
almost equal. If an easy item deviates from the model, the power is greater if the deviation
is positive than if the item is negative. With a difficult item, a contrasting trend
can be observed with the difference that the range of the power values is relevantly
higher. Neither the burn-in phase nor the step parameter has any influence on the
accuracy of the Rasch Sampler. Due to more efficient calculation the Rasch Sampler
should be used in any case. The results concerning the behaviour of the power
under variation of different parameters correspond to the observations of Draxler &
Zessin (2015).

**Keywords:** Rasch model, Power, Pseudo-exact tests, Conditional tests, Rasch Sampler, Exact Sampler
