Used car valuation is easy for professional dealers, but deciding whether a price is fair or not presents a major problem for most buyers looking to buy a car e.g. at an online marketplace.
In order improve the experience users have on mobile.de, we aim to display a "mobile.de market price" on any listing page for a used car. To achive this, an automatic valuation of used cars needs to be set up. 

You have data from the mobile.de listing data base. It contains data from each listing as well as data about the car to be sold. There are two data sets, test & train.
Their columns are specifically:
- category (which type of car, e.g. limousine, Estate Car...)
- make_id (one per make, e.g. VW, Audi...)
- model_id (e.g. Golf, A3...)
- price ("price__consumer_gross_euro")
- mileage
- first registration
- condition (used, new, etc..)
- modification time (when was the listing deleted from the market place?)
- a number of features (several columns) 

Please predict the prices in the test set. Include an easily interpretable evaluation of the quality of your model (imagine you need to explain this to a product manager), this could be e.g. a plot of predicted vs. observed prices, but also anything else that seems suitable to you. 
How good is the model? Would you use it on the website? Why (not)? Is there anything else you noticed?


##########################


Gebrauchtwagen sind für einen Händler einfach zu bewerten, für einen Käufer ist diese Bewertung aber häufig schwierig.
Um Nutzern von mobile.de eine Orientierungshilfe für Gebrauchtwagenpreise zu bieten, soll ein geschätzter "mobile.de Marktpreis" auf der Anzeigeseite dargestellt werden.  


Sie haben Daten aus der Anzeigendatenbank zur Verfügung. Es handelt sich um zwei Datensätze (Train + Test).

Enhalten sind:
- Fahrzeugtyp ("category")
- Marke ("make_id")
- Modell ("model_id")
- Preis ("price__consumer_gross_euro")
- Kilometerstand ("mileage")
- Erstregistrierung ("first_registration" )
- Zustand ("condition")
- Löschzeitpunkt ("modification_time") - Wann wurde das Fahrzeug vom Verkäufer vom Marktplatz genommen?
- eine Reihe an Ausstattungsdetails des Autos ("features")

Bitte erstellen Sie ein Modell für eine Preisvorhersage, und sagen Sie die Preise im Test-Set voraus. Erstellen Sie eine leicht verständliche Auswertung Ihres Modells (z.B. Graph predicted vs. observed prices im Test-Set). Dieses sollte z.B. von einem Produktmanager verstanden werden. 
Was ist Ihnen ausserdem aufgefallen? Wie sind sie bei der Vorhersage vorgegangen und warum? Wie gut ist Ihr Modell? Würden Sie es auf der mobile.de Web-Seite einsetzen? Warum (nicht)? 

