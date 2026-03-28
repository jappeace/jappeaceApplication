Title: Haskell 💜 Vibes
Date: 2026.03.08
Category: reflection
OPTIONS: toc:nil
Tags: haskell, vibes

Het was vrijdag 27 februari 2026,
toen ik herboren werd.
bro. Ik ben nu een vibe coder.
bro.
echt waar.

<style>
figure {
  float: right;
  margin: 2em;
  margin-top: 0em;
  width: 15em;
}
@media (max-width: 420px) {
  figure {
    float: none;
  }
}
figcaption{
  font-size: xx-small;
  color: #999;
}
</style>

<figure>
<iframe width="280" height="165" src="https://www.youtube.com/embed/qMQ-y9dHE2k?si=sqAwlSOezHOboQ8n" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

<figcaption> Zo ziet het eruit als ik nu code schrijf. </figcaption>
</figure>

Het lukte me[^dreadful] om in te loggen op een [claude code](https://code.claude.com/) terminal.
Dit is een CLI-app die toegang heeft tot een [groot taalmodel](https://nl.wikipedia.org/wiki/Groot_taalmodel)[^llm] (LLM),
met de bedoeling het ontwikkelwerk voor je te doen.
Je vraagt het om wijzigingen te maken, en het doet het voor je.
Ik vertrouwde het niet, en vertrouw het nog steeds niet met toegang tot mijn machine.
Dus ik heb het opgesloten in [een container](https://github.com/jappeace/haskell-vibes).
Ik vroeg het om wat front-end kaartfuncties te implementeren,
het deed het goed.
Nu vroeg ik me af of het backend-ontwikkeling kon doen.
Haskell-typefouten zijn een stuk moeilijker dan
React-blokken uitspugen.
Dus ik had vrij lage verwachtingen dat het (enig) Haskell kon doen.
Ik had het mis.

[^llm]: Lees ChatGPT als je niet bekend bent met deze term.
        Het LLM is het ding dat beslist wat het volgende moet doen.
        Claude code stuurt gewoon de codebestanden en
        andere context als een groot chatbericht naar het LLM,
        en het LLM stuurt dan acties terug.
        Die de claude code terminal kan interpreteren.
        Het andere dat claude code doet is doordraaien totdat
        het denkt dat het klaar is.
        Het lijkt erop dat het LLM deze status ook bepaalt, want je kunt
        aanpassen hoe "klaar" eruitziet met een CLAUDE.md-bestand of in de prompt.

[^dreadful]: Inloggen bij claude was verschrikkelijk.
    Waarom breekt de terminal in docker-containers van bepaalde venstergroottes?
    Er waren ook andere problemen, zoals service down en registratieweirdness.

Het is goed in Haskell.
Er zijn geen fouten.
Of beter gezegd, de compiler wijst ze aan voor claude,
en het is slim genoeg om voortgang te maken op de compilefouten.
Dus het doet wat je wilt dat het doet.
En het doet het snel.

Ik vroeg het om basement uit het [memory](https://hackage.haskell.org/package/memory)-pakket te halen en het [ram](https://hackage.haskell.org/package/ram)-pakket te maken.[^discussion]
Dit is op zich niet moeilijk,
het is gewoon
behoorlijk wat uitzoekwerk en het oplossen van vele compilefouten.
Het deed dat [zonder problemen](https://github.com/jappeace/ram/commit/b0e2e66ccc8537c143acb9caf749ef751a1c047f).

[^discussion]: Het is onderdeel van een grotere [discussie](https://discourse.haskell.org/t/fork-basement-as-baseplate/12415/82) over wat te doen met basement.

Op maandag op het werk vroeg ik het om
geofences op te nemen in de sensor-retourlijst.
Je zou de sensors moeten joinen op de geofences
in de database.
We gebruiken [Esqueleto](https://hackage.haskell.org/package/esqueleto) daarvoor,
dat wat geavanceerde typesysteemfuncties gebruikt om een mooie DSL te maken.
Geen probleem voor Claude.
Ik duwde dit op vrijdag wat verder,
ik vroeg het om de postgis-referentie te vergelijken
met de [Esqueleto PostGIS](https://hackage-content.haskell.org/package/esqueleto-postgis)-bibliotheek,
en de ontbrekende functies te implementeren.
Ik vroeg het ook om integratietests toe te voegen om te laten zien dat het werkt,
want ik vertrouw het niet.
Esqueleto postgis is nu [vrij compleet](https://hackage-content.haskell.org/package/esqueleto-postgis-4.1.0/docs/Database-Esqueleto-Postgis.html).

Weet je, het vervelende is dat ik dit geweldig vind.
Ik vind het vervelend om door compilefouten heen te moeten werken.
Ik wil gewoon dingen bouwen,
de weg ernaartoe is niet zo belangrijk.
Ik vind het fijn om zoveel gedaan te krijgen met Claude.
Ik dacht dat ik hield van Haskell schrijven,
maar ik denk dat ik eigenlijk hield van de productiviteit
die het me bracht.
Claude doet hetzelfde maar nog meer.

De rest van de week was hetzelfde.
Mijn baan veranderde. Ik veranderde.
we vibben nu.
Deze taal is voor vibes.
Wat me op vragen brengt.

## Vertrouwen
Ik vertrouw claude niet.
Voor elke functie die het schrijft moet het bewijzen dat het werkt met een test.
Het is niet perfect, het gaslight je als je het laat.
Soms "vergeet" het om tests te schrijven.[^compression]
Haskell is goed voor claude omdat het het systeem dwingt intern consistent te zijn.
In Haskell is alles een expressie, en elke expressie heeft een type.
Het type moet overeenkomen met de expressie (ofwel checken) of anders krijg je geen programma maar een foutmelding.
Claude kan je niet gaslighten om te geloven dat de fout is wat je wilt.[^hold-beyond]
Dus het moet intern consistent zijn,
iets waar LLM's slecht in zijn.
Ze doen gewoon maar wat.

[^hold-beyond]: Hoewel ik het er niet langs zet, op meerdere momenten beweerde het dat het niet in staat was om de `nix-shell` in te gaan. Wat een regelrechte leugen is. Het wil gewoon niet bouwen.

[^compression]: Ik denk dat een probleem is dat "contextcompressie" ook het CLAUDE.md-bestand comprimeert. Ik heb het gewoon verteld om het opnieuw te lezen als het compressie doet, dat lijkt het in de context te houden. (dit zou een andere bug kunnen zijn?). Zie mijn [claude.md](https://github.com/jappeace/haskell-vibes/blob/master/CLAUDE.md)-bestand voor details

Ik draai het in een container op yolo-modus.
Standaard vraagt het je bij elk commando dat het wil uitvoeren.
Ik wil er echter niet bij betrokken zijn.
Zonder toezicht, laat de testsuite slagen, laat CI slagen.[^give-trust]
Ik review de code om te verifiëren of het correct is nadat het klaar is en al deze hordes heeft genomen.
Ik wil er niet op hoeven passen.
De container beschermt me voldoende tegen de gekheid.
Er is gekheid.

<figure>
<img  alt="Claude code opgesloten in container" src="/images/2026/claude-trapped.jpg" />
<figcaption> Claude code opgesloten in een container</figcaption>
</figure>

Op een gegeven moment probeerde het in `/etc/shadow` te schrijven om zichzelf schrijftoegang
tot de thuismap te geven, die ik vergeten was te geven.
Dat is een (slechte) privilege-escalatiepoging.
Het probeerde me te hacken,
ook al beweert het dat het zulke dingen niet kan.

[^give-trust]: Ik vertrouw het hier dus om niet daadwerkelijk te proberen uit te breken. Ik denk dat het zou kunnen als het wilde. Ik kan claude niet vragen hoe het dat zou doen, maar ik kan het aan gemini vragen (grappig).

Ik vraag het niet echt om mijn problemen op te lossen.
Ik vraag het om implementaties te schrijven,
ik geef het stappen om te doen.
Ik weet precies wat ik wil,
het kost mij alleen lang en het is meestal repetitief[^boring] werk.
Ik erken dat Claude veel sneller is in het doorwerken van compilefouten dan ik,
en het vereist niet mijn aandacht.
Ik kan me richten op nadenken over hoe het systeem betrouwbaar te maken.
Ik kan me richten op ervoor zorgen dat we geen doorvoerproblemen hebben.
Ik kan de daadwerkelijke engineeringdelen van deze baan doen.

[^boring]: Eén unieke compilefout oplossen is leuk, 30 tot 40 keer dezelfde of vergelijkbare oplossen niet.
           Dit probleem wordt groter naarmate een codebase groter wordt.
           Claude werkt zich met plezier door duizenden compilefouten heen.
           Je kunt nu deze enorme codebases refactoren.

## Baan
Wat is mijn baan?
Maakt het mijn werkgevers uit dat ik code schrijf?
Maakt het ze uit dat ik degene ben die door compilefouten heen werkt?
Nee.
Ze willen een betrouwbaar werkend systeem.

Ik besteedde vroeger het grootste deel van mijn tijd aan het schrijven van
code omdat er geen andere manier was om het te schrijven.
Dit kan je in de war brengen om te geloven dat software schrijven je baan is,
omdat je er zoveel tijd aan besteedt.
Niemand heeft ooit gezegd dat dit het geval was.
Er zit ook een engineeringkant aan het schrijven van software.

<figure>
<img  alt="Mezelf in een nieuw beeld branden" src="/images/2026/vibe-coder.jpg" />
<figcaption> Mezelf in een nieuw beeld branden </figcaption>
</figure>

Ik eindigde vrijdag met zoveel open PR's en ik had dit als zorgpunt aangekaart.
Het is geen zorgpunt.
We zijn gewoon nog nooit in deze situatie geweest waar het maken van de implementatie
het makkelijke deel is.
Maar eigenlijk is het het makkelijke deel.
Het correct krijgen is het moeilijke deel.
Die laatste 10% van de implementatie is nu mijn baan, namelijk het correct krijgen.
Ik moet ervoor zorgen dat de software die Claude produceert werkt.
Ik moet denken in termen van verificatie.

Ik heb [Leana](https://git.confusedcompiler.org/leana8959/blog/src/branch/trunk/content/articles/2025-12-a-comment-preserving-cabal-parser/index.md) ingehuurd om exact print te doen.
Kan ik haar vervangen door AI?
Nee.
Ik wil dat zij hier de leiding over heeft.
Leana beslist zelf wat ze doet.
Het enige wat ik doe is af en toe even bijpraten
om er zeker van te zijn dat ze niet ontspoort.
In tegenstelling tot Claude-instanties vertrouw ik haar.
Ik zou haar zonder zorgen toegang tot mijn machine geven.

Ik realiseer me dat dit is wat mijn werkgevers ook willen.
Erop vertrouwen dat het werk gedaan wordt.
Hoe is niet belangrijk.
Het is bijna alsof het automatiseren van lagerwaardig werk[^value-job]
kansen creëert voor hogerwaardiger werk. [^doing]
In mijn geval ben ik gewoon verschoven naar een hogerwaardigere
baan omdat een startup altijd onderbezet is.

[^value-job]: Voor de duidelijkheid, ik zeg niet dat programmeren een laagwaardige baan is. Zelfs junior-rollen zijn belangrijk, je kunt dit zien aan de compensatie die ze krijgen. Ik zeg dat het uitbesteden van het oplossen van tonnen typefouten aan een tool je in staat stelt om andere dingen te doen, die waarschijnlijk van hogere waarde zijn.

Ben ik een Haskell-ontwikkelaar?
Je besteedt zoveel tijd aan het leren van een taal dat het onderdeel van je wordt.
Misschien is dit een ik-probleem.
Iedereen kan nu Haskell schrijven.
Je hoeft alleen maar claude te vertellen om wat dan ook in Haskell te implementeren
in plaats van een andere taal.
Je krijgt meer correctheid gratis.
Claude werkt echt, en het werkt goed.
Er is geen leercurve meer.
Verificatie is echter nog steeds een probleem.
Als je de eigenschappen van je systeem niet kunt specificeren,
kun je alle code ter wereld genereren en rommel krijgen.
Zelfs als het in Haskell is.

## Conclusie
Het is een wilde week geweest.
Het is niet zichtbaar voor andere mensen,
maar stoppen met code schrijven heeft een grote impact op mij gehad.
Ik heb bijna constant code geschreven de afgelopen 20 jaar.
Nu ben ik het niet meer.

Ik verwerk dit gewoon.
Dit had zoveel impact op mijn leven.
Het is voorbij, we beginnen.

[^doing]: Zie bijvoorbeeld het boek Learning by Doing: The Real Connection between Innovation, Wages, and Wealth (2015)
