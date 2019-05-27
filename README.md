# API for Bedkom
Dette er ett system for administrering av bedriftspresentasjon ved Institutt for informatikk, UiB. 
Dette er API-et for [Bedkom-systemet](https://github.com/JonasTriki/bedkom), og er utviklet av Jonas Triki i forbindelse med ett studentprosjekt (INF219) våren 2019.

## Installasjon
```
npm install
```

## Konfigurasjon
Før man kjører systemet er man nødt til å lage en kopi av filen ```config-template.ts``` og kalle den nye filen for ```config.ts```. Bytt ut alle *TODO* felter med faktisk konfigurasjon. For mer informasjon, vennligst se kommentarene over hver *TODO*-seksjon.

## Bruk
Kjør følgende
```
npm start
```
Når applikasjonen er oppe og kjører, kan man navigere til http://localhost:8080/ og f.eks teste det i [Postman](https://www.getpostman.com/).

## Endepunkter
Se mappen ```src/api/routes``` for endepunkter.