# Instructions for participants

## Découpage

Pour calculer le total de la facture, il faut prendre en compte les taxes du pays d'où provient la facture, ainsi que la réduction qu'il convient d'appliquer.

### Taxes

Voici la grille de référence des taxes par pays :

| *Pays*             | *Code* | *Taxe* |
| ------------------ | ------ | ------ |
| Allemagne          | DE     | 20%    |
| Royaume-Uni        | UK     | 21%    |
| France             | FR     | 20%    |
| Italie             | IT     | 25%    |
| Espagne            | ES     | 19%    |
| Pologne            | PL     | 21%    |
| Roumanie           | RO     | 20%    |
| Pays-Bas           | NL     | 20%    |
| Belgique           | BE     | 24%    |
| Grèce              | EL     | 20%    |
| République Tchèque | CZ     | 19%    |
| Portugal           | PT     | 23%    |
| Hongrie            | HU     | 27%    |
| Suède              | SE     | 23%    |
| Autriche           | AT     | 22%    |
| Bulgarie           | BG     | 21%    |
| Danemark           | DK     | 21%    |
| Finlande           | FI     | 17%    |
| Slovaquie          | SK     | 18%    |
| Ireland            | IE     | 21%    |
| Croatie            | HR     | 23%    |
| Lituanie           | LT     | 23%    |
| Slovénie           | SI     | 24%    |
| Lettonie           | LV     | 20%    |
| Estonie            | EE     | 22%    |
| Chypre             | CY     | 21%    |
| Luxembourg         | LU     | 25%    |
| Malte              | MT     | 20%    |

Par exemple : pour la commande `{"prices":[15.99],"quantities":[1],"country":"ES","reduction":"STANDARD"}`, la réponse devrait être `{"total":19.03}`.

### Réductions

Ci-dessous la grille des réductions STANDARD appliquées pour la plupart des commandes :

| *Total*       | *Réduction* |
| ------------- | ----------- |
| >= 50 000 EUR | 15 %        |
| >= 10 000 EUR | 10 %        |
| >= 7 000 EUR  | 7 %         |
| >= 5 000 EUR  | 5 %         |
| >= 1 000 EUR  | 3 %         |

Par exemple : pour la commande `{"prices":[4.1,8.03,86.83,65.62,44.82],"quantities":[10,3,5,4,5],"country":"AT","reduction":"STANDARD"}`, la réponse devrait être `{"total":1166.62}`.

Remarques :

1. les réductions sont appliquées *après* les taxes
2. *d'autres types de réductions peuvent apparaître durant le jeu*. Il faudra rester vigilant sur les feedbacks que vous donnera le server pour pouvoir comprendre comment calculer la réduction.

## Développement

1. Pour pouvoir jouer, il vous faudra démarrer un serveur HTTP sur votre machine locale. Beaucoup de serveurs sont d'ores et déjà disponibles pour commencer dans ce répertoire, il suffit de cloner ce repository et de choisir l'un d'entre eux. Sinon, vous pouvez choisir de créer votre propre serveur HTTP. **Il n'y a pas besoin d'installer un quelconque serveur HTTP** comme Tomcat, Apache ou ngnix, les clients sont déjà eux-mêmes des serveurs HTTP.

2. Le facilitateur commencera un serveur central qui enverra des requêtes au serveur local de chacun des participants. Le facilitateur vous communiquera une URL pour le dashboard, allez dessus et enregistrez votre adresse IP et le port sur lequel votre serveur écoutera (exemple d'URL : http://<votre adresse IP>:<le port de votre serveur HTTP local>)

3. Le serveur central vous enverra des requêtes sur votre client local qui ressembleront à celle-ci :

   ```
   POST /order HTTP/1.1
   {
       "prices": [65.6,27.26,32.68],
       "quantities": [6,8,10],
       "country": "IE",
       "reduction":"STANDARD"
   }
   ```

4. Votre travail consiste à calculer le prix de la facture et répondre avec un objet JSON comme suit : `{ "total": 1000.0 }` (le serveur vérifie les réponses en utilisant deux décimales de précision, du coup les valeurs 10.1234 et 10.12 sont considérées comme égale).

5. Votre score sera affiché en temps réel sur le dashboard.

6. Le serveur vous enverra du feedback basé sur le contenu de votre réponse. Vérifiez bien que votre serveur HTTP local est capable de gérer des appels `POST /feedback` , et *dans le cas contraire, implémentez cet endpoint, sinon vous serez dans l'incapacité de savoir comment vos réponses sont traités par le serveur central*. Voici un exemple de feedback que pourrait vous envoyer le serveur central : 

   ```
   POST /feedback HTTP/1.1
   {
       "type": "ERROR",
       "content": "The field \"total\" in the response is missing."
   }
   ```

## Règles du jeu

Si la réponse correspond au montant attendu, alors félicitation, vous venez de gagner le montant de la facture. 

Si le total que vous avez répondu ne correspond pas, alors vous allez être pénalisé de 50% du montant correct de la facture.

Votre réponse sera ignorée si une de ces deux conditions est réalisée : 

- votre serveur est injoignable
- vous avez répondu avec un code HTTP 404