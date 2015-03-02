# Extreme Carpaccio
kata + [extreme startup](https://github.com/rchatley/extreme_startup) + [elephant carpaccio](https://docs.google.com/document/d/1TCuuu-8Mm14oxsOnlk8DqfZAA1cvtYu9WGv67Yj_sSk/pub)

## Instructions
1. Go to the sellers view and register yourself (URL example: http://192.168.1.12:8080/)
2. The server will start sending orders like this:

    ```
    POST /order HTTP/1.1
    {
        "prices": [65.6,27.26,32.68],
        "quantities": [6,8,10],
        "country": "IE"
    }
    ```

3. You should calculate the total and answer with an object bill, i.e.: `{ "total": 1000.0 }`
4. Your score will be shown in the dashboard

To calculate the bill, you need to consider the tax of the country from which the order came from and the reduction.

### Taxes
*Country* | *Tax*
--- | ---
BG | 10 %
CZ | 11 %
DK | 12 %
DE | 13 %
EE | 14 %
IE | 15 %
EL | 16 %
ES | 17 %
FR | 18 %
HR | 19 %
IT | 20 %
CY | 10 %
LV | 11 %
LT | 12 %
LU | 13 %
HU | 14 %
MT | 15 %
NL | 16 %
AT | 17 %
PL | 18 %
PT | 19 %
RO | 20 %
SI | 10 %
SK | 11 %
FI | 12 %
SE | 13 %
UK | 14 %

### Reductions
*Total* | *Reduction*
--- | ---
>= 50 000 EUR | 15 %
>= 10 000 EUR | 10 %
>= 7 000 EUR | 7 %
>= 5 000 EUR | 5 %
>= 1 000 EUR | 3 %
