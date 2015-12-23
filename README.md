# Extreme Carpaccio
This exercise merges two well known exercises: [Extreme Startup](https://github.com/rchatley/extreme_startup) and [Elephant Carpaccio](https://docs.google.com/document/d/1TCuuu-8Mm14oxsOnlk8DqfZAA1cvtYu9WGv67Yj_sSk/pub).

À la Extreme Startup, someones computer plays the central server role and start sending HTTP requests to participant computers. Each request is in fact a purchasing order and participants should calculate the order's total amount and answer the server. For every good response the participant earn points and increase his score. For bad responses, penalties are applied and the participant lose some points. Therefore, as a participant, you need to try to slice the problem and go to production with a small part of the problem solved as soon as possible in order to start collecting points. If you don't slice and try to go to production once the whole problem is solved, you risk to take to much time and other participants will start scoring before you. Slice the problem, try to go live as soon as possible and start collecting feedbacks, this is the Elephant Carpaccio part.

Ready for the challenge? Read the problem description, define your slicing strategy and start coding!

## Technical Instructions
1. To be able to play, you will need to start an http server in your local machine. Many servers are already available in the [clients directory](https://github.com/dlresende/extreme-carpaccio/tree/master/clients), you only need to clone this repository and pick will. Otherwise, you can create your own http server.
2. The facilitator will start a central server which will send requests to each participant's server. Since the facilitator communicates the URL for the server dashboard, go there and register your local server with your local IP address and the port under your http server is listening on (URL example: http://\<you IP address\>:\<port for your local http server\>/)
3. The central server will start sending orders to your local client like this:

    ```
    POST /order HTTP/1.1
    {
        "prices": [65.6,27.26,32.68],
        "quantities": [6,8,10],
        "country": "IE",
        "reduction":"STANDARD"
    }
    ```

4. Your job is to calculate the amount of the received orders and answer with a JSON object bill, i.e.: `{ "total": 1000.0 }` (the server checks responses using two decimal digits of precision, so, i. e., 10.1234 and 10.12 are equal).
5. Your score will be shown in the dashboard
6. The server will send you feedback based on what you have responded. So check if your local http server already handles POST /feedback and, *if not, implement it, otherwise you will not be able to figure out what is going on with your responses*. Here is an example of a feedback the central server can send to you:

    ```
    POST /feedback HTTP/1.1
    {
        "type": "ERROR",
        "content": "The field \"total\" in the response is missing."
    }
    ```
    
## Functional Instructions
To calculate the bill, you need to consider the tax of the country from which the order came from and the reduction.

### Taxes
Here is the tax table used in the exercise:

*Country* | *Code* | *Tax*
--- | --- | ---
Germany | DE | 20%
United Kingdom | UK | 21%
France | FR | 20%
Italy | IT | 25%
Spain | ES | 19%
Poland | PL | 21%
Romania | RO | 20%
Netherlands | NL | 20%
Belgium | BE | 24%
Greece | EL | 20%
Czech Republic | CZ | 19%
Portugal | PT | 23%
Hungary | HU | 27%
Sweden | SE | 23%
Austria | AT | 22%
Bulgaria | BG | 21%
Denmark | DK | 21%
Finland | FI | 17%
Slovakia | SK | 18%
Ireland | IE | 21%
Croatia | HR | 23%
Lithuania | LT | 23%
Slovenia | SI | 24%
Latvia | LV | 20%
Estonia | EE | 22%
Cyprus | CY | 21%
Luxembourg | LU | 25%
Malta | MT | 20%

For the order `{"prices":[15.99],"quantities":[1],"country":"ES","reduction":"STANDARD"}`, for example, the response should be `{"total":19.03}`.

### Reductions
Following the STANDARD reductions applied for the most part of the orders:

*Total* | *Reduction*
--- | ---
>= 50 000 EUR | 15 %
>= 10 000 EUR | 10 %
>= 7 000 EUR | 7 %
>= 5 000 EUR | 5 %
>= 1 000 EUR | 3 %

For the order `{"prices":[4.1,8.03,86.83,65.62,44.82],"quantities":[10,3,5,4,5],"country":"AT","reduction":"STANDARD"}`, for example, the response should be `{"total":1166.62}`.

Note that:

1. reductions are applied *after* the taxes;
2. *another reduction types can appear during the game*. You need to stay tuned in the server's feedback to figure out how to calculate it.

### Penalties and Cash
Note that if you answer something that does not match the expected bill, you will be charged with 50% of the amount of the right bill. If your answer is correct, you earn the total of the bill. If your answer does not match the expected object bill or is empty, no penalties are applied.

Have fun :)
