"""
Carpaccio server using Flask.
To start the kata, complete the order() function
"""

from flask import Flask, request, jsonify
import re

app = Flask(__name__)

#Returns 'hello world' on any get request
@app.route("/", methods = ['GET'])
@app.route("/<path:path>", methods = ['GET'])
def index2(path=''):
    return "hello world"

#response to ping post.
@app.route("/ping", methods = ['POST'])
def ping():
    return "pong"

@app.route("/order", methods = ['POST'])
def order():
    order = request.get_json()

    #YOUR CODE HERE

    result = {'total' : 1000}
    #You should probably comment this line before you register your client 
    return jsonify(result)


def start_server():
    app.run(host='0.0.0.0',
            port=int("5000"))
