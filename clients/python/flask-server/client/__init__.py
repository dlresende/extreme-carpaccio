import os
from flask import Flask

app = Flask(__name__)

@app.route('/ping', methods=['POST'])
def ping():
    return 'pong'

def start_server():
    port = int(os.environ.get('PORT', 3000))
    app.run(host='0.0.0.0', port=port)
