import os
from http.server import HTTPServer, BaseHTTPRequestHandler

class ServerHandler(BaseHTTPRequestHandler):
    def do_POST(self):
        if self.path == '/ping':
            self.send_response(200)
            self.send_header('Content-Type', 'text/plain')
            self.end_headers()
            self.wfile.write(b'pong')
        else:
            self.send_response(404)
            self.end_headers()

    def log_message(self, format, *args):
        pass

def create_server(host='localhost', port=3000):
    return HTTPServer((host, port), ServerHandler)

def start_server():
    port = int(os.environ.get('PORT', 3000))
    server = create_server('0.0.0.0', port)
    print(f'Server listening on port {port}')
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        server.server_close()

if __name__ == '__main__':
    start_server()
