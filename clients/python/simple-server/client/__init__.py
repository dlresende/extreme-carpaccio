"""
Very simple HTTP server in python

"""

from BaseHTTPServer import BaseHTTPRequestHandler
import json

# Server Configuration
HOST_NAME = 'localhost'
PORT_NUMBER = 8080


class ServerHandler(BaseHTTPRequestHandler):

    def __write_response(self, body_html_, code):
        self.send_response(code)
        self.end_headers()
        self.wfile.write(body_html_)

    def __get_object(self):
        length = int(self.headers['content-length'])
        content = self.rfile.read(length)
        return json.loads(content)

    def __your_path(self):
        object = self.__get_object()

        # log
        print object
        # Only for test
        #total = calculate(object)
        self.__write_response(json.dumps({'total': 1000}), 200)

    def do_GET(self):
        self.__write_response('hello world', 200)

    def do_POST(self):
        {

            '/ping': lambda: self.__write_response('pong', 200),
            '/path': self.__your_path

        }.get(self.path, lambda: self.__write_response('Unknown', 404))()


def start_server():
    global server
    from BaseHTTPServer import HTTPServer

    server = HTTPServer((HOST_NAME, PORT_NUMBER), ServerHandler)
    print 'Starting server %s:%s use <Ctrl-C> to stop' % (HOST_NAME, PORT_NUMBER)
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        server.server_close()
    print 'Server interrupted'


def shutdown_server():
    global server
    server.server_close()
    print 'Shutdown server %s:%s ' % (HOST_NAME, PORT_NUMBER)

if __name__ == '__main__':
    start_server()