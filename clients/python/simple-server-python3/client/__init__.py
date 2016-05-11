#!python3

"""
Very simple HTTP server in python

"""

from http.server import BaseHTTPRequestHandler
import json

# Server Configuration
HOST_NAME = 'localhost'
HOST_NAME_TEST = HOST_NAME
PORT_NUMBER = 8080
PORT_NUMBER_TEST = PORT_NUMBER + 10


class ServerHandler(BaseHTTPRequestHandler):

    def __write_response(self, body_html_, code):
        self.send_response(code)
        self.end_headers()
        self.wfile.write(body_html_.encode("utf_8"))

    def __get_object(self):
        length = int(self.headers['content-length'])
        content = self.rfile.read(length).decode("utf-8")
        return json.loads(content)

    def __feedback(self):
        object = self.__get_object()

        print("LOG in Feedback >> " , object)
        self.__write_response(json.dumps(object), 204)
        return object

    def __your_path(self):
        object = self.__get_object()

        # log
        print("LOG in PATH >> " , object)
        # Only for test
        #total = calculate(object)
        self.__write_response((json.dumps({'total': 1000})), 200)

    def do_GET(self):
        self.__write_response('hello world', 200)

    def do_POST(self):
        {

            '/ping': lambda: self.__write_response('pong', 200),
            '/feedback': self.__feedback,
            '/path': self.__your_path

        }.get(self.path, lambda: self.__write_response('Unknown', 404))()


def start_server(testMode=False):
    global server
    from http.server import HTTPServer

    if testMode:
        host_name = HOST_NAME_TEST
        port_number = PORT_NUMBER_TEST
    else:
        host_name = HOST_NAME
        port_number = PORT_NUMBER

    server = HTTPServer((host_name, port_number), ServerHandler)
    print('Starting server %s:%s use <Ctrl-C> to stop' % (host_name, port_number))
    try:
        server.serve_forever()
    except KeyboardInterrupt:
        server.server_close()
    print('Server interrupted')


def shutdown_server():
    global server
    server.server_close()
    print('Shutdown server %s:%s ' % (HOST_NAME, PORT_NUMBER))

if __name__ == '__main__':
    start_server()