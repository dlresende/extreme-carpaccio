#!python3

"""
HTTP server integration test in python

"""
import unittest
import urllib.request, urllib.error, urllib.parse
import urllib.request, urllib.parse, urllib.error

import _thread
import time
import json

from client import start_server, shutdown_server
from client import HOST_NAME_TEST, PORT_NUMBER_TEST

CLIENT_URL = "http://" + HOST_NAME_TEST + ":" + str(PORT_NUMBER_TEST)

def start_tested_server(threadname):
    print('start %s' % threadname)
    start_server(testMode=True)


class ServerHandlerTest(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        _thread.start_new(start_tested_server, ('Server Thread',))
        time.sleep(1)

    @classmethod
    def tearDownClass(cls):
        shutdown_server()

    def assertContent(self, content, response):
        found = False
        for line in response.readlines():
            if line == content:
                found = True
        self.assertTrue(found)

    def test_should_call_get(self):
        response = urllib.request.urlopen(CLIENT_URL)
        self.assertContent(b'hello world', response)

    def test_should_call_post_ping(self):
        data = urllib.parse.urlencode({'q': 'Ping'})
        response = urllib.request.urlopen(CLIENT_URL + "/ping", data.encode("utf-8"))
        self.assertContent(b'pong', response)

    def test_should_call_post_path(self):
        req = urllib.request.Request(CLIENT_URL + "/path")
        req.add_header(b'Content-Type', b"application/json")
        data = (json.dumps({'q': 'Path'})).encode("utf-8")
        response = urllib.request.urlopen(req, data)
        self.assertEqual(response.readlines(), [b'{"total": 1000}'])

    def test_should_call_feedback(self):
        req = urllib.request.Request(CLIENT_URL + "/feedback")
        req.add_header('Content-Type', 'application/json')
        data = json.dumps({'reason': 'Feedback Test'}).encode("utf-8")
        response = urllib.request.urlopen(req, data)
        self.assertEqual(response.getcode(), 204)

    @unittest.expectedFailure
    def test_should_call_post_unknown(self):
        data = urllib.parse.urlencode({'answer': 'hello'}).encode("utf-8")
        response = urllib.request.urlopen(CLIENT_URL + "/unknown", data)


if __name__ == '__main__':
    unittest.main()