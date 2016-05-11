"""
HTTP server integration test in python

!!!! NEED PYTHON 2.7.8
"""
import unittest
import urllib2
import urllib
import thread
import time
import json

from client import start_server, shutdown_server
from client import HOST_NAME_TEST, PORT_NUMBER_TEST

CLIENT_URL = "http://" + HOST_NAME_TEST + ":" + str(PORT_NUMBER_TEST)

def start_tested_server(threadname):
    print 'start %s' % threadname
    start_server(testMode=True)


class ServerHandlerTest(unittest.TestCase):

    @classmethod
    def setUpClass(cls):
        thread.start_new(start_tested_server, ('Server Thread',))
        time.sleep(1)

    @classmethod
    def tearDownClass(cls):
        shutdown_server()

    def assertContent(self, content, response):
        for line in response.readlines():
            if line == content:
                found = True
        self.assertTrue(found)

    def test_should_call_get(self):
        response = urllib2.urlopen(CLIENT_URL)
        self.assertContent('hello world', response)

    def test_should_call_post_ping(self):
        data = urllib.urlencode({'q': 'Ping'})
        response = urllib2.urlopen(CLIENT_URL + "/ping", data)
        self.assertContent('pong', response)

    def test_should_call_post_path(self):
        req = urllib2.Request(CLIENT_URL + "/path")
        req.add_header('Content-Type', 'application/json')
        response = urllib2.urlopen(req, json.dumps({'q': 'Path'}))
        self.assertEqual(response.readlines(), ['{"total": 1000}'])

    def test_should_call_feedback(self):
        req = urllib2.Request(CLIENT_URL + "/feedback")
        req.add_header('Content-Type', 'application/json')
        response = urllib2.urlopen(req, json.dumps({'reason': 'Feedback Test'}))
        self.assertEqual(response.getcode(), 204)

    @unittest.expectedFailure
    def test_should_call_post_unknown(self):
        data = urllib.urlencode({'answer': 'hello'})
        urllib2.urlopen(CLIENT_URL + "/unknown", data)

if __name__ == '__main__':
    unittest.main()