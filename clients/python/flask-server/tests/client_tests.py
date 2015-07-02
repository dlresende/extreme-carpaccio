"""
integration test in python
!!!! NEED PYTHON 2.7.8
"""

import unittest
import urllib2
import urllib
import thread
import time
import json
from client import app
from multiprocessing import Process

class ServerHandlerTest(unittest.TestCase):
    server = Process(target=app.run)

    @classmethod
    def setUpClass(cls):
        cls.server.start()
        time.sleep(1)

    @classmethod
    def tearDownClass(cls):
        cls.server.terminate()
        cls.server.join()

    def assertContent(self, content, response):
        for line in response.readlines():
            if line == content:
                found = True
        self.assertTrue(found)

    def test_should_call_get(self):
        response = urllib2.urlopen("http://localhost:5000/")
        self.assertContent('hello world', response)

    def test_should_call_post_ping(self):
        data = urllib.urlencode({'q': 'Ping'})
        response = urllib2.urlopen("http://localhost:5000/ping", data)
        self.assertContent('pong', response)

    def test_should_call_post_order(self):
        req = urllib2.Request('http://localhost:5000/order')
        req.add_header('Content-Type', 'application/json')
        response = urllib2.urlopen(req, json.dumps({'q': 'Path'}))
        self.assertEqual(json.loads(response.read()), {u'total' : 1000})

    @unittest.expectedFailure
    def test_should_call_post_unknown(self):
        data = urllib.urlencode({'answer': 'hello'})
        urllib2.urlopen("http://localhost:5000/unknown", data)

if __name__ == '__main__':
    unittest.main()
