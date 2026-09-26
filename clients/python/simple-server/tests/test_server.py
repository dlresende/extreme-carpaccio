import threading
import urllib.request
import unittest
from server import create_server

class PingTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.server = create_server('localhost', 13001)
        cls.thread = threading.Thread(target=cls.server.serve_forever)
        cls.thread.daemon = True
        cls.thread.start()

    @classmethod
    def tearDownClass(cls):
        cls.server.shutdown()
        cls.server.server_close()

    def test_post_ping_responds_with_pong(self):
        req = urllib.request.Request('http://localhost:13001/ping', data=b'', method='POST')
        with urllib.request.urlopen(req) as response:
            self.assertEqual(response.status, 200)
            self.assertEqual(response.read(), b'pong')

if __name__ == '__main__':
    unittest.main()
