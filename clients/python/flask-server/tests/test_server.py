import unittest
from client import app

class PingTest(unittest.TestCase):
    def setUp(self):
        self.client = app.test_client()

    def test_post_ping_responds_with_pong(self):
        response = self.client.post('/ping')
        self.assertEqual(response.status_code, 200)
        self.assertEqual(response.data.decode('utf-8'), 'pong')

if __name__ == '__main__':
    unittest.main()
