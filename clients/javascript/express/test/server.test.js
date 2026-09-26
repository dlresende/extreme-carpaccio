const test = require('node:test')
const assert = require('node:assert')
const request = require('supertest')
const app = require('../server')

test('POST /ping responds with pong', async () => {
  const response = await request(app)
    .post('/ping')
    .expect(200)

  assert.strictEqual(response.text, 'pong')
})
