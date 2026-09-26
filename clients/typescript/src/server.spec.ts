import request from 'supertest'
import { app } from './server'

describe('POST /ping', () => {
  it('responds with pong', async () => {
    const response = await request(app).post('/ping').expect(200)
    expect(response.text).toBe('pong')
  })
})
