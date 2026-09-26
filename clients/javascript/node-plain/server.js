const http = require('http')

const server = http.createServer((req, res) => {
  if (req.method === 'POST' && req.url === '/ping') {
    res.writeHead(200, { 'Content-Type': 'text/plain' })
    res.end('pong')
    return
  }

  res.writeHead(404)
  res.end()
})

const port = process.env.PORT || 3000

if (require.main === module) {
  server.listen(port, () => {
    console.log(`Node plain client listening on port ${port}`)
  })
}

module.exports = server
