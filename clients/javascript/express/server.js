const express = require('express')

const app = express()
app.use(express.json())

app.post('/ping', (req, res) => {
  res.send('pong')
})

const port = process.env.PORT || 3000

if (require.main === module) {
  app.listen(port, () => {
    console.log(`Express client listening on port ${port}`)
  })
}

module.exports = app
