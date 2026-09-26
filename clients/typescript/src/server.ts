import express from 'express'

export const app = express()
app.use(express.json())

app.post('/ping', (_req, res) => {
  res.send('pong')
})

const port = process.env.PORT || 3000

if (require.main === module) {
  app.listen(port, () => {
    console.log(`TypeScript client listening on port ${port}`)
  })
}
