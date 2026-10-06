const fs = require('fs')
const utils = require('../javascripts/utils')
const chalk = require('chalk')

const Configuration = function (filepath) {
  this.filepath = filepath
  this.props = {}
  this.loaded = false
}

Configuration.prototype = (function () {
  function loadFile (self, callback) {
    console.info(chalk.red('Reloading ' + self.filepath))

    fs.readFile(self.filepath, function (err, data) {
      if (err) {
        console.error('%j', err)
      } else {
        try {
          self.props = utils.jsonify(data)
          self.loaded = true

          if (typeof callback !== 'undefined') {
            callback(err)
          }
        } catch (exception) {
          console.error('Oops something wrong happened', exception)
        }
      }
    })
  }

  function readContent (self) {
    console.info('Reading %s.', self.filepath)
    const fileContent = fs.readFileSync(self.filepath)
    return utils.jsonify(fileContent)
  }

  return {
    load: function (callback) {
      return loadFile(this, callback)
    },

    all: function () {
      const self = this

      // _.isEmpty would re-read for a legitimately empty configuration.
      if (!self.loaded) {
        self.props = readContent(self)
        self.loaded = true
      }

      return self.props
    },

    watch: function (callback, watchOnce, interval) {
      const self = this

      fs.watchFile(self.filepath, { persistent: !watchOnce, interval }, function (curr, prev) {
        self.load(callback)
      })
    }
  }
})()

module.exports = {
  Configuration
}
