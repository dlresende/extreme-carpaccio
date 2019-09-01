var fs = require('fs')
var utils = require('../javascripts/utils')
var _ = require('lodash')
var colors = require('colors')

var Configuration = function (filepath) {
  this.filepath = filepath
  this.props = {}
}

Configuration.prototype = (function () {
  function loadFile (self, callback) {
    console.info(colors.red('Reloading ' + self.filepath))

    fs.readFile(self.filepath, function (err, data) {
      if (err) {
        console.error('%j', err)
      } else {
        try {
          self.props = utils.jsonify(data)

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
    var fileContent = fs.readFileSync(self.filepath)
    return utils.jsonify(fileContent)
  }

  return {
    load: function (callback) {
      return loadFile(this, callback)
    },

    all: function () {
      var self = this

      if (_.isEmpty(self.props)) {
        self.props = readContent(self)
      }

      return self.props
    },

    watch: function (callback, watchOnce, interval) {
      var self = this

      fs.watchFile(self.filepath, { persistent: !watchOnce, interval: interval }, function (curr, prev) {
        self.load(callback)
      })
    }
  }
})()

var exports = module.exports
exports.Configuration = Configuration
