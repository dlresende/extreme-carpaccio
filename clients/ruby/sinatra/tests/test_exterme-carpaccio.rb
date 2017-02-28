ENV['RACK_ENV'] = 'test'

require "./lib/extreme-carpaccio.rb"
require "test/unit"
require 'rack/test'

class ExtremeCarpaccioTest < Test::Unit::TestCase
  include Rack::Test::Methods

  def app
    Sinatra::Application
  end

  def test_dummy
    assert_equal(4, 2+2)
  end

  def test_should_answer_pong
    get '/ping'
    assert last_response.ok?
    assert_equal 'pong', last_response.body
  end

end