require 'minitest/autorun'
require 'rack/test'
require_relative '../app'

class PingTest < Minitest::Test
  include Rack::Test::Methods

  def app
    ExtremeCarpaccio::App
  end

  def test_post_ping_responds_with_pong
    post '/ping'

    assert last_response.ok?
    assert_equal 'pong', last_response.body
  end
end
