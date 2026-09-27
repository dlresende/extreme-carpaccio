require "test_helper"

class PingControllerTest < ActionDispatch::IntegrationTest
  test "POST /ping responds with pong" do
    post "/ping"

    assert_response :ok
    assert_equal "pong", response.body
  end
end
