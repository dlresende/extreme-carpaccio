class PingController < ApplicationController
  def create
    render plain: "pong"
  end
end
