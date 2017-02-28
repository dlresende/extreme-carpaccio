require 'pp'

class FeedbackController < ApplicationController

  def feedback
    pp params
  end

end
