require 'sinatra/base'

module ExtremeCarpaccio
  class App < Sinatra::Base
    # Teams reach the facilitator's machine over the local network, so do not
    # restrict the Host header the way Sinatra does outside development.
    set :host_authorization, { permitted_hosts: [] }

    post '/ping' do
      'pong'
    end
  end
end
