require 'sinatra'
require 'sinatra/reloader'
require 'json'

set :environment, :production
enable :reloader
disable :logging

get '/ping' do
  "pong"
end

post '/feedback' do
  message = JSON.parse(request.body.read)
  puts "#{message["type"]} #{message["content"]}"
  status 204
end

post /\/(?!(ping|feedback))/ do
  puts "#{request.request_method} #{request.path} #{request.body.read}"
  status 204
end
