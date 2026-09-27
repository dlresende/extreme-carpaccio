Rails.application.routes.draw do
  post "ping" => "ping#create"
end
