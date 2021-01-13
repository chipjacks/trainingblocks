Rails.application.routes.draw do
  # For details on the DSL available within this file, see http://guides.rubyonrails.org/routing.html

  devise_for :users, controllers: { omniauth_callbacks: 'users/omniauth_callbacks' }

  root to: "home#index"

  get 'activities', to: 'activities#index'
  post 'activities', to: 'activities#batch_update'

  get 'imports', to: 'imports#index'
end
