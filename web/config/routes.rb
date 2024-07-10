Rails.application.routes.draw do
  devise_for :users,
             controllers: {
               omniauth_callbacks: 'users/omniauth_callbacks',
               confirmations: 'users/confirmations',
               sessions: 'users/sessions',
               registrations: 'users/registrations'
             }

  root to: 'home#index'

  get 'calendar', to: 'calendar#index'
  get 'trends', to: 'trends#index'

  resource :settings, only: %i[show update]

  get 'account', to: 'account#index'
  delete 'account/strava', to: 'account#disconnect_strava'

  get 'activities', to: 'activities#index'
  post 'activities', to: 'activities#batch_update'

  get 'imports/strava_push', to: 'imports#validate_strava_push'
  post 'imports/strava_push', to: 'imports#strava_push'

  get 'terms', to: 'static#terms'
  get 'privacy', to: 'static#privacy'
  get 'contact', to: 'static#contact'
  get 'mobile-apps', to: 'static#mobile_apps'
end
