Rails
  .application
  .routes
  .draw do
    # For details on the DSL available within this file, see http://guides.rubyonrails.org/routing.html

    devise_for :users,
               controllers: {
                 omniauth_callbacks: 'users/omniauth_callbacks',
                 confirmations: 'users/confirmations',
                 sessions: 'users/sessions',
               }

    root to: 'home#index'

    get 'calendar', to: 'calendar#index'
    get 'trends', to: 'trends#index'

    resource :settings, only: %i[show update]

    get 'activities', to: 'activities#index'
    post 'activities', to: 'activities#batch_update'

    get 'imports/strava_push', to: 'imports#validate_strava_push'
    post 'imports/strava_push', to: 'imports#strava_push'
  end
