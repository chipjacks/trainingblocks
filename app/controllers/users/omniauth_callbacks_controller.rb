class Users::OmniauthCallbacksController < Devise::OmniauthCallbacksController
  include Devise::Controllers::Rememberable

  def strava
    @user = User.from_omniauth(request.env['omniauth.auth'])
    credentials = request.env.dig('omniauth.auth', 'credentials')

    if @user.persisted? && credentials
      if @user.previously_new_record? || !@user.auth_token
        InitialStravaImportJob.perform_now(@user, credentials['token'])
        store_location_for(@user, settings_path)
      else
        store_location_for(@user, calendar_path)
      end
      @user.auth_token = credentials['refresh_token']
      remember_me(@user)
      @user.save!
      if is_navigational_format?
        set_flash_message(:notice, :success, kind: 'Strava')
      end
      sign_in_and_redirect @user, event: :authentication
    elsif !@user.email
      flash[:notice] = 'Please enter an email for your account.'
      redirect_to new_user_registration_path
    else
      set_flash_message(
        :alert,
        :failure,
        kind: 'Strava',
        reason: "#{@user.errors.full_messages.join(', ')}",
      )
      redirect_to root_path
    end
  end

  def failure
    message = 'Internal error'
    begin
      message =
        JSON.parse(request.env['omniauth.error'].message.delete_prefix(': '))[
          'message'
        ]
    rescue StandardError
    end
    set_flash_message(:alert, :failure, kind: 'Strava', reason: message)
    redirect_to root_path
  end
end
