class Users::OmniauthCallbacksController < Devise::OmniauthCallbacksController
  def strava
    # You need to implement the method below in your model (e.g. app/models/user.rb)
    @user = User.from_omniauth(request.env["omniauth.auth"])

    if @user.persisted?
      sign_in_and_redirect @user, event: :authentication
      access_token = request.env.dig("omniauth.auth", "credentials", "token")
      session["devise.strava_access_token"] = access_token
      set_flash_message(:notice, :success, kind: "Strava") if is_navigational_format?
    else
      # TODO: error message
      session["devise.strava_data"] = request.env.dig("omniauth.auth", "info")
      redirect_to new_user_registration_url
    end
  end

  def failure
    redirect_to root_path
  end
end