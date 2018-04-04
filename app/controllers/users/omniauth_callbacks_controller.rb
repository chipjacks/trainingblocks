class Users::OmniauthCallbacksController < Devise::OmniauthCallbacksController
  def strava
    @user = User.from_omniauth(request.env["omniauth.auth"])

    if @user.persisted?
      sign_in_and_redirect @user, event: :authentication
      access_token = request.env.dig("omniauth.auth", "credentials", "token")
      session["devise.strava_access_token"] = access_token
      set_flash_message(:notice, :success, kind: "Strava") if is_navigational_format?
    else
      set_flash_message(:alert, :failure, kind: "Strava", reason: "log in failed")
      redirect_to root_path
    end
  end

  def failure
    redirect_to root_path
  end
end